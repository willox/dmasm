//! Fold constant expressions using DreamMaker's arithmetic and conversion rules.

use dreammaker::ast::{BinaryOp, Expression, Follow, Term, UnaryOp};

use super::Compiler;

/// A folded constant.
#[derive(Clone, Debug, PartialEq)]
pub(super) enum Constant {
    Null,
    Number(f32),
    Text(Vec<u8>),
    /// A type path, which only survives `&&`, `||`, `!` and `? :`.
    Path(String),
    /// Recognize constant constructors known to be truthy. Conditions can omit
    /// allocation, but value contexts such as `new /T || a` must retain it.
    New,
}

impl Constant {
    pub fn truthy(&self) -> bool {
        match self {
            Constant::Null => false,
            Constant::Number(n) => *n != 0.0,
            Constant::Text(t) => !t.is_empty(),
            Constant::Path(_) | Constant::New => true,
        }
    }
}

/// A number as BYOND makes it an integer: truncated, and out of range (or NaN)
/// it's i32::MIN, as x86's conversion gives (Rust's `as` saturates instead).
pub(super) fn int(x: f32) -> i32 {
    if x.is_nan() || !(-2147483648.0..2147483648.0).contains(&x) {
        i32::MIN
    } else {
        x as i32
    }
}

/// Bit operations work on 24 bits: -1 is 0xFFFFFF. Out of the i32 range a
/// number is the nearest i32 (2**33 is 0x7FFFFFFF, so 0xFFFFFF).
pub(super) fn bits(x: f32) -> u32 {
    ((x as i32) as u32) & 0xFFFFFF
}

/// `~x` takes x as a u32 instead: negative numbers are 0, those over 2**32
/// 0xFFFFFFFF, and the rest wrap (2**32 is 0).
fn not_bits(x: f32) -> u32 {
    let n = if x.is_nan() || x < 0.0 {
        0
    } else if x > 4294967295.0 {
        u32::MAX
    } else {
        (x as u64) as u32
    };
    !n & 0xFFFFFF
}

/// The value of a constant numeric expression, if it is one.
pub(super) fn number(c: &Compiler, expr: &Expression) -> Option<f32> {
    match value(c, expr)? {
        Constant::Number(n) => Some(n),
        _ => None,
    }
}

/// The value of a constant expression, where folding it changes the code (a
/// lone string or null literal is left alone).
pub(super) fn constant(c: &Compiler, expr: &Expression) -> Option<Constant> {
    // `&&`, `||` and `? :` are the operand they choose, which may be a const
    // read alone (`7 && CONST` reads CONST's global)
    match expr {
        Expression::BinaryOp {
            op: op @ (BinaryOp::And | BinaryOp::Or),
            lhs,
            rhs,
        } => {
            let decides = value(c, lhs)?.truthy() == (*op == BinaryOp::Or);
            return constant(c, if decides { lhs } else { rhs });
        }
        Expression::TernaryOp { cond, if_, else_ } => {
            let chosen = if value(c, cond)?.truthy() { if_ } else { else_ };
            return constant(c, chosen);
        }
        _ => {}
    }
    if let Expression::Base { term, follow } = expr {
        if follow.is_empty() && matches!(term.elem, Term::String(_) | Term::Null | Term::Prefab(_))
        {
            return None;
        }
        // A const alone, even in parentheses, is read from its global
        if !follow.is_empty()
            && (follow
                .iter()
                .all(|f| matches!(f.elem, Follow::StaticField(_)))
                || follow.iter().all(|f| matches!(f.elem, Follow::Field(..))))
        {
            return None;
        }
        if follow.is_empty() {
            match &term.elem {
                Term::Ident(name) if !super::procs::is_builtin_const(c, name.as_str()) => {
                    return None
                }
                Term::GlobalIdent(_) => return None,
                Term::Expr(inner) if constant(c, inner).is_none() => return None,
                _ => {}
            }
        }
    }
    value(c, expr).filter(|v| *v != Constant::New)
}

/// Constant value used to select the live operands of `&&`, `||` and `? :`,
/// even during resource discovery with other folding disabled (`live_only`).
pub(super) fn choice(c: &mut Compiler, expr: &Expression) -> Option<Constant> {
    let no_fold = c.no_fold;
    c.no_fold = no_fold && !c.live_only;
    let found = value(c, expr);
    c.no_fold = no_fold;
    found
}

/// The value of a constant expression.
pub(super) fn value(c: &Compiler, expr: &Expression) -> Option<Constant> {
    if c.no_fold {
        return None;
    }
    match expr {
        Expression::Base { term, follow } => {
            // x::a::CONST
            let statics = follow
                .iter()
                .take_while(|f| matches!(f.elem, Follow::StaticField(_)))
                .count();
            // x.a.CONST (`x?.CONST` is null when x is)
            let fields = follow
                .iter()
                .take_while(|f| {
                    matches!(
                        f.elem,
                        Follow::Field(
                            dreammaker::ast::PropertyAccessKind::Dot
                                | dreammaker::ast::PropertyAccessKind::Colon,
                            _
                        )
                    )
                })
                .count();
            let (mut v, follow) = if statics > 0 {
                (
                    static_const(c, &term.elem, &follow[..statics])?,
                    &follow[statics..],
                )
            } else if fields > 0 {
                (field_const(c, term, &follow[..fields])?, &follow[fields..])
            } else {
                (base_value(c, &term.elem)?, &follow[..])
            };
            // Unary operators come innermost first (`-~1` is -(~1))
            for f in follow.iter() {
                v = match (&f.elem, v) {
                    (Follow::Unary(UnaryOp::Neg), Constant::Number(n)) => Constant::Number(-n),
                    (Follow::Unary(UnaryOp::Not), c) => Constant::Number(!c.truthy() as i32 as f32),
                    (Follow::Unary(UnaryOp::BitNot), Constant::Number(n)) => {
                        Constant::Number(not_bits(n) as f32)
                    }
                    // text and null are 0 to it
                    (Follow::Unary(UnaryOp::BitNot), Constant::Text(_) | Constant::Null) => {
                        Constant::Number(not_bits(0.0) as f32)
                    }
                    _ => return None,
                };
            }
            Some(v)
        }
        _ => binary_value(c, expr),
    }
}

/// The value of `x.a.name` when name is a const var.
fn field_const(
    c: &Compiler,
    term: &dreammaker::ast::Spanned<Term>,
    fields: &[dreammaker::ast::Spanned<Follow>],
) -> Option<Constant> {
    let Follow::Field(_, name) = &fields[fields.len() - 1].elem else {
        return None;
    };
    // `global.name` is the global const, whatever else has the name
    if fields.len() == 1 && matches!(&term.elem, Term::Ident(i) if i.as_str() == "global") {
        return from_objtree(&super::procs::global_const_value(c, name.as_str())?);
    }
    let holder = Expression::Base {
        term: Box::new(term.clone()),
        follow: fields[..fields.len() - 1].to_vec().into_boxed_slice(),
    };
    let ty = super::procs::expr_type(c, &holder)?;
    let decl = super::procs::member_var(ty, name.as_str())?;
    // (builtin vars like `type` are declared const but differ per object)
    if !decl.get().var_type.flags.is_const()
        || decl.get().location.file == dreammaker::FileId::BUILTINS
    {
        return None;
    }
    from_objtree(decl.ty().get_value(name.as_str())?.constant.as_ref()?)
}

/// The value of `x::a::name` when name is a const var.
fn static_const(
    c: &Compiler,
    term: &Term,
    steps: &[dreammaker::ast::Spanned<Follow>],
) -> Option<Constant> {
    let ctx = c.proc.as_ref()?;
    // (a debugger's code doesn't have the world's types to go by)
    if c.expr_scope.is_some() {
        return None;
    }
    let ty = match term {
        Term::Prefab(prefab) if prefab.vars.is_empty() => ctx.tree.find(&prefab.path.to_string()),
        other => super::procs::term_type(c, other),
    };
    let names: Vec<&str> = steps
        .iter()
        .map(|f| match &f.elem {
            Follow::StaticField(name) => name.as_str(),
            _ => unreachable!(),
        })
        .collect();
    let decl = super::procs::static_chain(c, ty, &names)?;
    let flags = decl.get().var_type.flags;
    // `/type::var` is the var's initial value, known when compiling; through a
    // variable it's only that for a const
    let from_type = matches!(term, Term::Prefab(_)) && names.len() == 1;
    if !(flags.is_const() || from_type && !flags.is_static()) {
        return None;
    }
    // Through a variable, a builtin const like `type` differs per object
    if !from_type && decl.get().location.file == dreammaker::FileId::BUILTINS {
        return None;
    }
    // (the named type's value, which may override the declaration's)
    let owner = if from_type { ty? } else { decl.ty() };
    from_objtree(owner.get_value(names[names.len() - 1])?.constant.as_ref()?)
}

/// Warn when Linux and Windows DreamMaker fold an angle differently.
/// DreamDaemon uses the Linux result at runtime on both platforms.
fn warn_angle(c: &Compiler, what: &str, folded: f32, radians: f64) {
    let windows = super::versions::degrees_on_windows(radians) as f32;
    if windows != folded && !folded.is_nan() {
        c.warn(&format!(
            "{what} is folded to {folded}, matching Linux DreamMaker and runtime evaluation: \
             Windows DreamMaker folds it to {windows}."
        ));
    }
}

/// A one-argument math builtin of a constant, as DreamMaker folds it: `None`
/// if `name` isn't one, `Some(None)` if the value is out of its domain.
fn math1(name: &str, x: f32, byond: crate::ByondVersion) -> Option<Option<f32>> {
    let d = x as f64;
    let degrees = |radians: f64| super::versions::degrees(byond, radians);
    let r = match name {
        // (a NaN isn't out of any of these domains: it folds to itself)
        "sqrt" => (!(d < 0.0)).then(|| d.sqrt()),
        "ceil" => Some(d.ceil()),
        "floor" | "round" => Some(d.floor()),
        "trunc" => Some(d.trunc()),
        // (of an infinity it's 0)
        "fract" => Some(if d.is_infinite() { 0.0 } else { d.fract() }),
        // Preserve a NaN's sign.
        "abs" => Some(if d < 0.0 { -d } else { d }),
        "sign" => Some(if d > 0.0 {
            1.0
        } else if d < 0.0 {
            -1.0
        } else {
            0.0
        }),
        "sin" | "cos" => Some(super::versions::sin_cos(byond, name == "sin", d)),
        // Infinity produces the negative x86 NaN. In 516, tan() removes the
        // angle's sign first, then restores it to the result, so tan(-1.#INF)
        // is positive NaN. A NaN input remains NaN.
        "tan" => Some(if x.is_finite() {
            super::versions::tan(byond, x) as f64
        } else if x.is_infinite() {
            let nan = super::versions::DEFAULT_NAN;
            if x < 0.0 && byond.at_least(super::versions::BYOND_516) {
                -nan
            } else {
                nan
            }
        } else {
            d
        }),
        // A NaN stays as it is, except that 515 arccos() clears its sign.
        // Not left to the host's functions: a 32-bit build's give another sign.
        "arccos" if d.is_nan() && !byond.at_least(super::versions::BYOND_516) => Some(d.abs()),
        "arcsin" | "arccos" | "arctan" if d.is_nan() => Some(d),
        "arcsin" => (!(d < -1.0 || d > 1.0)).then(|| degrees(d.asin())),
        "arccos" => (!(d < -1.0 || d > 1.0)).then(|| degrees(d.acos())),
        "arctan" => Some(degrees(d.atan())),
        _ => return None,
    };
    Some(r.map(|r| r as f32))
}

fn from_objtree(constant: &dreammaker::constants::Constant) -> Option<Constant> {
    use dreammaker::constants::Constant as C;
    Some(match constant {
        C::Null(_) => Constant::Null,
        C::Float(n) => Constant::Number(*n),
        // (the text as the source has it, escapes and all)
        C::String(s) => Constant::Text(super::strings::parse(s.as_str()).ok()?.0),
        _ => return None,
    })
}

fn base_value(c: &Compiler, term: &Term) -> Option<Constant> {
    Some(match term {
        Term::Int(i) => Constant::Number(*i as f32),
        Term::Float(f) => Constant::Number(*f),
        Term::Null => Constant::Null,
        Term::Prefab(prefab) if prefab.vars.is_empty() => Constant::Path(prefab.path.to_string()),
        Term::NewPrefab { prefab, args }
            if prefab.vars.is_empty() && args.as_ref().is_none_or(|a| constant_args(c, a)) =>
        {
            Constant::New
        }
        Term::List(args) | Term::Alist(args) if constant_args(c, args) => Constant::New,
        // (and the list rgb2num() makes of a constant colour)
        Term::Call(name, args)
            if name.as_str() == "rgb2num" && super::builtin_procs::rgb2num_is_constant(c, args) =>
        {
            Constant::New
        }
        // A proc's type (null in a global proc) and its own path
        Term::__TYPE__ if c.proc_macros_known() => match c.proc.as_ref()? {
            p if p.ty.is_root() => Constant::Null,
            p => Constant::Path(p.ty.get().path.clone()),
        },
        Term::__PROC__
            if c.proc_macros_known() && c.byond.at_least(super::versions::PROC_MACRO) =>
        {
            Constant::Path(c.proc.as_ref()?.proc_path.clone())
        }
        Term::Call(name, args) if name.as_str() == "newlist" && constant_args(c, args) => {
            Constant::New
        }
        Term::String(s) => Constant::Text(super::strings::parse(s).ok()?.0),
        Term::Expr(inner) => value(c, inner)?,
        // Math builtins of constants (not min and max, which DreamMaker leaves)
        Term::Call(name, args)
            if args.len() == 1
                && math1(name.as_str(), 0.0, c.byond).is_some()
                && super::versions::folds(c.byond, name.as_str()) =>
        {
            match value(c, &args[0])? {
                Constant::Number(n) => {
                    let folded = math1(name.as_str(), n, c.byond)??;
                    let radians = match name.as_str() {
                        "arcsin" => Some((n as f64).asin()),
                        "arccos" => Some((n as f64).acos()),
                        "arctan" => Some((n as f64).atan()),
                        _ => None,
                    };
                    if let Some(radians) = radians {
                        warn_angle(c, &format!("{name}({n})"), folded, radians);
                    }
                    Constant::Number(folded)
                }
                _ => return None,
            }
        }
        // md5() and sha1() of text (not of a number, null or a path)
        Term::Call(name, args) if args.len() == 1 && matches!(name.as_str(), "md5" | "sha1") => {
            let Constant::Text(text) = value(c, &args[0])? else {
                return None;
            };
            Constant::Text(hash(name.as_str(), &text).into_bytes())
        }
        // arctan(x, y): the angle of (x, y); of (0, 0) it's 0
        Term::Call(name, args) if name.as_str() == "arctan" && args.len() == 2 => {
            let (Constant::Number(x), Constant::Number(y)) =
                (value(c, &args[0])?, value(c, &args[1])?)
            else {
                return None;
            };
            if x == 0.0 && y == 0.0 {
                return Some(Constant::Number(0.0));
            }
            // (a y of -0 is 0 here: arctan(-1, -0) is 180, not -180)
            let radians = (y as f64 + 0.0).atan2(x as f64);
            let folded = super::versions::degrees(c.byond, radians) as f32;
            warn_angle(c, &format!("arctan({x}, {y})"), folded, radians);
            Constant::Number(folded)
        }
        // isnan(1) and isinf(1) (see versions::clean_nan_fold)
        Term::Call(name, args)
            if args.len() == 1
                && super::versions::nan_test(name.as_str(), 0.0).is_some()
                && super::versions::clean_nan_fold(c.byond) =>
        {
            match value(c, &args[0])? {
                Constant::Number(n) => {
                    Constant::Number(super::versions::nan_test(name.as_str(), n)? as i32 as f32)
                }
                _ => return None,
            }
        }
        // log(x) and log(base, x), worked out in single precision as BYOND does
        // nameof(x) is "x" (but not nameof(__PROC__), which depends on the proc)
        Term::Call(name, args) if name.as_str() == "nameof" && args.len() == 1 => {
            let name = super::builtin_procs::last_identifier(&args[0])?;
            if name == "__PROC__" {
                return None;
            }
            Constant::Text(name.into_bytes())
        }
        // rgb() of constants is its "#rrggbb" text, in any colour space
        Term::Call(name, args) if name.as_str() == "rgb" => {
            Constant::Text(super::builtin_procs::rgb_constant(c, args)?.into_bytes())
        }
        // hsl2rgb(h, s, l), the builtin
        Term::Call(name, args)
            if name.as_str() == "hsl2rgb"
                && args.len() == 3
                && !c
                    .proc
                    .as_ref()
                    .is_some_and(|p| p.tree.root().get_proc("hsl2rgb").is_some()) =>
        {
            let n = |e| match value(c, e)? {
                Constant::Number(n) => Some(n),
                _ => None,
            };
            Constant::Text(hsl_text(n(&args[0])?, n(&args[1])?, n(&args[2])?, 0.0).into_bytes())
        }
        // text2ascii("a") is 97, and of "" 0 (text2ascii_char the same: both
        // give the first character whole)
        Term::Call(name, args)
            if matches!(name.as_str(), "text2ascii" | "text2ascii_char") && args.len() == 1 =>
        {
            match value(c, &args[0])? {
                Constant::Text(t) => {
                    let c = String::from_utf8_lossy(&t).chars().next().unwrap_or('\0');
                    Constant::Number(c as u32 as f32)
                }
                _ => return None,
            }
        }
        Term::Call(name, args) if name.as_str() == "log" && matches!(args.len(), 1 | 2) => {
            let n = |e| match value(c, e)? {
                Constant::Number(n) => Some(n),
                _ => None,
            };
            let x = n(&args[args.len() - 1])?;
            if x <= 0.0 {
                return None;
            }
            if args.len() == 1 {
                Constant::Number(x.ln())
            } else {
                let base = n(&args[0])?;
                if base <= 0.0 || base == 1.0 {
                    return None;
                }
                Constant::Number(super::versions::log(c.byond, x, base))
            }
        }
        Term::Call(name, args)
            if name.as_str() == "round"
                && args.len() == 2
                && super::versions::folds(c.byond, "round") =>
        {
            let (Constant::Number(x), Constant::Number(to)) =
                (value(c, &args[0])?, value(c, &args[1])?)
            else {
                return None;
            };
            if to == 0.0 {
                return None;
            }
            Constant::Number(((x as f64 / to as f64 + 0.5).floor() * to as f64) as f32)
        }
        Term::Call(name, args) if name.as_str() == "clamp" && args.len() == 3 => {
            let n = |e| match value(c, e)? {
                Constant::Number(n) => Some(n),
                _ => None,
            };
            // (bounds the wrong way round are swapped)
            let (x, a, b) = (n(&args[0])?, n(&args[1])?, n(&args[2])?);
            let (low, high) = (a.min(b), a.max(b));
            // NaN compares neither below nor above the bounds and remains unchanged.
            Constant::Number(if x < low {
                low
            } else if x > high {
                high
            } else {
                x
            })
        }
        Term::Ident(name) => from_objtree(&super::procs::const_value(c, name.as_str())?)?,
        // (`::name` of a const, as the name alone)
        Term::GlobalIdent(name) => {
            from_objtree(&super::procs::global_const_value(c, name.as_str())?)?
        }
        _ => return None,
    })
}

/// Whether `expr` has a `&&` or `||` in it.
pub(super) fn has_logic(expr: &Expression) -> bool {
    match expr {
        Expression::BinaryOp {
            op: BinaryOp::And | BinaryOp::Or,
            ..
        } => true,
        Expression::BinaryOp { lhs, rhs, .. } => has_logic(lhs) || has_logic(rhs),
        Expression::TernaryOp { cond, if_, else_ } => {
            has_logic(cond) || has_logic(if_) || has_logic(else_)
        }
        Expression::AssignOp { .. } => false,
        Expression::Base { term, .. } => match &term.elem {
            Term::Expr(inner) => has_logic(inner),
            Term::Call(_, args) => args.iter().any(has_logic),
            _ => false,
        },
    }
}

/// Detect logical chains, including parenthesized ones. A chain folded to
/// a comparison still fetches and retests its value, unlike a standalone
/// comparison condition that leaves its result in the flag register.
pub(super) fn is_logic(expr: &Expression) -> bool {
    match expr {
        Expression::BinaryOp {
            op: BinaryOp::And | BinaryOp::Or,
            ..
        } => true,
        Expression::Base { term, follow } if follow.is_empty() => match &term.elem {
            Term::Expr(inner) => is_logic(inner),
            _ => false,
        },
        _ => false,
    }
}

/// Whether every argument is a constant (`k = v` too, a bare name for k).
fn constant_args(c: &Compiler, args: &[Expression]) -> bool {
    args.iter().all(|arg| match arg {
        Expression::AssignOp {
            op: dreammaker::ast::AssignOp::Assign,
            lhs,
            rhs,
        } => {
            let key = matches!(&**lhs, Expression::Base { term, follow }
                if follow.is_empty() && matches!(term.elem, Term::Ident(_)));
            (key || value(c, lhs).is_some()) && value(c, rhs).is_some()
        }
        arg => value(c, arg).is_some(),
    })
}

/// The hex digest of `text` by `md5` or `sha1`.
fn hash(name: &str, text: &[u8]) -> String {
    use sha1::Digest;
    let digest: Vec<u8> = if name == "md5" {
        md5::Md5::digest(text).to_vec()
    } else {
        sha1::Sha1::digest(text).to_vec()
    };
    digest.iter().map(|b| format!("{b:02x}")).collect()
}

fn binary_value(c: &Compiler, expr: &Expression) -> Option<Constant> {
    match expr {
        Expression::BinaryOp {
            op: BinaryOp::And,
            lhs,
            rhs,
        } => {
            let a = value(c, lhs)?;
            if !a.truthy() {
                return Some(a);
            }
            value(c, rhs)
        }
        Expression::BinaryOp {
            op: BinaryOp::Or,
            lhs,
            rhs,
        } => {
            let a = value(c, lhs)?;
            if a.truthy() {
                return Some(a);
            }
            value(c, rhs)
        }
        Expression::BinaryOp { op, lhs, rhs } => {
            let a = value(c, lhs)?;
            let b = value(c, rhs)?;
            match (a, b) {
                (Constant::Number(a), Constant::Number(b)) => {
                    // Let BYOND convert wide bit operands when evaluating debugger code.
                    if is_bit_op(*op) && c.expr_scope.is_some() && (wide(a) || wide(b)) {
                        return None;
                    }
                    if is_bit_op(*op) {
                        warn_bits(c, a);
                        warn_bits(c, b);
                    }
                    numbers(*op, a, b).map(Constant::Number)
                }
                (Constant::Text(mut a), Constant::Text(b)) if *op == BinaryOp::Add => {
                    a.extend(b);
                    Some(Constant::Text(a))
                }
                // Text or null shifted, or shifted by, is 0
                (a, b)
                    if matches!(op, BinaryOp::LShift | BinaryOp::RShift)
                        && [&a, &b].iter().all(|v| {
                            matches!(v, Constant::Number(_) | Constant::Text(_) | Constant::Null)
                        }) =>
                {
                    let n = |v: &Constant| match v {
                        Constant::Number(n) => *n,
                        _ => 0.0,
                    };
                    numbers(*op, n(&a), n(&b)).map(Constant::Number)
                }
                // A number divided by text is the number (and by null, where
                // DreamMaker takes that: 516 calls it a division by zero)
                (Constant::Number(a), Constant::Text(_) | Constant::Null)
                    if *op == BinaryOp::Div =>
                {
                    Some(Constant::Number(a))
                }
                _ => None,
            }
        }
        Expression::TernaryOp { cond, if_, else_ } => {
            if value(c, cond)?.truthy() {
                value(c, if_)
            } else {
                value(c, else_)
            }
        }
        _ => None,
    }
}

fn is_bit_op(op: BinaryOp) -> bool {
    matches!(
        op,
        BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor | BinaryOp::LShift | BinaryOp::RShift
    )
}

/// Whether a bit operand is beyond the 32-bit range.
pub(super) fn wide(x: f32) -> bool {
    !(-2147483648.0..2147483648.0).contains(&x)
}

/// A constant beyond the 32-bit range next to a bit operator: DreamMaker
/// makes erratic bits of it, by build (an expected difference)
pub(super) fn warn_bits(c: &Compiler, x: f32) {
    if wide(x) {
        c.warn(&format!(
            "bit operand {x} is outside the 32-bit range; its conversion differs from DreamMaker"
        ));
    }
}

/// Convert a modulo operand to i32 with DreamMaker's signed saturation.
/// Exactly 2**31 remains i32::MIN, matching the `int` conversion.
fn mod_int(x: f32) -> i32 {
    if x > 2147483648.0 {
        i32::MAX
    } else if x < -2147483648.0 {
        -i32::MAX
    } else {
        int(x)
    }
}

fn numbers(op: BinaryOp, a: f32, b: f32) -> Option<f32> {
    // (a shift takes the low five bits of its count, as x86 does: `1 << 33`
    // is 2; the count is 65535 at most, and a negative one is none)
    let shift = |x: f32| {
        if x < 0.0 {
            0
        } else {
            x.min(65535.0) as u32 & 31
        }
    };
    Some(match op {
        BinaryOp::Add => a + b,
        BinaryOp::Sub => a - b,
        BinaryOp::Mul => a * b,
        BinaryOp::Div if b != 0.0 => a / b,
        // (0 to a negative power is 0 to DreamMaker, but for -1: 1/0)
        BinaryOp::Pow if a == 0.0 && (b < 0.0 || b.is_nan()) => {
            if b == -1.0 {
                f32::INFINITY
            } else {
                0.0
            }
        }
        BinaryOp::Pow => a.powf(b),
        BinaryOp::Mod if int(b) != 0 => mod_int(a).wrapping_rem(mod_int(b)) as f32,
        // fmod, worked out in double precision
        BinaryOp::FloatMod if b != 0.0 => (a as f64 % b as f64) as f32,
        BinaryOp::BitAnd => (bits(a) & bits(b)) as f32,
        BinaryOp::BitOr => (bits(a) | bits(b)) as f32,
        BinaryOp::BitXor => (bits(a) ^ bits(b)) as f32,
        // A negative number shifted left is 0, as with ~; shifted right it's
        // its 24 bits
        BinaryOp::LShift => ((bits(a.max(0.0)) << shift(b)) & 0xFFFFFF) as f32,
        BinaryOp::RShift => (bits(a) >> shift(b)) as f32,
        _ => return None,
    })
}

/// Format a rounded RGB channel as two hexadecimal digits.
fn hex_byte(v: f64) -> String {
    format!("{:02x}", v.clamp(0.0, 255.0) as u8)
}

/// A percentage as the share of 1 DreamMaker works with: in single precision
/// (the rest of a colour is worked out in double).
fn share(percent: f64) -> f64 {
    (percent / 100.0) as f32 as f64
}

/// A channel from its share of 1: scaled to 255, put in single precision
/// and then rounded half up (so 178.49999 is 179: it's 178.5 as a single).
fn channel(share: f64, nudge: f64) -> String {
    hex_byte(((share * 255.0) as f32 as f64 + 0.5 + nudge).floor())
}

/// Fold HSV to RGB. Normalize hue to [0, 360); clamp saturation/value
/// to [0, 100] from 516, then round channels.
pub(super) fn hsv_text(byond: crate::ByondVersion, h: f32, s: f32, v: f32, nudge: f64) -> String {
    // (515 works with a saturation or a value outside [0, 100] as it is)
    let range = if byond.at_least(super::versions::BYOND_516) {
        (0.0, 100.0)
    } else {
        (f64::MIN, f64::MAX)
    };
    let (h, s, v) = (
        h as f64,
        share((s as f64).clamp(range.0, range.1)),
        share((v as f64).clamp(range.0, range.1)),
    );
    let chroma = v * s;
    let h6 = h.rem_euclid(360.0) / 60.0;
    let x = chroma * (1.0 - (h6 % 2.0 - 1.0).abs());
    let (r, g, b) = match h6 as u32 {
        0 => (chroma, x, 0.0),
        1 => (x, chroma, 0.0),
        2 => (0.0, chroma, x),
        3 => (0.0, x, chroma),
        4 => (x, 0.0, chroma),
        _ => (chroma, 0.0, x),
    };
    let m = v - chroma;
    let byte = |c: f64| channel(c + m, nudge);
    format!("#{}{}{}", byte(r), byte(g), byte(b))
}

/// Fold HCY to RGB. Scale chroma to the hue's maximum at this luminance.
/// Match BYOND's weights: red 0.299, green 0.557, blue 0.114.
/// Do not clamp chroma or luminance.
pub(super) fn hcy_text(h: f32, c: f32, y: f32, nudge: f64) -> String {
    // (neither is kept to [0, 100]: only the channels they give are)
    let (h, c, y) = (h as f64, share(c as f64), share(y as f64));
    let h6 = h.rem_euclid(360.0) / 60.0;
    let k = 1.0 - (h6 % 2.0 - 1.0).abs();
    let (r, g, b) = match h6 as u32 {
        0 => (1.0, k, 0.0),
        1 => (k, 1.0, 0.0),
        2 => (0.0, 1.0, k),
        3 => (0.0, k, 1.0),
        4 => (k, 0.0, 1.0),
        _ => (1.0, 0.0, k),
    };
    let weight = 0.299 * r + 0.557 * g + 0.114 * b;
    let chroma = if y < weight {
        c * y / weight
    } else if weight < 1.0 {
        c * (1.0 - y) / (1.0 - weight)
    } else {
        0.0
    };
    let m = y - weight * chroma;
    let byte = |v: f64| channel(v * chroma + m, nudge);
    format!("#{}{}{}", byte(r), byte(g), byte(b))
}

/// Fold rgb channels in the selected color space (0 RGB, 1 HSV, 2 HSL,
/// 3 HCY), with optional alpha. Truncate RGB/alpha; round other channels.
///
/// Match Linux from 515.1647. Near half-integer boundaries, return a warning
/// flag because Windows may round differently. Before 515.1647, leave those
/// cases for runtime because the intermediate precision is unknown.
pub(super) fn rgb_text(
    byond: crate::ByondVersion,
    space: u32,
    channels: [f32; 3],
    alpha: Option<f32>,
) -> Option<(String, bool)> {
    let [a, b, c] = channels;
    let rounded = |nudge: f64| match space {
        1 => hsv_text(byond, a, b, c, nudge),
        2 => hsl_text(a, b, c, nudge),
        _ => hcy_text(a, b, c, nudge),
    };
    let mut boundary = false;
    let mut text = match space {
        0 => format!(
            "#{}{}{}",
            hex_byte(a as f64),
            hex_byte(b as f64),
            hex_byte(c as f64)
        ),
        1..=3 => {
            boundary = rounded(1e-3) != rounded(-1e-3);
            if boundary && byond.build < super::versions::LINUX_COLOURS {
                return None;
            }
            rounded(0.0)
        }
        _ => return None,
    };
    if let Some(alpha) = alpha {
        text += &hex_byte(alpha as f64);
    }
    Some((text, boundary))
}

pub(super) fn hsl_text(h: f32, s: f32, l: f32, nudge: f64) -> String {
    let (h, s, l) = (
        h as f64,
        share((s as f64).clamp(0.0, 100.0)),
        share((l as f64).clamp(0.0, 100.0)),
    );
    let chroma = (1.0 - (2.0 * l - 1.0).abs()) * s;
    let h6 = h.rem_euclid(360.0) / 60.0;
    let x = chroma * (1.0 - (h6 % 2.0 - 1.0).abs());
    let (r, g, b) = match h6 as u32 {
        0 => (chroma, x, 0.0),
        1 => (x, chroma, 0.0),
        2 => (0.0, chroma, x),
        3 => (0.0, x, chroma),
        4 => (x, 0.0, chroma),
        _ => (chroma, 0.0, x),
    };
    let m = l - chroma / 2.0;
    let byte = |v: f64| channel(v + m, nudge);
    format!("#{}{}{}", byte(r), byte(g), byte(b))
}
