//! Constant folding. DreamMaker evaluates constant expressions at compile
//! time: `1 + 2` is stored as 3, `-1` as a single value, `"a" + "b"` as "ab",
//! and a constant `if` condition removes the test.

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
}

impl Constant {
    pub fn truthy(&self) -> bool {
        match self {
            Constant::Null => false,
            Constant::Number(n) => *n != 0.0,
            Constant::Text(t) => !t.is_empty(),
            Constant::Path(_) => true,
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
                Term::Expr(inner) if constant(c, inner).is_none() => return None,
                _ => {}
            }
        }
    }
    value(c, expr)
}

/// The value of a constant expression.
pub(super) fn value(c: &Compiler, expr: &Expression) -> Option<Constant> {
    match expr {
        Expression::Base { term, follow } => {
            // x::a::CONST
            let statics = follow
                .iter()
                .take_while(|f| matches!(f.elem, Follow::StaticField(_)))
                .count();
            // x.a.CONST
            let fields = follow
                .iter()
                .take_while(|f| matches!(f.elem, Follow::Field(..)))
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
            // Unary operators are applied innermost first
            for f in follow.iter().rev() {
                v = match (&f.elem, v) {
                    (Follow::Unary(UnaryOp::Neg), Constant::Number(n)) => Constant::Number(-n),
                    (Follow::Unary(UnaryOp::Not), c) => Constant::Number(!c.truthy() as i32 as f32),
                    (Follow::Unary(UnaryOp::BitNot), Constant::Number(n)) => {
                        Constant::Number(not_bits(n) as f32)
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
    let holder = Expression::Base {
        term: Box::new(term.clone()),
        follow: fields[..fields.len() - 1].to_vec().into_boxed_slice(),
    };
    let ty = super::procs::expr_type(c, &holder)?;
    let Follow::Field(_, name) = &fields[fields.len() - 1].elem else {
        return None;
    };
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

/// A one-argument math builtin of a constant, as DreamMaker folds it: `None`
/// if `name` isn't one, `Some(None)` if the value is out of its domain.
fn math1(name: &str, x: f32) -> Option<Option<f32>> {
    let d = x as f64;
    let r = match name {
        "sqrt" => (d >= 0.0).then(|| d.sqrt()),
        "ceil" => Some(d.ceil()),
        "floor" | "round" => Some(d.floor()),
        "trunc" => Some(d.trunc()),
        "fract" => Some(d.fract()),
        "abs" => Some(d.abs()),
        "sign" => Some(if d > 0.0 {
            1.0
        } else if d < 0.0 {
            -1.0
        } else {
            0.0
        }),
        "sin" => Some(d.to_radians().sin()),
        "cos" => Some(d.to_radians().cos()),
        "arctan" => Some(d.atan().to_degrees()),
        _ => return None,
    };
    Some(r.map(|r| r as f32))
}

fn from_objtree(constant: &dreammaker::constants::Constant) -> Option<Constant> {
    use dreammaker::constants::Constant as C;
    Some(match constant {
        C::Null(_) => Constant::Null,
        C::Float(n) => Constant::Number(*n),
        C::String(s) => Constant::Text(s.as_str().as_bytes().to_vec()),
        _ => return None,
    })
}

fn base_value(c: &Compiler, term: &Term) -> Option<Constant> {
    Some(match term {
        Term::Int(i) => Constant::Number(*i as f32),
        Term::Float(f) => Constant::Number(*f),
        Term::Null => Constant::Null,
        Term::Prefab(prefab) if prefab.vars.is_empty() => Constant::Path(prefab.path.to_string()),
        Term::String(s) => Constant::Text(super::strings::parse(s).ok()?.0),
        Term::Expr(inner) => value(c, inner)?,
        // Math builtins of constants (not min and max, which DreamMaker leaves)
        Term::Call(name, args) if args.len() == 1 && math1(name.as_str(), 0.0).is_some() => {
            match value(c, &args[0])? {
                Constant::Number(n) => Constant::Number(math1(name.as_str(), n)??),
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
        // hsl2rgb(h, s, l), the builtin
        Term::Call(name, args)
            if name.as_str() == "hsl2rgb"
                && args.len() == 3
                && !c.proc.as_ref().is_some_and(|p| p.tree.root().get_proc("hsl2rgb").is_some()) =>
        {
            let n = |e| match value(c, e)? {
                Constant::Number(n) => Some(n),
                _ => None,
            };
            Constant::Text(hsl_text(n(&args[0])?, n(&args[1])?, n(&args[2])?).into_bytes())
        }
        // text2ascii("a") is 97
        Term::Call(name, args) if name.as_str() == "text2ascii" && args.len() == 1 => {
            match value(c, &args[0])? {
                Constant::Text(t) => {
                    let c = String::from_utf8_lossy(&t).chars().next()?;
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
                Constant::Number(x.ln() / base.ln())
            }
        }
        Term::Call(name, args) if name.as_str() == "round" && args.len() == 2 => {
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
            Constant::Number(x.max(a.min(b)).min(a.max(b)))
        }
        Term::Ident(name) => from_objtree(&super::procs::const_value(c, name.as_str())?)?,
        _ => return None,
    })
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
                    numbers(*op, a, b).map(Constant::Number)
                }
                (Constant::Text(mut a), Constant::Text(b)) if *op == BinaryOp::Add => {
                    a.extend(b);
                    Some(Constant::Text(a))
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

fn numbers(op: BinaryOp, a: f32, b: f32) -> Option<f32> {
    let shift = |x: f32| if x < 0.0 { 0 } else { x as i32 as u32 };
    Some(match op {
        BinaryOp::Add => a + b,
        BinaryOp::Sub => a - b,
        BinaryOp::Mul => a * b,
        BinaryOp::Div if b != 0.0 => a / b,
        BinaryOp::Pow => a.powf(b),
        BinaryOp::Mod if int(b) != 0 => int(a).wrapping_rem(int(b)) as f32,
        BinaryOp::BitAnd => (bits(a) & bits(b)) as f32,
        BinaryOp::BitOr => (bits(a) | bits(b)) as f32,
        BinaryOp::BitXor => (bits(a) ^ bits(b)) as f32,
        // A negative number shifted left is 0, as with ~; shifted right it's
        // its 24 bits
        BinaryOp::LShift => {
            (bits(a.max(0.0))
                .checked_shl(shift(b))
                .unwrap_or(0)
                & 0xFFFFFF) as f32
        }
        BinaryOp::RShift => bits(a).checked_shr(shift(b)).unwrap_or(0) as f32,
        _ => return None,
    })
}

/// The "#rrggbb" of a color given as hue (degrees), saturation and luminance
/// (percent), as DreamMaker works out `hsl2rgb()` or `rgb(h, s, l, space = 2)`
/// (channels rounded).
pub(super) fn hsl_text(h: f32, s: f32, l: f32) -> String {
    let (h, s, l) = (h as f64, s as f64 / 100.0, l as f64 / 100.0);
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
    let byte = |v: f64| ((v + m) * 255.0).round().clamp(0.0, 255.0) as u8;
    format!("#{:02x}{:02x}{:02x}", byte(r), byte(g), byte(b))
}
