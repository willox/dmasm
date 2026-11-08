use std::fmt;

use dreammaker::ast::Follow;
use dreammaker::{ast::Expression, Severity};

use crate::operands::{self, DMString, GlobalVar, Label, Variable};
use crate::Instruction;
use crate::Node;

mod assignment;
mod fold;
mod procs;
mod statements;

pub use procs::{compile_proc, CompiledProc, ProcSource};

mod args;
mod binary_ops;
mod builtin_procs;
mod chain_builder;
mod follow;
mod strings;
mod term;
mod ternary;
mod unary;

use chain_builder::ChainBuilder;

fn is_writable(var: &Variable) -> bool {
    match var {
        // Does Field count? We probably don't hit that code path but it might count
        Variable::Usr
        | Variable::Src
        | Variable::Args
        | Variable::Dot
        | Variable::CacheIndex
        // TODO: These can be constant too.
        | Variable::Arg { .. }
        | Variable::Local { .. }
        // TODO: Stuff like global.vars are constant and should return false here.
        | Variable::Global { .. } => true,
        _ => false,
    }
}

/// The name BYOND stores for a call it resolves by name at runtime: a proc or
/// verb's name with each `_` replaced by a space.
fn dynamic_name(ident: &str) -> DMString {
    DMString(ident.replace('_', " ").into_bytes())
}

#[derive(Debug)]
pub enum CompileError {
    ParseError(dreammaker::DMError),
    StringError(strings::StringError),

    ExpectedLValue,
    ExpectedFieldReference,

    // This is sort of snowflake
    ExpectedEnd,

    UnexpectedRange,
    UnexpectedGlobal,
    UnexpectedArgList,
    UnexpectedProbability,
    UnexpectedNamedArguments,

    UnsupportedPrefabWithVars,
    UnsupportedBuiltin {
        proc: String,
    },
    UnsupportedImplicitNew,
    UnsupportedRelativeCall,
    UnsupportedImplicitLocate,
    UnsupportedStringInterpolation,
    UnsupportedInput,
    UnsupportedCompilerMacro {
        name: String,
    },
    UnsupportedStatement(&'static str),
    /// Another error, at a source line.
    AtLine(u32, Box<CompileError>),

    AmbiguousListConstructor,
    InvalidLocateArgs,

    // TODO: Merge these
    IncorrectArgCount(String),
    MissingArgument {
        proc: String,
        index: u32,
    },
    TooManyArguments {
        proc: String,
        expected: u32,
    },
}

impl From<strings::StringError> for CompileError {
    fn from(err: strings::StringError) -> Self {
        Self::StringError(err)
    }
}

impl From<dreammaker::DMError> for CompileError {
    fn from(err: dreammaker::DMError) -> Self {
        Self::ParseError(err)
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompileError::ParseError(err) => {
                write!(f, "parser error: {}", err)
            }

            CompileError::StringError(err) => {
                write!(f, "string error: {}", err)
            }

            CompileError::ExpectedLValue => write!(f, "expected l-value"),
            CompileError::ExpectedFieldReference => write!(f, "expected field reference"),
            CompileError::ExpectedEnd => {
                write!(f, "expected end (received more code than expected)")
            }
            CompileError::UnexpectedRange => write!(f, "unexpected range"),
            CompileError::UnexpectedGlobal => write!(f, "unexpected global"),
            CompileError::UnexpectedArgList => write!(f, "unexpected arglist"),
            CompileError::UnexpectedProbability => write!(f, "unexpected prob()"),
            CompileError::UnexpectedNamedArguments => write!(f, "unexpected named arguments"),
            CompileError::UnsupportedPrefabWithVars => {
                write!(f, "prefabs with variable overrides are not supported")
            }
            CompileError::UnsupportedBuiltin { proc } => {
                write!(f, "unsupported built-in proc: {}", proc)
            }
            CompileError::UnsupportedImplicitNew => {
                write!(f, "implicit new() calls are not supported")
            }
            CompileError::UnsupportedRelativeCall => write!(f, "relative calls are not supported"),
            CompileError::UnsupportedImplicitLocate => {
                write!(f, "implicit locate() calls are not supported")
            }
            CompileError::UnsupportedStringInterpolation => {
                write!(f, "interpolated strings are not supported")
            }
            CompileError::UnsupportedInput => write!(f, "unsupported built-in proc: input"),
            CompileError::AmbiguousListConstructor => write!(
                f,
                "provided list constructor (or named parameters) are ambiguous"
            ),
            CompileError::InvalidLocateArgs => write!(f, "invalid arguments for locate()"),
            CompileError::IncorrectArgCount(proc) => {
                write!(f, "incorrect amount of arguments for: {}", proc)
            }
            CompileError::MissingArgument { proc, index: _ } => {
                write!(f, "missing argument(s) for: {}", proc)
            }
            CompileError::TooManyArguments { proc, expected } => write!(
                f,
                "too many argument(s) for: {} (expected {})",
                proc, expected
            ),
            CompileError::AtLine(line, err) => write!(f, "line {}: {}", line, err),
            CompileError::UnsupportedStatement(what) => {
                write!(f, "unsupported statement: {}", what)
            }
            CompileError::UnsupportedCompilerMacro { name } => {
                write!(f, "unsupported compiler macro: {}", name)
            }
        }
    }
}

pub fn compile_expr(code: &str, params: &[&str]) -> Result<Vec<Node>, CompileError> {
    let mut compiler = Compiler::new(params.iter().map(|x| x.to_string()).collect(), None);
    compiler.emit_ins(Instruction::DbgFile(DMString(
        b"<dmasm expression>".to_vec(),
    )));

    let ctx = dreammaker::Context::default();

    let mut lexer =
        dreammaker::lexer::Lexer::new(&ctx, dreammaker::FileId::BUILTINS, code.as_bytes());
    let mut indents = dreammaker::IndentProcessor::new(&ctx, &mut lexer);
    let expr = ctx.parse_expression(dreammaker::Location::BUILTINS, &mut indents)?;

    if !lexer.remaining().is_empty() {
        return Err(CompileError::ExpectedEnd);
    }

    for err in ctx.errors().iter() {
        if err.severity() >= Severity::Error {
            return Err(err.clone().into());
        }
    }

    let kind = compiler.emit_expr(expr)?;
    compiler.emit_move_to_stack(kind)?;

    let param_amt = params.len() as u32;
    for arg_id in 0..param_amt {
        compiler.emit_ins(Instruction::GetVar(Variable::Arg(arg_id)));
    }

    compiler.emit_ins(Instruction::NewList(param_amt + 1));
    compiler.emit_ins(Instruction::Ret);
    Ok(compiler.nodes)
}

#[derive(Debug, PartialEq)]
enum EvalKind {
    // The result of the expression will be on the top of the stack
    Stack,

    // The result of the expression is a list entry L[K] where the top of the stack is the index and the 2nd top of the stack is the list
    ListRef,

    // The result of the expression is 2 values on the Stack due to the `To` operator
    Range,

    // The result of the expression is the `global` pseudo-object
    Global,

    // The result of the expression is an arglist (which is on the top of the stack)
    ArgList,

    // The result of the expression can be accessed using a Variable operand
    Var(Variable),

    // Similar to Var, but more state
    Field(ChainBuilder, String),
    // TODO: Eval?
}

impl fmt::Display for EvalKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalKind::Stack => write!(f, "stack value"),
            EvalKind::ListRef => write!(f, "list access"),
            EvalKind::Range => write!(f, "range"),
            EvalKind::Global => write!(f, "global"),
            EvalKind::ArgList => write!(f, "arglist"),
            EvalKind::Var(_) => write!(f, "variable"),
            EvalKind::Field(_, _) => write!(f, "field access"),
        }
    }
}

#[derive(Clone)]
struct Compiler<'a> {
    params: Vec<String>,
    nodes: Vec<Node>,
    label_count: u32,
    short_circuit_labels: Vec<(String, bool)>,
    /// Set when compiling a whole proc: what it can see.
    proc: Option<procs::ProcCtx<'a>>,
    /// What the cache register holds, as the chain of bases that were loaded
    /// into it, or `None` when unknown. DreamMaker leaves out a `cache = x`
    /// when the cache already holds `x`.
    cache: Option<Vec<Variable>>,
    last_line: Option<u32>,
    /// The type a bare `new` creates: the declared type of what it's being
    /// assigned to.
    implicit_type: Option<String>,
    /// The next cache chain based on `src` was written as `src.x`, which
    /// DreamMaker always loads into the cache afresh.
    explicit_src: bool,
    /// Emit DbgFile and DbgLine.
    debug_info: bool,
    /// The cache state at the jumps seen so far to each label, merged.
    label_states: std::collections::HashMap<String, (u32, Option<Vec<Variable>>)>,
    /// Set while compiling an if or while condition that is a && or || chain.
    condition_chain: bool,
    /// Set while compiling the left side of a `*`.
    mul_lhs: bool,
    /// Set for an expression DreamMaker parses on its own (a return value, an
    /// argument, the inside of parentheses): only there does it regroup
    /// `a + (b + c) + d`. Taken by the next expression compiled.
    regroup_root: bool,
    /// Passes `regroup_root` on to the binary operation it applies to.
    regroup_chain: bool,
    /// Set when a field (`a.b`, `global.x`, a static) is read or a proc called
    /// through one: DreamMaker saves the cache around `||=`'s value for these
    /// even when the code doesn't use it
    global_field: bool,
    /// The element type of the `/list/T` var a follow starts from, for its
    /// first index. Taken by `follow::emit_typed`.
    list_elem_type: Option<dreammaker::objtree::TypeRef<'a>>,
}

impl<'a> Compiler<'a> {
    fn new(params: Vec<String>, proc: Option<procs::ProcCtx<'a>>) -> Self {
        Compiler {
            params,
            nodes: vec![],
            label_count: 0,
            short_circuit_labels: vec![],
            proc,
            cache: None,
            last_line: None,
            implicit_type: None,


            explicit_src: false,
            debug_info: true,
            label_states: Default::default(),
            condition_chain: false,
            mul_lhs: false,
            regroup_root: false,
            regroup_chain: false,
            global_field: false,
            list_elem_type: None,
        }
    }

    fn emit_ins(&mut self, mut ins: Instruction) {
        if !self.debug_info && matches!(ins, Instruction::DbgLine(_) | Instruction::DbgFile(_)) {
            return;
        }
        if let Instruction::DbgLine(line) = ins {
            // DreamMaker doesn't repeat the line it last recorded
            if self.last_line == Some(line) {
                return;
            }
            self.last_line = Some(line);
        }
        procs::track_cache(&mut self.cache, &mut ins, &mut self.explicit_src);
        // (see ProcCtx::clear_new_vars)
        if matches!(
            ins,
            Instruction::Switch(_)
                | Instruction::SwitchRange(_)
                | Instruction::PickSwitch(_)
                | Instruction::PickProb(_)
        ) {
            if let Some(ctx) = self.proc.as_mut() {
                ctx.clear_new_vars = true;
            }
        }
        // (a switch's cases to the same label are one way in)
        let mut targets = procs::label_targets(&ins);
        targets.sort();
        targets.dedup();
        for target in targets {
            // (after a spawned block DreamMaker doesn't know what the cache holds)
            let state = if matches!(ins, Instruction::Spawn(_)) {
                None
            } else {
                self.cache.clone()
            };
            self.label_states
                .entry(target)
                .and_modify(|(n, s)| {
                    *n += 1;
                    if *s != state {
                        *s = None
                    }
                })
                .or_insert((1, state));
        }
        self.nodes.push(Node::Instruction(ins, ()));
    }

    /// A jump target. DreamMaker only knows what the cache holds when there's
    /// one way in: a single jump, and no falling through from above.
    fn emit_label(&mut self, label: String) {
        let falls_through = !matches!(
            self.nodes.last(),
            Some(Node::Instruction(
                Instruction::Jmp(_)
                    | Instruction::JmpLoop(_)
                    | Instruction::Switch(_)
                    | Instruction::SwitchRange(_)
                    | Instruction::PickSwitch(_)
                    | Instruction::PickProb(_),
                _
            ))
        );
        let jumped = self.label_states.remove(&label);
        self.cache = match (falls_through, jumped) {
            (false, Some((1, state))) => state,
            _ => None,
        };
        self.nodes.push(Node::Label(Label::Named(label)));
    }

    /// A label jumped back to from later code, whose cache state isn't known.
    fn emit_loop_label(&mut self, label: String) {
        self.cache = None;
        self.nodes.push(Node::Label(Label::Named(label)));
    }

    /// Pushes a call's arguments and returns the argument count operand:
    /// named arguments and `arglist()` push one list and count as 65535.
    fn emit_call_args(&mut self, args: Vec<Expression>) -> Result<u32, CompileError> {
        let count = args.len() as u32;
        Ok(match args::emit(self, args::ArgsContext::Proc, args)? {
            args::ArgsResult::Normal => count,
            args::ArgsResult::Assoc => {
                self.emit_ins(Instruction::NewAssocList(count));
                65535
            }
            args::ArgsResult::ArgList => 65535,
        })
    }

    /// The type path an expression is declared as, if it's a variable with a
    /// declared type.
    fn static_type_name(&self, expr: &Expression) -> Option<String> {
        procs::expr_type(self, expr).map(|t| t.get().path.clone())
    }

    /// DreamMaker forgets a chain through fields (`a.b.c`) held in the cache
    /// at the end of an assignment statement (`x = y`, `x += y`, `x++`, even to
    /// a local; not a list element's or a var declaration).
    pub(crate) fn end_assignment_statement(&mut self) {
        if self.cache.as_ref().is_some_and(|h| h.len() > 1) {
            self.cache = None;
        }
    }

    fn new_label(&mut self) -> String {
        let label = format!("LAB_{:0>4X}", self.label_count);
        self.label_count += 1;
        label
    }

    fn emit_find_var(&mut self, ident: dreammaker::ast::Ident) -> EvalKind {
        if let Some(kind) = procs::find_var(self, ident.as_str()) {
            return kind;
        }

        if let Some(index) = self.params.iter().rposition(|x| *x == ident.as_str()) {
            return EvalKind::Var(Variable::Arg(index as u32));
        }

        match ident.as_str() {
            "." => EvalKind::Var(Variable::Dot),
            "usr" => EvalKind::Var(Variable::Usr),
            "src" => EvalKind::Var(Variable::Src),
            "args" => EvalKind::Var(Variable::Args),
            "world" => EvalKind::Var(Variable::World),
            "caller" => EvalKind::Var(Variable::Caller),
            "callee" => EvalKind::Var(Variable::Callee),
            "global" => EvalKind::Global,

            // Anything else is treated as a global var
            _ => EvalKind::Var(Variable::Global(GlobalVar::named(
                ident.as_str().as_bytes(),
                None,
            ))),
        }
    }

    fn emit_move_to_stack(&mut self, kind: EvalKind) -> Result<EvalKind, CompileError> {
        match kind {
            EvalKind::Stack => {}

            EvalKind::ListRef => {
                self.emit_ins(Instruction::ListGet);
            }

            EvalKind::Range => return Err(CompileError::UnexpectedRange),
            EvalKind::Global => return Err(CompileError::UnexpectedGlobal),
            EvalKind::ArgList => return Err(CompileError::UnexpectedArgList),

            EvalKind::Var(var) => {
                self.emit_ins(Instruction::GetVar(var));
            }

            EvalKind::Field(builder, field) => {
                let var = builder.get_field(DMString(field.into()));
                self.emit_ins(Instruction::GetVar(var));
            }
        }

        Ok(EvalKind::Stack)
    }

    fn short_circuit(&mut self) -> String {
        let label = self.short_circuit_labels.last_mut().unwrap();
        label.1 = true;
        label.0.to_owned()
    }

    /// Pushes the value of a `const` var, which DreamMaker inlines.
    fn emit_constant(
        &mut self,
        value: dreammaker::constants::Constant,
    ) -> Result<(), CompileError> {
        use dreammaker::constants::Constant;
        match value {
            Constant::Float(n) => self.emit_number(n),
            Constant::Null(_) => self.emit_ins(Instruction::PushVal(operands::Value::Null)),
            Constant::String(s) => self.emit_ins(Instruction::PushVal(operands::Value::DMString(
                strings::parse(s.as_str())?,
            ))),
            Constant::Resource(r) => self.emit_ins(Instruction::PushVal(
                operands::Value::Resource(r.as_str().to_owned()),
            )),
            Constant::Prefab(pop) if pop.vars.is_empty() => self.emit_ins(Instruction::PushVal(
                operands::Value::Path(pop.path.to_string()),
            )),
            _ => {
                return Err(CompileError::UnsupportedStatement(
                    "a const var with a complex value",
                ))
            }
        }
        Ok(())
    }

    /// Pushes a number the way DreamMaker does: small non-negative integers
    /// inline, anything else as a full value.
    fn emit_number(&mut self, value: f32) {
        if value.fract() == 0.0 && (0.0..=65535.0).contains(&value) {
            self.emit_ins(Instruction::PushInt(value as i32));
        } else {
            self.emit_ins(Instruction::PushVal(operands::Value::Number(value)));
        }
    }

    fn emit_inner_expr(&mut self, expr: Expression) -> Result<EvalKind, CompileError> {
        // (see `regroup_root`)
        let root = std::mem::take(&mut self.regroup_root);
        if self.proc.is_some() {
            match fold::constant(self, &expr) {
                Some(fold::Constant::Number(value)) => {
                    self.emit_number(value);
                    return Ok(EvalKind::Stack);
                }
                Some(fold::Constant::Text(text)) => {
                    self.emit_ins(Instruction::PushVal(operands::Value::DMString(DMString(
                        text,
                    ))));
                    return Ok(EvalKind::Stack);
                }
                Some(fold::Constant::Null) => {
                    self.emit_ins(Instruction::PushVal(operands::Value::Null));
                    return Ok(EvalKind::Stack);
                }
                Some(fold::Constant::Path(path)) => {
                    self.emit_ins(Instruction::PushVal(operands::Value::Path(path)));
                    return Ok(EvalKind::Stack);
                }
                None => {}
            }
            // A term on a later line than the last recorded one gets its own
            // line record, as in a list() written over several lines
            if let Expression::Base { term, .. } = &expr {
                if self.last_line.is_some_and(|l| term.location.line > l) {
                    self.emit_ins(Instruction::DbgLine(term.location.line));
                }
            }
        }
        match expr {
            // A constant condition picks its branch at compile time
            Expression::TernaryOp { cond, if_, else_ }
                if self.proc.is_some() && fold::value(self, &cond).is_some() =>
            {
                let chosen = if fold::value(self, &cond).unwrap().truthy() {
                    if_
                } else {
                    else_
                };
                let kind = self.emit_expr(*chosen)?;
                self.emit_move_to_stack(kind)
            }
            Expression::TernaryOp { cond, if_, else_ } => ternary::emit(self, *cond, *if_, *else_),
            Expression::BinaryOp { op, lhs, rhs } => {
                self.regroup_chain = root;
                binary_ops::emit(self, op, *lhs, *rhs)
            }
            Expression::AssignOp { op, lhs, rhs } => assignment::emit(self, op, *lhs, *rhs),

            Expression::Base { term, follow } => {
                let unspanned_follows: Vec<Follow> =
                    follow.iter().map(|f| f.elem.clone()).collect();
                if self.proc.is_some()
                    && !follow.is_empty()
                    && matches!(&term.elem, dreammaker::ast::Term::Ident(i) if i.as_str() == "src")
                {
                    self.explicit_src = true;
                }
                // `/type::var` and `/type::proc()` don't evaluate the type (also
                // written `(/type)::var`)
                let mut type_term = &term.elem;
                while let dreammaker::ast::Term::Expr(inner) = type_term {
                    match &**inner {
                        Expression::Base { term, follow } if follow.is_empty() => type_term = &term.elem,
                        _ => break,
                    }
                }
                if let (
                    dreammaker::ast::Term::Prefab(prefab),
                    Some(Follow::StaticField(_) | Follow::ProcReference(_)),
                ) = (type_term, unspanned_follows.first())
                {
                    if self.proc.is_some() && prefab.vars.is_empty() {
                        let path = prefab.path.to_string();
                        let kind = procs::emit_type_scope(self, &path, &unspanned_follows[0])?;
                        let ty = self.proc.as_ref().unwrap().tree.find(&path);
                        return follow::emit_typed(self, unspanned_follows[1..].to_vec(), kind, ty);
                    }
                }
                let ty = procs::term_type(self, &term.elem);
                // (and, for a `/list/T` var, what its elements are)
                let elem_ty = match &term.elem {
                    dreammaker::ast::Term::Ident(_) | dreammaker::ast::Term::GlobalIdent(_) => {
                        procs::declared_type_path(
                            self,
                            &dreammaker::ast::Expression::Base { term: term.clone(), follow: Default::default() },
                        )
                        .and_then(|path| procs::list_elem_type(self, &path))
                    }
                    _ => None,
                };
                // An argument left empty, as in `f(a, , b)`, comes from the parser
                // with an unknown location; DreamMaker pushes it rather than reading null
                if matches!(term.elem, dreammaker::ast::Term::Null)
                    && term.location.file == dreammaker::FileId::UNKNOWN
                    && unspanned_follows.is_empty()
                    && self.proc.is_some()
                {
                    self.emit_ins(Instruction::PushVal(operands::Value::Null));
                    return Ok(EvalKind::Stack);
                }
                // `var/L[n]`, which the parser turns into a `new /list(n)` without a
                // location, makes its list with EmptyList
                if let dreammaker::ast::Term::NewPrefab {
                    prefab,
                    args: Some(args),
                } = &term.elem
                {
                    if term.location.file == dreammaker::FileId::UNKNOWN
                        && args.len() == 1
                        && prefab.path.to_string() == "/list"
                        && self.proc.is_some()
                    {
                        let kind = self.emit_expr(args[0].clone())?;
                        self.emit_move_to_stack(kind)?;
                        self.emit_ins(Instruction::EmptyList);
                        return follow::emit_typed(self, unspanned_follows, EvalKind::Stack, ty);
                    }
                }
                let is_ident = matches!(term.elem, dreammaker::ast::Term::Ident(_));
                let mut kind = term::emit(self, term.elem)?;
                // A static var of src's type, named alone, is reached through src
                // when a field or call follows: `cache = src; cache = global(x); ...`
                if is_ident
                    && self.proc.as_ref().is_some_and(|c| !c.ty.is_root())
                    && matches!(
                        unspanned_follows.first(),
                        Some(
                            Follow::Field(
                                dreammaker::ast::PropertyAccessKind::Dot
                                    | dreammaker::ast::PropertyAccessKind::Colon,
                                _
                            ) | Follow::Call(
                                dreammaker::ast::PropertyAccessKind::Dot
                                    | dreammaker::ast::PropertyAccessKind::Colon,
                                _,
                                _
                            )
                        )
                    )
                {
                    if let EvalKind::Var(Variable::Global(g)) = &kind {
                        if let Some(scope) = g
                            .scope
                            .as_deref()
                            .filter(|s| !s.is_empty() && !s.starts_with("proc:"))
                        {
                            let mut marker = chain_builder::STATIC_MARKER.to_vec();
                            marker.extend_from_slice(&g.name.0);
                            marker.push(0);
                            marker.extend_from_slice(scope.as_bytes());
                            kind = EvalKind::Field(
                                chain_builder::ChainBuilder::begin(Variable::Src),
                                String::from_utf8(marker).unwrap(),
                            );
                        }
                    }
                }
                self.list_elem_type = elem_ty;
                let kind = follow::emit_typed(self, unspanned_follows, kind, ty)?;
                Ok(kind)
            }
        }
    }

    fn emit_expr(&mut self, expr: Expression) -> Result<EvalKind, CompileError> {
        let label = format!("LAB_{:0>4X}", self.label_count);
        self.label_count += 1;
        self.short_circuit_labels.push((label, false));

        let kind = self.emit_inner_expr(expr)?;

        let (label, used) = self.short_circuit_labels.pop().unwrap();

        // TODO: BYOND would put this jump destination before any unary ops (if this is Expression::Base), idk if that is sane.
        if used {
            self.emit_move_to_stack(kind)?;
            self.emit_label(label);
            return Ok(EvalKind::Stack);
        }

        Ok(kind)
    }
}

#[test]
fn compiles_safe_field_increment() {
    let nodes = compile_expr("a?.b++", &["a"]).unwrap();
    let bytecode = crate::assembler::assemble(&nodes, &mut crate::TestAssembleEnv).unwrap();

    assert_eq!(
        bytecode,
        vec![0x84, 1337, 0x33, 0xFFD9, 0, 0x13D, 9, 0x63, 1337, 0x33, 0xFFD9, 0, 0x1A, 2, 0x12,]
    );
}
