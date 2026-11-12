use std::fmt;

use dreammaker::ast::Follow;
use dreammaker::{ast::Expression, Severity};

use crate::operands::{self, DMString, GlobalVar, Label, Variable};
use crate::Instruction;
use crate::{ByondVersion, Node};

mod assignment;
mod fold;
mod procs;
mod statements;

pub(crate) use procs::compile_proc_as;
pub use procs::{compile_proc, CompiledProc, ProcSource, StaticLocal};

mod args;
mod binary_ops;
mod builtin_procs;
mod chain_builder;
mod follow;
mod strings;
mod term;
mod ternary;
mod unary;
mod versions;

/// Decode a parsed string literal into BYOND text, including escapes
/// and text macros.
pub(crate) fn text(raw: &str) -> Option<String> {
    String::from_utf8(strings::parse(raw).ok()?.0).ok()
}

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
        // `*p = x` writes through the pointer
        Variable::PtrDeref(_) => true,
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
    /// Unresolved identifier in a debugger expression.
    UndefinedVar(String),
    /// Another error, at a source line.
    AtLine(u32, Box<CompileError>),

    AmbiguousListConstructor,
    /// Feature unavailable in the target BYOND build.
    NotInByond {
        what: String,
        byond: ByondVersion,
    },
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

            CompileError::UndefinedVar(name) => write!(f, "undefined var: {}", name),
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
            CompileError::NotInByond { what, byond } => {
                write!(f, "BYOND {} does not support {}", byond, what)
            }
            CompileError::UnsupportedCompilerMacro { name } => {
                write!(f, "unsupported compiler macro: {}", name)
            }
        }
    }
}

/// Runtime name resolution for debugger expressions.
///
/// Resolve bare names as parameters, src variables, then globals; unknown
/// names are errors. Resolve bare calls as src procs, then global procs.
/// Without a scope, assume non-parameter names and bare calls are global.
pub struct ExprScope<'a> {
    /// Whether the object (`src`) has a var of this name
    pub has_src_var: &'a dyn Fn(&str) -> bool,
    /// Whether the object has a proc or verb of this name
    pub has_src_proc: &'a dyn Fn(&str) -> bool,
    /// How to reach the world's global var of this name, if it has one
    pub global: &'a dyn Fn(&str) -> Option<ExprGlobal>,
    /// Parameter holding `global.vars` for name-based global access.
    /// Names alone cannot distinguish globals from same-named type statics.
    pub globals: &'a str,
}

/// How a debugger's expression reaches a global var
pub enum ExprGlobal {
    /// By its name, through the world's list of globals
    Listed,
    /// Direct variable ID for globals absent from `global.vars`, such as
    /// globals added by hot reload.
    Id(u32),
}

/// Compile a debugger expression for the newest supported build
/// (see `compile_expr_for`).
pub fn compile_expr(code: &str, params: &[&str]) -> Result<Vec<Node>, CompileError> {
    compile_expr_for(code, params, ByondVersion::LATEST)
}

/// Compiles an expression for the debugger's evals in a world running
/// `byond`.
pub fn compile_expr_for(
    code: &str,
    params: &[&str],
    byond: ByondVersion,
) -> Result<Vec<Node>, CompileError> {
    compile_expr_in(code, params, None, byond)
}

/// Compile a debugger expression for `byond` using runtime name resolution
/// from `scope` (see `ExprScope`).
pub fn compile_expr_in(
    code: &str,
    params: &[&str],
    scope: Option<&ExprScope>,
    byond: ByondVersion,
) -> Result<Vec<Node>, CompileError> {
    compile_expr_warned(code, params, scope, byond).map(|(nodes, _)| nodes)
}

/// Compile an expression with compatibility warnings (see `CompiledProc::warnings`).
pub fn compile_expr_warned(
    code: &str,
    params: &[&str],
    scope: Option<&ExprScope>,
    byond: ByondVersion,
) -> Result<(Vec<Node>, Vec<String>), CompileError> {
    let mut compiler = Compiler::new(params.iter().map(|x| x.to_string()).collect(), None);
    compiler.byond = byond;
    compiler.expr_scope = scope;
    compiler.emit_ins(Instruction::DbgFile(DMString(
        b"<dmasm expression>".to_vec(),
    )));

    let ctx = dreammaker::Context::default();
    byond.read_as(&ctx);

    let mut lexer =
        dreammaker::lexer::Lexer::new(&ctx, dreammaker::FileId::BUILTINS, code.as_bytes());
    let tokens: Vec<dreammaker::lexer::LocatedToken> =
        dreammaker::IndentProcessor::new(&ctx, &mut lexer).collect();
    let taken = std::cell::Cell::new(0usize);
    let expr = ctx.parse_expression(
        dreammaker::Location::BUILTINS,
        tokens
            .iter()
            .cloned()
            .inspect(|_| taken.set(taken.get() + 1)),
    )?;

    // Reject trailing input after the parsed expression.
    let rest = &tokens[taken.get().saturating_sub(1).min(tokens.len())..];
    let ends = |token: &dreammaker::lexer::LocatedToken| {
        use dreammaker::lexer::{Punctuation, Token};
        matches!(
            token.token,
            Token::Eof | Token::Punct(Punctuation::Newline | Punctuation::Semicolon)
        )
    };
    if taken.get() > 0 && !rest.iter().all(ends) {
        return Err(CompileError::ExpectedEnd);
    }

    for err in ctx.errors().iter() {
        if err.severity() == Severity::Error {
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
    let warnings = compiler.warnings.into_inner();
    Ok((compiler.nodes, warnings))
}

/// Compiled debugger proc body.
#[derive(Debug)]
pub struct CompiledBody {
    pub nodes: Vec<Node>,
    /// Local names by slot; the evaluation stub must allocate this many locals.
    pub locals: Vec<String>,
    /// Compatibility warnings (see `CompiledProc::warnings`), with lines relative to the input.
    pub warnings: Vec<String>,
}

/// The proc a body is parsed as, and the file its debug info names
const BODY_PROC: &str = "dmasm_body";
const BODY_FILE: &str = "<dmasm expression>";

/// Whether debugger input parses as a proc body containing multiple
/// statements, a single non-expression statement, or a single call to a
/// statement-only builtin such as `sleep(1)`. Leave invalid input to the
/// expression compiler for diagnostics.
pub fn is_body(code: &str) -> bool {
    use dreammaker::ast::Statement;
    let Ok(tree) = parse_body(code, ByondVersion::LATEST) else {
        return false;
    };
    match body_statements(&tree) {
        [] => false,
        [only] => match &only.elem {
            Statement::Expr(expr) => is_statement_builtin(expr, ByondVersion::LATEST),
            _ => true,
        },
        _ => true,
    }
}

/// Identify calls to builtins with no expression form, such as `sleep(1)`.
/// Compile these as statements rather than global proc calls or return values.
fn is_statement_builtin(expr: &Expression, byond: ByondVersion) -> bool {
    use dreammaker::ast::Term;
    let Expression::Base { term, follow } = expr else {
        return false;
    };
    let Term::Call(name, args) = &term.elem else {
        return false;
    };
    if !follow.is_empty() {
        return false;
    }
    // Try the builtin compilers on a scratch compiler and discard its code.
    let mut compiler = Compiler::new(vec![], None);
    compiler.byond = byond;
    if !matches!(
        builtin_procs::emit(&mut compiler, name.as_str(), &args.to_vec()),
        Ok(None)
    ) {
        return false;
    }
    !matches!(
        builtin_procs::emit_statement_builtin(&mut compiler, name.as_str(), args),
        Ok(false)
    )
}

/// Compile debugger input as a proc body using `scope` for names outside
/// its local declarations. Return `list(result, params...)` so the caller
/// can restore modified frame variables.
///
/// A final expression becomes the result; otherwise, as after a final
/// assignment or `x++`, return `.` on fallthrough.
/// Diagnostic line numbers are relative to `code`.
pub fn compile_body_in(
    code: &str,
    params: &[&str],
    scope: Option<&ExprScope>,
    byond: ByondVersion,
) -> Result<CompiledBody, CompileError> {
    use dreammaker::ast::{BinaryOp, Statement};

    let parsed = parse_body(code, byond)?;
    let mut statements = body_statements(&parsed).to_vec();
    // A bare goto label must not become the return value.
    let ends_with_label = matches!(
        statements.last().map(|last| &last.elem),
        Some(Statement::Expr(expr)) if statements::is_bare_label(&statements, expr)
    );
    if let Some(last) = statements.last_mut().filter(|_| !ends_with_label) {
        if let Statement::Expr(expr) = &last.elem {
            // Standalone shifts are input/output statements; `sleep(1)` also has no value.
            let io = matches!(
                expr,
                Expression::BinaryOp {
                    op: BinaryOp::LShift | BinaryOp::RShift,
                    ..
                }
            );
            // Assignments and increments leave `.` as the result, as in a proc.
            let assigns = match expr {
                Expression::AssignOp { .. } => true,
                Expression::Base { follow, .. } => matches!(
                    follow.last().map(|f| &f.elem),
                    Some(dreammaker::ast::Follow::Unary(
                        dreammaker::ast::UnaryOp::PreIncr
                            | dreammaker::ast::UnaryOp::PostIncr
                            | dreammaker::ast::UnaryOp::PreDecr
                            | dreammaker::ast::UnaryOp::PostDecr
                    ))
                ),
                _ => false,
            };
            if !io && !assigns && !is_statement_builtin(expr, byond) {
                last.elem = Statement::Return(Some(expr.clone()));
            }
        }
    }
    let value = dreammaker::objtree::ProcValue {
        code: Some(statements.into_boxed_slice()),
        ..parsed
            .root()
            .get()
            .procs
            .get(BODY_PROC)
            .expect("the body's proc")
            .value[0]
            .clone()
    };
    // What the code can see of types is BYOND's own: the world's aren't
    // known here
    let tree = builtins_tree();
    let root = tree.root();
    let source = procs::ProcSource {
        tree: &tree,
        ty: root,
        name: BODY_PROC,
        value: &value,
        file: BODY_FILE,
        debug_info: true,
        index: Some(0),
        byond,
        file_names: None,
    };

    let ctx = procs::proc_ctx(&source, vec![String::new(); params.len()]);
    let mut compiler = Compiler::new(params.iter().map(|x| x.to_string()).collect(), Some(ctx));
    compiler.byond = byond;
    compiler.expr_scope = scope;
    compiler.emit_ins(Instruction::DbgFile(DMString(
        BODY_FILE.as_bytes().to_vec(),
    )));
    statements::emit_proc_body(&mut compiler, &source, value.code.as_ref().unwrap()).map_err(
        |e| match e {
            // Adjust warning lines for the synthetic proc header.
            CompileError::AtLine(line, e) => CompileError::AtLine(line.saturating_sub(1), e),
            e => e,
        },
    )?;
    compiler.emit_ins(Instruction::End);
    let locals = compiler.proc.take().unwrap().locals;
    // Adjust warning lines for the synthetic proc header.
    let warnings = compiler
        .warnings
        .take()
        .into_iter()
        .map(|warning| {
            let line = warning
                .strip_prefix("line ")
                .and_then(|rest| rest.split_once(": "))
                .and_then(|(line, text)| Some((line.parse::<u32>().ok()?, text)));
            match line {
                Some((line, text)) => format!("line {}: {text}", line.saturating_sub(1)),
                None => warning,
            }
        })
        .collect();

    // Every way out returns the result and the params, as an expression
    // does; `End` returns `.`, which is the frame's when it's a param
    let count = params.len() as u32;
    let dot = match params.iter().rposition(|p| *p == ".") {
        Some(index) => Variable::Arg(index as u32),
        None => Variable::Dot,
    };
    let mut nodes = vec![];
    for node in compiler.nodes {
        let ins = match node {
            Node::Instruction(ins, ()) => ins,
            other => {
                nodes.push(other);
                continue;
            }
        };
        match ins {
            Instruction::DbgLine(line) => nodes.push(Node::Instruction(
                Instruction::DbgLine(line.saturating_sub(1)),
                (),
            )),
            Instruction::Ret | Instruction::End => {
                if ins == Instruction::End {
                    nodes.push(Node::Instruction(Instruction::GetVar(dot.clone()), ()));
                }
                for arg in 0..count {
                    nodes.push(Node::Instruction(
                        Instruction::GetVar(Variable::Arg(arg)),
                        (),
                    ));
                }
                nodes.push(Node::Instruction(Instruction::NewList(count + 1), ()));
                nodes.push(Node::Instruction(Instruction::Ret, ()));
            }
            ins => nodes.push(Node::Instruction(ins, ())),
        }
    }
    Ok(CompiledBody {
        nodes,
        locals,
        warnings,
    })
}

/// Parse debugger input as global proc `BODY_PROC`. Remove common
/// indentation, then indent the body one level beneath a synthetic header.
fn parse_body(
    code: &str,
    byond: ByondVersion,
) -> Result<dreammaker::objtree::ObjectTree, CompileError> {
    let lines: Vec<&str> = code.lines().collect();
    let indent = |line: &str| line.len() - line.trim_start_matches([' ', '\t']).len();
    let common = lines
        .iter()
        .filter(|line| !line.trim().is_empty())
        .map(|line| &line[..indent(line)])
        .reduce(|a, b| {
            let same = a.bytes().zip(b.bytes()).take_while(|(x, y)| x == y).count();
            &a[..same]
        })
        .unwrap_or("");
    let lines: Vec<&str> = lines
        .iter()
        .map(|line| line.strip_prefix(common).unwrap_or(line.trim_start()))
        .collect();
    // Use the body's indentation unit to avoid mixing tabs with user-supplied spaces.
    let unit = lines
        .iter()
        .filter(|line| !line.trim().is_empty())
        .map(|line| &line[..indent(line)])
        .find(|prefix| !prefix.is_empty())
        .unwrap_or("\t");
    let mut text = format!("/proc/{}()\n", BODY_PROC);
    for line in &lines {
        text.push_str(unit);
        text.push_str(line);
        text.push('\n');
    }

    let ctx = dreammaker::Context::default();
    byond.read_as(&ctx);
    let pp = dreammaker::Preprocessor::from_buffer(&ctx, BODY_FILE.into(), text);
    let mut parser = dreammaker::Parser::new(&ctx, pp);
    parser.enable_procs();
    // (with BYOND's types, the parser prints to stderr about a proc that
    // doesn't parse)
    let tree = parser.parse_object_tree_without_builtins();
    if let Some(err) = ctx
        .errors()
        .iter()
        .find(|e| e.severity() == Severity::Error)
    {
        // Map locations back to the input by removing the header and added indentation,
        // then restoring the common indentation.
        let mut at = err.location();
        at.line = at
            .line
            .saturating_sub(1)
            .clamp(1, lines.len().max(1) as u32);
        at.column = (at.column.saturating_sub(unit.len() as u16) + common.len() as u16).max(1);
        return Err(CompileError::ParseError(dreammaker::DMError::new(
            at,
            err.description(),
        )));
    }
    if tree
        .root()
        .get()
        .procs
        .get(BODY_PROC)
        .is_none_or(|p| p.value.is_empty())
    {
        return Err(CompileError::UnsupportedStatement("code outside the body"));
    }
    Ok(tree)
}

/// A tree of BYOND's own types, and nothing else
fn builtins_tree() -> dreammaker::objtree::ObjectTree {
    let ctx = dreammaker::Context::default();
    let pp = dreammaker::Preprocessor::from_buffer(&ctx, BODY_FILE.into(), "");
    dreammaker::Parser::new(&ctx, pp).parse_object_tree()
}

/// The statements of a body `parse_body` parsed
fn body_statements(
    tree: &dreammaker::objtree::ObjectTree,
) -> &[dreammaker::ast::Spanned<dreammaker::ast::Statement>] {
    tree.root()
        .get()
        .procs
        .get(BODY_PROC)
        .and_then(|p| p.value[0].code.as_deref())
        .unwrap_or(&[])
}

/// `src.member`, for a name a debugger's expression leaves `src.` off
fn src_member(member: Follow) -> Expression {
    let location = dreammaker::Location::BUILTINS;
    Expression::Base {
        term: Box::new(dreammaker::ast::Spanned::new(
            location,
            dreammaker::ast::Term::Ident("src".into()),
        )),
        follow: vec![dreammaker::ast::Spanned::new(location, member)].into(),
    }
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
    /// Set when compiling a debugger's expression that knows its
    /// surroundings: what its names mean.
    expr_scope: Option<&'a ExprScope<'a>>,
    /// Known cache-register access chain, or `None`. Omit redundant cache
    /// loads using DreamMaker's reuse rules.
    cache: Option<Vec<Variable>>,
    last_line: Option<u32>,
    /// The type a bare `new` creates: the declared type of what it's being
    /// assigned to.
    implicit_type: Option<String>,
    /// The next src-based chain was written with explicit `src.` and must
    /// reload the cache.
    explicit_src: bool,
    /// Emit DbgFile and DbgLine.
    debug_info: bool,
    /// The build whose DreamMaker the code has to match.
    byond: ByondVersion,
    /// The cache state at the jumps seen so far to each label, merged.
    label_states: std::collections::HashMap<String, (u32, Option<Vec<Variable>>)>,
    /// Set while compiling an if or while condition that is a && or || chain.
    condition_chain: bool,
    /// Set while compiling the left side of a `*`.
    mul_lhs: bool,
    /// Regroup the next root expression using DreamMaker's rules for return
    /// values, arguments and parentheses. Consumed by expression compilation.
    regroup_root: bool,
    /// Passes `regroup_root` on to the binary operation it applies to.
    regroup_chain: bool,
    /// Whether a field access or receiver call requires saving the cache
    /// around a logical assignment RHS. Static/const reads through `.` and
    /// `global.x` do not set this flag; `::` accesses do.
    global_field: bool,
    /// Debug info currently names another file, such as a variable default.
    /// Restore the proc's file before recording its next line.
    other_file: bool,
    /// Set after a var default's line is recorded: the next compound term
    /// records its own line, whatever it is (see `compound_line`)
    line_pending: bool,
    /// Suppress target debug lines for compound assignments. Nested expressions
    /// still record lines. Consumed by the next expression compilation.
    pub(super) place: bool,
    /// Set while compiling `call(x, __PROC__)`'s proc, where builds before
    /// 515.1644 take `__PROC__`
    proc_macro_ok: bool,
    /// Set while compiling an expression with nothing folded (to see what
    /// DreamMaker sees in its source)
    no_fold: bool,
    /// Compatibility warnings (see `CompiledProc::warnings`). Interior
    /// mutability allows constant folding to report warnings through `&self`.
    warnings: std::cell::RefCell<Vec<String>>,
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
            expr_scope: None,
            cache: None,
            last_line: None,
            implicit_type: None,

            explicit_src: false,
            debug_info: true,
            byond: ByondVersion::LATEST,
            label_states: Default::default(),
            condition_chain: false,
            mul_lhs: false,
            regroup_root: false,
            regroup_chain: false,
            global_field: false,
            other_file: false,
            line_pending: false,
            place: false,
            warnings: Default::default(),
            proc_macro_ok: false,
            no_fold: false,
            list_elem_type: None,
        }
    }

    fn emit_ins(&mut self, mut ins: Instruction) {
        if !self.debug_info && matches!(ins, Instruction::DbgLine(_) | Instruction::DbgFile(_)) {
            return;
        }
        if let (Instruction::DbgLine(_), true) = (&ins, self.other_file) {
            self.other_file = false;
            self.last_line = None;
            let file = self
                .proc
                .as_ref()
                .map(|p| p.file.clone())
                .unwrap_or_default();
            self.emit_ins(Instruction::DbgFile(DMString(file.into_bytes())));
        }
        if let Instruction::DbgLine(line) = ins {
            self.line_pending = false;
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

    /// Merge incoming cache states at a jump target. DreamMaker preserves
    /// knowledge only for a single incoming edge without fallthrough.
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

    /// Whether proc macros have a valid source proc. Debugger bodies use a synthetic
    /// proc, so `__TYPE__` and `__PROC__` would refer to it rather than the paused frame.
    fn proc_macros_known(&self) -> bool {
        self.proc.is_some() && self.expr_scope.is_none()
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

    fn emit_find_var(&mut self, ident: dreammaker::ast::Ident) -> Result<EvalKind, CompileError> {
        if let Some(kind) = procs::find_var(self, ident.as_str()) {
            return Ok(kind);
        }

        if let Some(index) = self.params.iter().rposition(|x| *x == ident.as_str()) {
            return Ok(EvalKind::Var(Variable::Arg(index as u32)));
        }

        Ok(match ident.as_str() {
            "." => EvalKind::Var(Variable::Dot),
            "usr" => EvalKind::Var(Variable::Usr),
            "src" => EvalKind::Var(Variable::Src),
            "args" => EvalKind::Var(Variable::Args),
            "world" => EvalKind::Var(Variable::World),
            "caller" => EvalKind::Var(Variable::Caller),
            "callee" => EvalKind::Var(Variable::Callee),
            "global" => EvalKind::Global,

            name => match self.expr_scope {
                // A debugger's expression: a var of the object it's
                // evaluated for, then a global, as in a proc's code
                Some(scope) => {
                    if (scope.has_src_var)(name) {
                        return self.emit_expr(src_member(Follow::Field(
                            dreammaker::ast::PropertyAccessKind::Dot,
                            ident.clone(),
                        )));
                    }
                    return self.emit_global_by_name(name);
                }

                // Anything else is treated as a global var
                None => EvalKind::Var(Variable::Global(GlobalVar::named(name.as_bytes(), None))),
            },
        })
    }

    /// A global var in a debugger's expression: read through the list of
    /// them that it's given (see `ExprScope::globals`), `globals["name"]`, or
    /// by the id it's told
    fn emit_global_by_name(&mut self, name: &str) -> Result<EvalKind, CompileError> {
        let scope = self
            .expr_scope
            .expect("a global by name outside a debugger's expression");
        match (scope.global)(name) {
            None => return Err(CompileError::UndefinedVar(name.to_owned())),
            Some(ExprGlobal::Id(id)) => {
                let mut global = GlobalVar::named(name.as_bytes(), Some(String::new()));
                global.id = Some(id);
                return Ok(EvalKind::Var(Variable::Global(global)));
            }
            Some(ExprGlobal::Listed) => {}
        }
        let Some(index) = self.params.iter().rposition(|x| *x == scope.globals) else {
            return Err(CompileError::UndefinedVar(scope.globals.to_owned()));
        };
        self.emit_ins(Instruction::GetVar(Variable::Arg(index as u32)));
        self.emit_ins(Instruction::PushVal(operands::Value::DMString(DMString(
            name.as_bytes().to_vec(),
        ))));
        Ok(EvalKind::ListRef)
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
        let place = std::mem::take(&mut self.place);
        if self.proc.is_some() {
            // A compound term on a later line than the last recorded one gets
            // its own line record, as in a list() written over several lines,
            // and so does the first one after a var default whatever its line
            // (DreamMaker goes by the source: one that folds counts too)
            if let Some(line) = compound_line(&expr).filter(|_| !place) {
                if self.line_pending || self.last_line.is_some_and(|l| line > l) {
                    self.emit_ins(Instruction::DbgLine(line));
                }
            }
            let folded = fold::constant(self, &expr);
            // (DreamMaker forgets the cache where it folds `&&` or `||`)
            if folded.as_ref().is_some_and(|c| *c != fold::Constant::New) && fold::has_logic(&expr)
            {
                self.cache = None;
            }
            // A `/type::var` default from another file that folds away
            // records nothing, but the next line names the proc's file again
            if folded.as_ref().is_some_and(|c| *c != fold::Constant::New)
                && self.debug_info
                && !self.other_file
                && format!("{expr:?}").contains("StaticField")
                && self.names_other_file(&expr)
            {
                self.other_file = true;
            }
            match folded {
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
                // (`constant` leaves it to be made)
                Some(fold::Constant::New) | None => {}
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

            // `locate(X) in a in b` is `(locate(X) in a) in b`
            Expression::Base { term, follow }
                if follow.is_empty()
                    && matches!(&term.elem, dreammaker::ast::Term::Locate { in_list: Some(list), .. }
                        if matches!(**list, Expression::BinaryOp { op: dreammaker::ast::BinaryOp::In, .. })) =>
            {
                let dreammaker::ast::Term::Locate {
                    args,
                    in_list: Some(list),
                } = term.elem
                else {
                    unreachable!()
                };
                let Expression::BinaryOp { lhs, rhs, .. } = *list else {
                    unreachable!()
                };
                let locate = Expression::Base {
                    term: Box::new(dreammaker::ast::Spanned::new(
                        term.location,
                        dreammaker::ast::Term::Locate {
                            args,
                            in_list: Some(lhs),
                        },
                    )),
                    follow,
                };
                self.emit_expr(Expression::BinaryOp {
                    op: dreammaker::ast::BinaryOp::In,
                    lhs: Box::new(locate),
                    rhs,
                })
            }
            // `&(x)` is `&x`, and `*(p)` is `*p` (but `*(&x)` reads the
            // pointer it makes)
            Expression::Base { term, follow }
                if self.proc.is_some()
                    && match follow.first().map(|f| &f.elem) {
                        Some(Follow::Unary(dreammaker::ast::UnaryOp::Reference)) => true,
                        Some(Follow::Unary(dreammaker::ast::UnaryOp::Dereference)) => matches!(
                            &term.elem,
                            dreammaker::ast::Term::Expr(inner) if matches!(&**inner,
                                Expression::Base { follow, .. }
                                    if !follow.iter().any(|f| matches!(f.elem, Follow::Unary(_))))
                        ),
                        _ => false,
                    }
                    && matches!(&term.elem, dreammaker::ast::Term::Expr(inner)
                        if matches!(**inner, Expression::Base { .. })) =>
            {
                let dreammaker::ast::Term::Expr(inner) = term.elem else {
                    unreachable!()
                };
                let Expression::Base {
                    term: inner_term,
                    follow: inner_follow,
                } = *inner
                else {
                    unreachable!()
                };
                let follow = inner_follow.iter().chain(follow.iter()).cloned().collect();
                self.emit_expr(Expression::Base {
                    term: inner_term,
                    follow,
                })
            }
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
                        Expression::Base { term, follow } if follow.is_empty() => {
                            type_term = &term.elem
                        }
                        _ => break,
                    }
                }
                if let (
                    dreammaker::ast::Term::Prefab(prefab),
                    Some(Follow::StaticField(_) | Follow::ProcReference(_)),
                ) = (type_term, unspanned_follows.first())
                {
                    // (a debugger's code reads it at runtime: it doesn't
                    // have the world's types)
                    if self.proc.is_some() && prefab.vars.is_empty() && self.expr_scope.is_none() {
                        self.global_field = true;
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
                            &dreammaker::ast::Expression::Base {
                                term: term.clone(),
                                follow: Default::default(),
                            },
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

    /// Whether `expr`, compiled with nothing folded, records another file
    /// (a `/type::var` default's): compiled and thrown away
    fn names_other_file(&mut self, expr: &Expression) -> bool {
        let start = self.nodes.len();
        let saved = (
            self.label_count,
            self.cache.clone(),
            self.last_line,
            self.label_states.clone(),
            self.global_field,
            self.line_pending,
            self.short_circuit_labels.len(),
        );
        self.no_fold = true;
        let ok = self.emit_expr(expr.clone()).is_ok();
        self.no_fold = false;
        let found = ok && self.other_file;
        self.nodes.truncate(start);
        self.short_circuit_labels.truncate(saved.6);
        (
            self.label_count,
            self.cache,
            self.last_line,
            self.label_states,
            self.global_field,
            self.line_pending,
        ) = (saved.0, saved.1, saved.2, saved.3, saved.4, saved.5);
        self.other_file = false;
        found
    }

    /// Record a warning once for the current source line.
    fn warn(&self, text: &str) {
        let text = match self.last_line {
            Some(line) => format!("line {line}: {text}"),
            None => text.to_owned(),
        };
        let mut warnings = self.warnings.borrow_mut();
        if !warnings.contains(&text) {
            warnings.push(text);
        }
    }

    /// Compiles the place an assignment writes to (see `place`)
    fn emit_place(
        &mut self,
        op: dreammaker::ast::AssignOp,
        expr: Expression,
    ) -> Result<EvalKind, CompileError> {
        self.place = op != dreammaker::ast::AssignOp::Assign;
        self.emit_expr(expr)
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

/// The line of `expr` if DreamMaker records lines for it: anything but a leaf,
/// which is a variable, a field of one (`a.b`, `a:b`), a literal (`-1` too)
/// or a path.
fn compound_line(expr: &Expression) -> Option<u32> {
    use dreammaker::ast::{PropertyAccessKind, Term, UnaryOp};
    match expr {
        Expression::Base { term, follow } => {
            let number = matches!(term.elem, Term::Int(_) | Term::Float(_));
            let leaf_term = match &term.elem {
                Term::Null
                | Term::Int(_)
                | Term::Float(_)
                | Term::Ident(_)
                | Term::String(_)
                | Term::Resource(_)
                | Term::Prefab(_)
                | Term::GlobalIdent(_) => true,
                Term::Expr(inner) => compound_line(inner).is_none(),
                _ => false,
            };
            let leaf_follow = follow.iter().all(|f| match &f.elem {
                Follow::Field(PropertyAccessKind::Dot | PropertyAccessKind::Colon, _) => true,
                Follow::Unary(UnaryOp::Neg) => number && follow.len() == 1,
                _ => false,
            });
            (!(leaf_term && leaf_follow)).then_some(term.location.line)
        }
        Expression::BinaryOp { lhs, .. } | Expression::AssignOp { lhs, .. } => {
            Some(first_line(lhs))
        }
        Expression::TernaryOp { cond, .. } => Some(first_line(cond)),
    }
}

/// The line `expr` starts on.
fn first_line(expr: &Expression) -> u32 {
    match expr {
        Expression::Base { term, .. } => term.location.line,
        Expression::BinaryOp { lhs, .. } | Expression::AssignOp { lhs, .. } => first_line(lhs),
        Expression::TernaryOp { cond, .. } => first_line(cond),
    }
}
