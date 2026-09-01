use std::fmt;

use dreammaker::ast::Follow;
use dreammaker::{ast::Expression, Severity};

use crate::operands::{self, DMString, Label, Variable};
use crate::Instruction;
use crate::Node;

mod assignment;

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
    UnsupportedBuiltin { proc: String },
    UnsupportedImplicitNew,
    UnsupportedRelativeCall,
    UnsupportedImplicitLocate,
    UnsupportedStringInterpolation,
    UnsupportedInput,
    UnsupportedCompilerMacro { name: String },

    AmbiguousListConstructor,
    InvalidLocateArgs,

    // TODO: Merge these
    IncorrectArgCount(String),
    MissingArgument { proc: String, index: u32 },
    TooManyArguments { proc: String, expected: u32 },
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
            CompileError::UnsupportedCompilerMacro { name } => {
                write!(f, "unsupported compiler macro: {}", name)
            }
        }
    }
}

pub fn compile_expr(code: &str, params: &[&str]) -> Result<Vec<Node>, CompileError> {
    let mut compiler = Compiler {
        params,
        nodes: vec![Node::Instruction(
            Instruction::DbgFile(DMString(b"<dmasm expression>".to_vec())),
            (),
        )],
        label_count: 0,
        short_circuit_labels: vec![],
    };

    let ctx = dreammaker::Context::default();

    let mut lexer = dreammaker::lexer::Lexer::new(&ctx, Default::default(), code.as_bytes());
    let mut indents = dreammaker::indents::IndentProcessor::new(&ctx, &mut lexer);
    let expr = dreammaker::parser::parse_expression(&ctx, Default::default(), &mut indents)?;

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
    params: &'a [&'a str],
    nodes: Vec<Node>,
    label_count: u32,
    short_circuit_labels: Vec<(String, bool)>,
}

impl Compiler<'_> {
    fn emit_ins(&mut self, ins: Instruction) {
        self.nodes.push(Node::Instruction(ins, ()));
    }

    fn emit_label(&mut self, label: String) {
        self.nodes.push(Node::Label(Label::Named(label)));
    }

    fn emit_find_var(&mut self, ident: dreammaker::ast::Ident) -> EvalKind {
        if let Some(index) = self.params.iter().rposition(|x| *x == ident) {
            return EvalKind::Var(Variable::Arg(index as u32));
        }

        match ident.as_str() {
            "." => EvalKind::Var(Variable::Dot),
            "usr" => EvalKind::Var(Variable::Usr),
            "src" => EvalKind::Var(Variable::Src),
            "args" => EvalKind::Var(Variable::Args),
            "world" => EvalKind::Var(Variable::World),
            "global" => EvalKind::Global,

            // Anything else is treated as a global var
            _ => EvalKind::Var(Variable::Global(DMString(ident.into()))),
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

    fn emit_inner_expr(&mut self, expr: Expression) -> Result<EvalKind, CompileError> {
        match expr {
            Expression::TernaryOp { cond, if_, else_ } => ternary::emit(self, *cond, *if_, *else_),
            Expression::BinaryOp { op, lhs, rhs } => binary_ops::emit(self, op, *lhs, *rhs),
            Expression::AssignOp { op, lhs, rhs } => assignment::emit(self, op, *lhs, *rhs),

            Expression::Base { term, follow } => {
                let unspanned_follows: Vec<Follow> =
                    follow.iter().map(|f| f.elem.clone()).collect();
                let kind = term::emit(self, term.elem)?;
                let kind = follow::emit(self, unspanned_follows, kind)?;
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
