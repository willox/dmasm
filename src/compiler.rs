use dreammaker::ast::{Expression, Spanned};
use dreammaker::ast::Follow;
use dreammaker::ast::IndexKind;
use dreammaker::ast::Term;
use dreammaker::ast::UnaryOp;

use crate::operands::{DMString, Value, Variable};
use crate::Instruction;
use crate::Node;

#[derive(Debug, PartialEq)]
struct VariableChainBuilder {
    var: Variable,
}

impl VariableChainBuilder {
    fn begin(base: Variable) -> Self {
        let var = match base {
            Variable::Cache => {
                // We're already in the cache so there's nothing to do here
                // This null gets replaced later
                Variable::Null
            }

            other => Variable::SetCache(Box::new(other), Box::new(Variable::Null)),
        };

        Self {
            var,
        }
    }

    fn last_setcache_rhs(&mut self) -> Option<&mut Box<Variable>> {
        if let Variable::SetCache(_, rhs) = &mut self.var {
            if let Variable::SetCache { .. } = **rhs {
                return last_setcache_rhs(rhs.as_mut());
            }

            return Some(rhs);
        }

        None
    }

    fn append(&mut self, field: DMString) {
        if let Some(rhs) = self.last_setcache_rhs() {
            **rhs = Variable::SetCache(Box::new(Variable::Field(field)), Box::new(Variable::Null));
            return;
        }

        // This is the first SetCache - discard the current var (which should be null)
        assert!(self.var == Variable::Null);
        self.var = Variable::SetCache(Box::new(Variable::Field(field)), Box::new(Variable::Null));
    }

    fn finalise(mut self, field: DMString) -> Variable {
        if let Some(rhs) = self.last_setcache_rhs() {
            **rhs = Variable::Field(field);
            return self.var;
        }

        Variable::Field(field)
    }
}

fn last_setcache_rhs(var: &mut Variable) -> Option<&mut Box<Variable>> {
    if let Variable::SetCache(_, rhs) = var {
        if let Variable::SetCache { .. } = **rhs {
            return last_setcache_rhs(rhs.as_mut());
        }

        return Some(rhs);
    }

    None
}

fn append_field(var: &mut Variable, field: DMString) {
    if let Some(rhs) = last_setcache_rhs(var) {
        **rhs = Variable::SetCache(Box::new(Variable::Field(field)), Box::new(Variable::Null));
        return;
    }

    // This is the first SetCache - discard the current var (which should be null)
    *var = Variable::SetCache(Box::new(Variable::Field(field)), Box::new(Variable::Null));
}

fn final_field(var: &mut Variable, field: DMString) {
    if let Some(rhs) = last_setcache_rhs(var) {
        **rhs = Variable::Field(field);
        return;
    }

    // This is the first SetCache - discard the current var (which should be null)
    *var = Variable::Field(field);
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct CompileData {
    pub line: u32,
    pub column: u16,
}

#[derive(Debug)]
pub enum CompileError {
    ParseError(dreammaker::DMError),
    AssignOpNotImplemented,
    BinaryOpNotImplemented,
    TernaryOpNotImplemented,
    UnsupportedExpressionTerm(dreammaker::ast::Term),
    UnsupportedUnaryOp(dreammaker::ast::UnaryOp),
    UnsupportedSubExpr(dreammaker::ast::Follow),
    UnsupportedIndexKind(dreammaker::ast::IndexKind),
    UnsupportedPrefabWithVars,
    MultipleUnaryMutations,
    UnaryMutationsNotImplemented,
}

impl From<dreammaker::DMError> for CompileError {
    fn from(err: dreammaker::DMError) -> Self {
        Self::ParseError(err)
    }
}

pub fn compile_expr(
    code: &str,
    params: &[&str],
) -> Result<Vec<Node<Option<CompileData>>>, CompileError> {
    let mut compiler = Compiler {
        params,
        nodes: vec![Node::Instruction(
            Instruction::DbgFile(DMString(b"<dmasm expression>".to_vec())),
            None,
        )],
    };

    // Expression begin
    let ctx = dreammaker::Context::default();
    let lexer = dreammaker::lexer::Lexer::new(&ctx, Default::default(), code.as_bytes());
    let expr = dreammaker::parser::parse_expression(&ctx, Default::default(), lexer)?;

    match compiler.parse_expr(expr)? {
        EvalKind::Stack => {
            compiler.emit_ins(Instruction::Ret);
        }

        EvalKind::ListRef => {
            compiler.emit_ins(Instruction::ListGet);
            compiler.emit_ins(Instruction::Ret);
        }

        EvalKind::Var(v) => {
            compiler.emit_ins(Instruction::GetVar(v));
            compiler.emit_ins(Instruction::Ret);
        }

        EvalKind::Field(builder, f) => {
            let var = builder.finalise(DMString(f.into()));
            compiler.emit_ins(Instruction::GetVar(var));
            compiler.emit_ins(Instruction::Ret);
        }
    }

    Ok(compiler.nodes)
}

#[derive(Debug, PartialEq)]
enum EvalKind {
    // The result of the expression will be on the top of the stack
    Stack,
    // The result of the expression is a list entry L[K] where the top of the stack is the index and the 2nd top of the stack is the list
    ListRef,
    // The result of the expression can be accessed using a Variable operand
    Var(Variable),
    // Similar to Var, but more state
    Field(VariableChainBuilder, String),
}

struct Compiler<'a> {
    params: &'a [&'a str],
    nodes: Vec<Node<Option<CompileData>>>,
}

impl<'a> Compiler<'a> {
    fn emit_ins(&mut self, ins: Instruction) {
        self.nodes.push(Node::Instruction(ins, None));
    }

    fn emit_find_var(&mut self, ident: dreammaker::ast::Ident) -> EvalKind {
        if let Some(index) = self.params.iter().rposition(|x| *x == ident) {
            return EvalKind::Var(Variable::Arg(index as u32));
        }

        EvalKind::Var(Variable::Global(DMString(ident.into())))
    }

    fn emit_follow(&mut self, follow: Vec<Spanned<Follow>>, mut kind: EvalKind) -> Result<EvalKind, CompileError> {
        if follow.is_empty() {
            return Ok(kind);
        }

        // sequential field accessors (example: a.b.c) get buffered into a single operand
        let mut field_buffer = vec![];

        fn commit_field_buffer(compiler: &mut Compiler, kind: EvalKind, field_chain: &mut Vec<String>) -> EvalKind {
            if field_chain.is_empty() {
                return kind;
            }

            // We need a value
            let mut builder = match kind {
                EvalKind::Stack => {
                    compiler.emit_ins(Instruction::SetVar(Variable::Cache));
                    VariableChainBuilder::begin(Variable::Cache)
                }

                EvalKind::ListRef => {
                    compiler.emit_ins(Instruction::ListGet);
                    compiler.emit_ins(Instruction::SetVar(Variable::Cache));
                    VariableChainBuilder::begin(Variable::Cache)
                }

                EvalKind::Field(mut builder, field) => {
                    builder.append(DMString(field.into()));
                    builder
                }

                EvalKind::Var(var) => {
                    VariableChainBuilder::begin(var)
                }
            };

            let last = field_chain.pop().unwrap();

            for field in field_chain.iter() {
                builder.append(DMString(field.clone().into()));
            }

            field_chain.clear();
            EvalKind::Field(builder, last)
        }

        for sub_expr in follow {
            match sub_expr.elem {
                Follow::Field(index_kind, ident) => {
                    match index_kind {
                        // We just treat these as the same
                        // TODO: Should we type check?
                        IndexKind::Dot | IndexKind::Colon => {
                            field_buffer.push(ident);
                        }

                        other => return Err(CompileError::UnsupportedIndexKind(other)),
                    }
                }

                Follow::Index(expr) => {
                    kind = commit_field_buffer(self, kind, &mut field_buffer);

                    // Move base to the stack
                    match kind {
                        EvalKind::Stack => {},

                        EvalKind::ListRef => {
                            self.emit_ins(Instruction::ListGet);
                        }

                        EvalKind::Var(var) => {
                            self.emit_ins(Instruction::GetVar(var));
                        }

                        EvalKind::Field(builder, field) => {
                            let var = builder.finalise(DMString(field.into()));
                            self.emit_ins(Instruction::GetVar(var));
                        }
                    }

                    // Handle inner expression and move it to the stack
                    match self.parse_expr(*expr)? {
                        EvalKind::Stack => {}

                        EvalKind::ListRef => {
                            self.emit_ins(Instruction::ListGet);
                        }

                        EvalKind::Var(var) => {
                            self.emit_ins(Instruction::GetVar(var));
                        }

                        EvalKind::Field(builder, field) => {
                            let var = builder.finalise(DMString(field.into()));
                            self.emit_ins(Instruction::GetVar(var));
                        }
                    }

                    kind = EvalKind::ListRef;
                }

                other => return Err(CompileError::UnsupportedSubExpr(other)),
            }
        }

        kind = commit_field_buffer(self, kind, &mut field_buffer);
        Ok(kind)
    }

    fn emit_unary_ops(&mut self, unary: Vec<UnaryOp>, kind: EvalKind) -> Result<EvalKind, CompileError> {
        if unary.is_empty() {
            return Ok(kind);
        }

        // Unary ops need our value on the stack
        match kind {
            EvalKind::Stack => {},

            EvalKind::ListRef => {
                self.emit_ins(Instruction::ListGet);
            }

            EvalKind::Field(builder, field) => {
                let var = builder.finalise(DMString(field.into()));
                self.emit_ins(Instruction::GetVar(var));
            }

            EvalKind::Var(var) => {
                self.emit_ins(Instruction::GetVar(var));
            }
        }

        for op in unary.into_iter().rev() {
            match op {
                UnaryOp::Neg => self.emit_ins(Instruction::UnaryNeg),
                UnaryOp::Not => self.emit_ins(Instruction::Not),
                UnaryOp::BitNot => self.emit_ins(Instruction::Bnot),
                other => return Err(CompileError::UnsupportedUnaryOp(other)),
            }
        }

        return Ok(EvalKind::Stack);
    }

    fn parse_expr(&mut self, expr: Expression) -> Result<EvalKind, CompileError> {
        match expr {
            Expression::AssignOp { .. } => return Err(CompileError::AssignOpNotImplemented),
            Expression::BinaryOp { .. } => return Err(CompileError::BinaryOpNotImplemented),
            Expression::TernaryOp { .. } => return Err(CompileError::TernaryOpNotImplemented),

            Expression::Base {
                unary,
                term,
                follow,
            } => {
                // r-value mutation will be a special case
                // TODO: this won't be necessary
                {
                    let r_val_mutate_count = unary
                        .iter()
                        .filter(|x| {
                            **x == UnaryOp::PostIncr
                                || **x == UnaryOp::PostDecr
                                || **x == UnaryOp::PreIncr
                                || **x == UnaryOp::PreDecr
                        })
                        .count();

                    if r_val_mutate_count > 1 {
                        return Err(CompileError::MultipleUnaryMutations);
                    }

                    if r_val_mutate_count > 0 {
                        return Err(CompileError::UnaryMutationsNotImplemented);
                    }
                }

                let kind = match term.elem {
                    // Nested expression, probably something in brackets
                    Term::Expr(expr) => self.parse_expr(*expr)?,

                    // Simple stack pushes
                    Term::Null => {
                        self.emit_ins(Instruction::PushVal(Value::Null));
                        EvalKind::Stack
                    }
                    Term::Int(i) => {
                        self.emit_ins(Instruction::PushInt(i));
                        EvalKind::Stack
                    }
                    Term::Float(f) => {
                        self.emit_ins(Instruction::PushVal(Value::Number(f)));
                        EvalKind::Stack
                    }
                    Term::String(str) => {
                        self.emit_ins(
                            Instruction::PushVal(Value::DMString(DMString(str.into())))
                        );
                        EvalKind::Stack
                    }

                    // Identifiers. These could be params or globals.
                    Term::Ident(ident) => self.emit_find_var(ident),

                    // Resources
                    Term::Resource(resource) => {
                        self.emit_ins(Instruction::PushVal(Value::Resource(resource)));
                        EvalKind::Stack
                    }

                    // Type paths: We don't support the anonymous kind with variable declarations.
                    Term::Prefab(prefab) => {
                        if !prefab.vars.is_empty() {
                            return Err(CompileError::UnsupportedPrefabWithVars);
                        }

                        let mut path = String::new();

                        // TODO: Relative stuff
                        for (op, part) in prefab.path {
                            use std::fmt::Write;
                            write!(&mut path, "{}{}", op, part).unwrap();
                        }

                        self.emit_ins(Instruction::PushVal(Value::Path(path)));
                        EvalKind::Stack
                    }

                    other => return Err(CompileError::UnsupportedExpressionTerm(other)),
                };

                let kind = self.emit_follow(follow, kind)?;
                let kind = self.emit_unary_ops(unary, kind)?;

                return Ok(kind);
            }
        }
    }
}

#[test]
fn compile_test() {
    let context: dreammaker::Context = Default::default();
    let lexer = dreammaker::lexer::Lexer::new(&context, Default::default(), "a.b[\"c\"].d".as_bytes());
    //let code = dreammaker::indents::IndentProcessor::new(&context, lexer);
    let expr = dreammaker::parser::parse_expression(&context, Default::default(), lexer);
    context.assert_success();
    println!("{:#?}\n\n\n", expr);

    let expr = compile_expr("-a.b[a.c].g.f[2]", &["a"]);
    println!("{:#?}", expr);

    if let Ok(expr) = expr {
        println!("{}", crate::format(&expr));

        let stripped: Vec<Node> = expr
            .into_iter()
            .map(Node::<Option<CompileData>>::strip_debug_data)
            .collect();
        let code = crate::assembler::assemble(&stripped, &mut crate::TestAssembleEnv);
        println!("{:#x?}", code);
    }
}
