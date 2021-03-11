use dreammaker::ast::Expression;
use dreammaker::ast::Follow;
use dreammaker::ast::IndexKind;
use dreammaker::ast::Term;
use dreammaker::ast::UnaryOp;

use crate::operands::{DMString, Value, Variable};
use crate::Instruction;
use crate::Node;

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
            compiler.emit_ins(Instruction::Ret, None);
        }

        EvalKind::Var(v) => {
            compiler.emit_ins(Instruction::GetVar(v), None);
            compiler.emit_ins(Instruction::Ret, None);
        }

        EvalKind::Field(mut v, f) => {
            final_field(&mut v, DMString(f.into()));
            compiler.emit_ins(Instruction::GetVar(v), None);
            compiler.emit_ins(Instruction::Ret, None);
        }
    }

    Ok(compiler.nodes)
}

#[derive(Debug, PartialEq)]
enum EvalKind {
    // The result of the expression will be on the top of the stack
    Stack,
    // The result of the expression can be accessed using a Variable operand
    Var(Variable),
    // Similar to Var, but more state
    Field(Variable, String),
}

struct Compiler<'a> {
    params: &'a [&'a str],
    nodes: Vec<Node<Option<CompileData>>>,
}

impl<'a> Compiler<'a> {
    fn emit_ins(&mut self, ins: Instruction, data: Option<CompileData>) {
        self.nodes.push(Node::Instruction(ins, data));
    }

    fn emit_find_var(&mut self, ident: dreammaker::ast::Ident) -> EvalKind {
        if let Some(index) = self.params.iter().rposition(|x| *x == ident) {
            self.emit_ins(Instruction::GetVar(Variable::Arg(0)), None);
            self.emit_ins(Instruction::PushInt(index as i32 + 1), None);
            self.emit_ins(Instruction::ListGet, None);
            return EvalKind::Stack;
        }

        EvalKind::Var(Variable::Global(DMString(ident.into())))
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
                let data = CompileData {
                    line: term.location.line,
                    column: term.location.column,
                };

                // r-value mutation will be a special case
                // TODO:
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
                        self.emit_ins(Instruction::PushVal(Value::Null), Some(data));
                        EvalKind::Stack
                    }
                    Term::Int(i) => {
                        self.emit_ins(Instruction::PushInt(i), Some(data));
                        EvalKind::Stack
                    }
                    Term::Float(f) => {
                        self.emit_ins(Instruction::PushVal(Value::Number(f)), Some(data));
                        EvalKind::Stack
                    }
                    Term::String(str) => {
                        self.emit_ins(
                            Instruction::PushVal(Value::DMString(DMString(str.into()))),
                            Some(data),
                        );
                        EvalKind::Stack
                    }

                    // Identifiers. These could be params or globals.
                    Term::Ident(ident) => self.emit_find_var(ident),

                    // Resources: We just find these using Text2File
                    // TODO: Runtime if not found
                    Term::Resource(resource) => {
                        self.emit_ins(
                            Instruction::PushVal(Value::DMString(DMString(resource.into()))),
                            Some(data),
                        );
                        self.emit_ins(Instruction::Text2File, Some(data));
                        EvalKind::Stack
                    }

                    // Type paths: We don't support the anonymous kind with variable declarations.
                    // We also just find these by using Text2Path at runtime
                    // TODO: Runtime if not found
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

                        self.emit_ins(
                            Instruction::PushVal(Value::DMString(DMString(path.into()))),
                            Some(data),
                        );
                        self.emit_ins(Instruction::Text2Path, Some(data));
                        EvalKind::Stack
                    }

                    other => return Err(CompileError::UnsupportedExpressionTerm(other)),
                };

                // Early exit that allows for propagating stack and l-values
                if follow.is_empty() && unary.is_empty() {
                    return Ok(kind);
                }

                // To emit follow sub-expressions and unary operations we need
                // a Variable reference to our value
                let mut var = match kind {
                    // Results on the stack are moved into the `Cache` register
                    EvalKind::Stack => {
                        self.emit_ins(Instruction::SetVar(Variable::Cache), Some(data));
                        Variable::Cache
                    }

                    // Convert l-value field references to a normal variable chain
                    EvalKind::Field(mut var, field) => {
                        append_field(&mut var, DMString(field.into()));
                        var
                    }

                    EvalKind::Var(var) => var,
                };

                let mut field_chain = vec![];

                for sub_expr in follow {
                    match sub_expr.elem {
                        Follow::Field(index_kind, ident) => {
                            match index_kind {
                                // We just treat these as the same
                                // TODO: Should we type check?
                                IndexKind::Dot | IndexKind::Colon => {
                                    field_chain.push(ident);
                                }

                                other => return Err(CompileError::UnsupportedIndexKind(other)),
                            }
                        }

                        other => return Err(CompileError::UnsupportedSubExpr(other)),
                    }
                }

                // BEGIN SUBEXPR BUILDER
                // TODO: Move into helper code
                var = match var {
                    Variable::Cache => {
                        // We're already in the cache so there's nothing to do here
                        // This null gets replaced later
                        Variable::Null
                    }

                    other => Variable::SetCache(Box::new(other), Box::new(Variable::Null)),
                };

                // We pull the last field out now so we can return an l-value reference later
                let last = field_chain.pop().unwrap();

                for field in field_chain {
                    append_field(&mut var, DMString(field.into()));
                }

                // Early return if we have no unary ops
                // This is necessary so that we can return l-values
                if unary.is_empty() {
                    return Ok(EvalKind::Field(var, last));
                }

                // Before applying binary operations we have to move our value to the stack
                final_field(&mut var, DMString(last.into()));
                self.emit_ins(Instruction::GetVar(var), Some(data));

                for op in unary.into_iter().rev() {
                    match op {
                        UnaryOp::Neg => self.emit_ins(Instruction::UnaryNeg, Some(data)),
                        UnaryOp::Not => self.emit_ins(Instruction::Not, Some(data)),
                        UnaryOp::BitNot => self.emit_ins(Instruction::Bnot, Some(data)),
                        other => return Err(CompileError::UnsupportedUnaryOp(other)),
                    }
                }

                return Ok(EvalKind::Stack);
            }
        }
    }
}

#[test]
fn compile_test() {
    let context: dreammaker::Context = Default::default();
    let lexer = dreammaker::lexer::Lexer::new(&context, Default::default(), "a.b.c".as_bytes());
    //let code = dreammaker::indents::IndentProcessor::new(&context, lexer);
    let expr = dreammaker::parser::parse_expression(&context, Default::default(), lexer);
    context.assert_success();
    println!("{:#?}\n\n\n", expr);

    let expr = compile_expr("a.b.c.d", &["a"]);
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
