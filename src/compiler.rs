use dreammaker::ast::Follow;
use dreammaker::ast::IndexKind;
use dreammaker::ast::Term;
use dreammaker::ast::{AssignOp, BinaryOp, UnaryOp};
use dreammaker::ast::{Expression, Spanned};

use crate::operands::{self, DMString, Label, Value, Variable};
use crate::Instruction;
use crate::Node;

mod assignment;
mod binary_ops;
mod builtin_procs;

// TODO: Think
fn is_l_value(var: &Variable) -> bool {
    match var {
        // Does Field count? We probably don't hit that code path but it might count
        Variable::Usr
        | Variable::Src
        | Variable::Args
        | Variable::Dot
        | Variable::CacheIndex
        | Variable::Arg { .. }
        | Variable::Local { .. }
        | Variable::Global { .. } => true,
        _ => false,
    }
}

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

        Self { var }
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

    fn get_push_cache(mut self) -> Option<Variable> {
        if let Some(_rhs) = self.last_setcache_rhs() {
            return Some(self.var);
        }

        None
    }

    fn get_field(mut self, field: DMString) -> Variable {
        if let Some(rhs) = self.last_setcache_rhs() {
            **rhs = Variable::Field(field);
            return self.var;
        }

        Variable::Field(field)
    }

    fn get_initial_field(mut self, field: DMString) -> Variable {
        if let Some(rhs) = self.last_setcache_rhs() {
            **rhs = Variable::Initial(Box::new(Variable::Field(field)));
            return self.var;
        }

        Variable::Initial(Box::new(Variable::Field(field)))
    }

    fn get_dynamic_proc(mut self, proc: DMString) -> Variable {
        if let Some(rhs) = self.last_setcache_rhs() {
            **rhs = Variable::DynamicProc(proc);
            return self.var;
        }

        Variable::DynamicProc(proc)
    }
}

// TODO: dedupe
fn last_setcache_rhs(var: &mut Variable) -> Option<&mut Box<Variable>> {
    if let Variable::SetCache(_, rhs) = var {
        if let Variable::SetCache { .. } = **rhs {
            return last_setcache_rhs(rhs.as_mut());
        }

        return Some(rhs);
    }

    None
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct CompileData {
    pub line: u32,
    pub column: u16,
}

#[derive(Debug)]
pub enum CompileError {
    ParseError(dreammaker::DMError),
    UnsupportedExpressionTerm(dreammaker::ast::Term),
    UnsupportedPrefabWithVars,
    ExpectedLValue,
    ExpectedFieldReference,
    NamedArgumentsNotImplemented,
    IncorrectArgCount(String),
    MissingArgument {
        proc: String,
        index: u32,
        name: String,
    },
    TooManyArguments {
        proc: String,
        expected: u32,
    },
    UnsupportedBuiltin {
        proc: String,
    },
    UnexpectedRange,
    UnexpectedGlobal,
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
        label_count: 0,
    };

    // Expression begin
    let ctx = dreammaker::Context::default();
    let lexer = dreammaker::lexer::Lexer::new(&ctx, Default::default(), code.as_bytes());
    let expr = dreammaker::parser::parse_expression(&ctx, Default::default(), lexer)?;

    // TODO: Error check expr

    match compiler.emit_expr(expr)? {
        EvalKind::Stack => {}

        EvalKind::ListRef => {
            compiler.emit_ins(Instruction::ListGet);
        }

        EvalKind::Range => return Err(CompileError::UnexpectedRange),
        EvalKind::Global => return Err(CompileError::UnexpectedGlobal),

        EvalKind::Var(v) => {
            compiler.emit_ins(Instruction::GetVar(v));
        }

        EvalKind::Field(builder, f) => {
            let var = builder.get_field(DMString(f.into()));
            compiler.emit_ins(Instruction::GetVar(var));
        }
    }

    let mut arg_id = 0;
    for _ in params {
        compiler.emit_ins(Instruction::GetVar(Variable::Arg(arg_id)));
        arg_id += 1;
    }

    compiler.emit_ins(Instruction::NewList(params.len() as u32 + 1));
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

    // The result of the expression can be accessed using a Variable operand
    Var(Variable),

    // Similar to Var, but more state
    Field(VariableChainBuilder, String),
    // TODO: Eval?
}

struct Compiler<'a> {
    params: &'a [&'a str],
    nodes: Vec<Node<Option<CompileData>>>,
    label_count: u32,
}

impl<'a> Compiler<'a> {
    fn emit_ins(&mut self, ins: Instruction) {
        self.nodes.push(Node::Instruction(ins, None));
    }

    fn emit_label(&mut self, label: String) {
        self.nodes.push(Node::Label(label));
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

    fn emit_follow(
        &mut self,
        follow: Vec<Spanned<Follow>>,
        mut kind: EvalKind,
    ) -> Result<EvalKind, CompileError> {
        if follow.is_empty() {
            return Ok(kind);
        }

        // sequential field accessors (example: a.b.c) get buffered into a single operand
        let mut field_buffer = vec![];
        let mut skip_label = None;

        fn commit_field_buffer(
            compiler: &mut Compiler,
            kind: EvalKind,
            field_chain: &mut Vec<String>,
            skip_label: &mut Option<String>,
        ) -> Result<EvalKind, CompileError> {
            if field_chain.is_empty() {
                assert!(skip_label.is_none());
                return Ok(kind);
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

                EvalKind::Range => return Err(CompileError::UnexpectedRange),

                // Bit hacky.
                EvalKind::Global => {
                    let name = field_chain.remove(0);
                    let var = Variable::Global(DMString(name.into()));
                    return commit_field_buffer(
                        compiler,
                        EvalKind::Var(var),
                        field_chain,
                        skip_label,
                    );
                }

                EvalKind::Field(mut builder, field) => {
                    builder.append(DMString(field.into()));
                    builder
                }

                EvalKind::Var(var) => VariableChainBuilder::begin(var),
            };

            let last = field_chain.pop().unwrap();

            for field in field_chain.iter() {
                builder.append(DMString(field.clone().into()));
            }

            let kind = EvalKind::Field(builder, last);

            // If we had a skip label, we have to go on to the stack
            let kind = match skip_label {
                Some(skip_label) => {
                    compiler.emit_move_to_stack(kind)?;
                    compiler.emit_label(skip_label.clone());
                    EvalKind::Stack
                }

                None => kind,
            };

            field_chain.clear();
            *skip_label = None;
            Ok(kind)
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

                        // We just treat these as the same
                        // TODO: Should we type check?
                        // TODO: Generates kind of badly compared to BYOND.
                        IndexKind::SafeDot | IndexKind::SafeColon => {
                            kind = commit_field_buffer(
                                self,
                                kind,
                                &mut field_buffer,
                                &mut skip_label,
                            )?;
                            self.emit_move_to_stack(kind)?;

                            let label = format!("LAB_{:0>4X}", self.label_count);
                            self.label_count += 1;

                            self.emit_ins(Instruction::SetCacheJmpIfNull(Label(label.clone())));
                            self.emit_ins(Instruction::GetVar(Variable::Cache));

                            assert!(skip_label.is_none());
                            field_buffer.push(ident);
                            skip_label = Some(label);

                            kind = EvalKind::Stack;
                        }
                    }
                }

                Follow::Index(expr) => {
                    kind = commit_field_buffer(self, kind, &mut field_buffer, &mut skip_label)?;

                    // Move base to the stack
                    self.emit_move_to_stack(kind)?;

                    // Handle inner expression and move it to the stack
                    match self.emit_expr(*expr)? {
                        EvalKind::Stack => {}

                        EvalKind::ListRef => {
                            self.emit_ins(Instruction::ListGet);
                        }

                        EvalKind::Range => return Err(CompileError::UnexpectedRange),
                        EvalKind::Global => return Err(CompileError::UnexpectedGlobal),

                        EvalKind::Var(var) => {
                            self.emit_ins(Instruction::GetVar(var));
                        }

                        EvalKind::Field(builder, field) => {
                            let var = builder.get_field(DMString(field.into()));
                            self.emit_ins(Instruction::GetVar(var));
                        }
                    }

                    kind = EvalKind::ListRef;
                }

                Follow::Call(index_kind, ident, args) => {
                    // If any of the arguments are a Expression:AssignOp, byond does _crazy_ not-so-well defined things.
                    // We can implement this later...
                    if args
                        .iter()
                        .any(|x| matches!(x, Expression::AssignOp { .. }))
                    {
                        return Err(CompileError::NamedArgumentsNotImplemented);
                    }

                    match index_kind {
                        // Global call syntax `global.f()`
                        IndexKind::Dot | IndexKind::Colon
                            if matches!(kind, EvalKind::Global) && field_buffer.is_empty() =>
                        {
                            assert!(skip_label.is_none());

                            let arg_count = args.len() as u32;

                            // Bring all arguments onto the stack
                            for arg in args {
                                let expr = self.emit_expr(arg)?;
                                self.emit_move_to_stack(expr)?;
                            }

                            // We're treating all Term::Call expressions as global calls
                            self.emit_ins(Instruction::CallGlob(
                                arg_count,
                                operands::Proc(format!("/proc/{}", ident)),
                            ));
                        }

                        // We just treat these as the same
                        // TODO: Should we type check?
                        IndexKind::Dot | IndexKind::Colon => {
                            let arg_count = args.len() as u32;

                            // TODO: Can emit much cleaner code when no params
                            kind = commit_field_buffer(
                                self,
                                kind,
                                &mut field_buffer,
                                &mut skip_label,
                            )?;
                            self.emit_move_to_stack(kind)?;

                            // We'll need our src after pushing the parameters
                            self.emit_ins(Instruction::SetVar(Variable::Cache));
                            self.emit_ins(Instruction::PushCache);

                            // Push args to the stack
                            for arg in args {
                                let arg = self.emit_expr(arg)?;
                                self.emit_move_to_stack(arg)?;
                            }

                            self.emit_ins(Instruction::PopCache);

                            // Move base to the stack
                            self.emit_ins(Instruction::Call(
                                Variable::DynamicProc(DMString(ident.into())),
                                arg_count,
                            ));
                        }

                        IndexKind::SafeDot | IndexKind::SafeColon => {
                            let args_count = args.len() as u32;

                            // TODO: Can emit much cleaner code when no params
                            kind = commit_field_buffer(
                                self,
                                kind,
                                &mut field_buffer,
                                &mut skip_label,
                            )?;
                            self.emit_move_to_stack(kind)?;

                            let label = format!("LAB_{:0>4X}", self.label_count);
                            self.label_count += 1;

                            // We'll need our src after pushing the parameters
                            self.emit_ins(Instruction::SetCacheJmpIfNull(Label(label.clone())));
                            self.emit_ins(Instruction::PushCache);

                            // Push args to the stack
                            for arg in args {
                                let arg = self.emit_expr(arg)?;
                                self.emit_move_to_stack(arg)?;
                            }

                            self.emit_ins(Instruction::PopCache);

                            // Move base to the stack
                            self.emit_ins(Instruction::Call(
                                Variable::DynamicProc(DMString(ident.into())),
                                args_count,
                            ));

                            self.emit_label(label);
                        }
                    }

                    kind = EvalKind::Stack;
                }
            }
        }

        kind = commit_field_buffer(self, kind, &mut field_buffer, &mut skip_label)?;
        Ok(kind)
    }

    fn emit_unary_ops(
        &mut self,
        unary: Vec<UnaryOp>,
        mut kind: EvalKind,
    ) -> Result<EvalKind, CompileError> {
        if unary.is_empty() {
            return Ok(kind);
        }

        fn emit_unary_op(
            compiler: &mut Compiler,
            op: UnaryOp,
            kind: EvalKind,
        ) -> Result<EvalKind, CompileError> {
            match op {
                // Simple unary ops
                UnaryOp::Neg | UnaryOp::Not | UnaryOp::BitNot => {
                    // These ops need the value on the stack
                    compiler.emit_move_to_stack(kind)?;

                    match op {
                        UnaryOp::Neg => compiler.emit_ins(Instruction::UnaryNeg),
                        UnaryOp::Not => compiler.emit_ins(Instruction::Not),
                        UnaryOp::BitNot => compiler.emit_ins(Instruction::Bnot),
                        _ => unreachable!(),
                    }
                }

                // l-value mutating unary ops
                UnaryOp::PreIncr | UnaryOp::PostIncr | UnaryOp::PreDecr | UnaryOp::PostDecr => {
                    // These ops require an l-value
                    let var = match kind {
                        EvalKind::Var(var) if is_l_value(&var) => var,

                        EvalKind::Field(builder, field) => {
                            builder.get_field(DMString(field.into()))
                        }

                        EvalKind::ListRef => {
                            compiler.emit_ins(Instruction::SetVar(Variable::CacheKey));
                            compiler.emit_ins(Instruction::SetVar(Variable::Cache));
                            Variable::CacheIndex
                        }

                        _ => return Err(CompileError::ExpectedLValue),
                    };

                    match op {
                        UnaryOp::PreIncr => compiler.emit_ins(Instruction::PreInc(var)),
                        UnaryOp::PostIncr => compiler.emit_ins(Instruction::PostInc(var)),
                        UnaryOp::PreDecr => compiler.emit_ins(Instruction::PreDec(var)),
                        UnaryOp::PostDecr => compiler.emit_ins(Instruction::PostDec(var)),
                        _ => unreachable!(),
                    }
                }
            }

            Ok(EvalKind::Stack)
        }

        // Unary ops need our value on the stack

        for op in unary.into_iter().rev() {
            kind = emit_unary_op(self, op, kind)?;
        }

        return Ok(kind);
    }

    fn emit_expr(&mut self, expr: Expression) -> Result<EvalKind, CompileError> {
        match expr {
            Expression::TernaryOp {
                cond,
                if_: lhs,
                else_: rhs,
            } => {
                // Bring condition to stack
                let cond = self.emit_expr(*cond)?;
                self.emit_move_to_stack(cond)?;

                let label_rhs = format!("LAB_RHS_{:0>4X}", self.label_count);
                let label_end = format!("LAB_END_{:0>4X}", self.label_count);
                self.label_count += 1;

                self.emit_ins(Instruction::Test);
                self.emit_ins(Instruction::Jz(Label(label_rhs.clone())));

                // LHS
                let lhs = self.emit_expr(*lhs)?;
                self.emit_move_to_stack(lhs)?;
                self.emit_ins(Instruction::Jmp(Label(label_end.clone())));

                // RHS
                self.emit_label(label_rhs);
                let rhs = self.emit_expr(*rhs)?;
                self.emit_move_to_stack(rhs)?;

                // End
                self.emit_label(label_end);
                Ok(EvalKind::Stack)
            }

            Expression::Base {
                unary,
                term,
                follow,
            } => {
                let kind = match term.elem {
                    // Nested expression, probably something in brackets
                    Term::Expr(expr) => self.emit_expr(*expr)?,

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
                        self.emit_ins(Instruction::PushVal(Value::DMString(DMString(str.into()))));
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

                    Term::Call(ident, args) => {
                        // If any of the arguments are a Expression:AssignOp, byond does _crazy_ not-so-well defined things.
                        // We can implement this later...
                        if args
                            .iter()
                            .any(|x| matches!(x, Expression::AssignOp { .. }))
                        {
                            return Err(CompileError::NamedArgumentsNotImplemented);
                        }

                        match builtin_procs::eval(self, &ident, &args)? {
                            // Handled by builtin_procs
                            Some(kind) => kind,

                            // We've got to call a proc
                            None => {
                                let arg_count = args.len() as u32;

                                // Bring all arguments onto the stack
                                for arg in args {
                                    let expr = self.emit_expr(arg)?;
                                    self.emit_move_to_stack(expr)?;
                                }

                                // We're treating all Term::Call expressions as global calls
                                self.emit_ins(Instruction::CallGlob(
                                    arg_count,
                                    operands::Proc(format!("/proc/{}", ident)),
                                ));

                                EvalKind::Stack
                            }
                        }
                    }

                    other => return Err(CompileError::UnsupportedExpressionTerm(other)),
                };

                let kind = self.emit_follow(follow, kind)?;
                let kind = self.emit_unary_ops(unary, kind)?;

                return Ok(kind);
            }

            Expression::BinaryOp { op, lhs, rhs } => binary_ops::emit(self, op, *lhs, *rhs),
            Expression::AssignOp { op, lhs, rhs } => assignment::emit(self, op, *lhs, *rhs),
        }
    }
}

#[test]
fn compile_test() {
    let context: dreammaker::Context = Default::default();
    let lexer =
        dreammaker::lexer::Lexer::new(&context, Default::default(), "global.f()".as_bytes());
    //let code = dreammaker::indents::IndentProcessor::new(&context, lexer);
    let expr = dreammaker::parser::parse_expression(&context, Default::default(), lexer);
    context.assert_success();
    // println!("{:#?}\n\n\n", expr);

    let expr = compile_expr("a &&= 2", &["a"]);
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
