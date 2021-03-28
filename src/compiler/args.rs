use dreammaker::ast::Term;
use dreammaker::ast::{ListAccessKind, PropertyAccessKind};

use crate::compiler::*;
use crate::Instruction;

// (((x))) -> x
// ((-(x))) -> -(x)
// (((x).y)) -> (x).y
// (x = y) -> x = y
fn unroll(expr: &Expression) -> &Expression {
    match expr {
        Expression::Base {
            unary,
            term,
            follow,
        } => {
            if unary.is_empty() && follow.is_empty() {
                if let Term::Expr(inner) = &term.elem {
                    unroll(inner.as_ref());
                }
            }

            expr
        }

        _ => expr,
    }
}

// Only true if any short-circuit behaviour would step _over_ the entire expression
// x.y -> false
// x[x?.y] -> false
// x?.y -> true
// x?.y && z -> false
// x?.y = z -> true
fn can_short_circuit(expr: &Expression) -> bool {
    match expr {
        Expression::Base {
            unary: _,
            term: _,
            follow,
        } => {
            for follow in follow {
                match follow.elem {
                    Follow::Index(ListAccessKind::Safe, _) => return true,
                    Follow::Field(PropertyAccessKind::SafeDot, _) => return true,
                    Follow::Field(PropertyAccessKind::SafeColon, _) => return true,
                    Follow::Call(PropertyAccessKind::SafeDot, _, _) => return true,
                    Follow::Call(PropertyAccessKind::SafeColon, _, _) => return true,

                    _ => {}
                }
            }

            false
        }

        Expression::AssignOp { op: _, lhs, rhs: _ } => {
            return can_short_circuit(lhs);
        }

        Expression::BinaryOp { .. } => false,
        Expression::TernaryOp { .. } => false,
    }
}

fn has_assoc_argument(context: ArgsContext, args: &[Expression]) -> Result<bool, CompileError> {
    let mut found_associative = false;

    for arg in args {
        match context {
            // Proc arguments are only treated as associative if they contain an unwrapped assign operation where the LHS is not a nested expression
            ArgsContext::Proc => {
                match arg {
                    Expression::AssignOp {
                        op: AssignOp::Assign,
                        lhs,
                        rhs: _,
                    } => {
                        // BYOND's behaviour for short-circuiting ops here is mad, so I'm going to error instead of matching it
                        // TODO: Move to emit code?
                        if can_short_circuit(unroll(lhs)) {
                            return Err(CompileError::AmbiguousListConstructor);
                        }

                        match lhs.as_ref() {
                            Expression::Base {
                                unary: _,
                                term,
                                follow: _,
                            } => {
                                if !matches!(term.elem, Term::Expr(_)) {
                                    found_associative = true;
                                }
                            }

                            _ => {}
                        }
                    }

                    _ => {}
                }
            }

            // Lists arguments are treated as associative if they contain _any_ assign operation (after unrolling)
            ArgsContext::List => {
                match unroll(arg) {
                    Expression::AssignOp {
                        op: AssignOp::Assign,
                        lhs,
                        rhs: _,
                    } => {
                        // BYOND's behaviour for short-circuiting ops here is mad, so I'm going to error instead of matching it
                        // TODO: Move to emit code?
                        if can_short_circuit(unroll(lhs)) {
                            return Err(CompileError::AmbiguousListConstructor);
                        }

                        // Finding an assign op means these args are associative
                        found_associative = true;
                    }

                    _ => {}
                }
            }
        }
    }

    Ok(found_associative)
}

// TODO: The differences between these are... weird. We should pick a behaviour that counts as a sub-set of BYOND and enforce it with errors.
pub(super) enum ArgsContext {
    Proc,
    List,
}

pub(super) enum ArgsResult {
    // Each argument has been pushed on to the stack
    Normal,

    // Each argument has its key pushed on the stack followed by its value
    Assoc,

    // Similar to assoc, except the arg list itself has been pushed to the stack
    ArgList,
}

pub(super) fn emit(
    compiler: &mut Compiler,
    context: ArgsContext,
    args: Vec<Expression>,
) -> Result<ArgsResult, CompileError> {
    if has_assoc_argument(context, &args)? {
        return do_assoc(compiler, args);
    }

    do_normal(compiler, args)
}

pub(super) fn emit_single_normal(
    compiler: &mut Compiler,
    context: ArgsContext,
    arg: Expression,
) -> Result<(), CompileError> {
    emit_normal(compiler, context, vec![arg])
}

pub(super) fn emit_normal(
    compiler: &mut Compiler,
    context: ArgsContext,
    args: Vec<Expression>,
) -> Result<(), CompileError> {
    let result = emit(compiler, context, args)?;

    match result {
        ArgsResult::Normal => Ok(()),
        ArgsResult::Assoc => Err(CompileError::UnexpectedNamedArguments),
        ArgsResult::ArgList => Err(CompileError::UnexpectedArgList),
    }
}

fn do_normal(compiler: &mut Compiler, args: Vec<Expression>) -> Result<ArgsResult, CompileError> {
    let args_len = args.len();

    for arg in args {
        let expr = compiler.emit_expr(arg)?;

        // If our param is an arg list and it's the only param, we're an arg list!
        if let EvalKind::ArgList = expr {
            if args_len == 1 {
                return Ok(ArgsResult::ArgList);
            }
        }

        compiler.emit_move_to_stack(expr)?;
    }

    Ok(ArgsResult::Normal)
}

fn do_assoc(compiler: &mut Compiler, args: Vec<Expression>) -> Result<ArgsResult, CompileError> {
    // TODO: Lots of cloning in here :(
    for (idx, arg) in args.into_iter().enumerate() {
        let idx = idx + 1;

        // Handle `ident = {expr}` case first. The identifier turns into a string literal intead of being evaluated.
        if let Expression::AssignOp {
            op: AssignOp::Assign,
            lhs,
            rhs,
        } = &arg
        {
            if let Expression::Base {
                unary,
                term,
                follow,
            } = lhs.as_ref()
            {
                if unary.is_empty() && follow.is_empty() {
                    // TODO: BYOND would change null to "null" here.

                    if let Term::Ident(ident) = &term.elem {
                        compiler.emit_ins(Instruction::PushVal(Value::DMString(DMString(
                            ident.to_owned().into(),
                        ))));

                        let kind = compiler.emit_expr((**rhs).to_owned())?;
                        compiler.emit_move_to_stack(kind)?;
                        continue;
                    }
                }
            }
        }

        // Now we unroll. Cases like f((x = y)) will treat the lhs as an expression instead of an identifier
        let arg = unroll(&arg);

        match arg {
            // Assoc entry
            Expression::AssignOp {
                op: AssignOp::Assign,
                lhs,
                rhs,
            } => {
                let kind = compiler.emit_expr((**lhs).to_owned())?;
                compiler.emit_move_to_stack(kind)?;

                let kind = compiler.emit_expr((**rhs).to_owned())?;
                compiler.emit_move_to_stack(kind)?;
            }

            // Normal entry
            other => {
                compiler.emit_ins(Instruction::PushInt(idx as i32));

                let kind = compiler.emit_expr(other.to_owned())?;
                compiler.emit_move_to_stack(kind)?;
            }
        }
    }

    Ok(ArgsResult::Assoc)
}
