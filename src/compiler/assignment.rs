use dreammaker::ast::Expression;

use crate::compiler::*;
use crate::Instruction;

// This is not meant to be recursive through all of the sub-expressions!
// We only care if the LHS of an assignment would result in the RHS potentially be skipped!
fn peek_is_conditional(expr: &Expression) -> bool {
    match expr {
        Expression::Base {
            unary,
            term,
            follow,
        } => {
            for follow in follow {
                match follow.elem {
                    Follow::Index(kind, _) => match kind {
                        dreammaker::ast::ListAccessKind::Normal => {}
                        dreammaker::ast::ListAccessKind::Safe => return true,
                    },

                    Follow::Field(kind, _) => match kind {
                        PropertyAccessKind::Dot | PropertyAccessKind::Colon => {}
                        PropertyAccessKind::SafeDot | PropertyAccessKind::SafeColon => return true,
                    },

                    // We can't assign to a call's result, but I'm adding this to make the function correct anyway.
                    Follow::Call(kind, _, _) => match kind {
                        PropertyAccessKind::Dot | PropertyAccessKind::Colon => {}
                        PropertyAccessKind::SafeDot | PropertyAccessKind::SafeColon => return true,
                    },
                }
            }
        }

        Expression::BinaryOp { .. } => {}
        Expression::AssignOp { .. } => {}
        Expression::TernaryOp { .. } => {}
    }

    false
}

fn emit_conditional(
    compiler: &mut Compiler<'_>,
    op: AssignOp,
    lhs: Expression,
    rhs: Expression,
) -> Result<EvalKind, CompileError> {
    let var = match compiler.emit_inner_expr(lhs)? {
        EvalKind::Field(builder, field) => {
            let holder = builder.get();
            compiler.emit_ins(Instruction::GetVar(holder));
            compiler.emit_ins(Instruction::SetVar(Variable::Cache));

            Variable::Field(DMString(field.into()))
        }

        EvalKind::ListRef => {
            compiler.emit_ins(Instruction::SetVar(Variable::CacheKey));
            compiler.emit_ins(Instruction::SetVar(Variable::Cache));

            Variable::CacheIndex
        }

        _ => unreachable!(),
    };

    match op {
        AssignOp::Assign
        | AssignOp::AddAssign
        | AssignOp::SubAssign
        | AssignOp::MulAssign
        | AssignOp::DivAssign
        | AssignOp::ModAssign
        | AssignOp::AssignInto
        | AssignOp::BitAndAssign
        | AssignOp::BitOrAssign
        | AssignOp::BitXorAssign
        | AssignOp::LShiftAssign
        | AssignOp::RShiftAssign => {
            // Push holder - We'll need it later
            compiler.emit_ins(Instruction::PushCache);
            compiler.emit_ins(Instruction::PushCacheKey);

            let rhs = compiler.emit_expr(rhs)?;
            compiler.emit_move_to_stack(rhs)?;

            // Pop holder
            compiler.emit_ins(Instruction::PopCacheKey);
            compiler.emit_ins(Instruction::PopCache);

            match op {
                AssignOp::Assign => compiler.emit_ins(Instruction::SetVarExpr(var)),
                AssignOp::AddAssign => {
                    compiler.emit_ins(Instruction::AugAdd(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                AssignOp::SubAssign => {
                    compiler.emit_ins(Instruction::AugSub(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                AssignOp::MulAssign => {
                    compiler.emit_ins(Instruction::AugMul(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                AssignOp::DivAssign => {
                    compiler.emit_ins(Instruction::AugDiv(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                AssignOp::ModAssign => {
                    compiler.emit_ins(Instruction::AugMod(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                AssignOp::AssignInto => {
                    compiler.emit_ins(Instruction::AssignInto(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                AssignOp::BitAndAssign => {
                    compiler.emit_ins(Instruction::AugBand(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                AssignOp::BitOrAssign => {
                    compiler.emit_ins(Instruction::AugBor(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                AssignOp::BitXorAssign => {
                    compiler.emit_ins(Instruction::AugXor(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                AssignOp::LShiftAssign => {
                    compiler.emit_ins(Instruction::AugLShift(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                AssignOp::RShiftAssign => {
                    compiler.emit_ins(Instruction::AugRShift(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                _ => unreachable!(),
            };
        }

        AssignOp::AndAssign | AssignOp::OrAssign => {
            let label = format!("LAB_{:0>4X}", compiler.label_count);
            compiler.label_count += 1;

            let test_ins = match op {
                AssignOp::AndAssign => Instruction::JmpAnd(Label(label.clone())),
                AssignOp::OrAssign => Instruction::JmpOr(Label(label.clone())),
                _ => unreachable!(),
            };

            compiler.emit_ins(Instruction::GetVar(var.clone()));
            compiler.emit_ins(test_ins);

            // Push holder - We'll need it later
            compiler.emit_ins(Instruction::PushCache);
            compiler.emit_ins(Instruction::PushCacheKey);

            let rhs = compiler.emit_expr(rhs)?;
            compiler.emit_move_to_stack(rhs)?;

            compiler.emit_ins(Instruction::PopCacheKey);
            compiler.emit_ins(Instruction::PopCache);
            compiler.emit_ins(Instruction::SetVarExpr(var));

            compiler.emit_label(label);
        }
    }

    Ok(EvalKind::Stack)
}

pub(super) fn emit(
    compiler: &mut Compiler<'_>,
    op: AssignOp,
    lhs: Expression,
    rhs: Expression,
) -> Result<EvalKind, CompileError> {
    // Conditional assignments (x?.y = z) take a different path
    if peek_is_conditional(&lhs) {
        return emit_conditional(compiler, op, lhs, rhs);
    }

    match op {
        AssignOp::Assign
        | AssignOp::AddAssign
        | AssignOp::SubAssign
        | AssignOp::MulAssign
        | AssignOp::DivAssign
        | AssignOp::ModAssign
        | AssignOp::AssignInto
        | AssignOp::BitAndAssign
        | AssignOp::BitOrAssign
        | AssignOp::BitXorAssign
        | AssignOp::LShiftAssign
        | AssignOp::RShiftAssign => {
            // RHS evalutes before LHS for these assignments
            let rhs = compiler.emit_expr(rhs)?;
            compiler.emit_move_to_stack(rhs)?;

            // These ops require an l-value
            let var = match compiler.emit_expr(lhs)? {
                EvalKind::Var(var) if is_writable(&var) => var,

                EvalKind::Field(builder, field) => builder.get_field(DMString(field.into())),

                EvalKind::ListRef => {
                    compiler.emit_ins(Instruction::SetVar(Variable::CacheKey));
                    compiler.emit_ins(Instruction::SetVar(Variable::Cache));
                    Variable::CacheIndex
                }

                _ => return Err(CompileError::ExpectedLValue),
            };

            match op {
                AssignOp::Assign => compiler.emit_ins(Instruction::SetVarExpr(var)),
                AssignOp::AddAssign => {
                    compiler.emit_ins(Instruction::AugAdd(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                AssignOp::SubAssign => {
                    compiler.emit_ins(Instruction::AugSub(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                AssignOp::MulAssign => {
                    compiler.emit_ins(Instruction::AugMul(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                AssignOp::DivAssign => {
                    compiler.emit_ins(Instruction::AugDiv(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                AssignOp::ModAssign => {
                    compiler.emit_ins(Instruction::AugMod(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                AssignOp::AssignInto => {
                    compiler.emit_ins(Instruction::AssignInto(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                AssignOp::BitAndAssign => {
                    compiler.emit_ins(Instruction::AugBand(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                AssignOp::BitOrAssign => {
                    compiler.emit_ins(Instruction::AugBor(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                AssignOp::BitXorAssign => {
                    compiler.emit_ins(Instruction::AugXor(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                AssignOp::LShiftAssign => {
                    compiler.emit_ins(Instruction::AugLShift(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                AssignOp::RShiftAssign => {
                    compiler.emit_ins(Instruction::AugRShift(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                _ => unreachable!(),
            };
        }

        AssignOp::AndAssign | AssignOp::OrAssign => {
            let label = format!("LAB_{:0>4X}", compiler.label_count);
            compiler.label_count += 1;

            // LHS first for this fucker
            let lhs = compiler.emit_expr(lhs)?;

            enum CacheKind {
                Var(Variable),
                Field(String),
                ListRef,
            }

            let test_ins = match op {
                AssignOp::AndAssign => Instruction::JmpAnd(Label(label.clone())),
                AssignOp::OrAssign => Instruction::JmpOr(Label(label.clone())),
                _ => unreachable!(),
            };

            // We need the l-value for later
            let assign_kind = match lhs {
                EvalKind::Var(var) if is_writable(&var) => {
                    compiler.emit_ins(Instruction::GetVar(var.clone()));
                    compiler.emit_ins(test_ins);
                    CacheKind::Var(var)
                }

                EvalKind::Field(builder, field) => {
                    compiler.emit_ins(Instruction::GetVar(
                        builder.get_field(DMString(field.clone().into())),
                    ));
                    compiler.emit_ins(test_ins);
                    compiler.emit_ins(Instruction::PushCache);
                    CacheKind::Field(field)
                }

                EvalKind::ListRef => {
                    compiler.emit_ins(Instruction::SetVar(Variable::CacheKey));
                    compiler.emit_ins(Instruction::SetVar(Variable::Cache));
                    compiler.emit_ins(Instruction::GetVar(Variable::CacheIndex));
                    compiler.emit_ins(test_ins);
                    compiler.emit_ins(Instruction::PushCache);
                    compiler.emit_ins(Instruction::PushCacheKey);
                    CacheKind::ListRef
                }

                _ => return Err(CompileError::ExpectedLValue),
            };

            let rhs = compiler.emit_expr(rhs)?;
            compiler.emit_move_to_stack(rhs)?;

            match assign_kind {
                CacheKind::Var(var) => {
                    compiler.emit_ins(Instruction::SetVarExpr(var));
                }

                CacheKind::Field(field) => {
                    compiler.emit_ins(Instruction::PopCache);
                    compiler.emit_ins(Instruction::SetVarExpr(Variable::Field(DMString(
                        field.into(),
                    ))))
                }

                CacheKind::ListRef => {
                    compiler.emit_ins(Instruction::PopCacheKey);
                    compiler.emit_ins(Instruction::PopCache);
                    compiler.emit_ins(Instruction::SetVarExpr(Variable::CacheIndex));
                }
            }

            compiler.emit_label(label);
        }
    }

    Ok(EvalKind::Stack)
}
