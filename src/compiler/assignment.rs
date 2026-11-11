use dreammaker::ast::{AssignOp, Expression, Follow, PropertyAccessKind};

use crate::{
    compiler::{is_writable, CompileError, Compiler, EvalKind},
    operands::{DMString, Label, Variable},
    Instruction,
};

// This is not meant to be recursive through all of the sub-expressions!
// We only care if the LHS of an assignment would result in the RHS potentially be skipped!
fn peek_is_conditional(expr: &Expression) -> bool {
    match expr {
        Expression::Base { term: _, follow } => {
            for follow in follow {
                match follow.elem {
                    Follow::Index(kind, _) => match kind {
                        dreammaker::ast::ListAccessKind::Normal => {}
                        dreammaker::ast::ListAccessKind::Safe => return true,
                    },

                    Follow::Field(kind, _) => match kind {
                        PropertyAccessKind::Dot
                        | PropertyAccessKind::Colon
                        | PropertyAccessKind::Scope => {}
                        PropertyAccessKind::SafeDot | PropertyAccessKind::SafeColon => return true,
                    },

                    // We can't assign to a call's result, but I'm adding this to make the function correct anyway.
                    Follow::Call(kind, _, _) => match kind {
                        PropertyAccessKind::Dot
                        | PropertyAccessKind::Colon
                        | PropertyAccessKind::Scope => {}
                        PropertyAccessKind::SafeDot | PropertyAccessKind::SafeColon => return true,
                    },

                    Follow::StaticField(_) => {}
                    Follow::ProcReference(_) => {}
                    Follow::Unary(_) => {}
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
    compiler.place = op != AssignOp::Assign;
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

        _ => return Err(CompileError::ExpectedLValue),
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
        | AssignOp::RShiftAssign
        | AssignOp::FloatModAssign => {
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
                AssignOp::FloatModAssign => {
                    compiler.emit_ins(Instruction::AugFloatMod(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                _ => unreachable!(),
            };
        }

        AssignOp::AndAssign | AssignOp::OrAssign => {
            let label = format!("LAB_{:0>4X}", compiler.label_count);
            compiler.label_count += 1;

            let test_ins = match op {
                AssignOp::AndAssign => Instruction::JmpAnd(Label::Named(label.clone())),
                AssignOp::OrAssign => Instruction::JmpOr(Label::Named(label.clone())),
                _ => unreachable!(),
            };

            compiler.emit_ins(Instruction::GetVar(var.clone()));
            compiler.emit_ins(test_ins);

            // Push holder - We'll need it later
            compiler.emit_ins(Instruction::PushCache);
            compiler.emit_ins(Instruction::PushCacheKey);

            let rhs = compiler.emit_expr(rhs)?;
            compiler.emit_move_to_stack(rhs)?;

            // These ops require an l-value
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
    // `(x) = y` assigns to x
    let mut lhs = lhs;
    while let Expression::Base { term, follow } = &lhs {
        match &term.elem {
            dreammaker::ast::Term::Expr(inner) if follow.is_empty() => lhs = (**inner).clone(),
            _ => break,
        }
    }
    // Conditional assignments (x?.y = z) take a different path
    if peek_is_conditional(&lhs) && !matches!(op, AssignOp::AndAssign | AssignOp::OrAssign) {
        use super::statements::SafeWrite;
        let to_field = move |var: Variable| {
            if op == AssignOp::Assign {
                vec![Instruction::SetVarExpr(var)]
            } else {
                vec![
                    super::statements::aug_instruction(op, var),
                    Instruction::PushEval,
                ]
            }
        };
        let element = matches!(&lhs, Expression::Base { follow, .. }
            if matches!(follow.last().map(|f| &f.elem), Some(Follow::Index(..))));
        let write = if element && op == AssignOp::Assign {
            SafeWrite::Assign
        } else {
            SafeWrite::Change(&to_field)
        };
        if super::statements::emit_safe_write(compiler, &lhs, Some(&rhs), write, true)? {
            return Ok(EvalKind::Stack);
        }
    }
    if peek_is_conditional(&lhs)
        && matches!(op, AssignOp::AndAssign | AssignOp::OrAssign)
        && super::statements::emit_safe_logical_assign(
            compiler,
            &lhs,
            &rhs,
            op == AssignOp::OrAssign,
        )?
    {
        return Ok(EvalKind::Stack);
    }
    if peek_is_conditional(&lhs) {
        return emit_conditional(compiler, op, lhs, rhs);
    }
    super::statements::warn_pointer_index(compiler, &lhs);

    if compiler.proc.is_some() {
        if let Some(kind) = emit_proc_assignment(compiler, op, &lhs, &rhs)? {
            return Ok(kind);
        }
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
        | AssignOp::RShiftAssign
        | AssignOp::FloatModAssign => {
            // RHS evalutes before LHS for these assignments
            let rhs = compiler.emit_expr(rhs)?;
            compiler.emit_move_to_stack(rhs)?;

            let var = match compiler.emit_place(op, lhs)? {
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
                AssignOp::FloatModAssign => {
                    compiler.emit_ins(Instruction::AugFloatMod(var));
                    compiler.emit_ins(Instruction::PushEval);
                }
                _ => unreachable!(),
            };
        }

        AssignOp::AndAssign | AssignOp::OrAssign => {
            let label = format!("LAB_{:0>4X}", compiler.label_count);
            compiler.label_count += 1;

            // LHS first for this fucker
            let lhs = compiler.emit_place(op, lhs)?;

            enum CacheKind {
                Var(Variable),
                Field(String),
                ListRef,
            }

            let test_ins = match op {
                AssignOp::AndAssign => Instruction::JmpAnd(Label::Named(label.clone())),
                AssignOp::OrAssign => Instruction::JmpOr(Label::Named(label.clone())),
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

pub(super) fn is_list_index(expr: &Expression) -> bool {
    matches!(expr, Expression::Base { follow, .. }
        if matches!(follow.last().map(|f| &f.elem), Some(Follow::Index(dreammaker::ast::ListAccessKind::Normal, _))))
}

/// Compile assignments used as values. List writes copy with PushTop before
/// ListSet; logical assignments read and conditionally replace the target.
fn emit_proc_assignment(
    compiler: &mut Compiler<'_>,
    op: AssignOp,
    lhs: &Expression,
    rhs: &Expression,
) -> Result<Option<EvalKind>, CompileError> {
    let list = is_list_index(lhs);
    let store = |compiler: &mut Compiler<'_>| -> Result<(), CompileError> {
        if list {
            compiler.emit_ins(Instruction::PushTop);
            let kind = compiler.emit_place(op, lhs.clone())?;
            if kind != EvalKind::ListRef {
                return Err(CompileError::ExpectedLValue);
            }
            compiler.emit_ins(Instruction::ListSet);
            return Ok(());
        }
        let start = compiler.nodes.len();
        let var = match compiler.emit_place(op, lhs.clone())? {
            EvalKind::Var(var) if is_writable(&var) => var,
            EvalKind::Field(builder, field) => builder.get_field(DMString(field.into())),
            _ => return Err(CompileError::ExpectedLValue),
        };
        // Copy the RHS before resolving a computed field target or pointer,
        // as for list-element assignment.
        let computed = matches!(var, Variable::PtrDeref(_))
            || compiler.nodes[start..].iter().any(|n| {
                matches!(
                    n,
                    crate::Node::Instruction(Instruction::SetVar(Variable::Cache), _)
                )
            });
        if computed {
            compiler
                .nodes
                .insert(start, crate::Node::Instruction(Instruction::PushTop, ()));
            compiler.emit_ins(Instruction::SetVar(var));
        } else {
            compiler.emit_ins(Instruction::SetVarExpr(var));
        }
        Ok(())
    };
    match op {
        AssignOp::Assign => {
            compiler.implicit_type = compiler.static_type_name(lhs);
            let kind = compiler.emit_expr(rhs.clone());
            compiler.implicit_type = None;
            compiler.emit_move_to_stack(kind?)?;
            store(compiler)?;
        }
        // Resolve the target once and save its cached receiver across an RHS
        // that uses the cache.
        AssignOp::AndAssign | AssignOp::OrAssign => {
            let end = compiler.new_label();
            let (var, cached) = match compiler.emit_place(op, lhs.clone())? {
                EvalKind::ListRef => {
                    compiler.emit_ins(Instruction::SetVar(Variable::CacheKey));
                    compiler.emit_ins(Instruction::SetVar(Variable::Cache));
                    (Variable::CacheIndex, true)
                }
                EvalKind::Field(builder, field) => {
                    (builder.get_field(DMString(field.into())), true)
                }
                EvalKind::Var(var) if is_writable(&var) => (var, false),
                _ => return Err(CompileError::ExpectedLValue),
            };
            compiler.emit_ins(Instruction::GetVar(var.clone()));
            compiler.emit_ins(if op == AssignOp::OrAssign {
                Instruction::JmpOr(Label::Named(end.clone()))
            } else {
                Instruction::JmpAnd(Label::Named(end.clone()))
            });
            // (a chain through fields the test loaded isn't reused by the
            // value, and the assignment writes it out in full)
            compiler.end_assignment_statement();
            let start = compiler.nodes.len();
            compiler.implicit_type = compiler.static_type_name(lhs);
            let kind = compiler.emit_expr(rhs.clone())?;
            compiler.emit_move_to_stack(kind)?;
            compiler.implicit_type = None;
            // (DreamMaker goes by the source, even what folding leaves out)
            let touches = |compiler: &Compiler<'_>, start: usize| {
                compiler.nodes[start..]
                    .iter()
                    .any(|n| matches!(n, crate::Node::Instruction(i, _) if super::procs::touches_cache(i)))
            };
            let saves = cached
                && (touches(compiler, start)
                    || (compiler.proc.is_some()
                        && super::statements::unfolded_has(compiler, rhs, touches)));
            if saves {
                compiler
                    .nodes
                    .insert(start, crate::Node::Instruction(Instruction::PushCache, ()));
                compiler.emit_ins(Instruction::PopCache);
            }
            compiler.emit_ins(Instruction::SetVarExpr(var));
            compiler.emit_label(end);
        }
        // `(x += y)` as a value: the result is fetched after
        AssignOp::AddAssign
        | AssignOp::SubAssign
        | AssignOp::MulAssign
        | AssignOp::DivAssign
        | AssignOp::ModAssign
        | AssignOp::FloatModAssign
        | AssignOp::BitAndAssign
        | AssignOp::BitOrAssign
        | AssignOp::BitXorAssign
        | AssignOp::LShiftAssign
        | AssignOp::RShiftAssign => {
            let kind = compiler.emit_expr(rhs.clone())?;
            compiler.emit_move_to_stack(kind)?;
            let var = match compiler.emit_place(op, lhs.clone())? {
                EvalKind::ListRef => {
                    compiler.emit_ins(Instruction::SetVar(Variable::CacheKey));
                    compiler.emit_ins(Instruction::SetVar(Variable::Cache));
                    Variable::CacheIndex
                }
                EvalKind::Field(builder, field) => builder.get_field(DMString(field.into())),
                EvalKind::Var(var) if is_writable(&var) => var,
                _ => return Err(CompileError::ExpectedLValue),
            };
            compiler.emit_ins(super::statements::aug_instruction(op, var));
            compiler.emit_ins(Instruction::PushEval);
        }
        _ => return Ok(None),
    }
    Ok(Some(EvalKind::Stack))
}
