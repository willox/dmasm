use dreammaker::ast::Expression;

use crate::compiler::*;
use crate::Instruction;

pub(super) fn emit(
    compiler: &mut Compiler<'_>,
    op: BinaryOp,
    lhs: Expression,
    rhs: Expression,
) -> Result<EvalKind, CompileError> {
    let kind = match op {
        // Short circuiting logic ops
        BinaryOp::And | BinaryOp::Or => {
            // Bring LHS to stack
            let lhs = compiler.emit_expr(lhs)?;
            compiler.emit_move_to_stack(lhs)?;

            let label = format!("LAB_{:0>4X}", compiler.label_count);
            compiler.label_count += 1;

            let test_ins = match op {
                BinaryOp::And => Instruction::JmpAnd(Label(label.clone())),
                BinaryOp::Or => Instruction::JmpOr(Label(label.clone())),
                _ => unreachable!(),
            };

            compiler.emit_ins(test_ins);

            // Bring RHS to stack
            let rhs = compiler.emit_expr(rhs)?;
            compiler.emit_move_to_stack(rhs)?;

            compiler.emit_label(label);
            EvalKind::Stack
        }

        // Simple stack operations
        BinaryOp::Add
        | BinaryOp::Sub
        | BinaryOp::Mul
        | BinaryOp::Div
        | BinaryOp::Pow
        | BinaryOp::Mod
        | BinaryOp::Eq
        | BinaryOp::NotEq
        | BinaryOp::Less
        | BinaryOp::LessEq
        | BinaryOp::Greater
        | BinaryOp::GreaterEq
        | BinaryOp::Equiv
        | BinaryOp::NotEquiv
        | BinaryOp::BitAnd
        | BinaryOp::BitXor
        | BinaryOp::BitOr
        | BinaryOp::LShift
        | BinaryOp::RShift
        | BinaryOp::FloatMod => {
            // Bring LHS to stack
            let lhs = compiler.emit_expr(lhs)?;
            compiler.emit_move_to_stack(lhs)?;

            // Bring RHS to stack
            let rhs = compiler.emit_expr(rhs)?;
            compiler.emit_move_to_stack(rhs)?;

            match op {
                BinaryOp::Add => compiler.emit_ins(Instruction::Add),
                BinaryOp::Sub => compiler.emit_ins(Instruction::Sub),
                BinaryOp::Mul => compiler.emit_ins(Instruction::Mul),
                BinaryOp::Div => compiler.emit_ins(Instruction::Div),
                BinaryOp::Pow => compiler.emit_ins(Instruction::Pow),
                BinaryOp::Mod => compiler.emit_ins(Instruction::Mod),
                BinaryOp::Eq => {
                    compiler.emit_ins(Instruction::Teq);
                    compiler.emit_ins(Instruction::Pop);
                    compiler.emit_ins(Instruction::GetFlag);
                }
                BinaryOp::NotEq => compiler.emit_ins(Instruction::Tne),
                BinaryOp::Less => compiler.emit_ins(Instruction::Tl),
                BinaryOp::LessEq => compiler.emit_ins(Instruction::Tle),
                BinaryOp::Greater => compiler.emit_ins(Instruction::Tg),
                BinaryOp::GreaterEq => compiler.emit_ins(Instruction::Tge),
                BinaryOp::Equiv => compiler.emit_ins(Instruction::TestEquiv),
                BinaryOp::NotEquiv => compiler.emit_ins(Instruction::TestNotEquiv),
                BinaryOp::BitAnd => compiler.emit_ins(Instruction::Band),
                BinaryOp::BitXor => compiler.emit_ins(Instruction::Bxor),
                BinaryOp::BitOr => compiler.emit_ins(Instruction::Bor),
                BinaryOp::LShift => compiler.emit_ins(Instruction::LShift),
                BinaryOp::RShift => compiler.emit_ins(Instruction::RShift),
                BinaryOp::FloatMod => compiler.emit_ins(Instruction::FloatMod),
                _ => unreachable!(),
            }

            EvalKind::Stack
        }

        BinaryOp::In => {
            match compiler.emit_expr(rhs)? {
                EvalKind::Range => {
                    // Bring LHS to stack (RHS already on stack)
                    let lhs = compiler.emit_expr(lhs)?;
                    compiler.emit_move_to_stack(lhs)?;
                    compiler.emit_ins(Instruction::IsIn(operands::IsInParams::Range))
                }

                other => {
                    // Bring RHS to stack
                    compiler.emit_move_to_stack(other)?;

                    // Bring LHS to stack
                    let lhs = compiler.emit_expr(lhs)?;
                    compiler.emit_move_to_stack(lhs)?;

                    compiler.emit_ins(Instruction::IsIn(operands::IsInParams::Value));
                }
            }

            compiler.emit_ins(Instruction::GetFlag);
            EvalKind::Stack
        }

        BinaryOp::To => {
            // Bring LHS to stack
            let lhs = compiler.emit_expr(lhs)?;
            compiler.emit_move_to_stack(lhs)?;

            // Bring RHS to stack
            let rhs = compiler.emit_expr(rhs)?;
            compiler.emit_move_to_stack(rhs)?;

            EvalKind::Range
        }
    };

    Ok(kind)
}
