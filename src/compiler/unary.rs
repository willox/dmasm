use crate::compiler::*;
use crate::Instruction;

pub(super) fn emit(
    compiler: &mut Compiler,
    unary: Vec<UnaryOp>,
    kind: EvalKind,
) -> Result<EvalKind, CompileError> {

    let mut kind = kind;
    for op in unary.into_iter().rev() {
        kind = emit_single(compiler, op, kind)?;
    }

    return Ok(kind);
}

fn emit_single(
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
