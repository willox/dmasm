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

    Ok(kind)
}

fn emit_single(
    compiler: &mut Compiler,
    op: UnaryOp,
    kind: EvalKind,
) -> Result<EvalKind, CompileError> {
    let mut return_kind = EvalKind::Stack;
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
        UnaryOp::PreIncr
        | UnaryOp::PostIncr
        | UnaryOp::PreDecr
        | UnaryOp::PostDecr
        | UnaryOp::Reference
        | UnaryOp::Dereference => {
            let label = format!("LAB_{:0>4X}", compiler.label_count);
            compiler.label_count += 1;

            // These ops require an l-value
            let var = match kind {
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
                UnaryOp::PreIncr => compiler.emit_ins(Instruction::PreInc(var)),
                UnaryOp::PostIncr => compiler.emit_ins(Instruction::PostInc(var)),
                UnaryOp::PreDecr => compiler.emit_ins(Instruction::PreDec(var)),
                UnaryOp::PostDecr => compiler.emit_ins(Instruction::PostDec(var)),
                UnaryOp::Reference => return_kind = EvalKind::Var(Variable::PtrRef(Box::new(var))),
                UnaryOp::Dereference => {
                    return_kind = EvalKind::Var(Variable::PtrDeref(Box::new(var)))
                }
                _ => unreachable!(),
            }

            compiler.emit_label(label);
        }
    }

    Ok(return_kind)
}
