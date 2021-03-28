use crate::compiler::*;
use crate::Instruction;

pub(super) fn emit(
    compiler: &mut Compiler,
    condition: Expression,
    lhs: Expression,
    rhs: Expression,
) -> Result<EvalKind, CompileError> {
    // Bring condition to stack
    let condition = compiler.emit_expr(condition)?;
    compiler.emit_move_to_stack(condition)?;

    let label_rhs = format!("LAB_RHS_{:0>4X}", compiler.label_count);
    let label_end = format!("LAB_END_{:0>4X}", compiler.label_count);
    compiler.label_count += 1;

    compiler.emit_ins(Instruction::Test);
    compiler.emit_ins(Instruction::Jz(Label(label_rhs.clone())));

    // LHS
    let lhs = compiler.emit_expr(lhs)?;
    compiler.emit_move_to_stack(lhs)?;
    compiler.emit_ins(Instruction::Jmp(Label(label_end.clone())));

    // RHS
    compiler.emit_label(label_rhs);
    let rhs = compiler.emit_expr(rhs)?;
    compiler.emit_move_to_stack(rhs)?;

    // End
    compiler.emit_label(label_end);
    Ok(EvalKind::Stack)
}
