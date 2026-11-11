use dreammaker::ast::Expression;

use crate::{
    compiler::{CompileError, Compiler, EvalKind},
    operands::Label,
    Instruction,
};

pub(super) fn emit(
    compiler: &mut Compiler,
    condition: Expression,
    lhs: Expression,
    rhs: Expression,
) -> Result<EvalKind, CompileError> {
    // Bring condition to stack
    let logic = super::fold::is_logic(&condition);
    let condition = compiler.emit_expr(condition)?;
    compiler.emit_move_to_stack(condition)?;
    // `==`, `in` and is*() leave their result in the test flag (but not as
    // what a `&&` or `||` folded to: see `fold::is_logic`)
    if compiler.proc.is_some() && !logic {
        if let Some(crate::Node::Instruction(Instruction::GetFlag, _)) = compiler.nodes.last() {
            compiler.nodes.pop();
            return emit_branches(compiler, lhs, rhs);
        }
    }

    let label_rhs = format!("LAB_RHS_{:0>4X}", compiler.label_count);
    let label_end = format!("LAB_END_{:0>4X}", compiler.label_count);
    compiler.label_count += 1;

    compiler.emit_ins(Instruction::Test);
    emit_branches_after_test(compiler, lhs, rhs, label_rhs, label_end)
}

fn emit_branches(
    compiler: &mut Compiler,
    lhs: Expression,
    rhs: Expression,
) -> Result<EvalKind, CompileError> {
    let label_rhs = format!("LAB_RHS_{:0>4X}", compiler.label_count);
    let label_end = format!("LAB_END_{:0>4X}", compiler.label_count);
    compiler.label_count += 1;
    emit_branches_after_test(compiler, lhs, rhs, label_rhs, label_end)
}

fn emit_branches_after_test(
    compiler: &mut Compiler,
    lhs: Expression,
    rhs: Expression,
    label_rhs: String,
    label_end: String,
) -> Result<EvalKind, CompileError> {
    compiler.emit_ins(Instruction::Jz(Label::Named(label_rhs.clone())));

    // LHS
    let lhs = compiler.emit_expr(lhs)?;
    compiler.emit_move_to_stack(lhs)?;
    compiler.emit_ins(Instruction::Jmp(Label::Named(label_end.clone())));

    // RHS (DreamMaker doesn't know what the cache holds there)
    compiler.emit_label(label_rhs);
    compiler.cache = None;
    let rhs = compiler.emit_expr(rhs)?;
    compiler.emit_move_to_stack(rhs)?;

    // End
    compiler.emit_label(label_end);
    Ok(EvalKind::Stack)
}
