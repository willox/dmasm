//! Compile statements using DreamMaker's control-flow and cache rules.

use dreammaker::ast::{
    AssignOp, Block, Expression, Follow, Spanned, Statement, Term, UnaryOp, VarStatement,
};

use super::procs::{LoopLabels, ProcSource};
use super::{CompileError, Compiler, EvalKind};
use crate::operands::{Label, Value, Variable};
use crate::{Instruction, Node};

pub(super) fn emit_proc_body(
    compiler: &mut Compiler,
    source: &ProcSource,
    code: &Block,
) -> Result<(), CompileError> {
    // The line DreamMaker gives code that precedes the first statement
    let first_line = code
        .first()
        .map(|stmt| stmt.location.line)
        .unwrap_or(source.value.location.line + 1);

    compiler.emit_ins(Instruction::DbgLine(first_line));
    label_iterators(code, 0, &mut ctx(compiler).label_iterators);

    // Default arguments: `if (isnull(arg)) arg = default`, at the header's line
    let params = source
        .value
        .parameters
        .iter()
        .filter(|p| !p.absolute && p.name.as_str() != "...");
    for (index, param) in params.enumerate() {
        let Some(default) = &param.default else {
            continue;
        };
        let arg = Variable::Arg(index as u32);
        let skip = compiler.new_label();
        compiler.emit_ins(Instruction::GetVar(arg.clone()));
        compiler.emit_ins(Instruction::IsNull);
        compiler.emit_ins(Instruction::Test);
        compiler.emit_ins(Instruction::Jz(Label::Named(skip.clone())));
        compiler.emit_ins(Instruction::DbgLine(param.location.line));
        if path_division(default) {
            compiler.warn(&format!(
                "parameter {}'s default divides a var by a number: written without spaces \
                 (`a.b/10`), DreamMaker reads it as a path and leaves the division out; dmasm divides",
                param.name
            ));
        }
        // `x = a in L` is the default a, with L what a verb's argument is
        // picked from: the `in` isn't the default's
        let default = match default {
            Expression::BinaryOp {
                op: dreammaker::ast::BinaryOp::In,
                lhs,
                ..
            } => &**lhs,
            other => other,
        };
        // (`datum/x = new` makes the parameter's type)
        compiler.implicit_type = implicit_type_for(compiler, &param.var_type.type_path.to_string());
        let kind = compiler.emit_expr(default.clone());
        compiler.implicit_type = None;
        let kind = kind?;
        compiler.emit_move_to_stack(kind)?;
        compiler.emit_ins(Instruction::SetVar(arg));
        compiler.emit_label(skip);
    }

    let has_defaults = source.value.parameters.iter().any(|p| p.default.is_some());
    emit_block_inner(compiler, code, !has_defaults)
}

fn emit_block(compiler: &mut Compiler, block: &Block) -> Result<(), CompileError> {
    emit_block_inner(compiler, block, false)
}

/// `skip_first_line`: the first statement's DbgLine was already emitted.
/// How many for-in iterators are active at each label of `block`.
fn label_iterators(block: &Block, depth: u32, out: &mut std::collections::HashMap<String, u32>) {
    for stmt in block.iter() {
        match &stmt.elem {
            Statement::Label { name, block } => {
                out.insert(name.as_str().to_owned(), depth);
                label_iterators(block, depth, out);
            }
            Statement::ForList(f) => {
                let counts = !matches!(
                    &f.in_list,
                    Some(Expression::BinaryOp {
                        op: dreammaker::ast::BinaryOp::To,
                        ..
                    })
                );
                label_iterators(&f.block, depth + counts as u32, out);
            }
            Statement::ForKeyValue(f) => label_iterators(&f.block, depth + 1, out),
            Statement::ForRange(f) => label_iterators(&f.block, depth, out),
            Statement::ForLoop { block, .. }
            | Statement::ForInfinite { block }
            | Statement::While { block, .. }
            | Statement::DoWhile { block, .. } => label_iterators(block, depth, out),
            Statement::If { arms, else_arm } => {
                for (_, block) in arms.iter() {
                    label_iterators(block, depth, out);
                }
                if let Some(block) = else_arm {
                    label_iterators(block, depth, out);
                }
            }
            Statement::Switch { cases, default, .. } => {
                for (_, block) in cases.iter() {
                    label_iterators(block, depth, out);
                }
                if let Some(block) = default {
                    label_iterators(block, depth, out);
                }
            }
            Statement::TryCatch {
                try_block,
                catch_block,
                ..
            } => {
                label_iterators(try_block, depth, out);
                label_iterators(catch_block, depth, out);
            }
            Statement::Spawn { block, .. } => label_iterators(block, depth, out),
            _ => {}
        }
    }
}

fn emit_block_inner(
    compiler: &mut Compiler,
    block: &Block,
    skip_first_line: bool,
) -> Result<(), CompileError> {
    compiler.proc.as_mut().unwrap().scopes.push(vec![]);
    let mut skip_next = false;
    for (i, stmt) in block.iter().enumerate() {
        // `spawn` without a block of its own spawns the statement after it
        if skip_next {
            skip_next = false;
            continue;
        }
        if !(skip_first_line && i == 0) {
            compiler.emit_ins(Instruction::DbgLine(stmt.location.line));
        }
        if let Statement::Spawn { delay, block: body } = &stmt.elem {
            if body.is_empty() && i + 1 < block.len() {
                let next = Statement::Spawn {
                    delay: delay.clone(),
                    block: block[i + 1..i + 2].to_vec().into(),
                };
                emit_statement(compiler, &next, stmt.location.line).map_err(|e| match e {
                    CompileError::AtLine(..) => e,
                    e => CompileError::AtLine(stmt.location.line, Box::new(e)),
                })?;
                skip_next = true;
                continue;
            }
        }
        emit_statement(compiler, &stmt.elem, stmt.location.line).map_err(|e| match e {
            CompileError::AtLine(..) => e,
            e => CompileError::AtLine(stmt.location.line, Box::new(e)),
        })?;
    }
    compiler.proc.as_mut().unwrap().scopes.pop();
    Ok(())
}

fn ctx<'c, 'a>(compiler: &'c mut Compiler<'a>) -> &'c mut super::procs::ProcCtx<'a> {
    compiler.proc.as_mut().unwrap()
}

fn emit_value(compiler: &mut Compiler, expr: &Expression) -> Result<(), CompileError> {
    let kind = compiler.emit_expr(expr.clone())?;
    compiler.emit_move_to_stack(kind)?;
    Ok(())
}

fn emit_condition_jz(
    compiler: &mut Compiler,
    expr: &Expression,
    target: &str,
) -> Result<(), CompileError> {
    emit_condition(compiler, expr)?;
    compiler.emit_ins(Instruction::Jz(Label::Named(target.to_owned())));
    Ok(())
}

/// Evaluates a condition into the test flag. `==` and `in` set the flag
/// themselves, so DreamMaker doesn't fetch it into a value and test it again.
fn emit_condition(compiler: &mut Compiler, expr: &Expression) -> Result<(), CompileError> {
    compiler.condition_chain = matches!(
        expr,
        Expression::BinaryOp {
            op: dreammaker::ast::BinaryOp::And | dreammaker::ast::BinaryOp::Or,
            ..
        }
    );
    emit_value(compiler, expr)?;
    compiler.condition_chain = false;
    // (not what a `&&` or `||` folded to: see `fold::is_logic`)
    if let (false, Some(Node::Instruction(Instruction::GetFlag, _))) =
        (super::fold::is_logic(expr), compiler.nodes.last())
    {
        compiler.nodes.pop();
        return Ok(());
    }
    compiler.emit_ins(Instruction::Test);
    Ok(())
}

fn emit_statement(
    compiler: &mut Compiler,
    stmt: &Statement,
    line: u32,
) -> Result<(), CompileError> {
    if matches!(
        stmt,
        Statement::ForLoop { .. }
            | Statement::ForList(_)
            | Statement::ForRange(_)
            | Statement::ForKeyValue(_)
            | Statement::Label { .. }
    ) {
        ctx(compiler).clear_new_vars = true;
    }
    // DreamMaker starts every for loop without knowing what the cache holds,
    // and ends every loop that way too
    let is_loop = matches!(
        stmt,
        Statement::ForLoop { .. }
            | Statement::ForInfinite { .. }
            | Statement::ForList(_)
            | Statement::ForRange(_)
            | Statement::ForKeyValue(_)
            | Statement::While { .. }
            | Statement::DoWhile { .. }
    );
    if is_loop {
        let res = emit_statement_inner(compiler, stmt, line);
        compiler.cache = None;
        return res;
    }
    emit_statement_inner(compiler, stmt, line)
}

fn emit_statement_inner(
    compiler: &mut Compiler,
    stmt: &Statement,
    line: u32,
) -> Result<(), CompileError> {
    if matches!(
        stmt,
        Statement::ForLoop { .. }
            | Statement::ForInfinite { .. }
            | Statement::ForList(_)
            | Statement::ForRange(_)
            | Statement::ForKeyValue(_)
    ) {
        compiler.cache = None;
    }
    match stmt {
        Statement::Expr(expr) => emit_expr_statement(compiler, expr),

        Statement::Return(value) => {
            let is_dot = |e: &Expression| {
                matches!(strip_parens(e), Expression::Base { term, follow }
                if follow.is_empty() && matches!(&term.elem, Term::Ident(i) if i.as_str() == "."))
            };
            match value {
                // End returns `.` anyway
                Some(expr) if is_dot(expr) => compiler.emit_ins(Instruction::End),
                Some(expr) => {
                    compiler.regroup_root = true;
                    emit_value(compiler, expr)?;
                    compiler.emit_ins(Instruction::Ret);
                }
                None => compiler.emit_ins(Instruction::End),
            }
            Ok(())
        }

        // Arms with constant conditions are dropped or become the else
        Statement::If { arms, else_arm }
            if arms
                .iter()
                .any(|(c, _)| super::fold::value(compiler, &c.elem).is_some()) =>
        {
            let mut live: Vec<(Spanned<Expression>, Block)> = vec![];
            let mut else_block = else_arm.clone();
            // (a false arm after the last live one still leaves its jump to
            // the end, as if there were an else)
            let mut dropped_after = false;
            for (cond, block) in arms.iter() {
                match super::fold::value(compiler, &cond.elem) {
                    Some(c) if !c.truthy() => dropped_after = !live.is_empty(),
                    Some(_) => {
                        else_block = Some(block.clone());
                        break;
                    }
                    None => {
                        live.push((cond.clone(), block.clone()));
                        dropped_after = false;
                    }
                }
            }
            if dropped_after && else_block.is_none() {
                else_block = Some(Default::default());
            }
            if live.is_empty() {
                // (DreamMaker forgets the cache even where nothing is left,
                // and after the arm it keeps)
                compiler.cache = None;
                if let Some(block) = &else_block {
                    emit_block(compiler, block)?;
                    compiler.cache = None;
                }
                return Ok(());
            }
            emit_statement(
                compiler,
                &Statement::If {
                    arms: live,
                    else_arm: else_block,
                },
                line,
            )
        }

        Statement::If { arms, else_arm } => {
            let end = compiler.new_label();
            let count = arms.len();
            for (i, (condition, block)) in arms.iter().enumerate() {
                if i > 0 {
                    compiler.emit_ins(Instruction::DbgLine(condition.location.line));
                }
                let next = compiler.new_label();
                emit_condition_jz(compiler, &condition.elem, &next)?;
                emit_block(compiler, block)?;
                let last = i + 1 == count && else_arm.is_none();
                if !last {
                    compiler.emit_ins(Instruction::Jmp(Label::Named(end.clone())));
                }
                compiler.emit_label(next);
            }
            if let Some(block) = else_arm {
                emit_block(compiler, block)?;
            }
            compiler.emit_label(end);
            Ok(())
        }

        Statement::While { condition, block } => {
            // A loop whose condition is always false is left out
            if super::fold::value(compiler, &condition.elem).is_some_and(|c| !c.truthy()) {
                return Ok(());
            }
            let head = compiler.new_label();
            let end = compiler.new_label();
            compiler.emit_loop_label(head.clone());
            // A condition that's always true isn't tested
            if super::fold::value(compiler, &condition.elem).map_or(true, |c| !c.truthy()) {
                emit_condition_jz(compiler, &condition.elem, &end)?;
            }
            emit_loop_body(compiler, block, &head, &end)?;
            compiler.emit_ins(Instruction::JmpLoop(Label::Named(head)));
            compiler.emit_label(end);
            Ok(())
        }

        Statement::DoWhile { block, condition } => {
            let head = compiler.new_label();
            let cont = compiler.new_label();
            let end = compiler.new_label();
            compiler.emit_loop_label(head.clone());
            emit_loop_body(compiler, block, &cont, &end)?;
            compiler.emit_label(cont);
            match super::fold::value(compiler, &condition.elem).map(|c| c.truthy()) {
                // `while (FALSE)` doesn't loop, `while (TRUE)` always does
                Some(false) => {}
                Some(_) => compiler.emit_ins(Instruction::JmpLoop(Label::Named(head))),
                None => {
                    compiler.emit_ins(Instruction::DbgLine(condition.location.line));
                    emit_condition(compiler, &condition.elem)?;
                    compiler.emit_ins(Instruction::JnzLoop(Label::Named(head)));
                }
            }
            compiler.emit_label(end);
            Ok(())
        }

        Statement::ForLoop {
            init,
            test,
            inc,
            block,
        } => {
            ctx(compiler).scopes.push(vec![]);
            if let Some(init) = init {
                emit_for_init(compiler, init, line)?;
            }
            let head = compiler.new_label();
            let cont = compiler.new_label();
            let end = compiler.new_label();
            compiler.emit_loop_label(head.clone());
            if let Some(test) = test {
                emit_condition_jz(compiler, test, &end)?;
            }
            emit_loop_body(compiler, block, &cont, &end)?;
            compiler.emit_label(cont);
            if let Some(inc) = inc {
                compiler.emit_ins(Instruction::DbgLine(line));
                emit_statement(compiler, inc, line)?;
            }
            compiler.emit_ins(Instruction::JmpLoop(Label::Named(head)));
            compiler.emit_label(end);
            ctx(compiler).scopes.pop();
            Ok(())
        }

        Statement::ForList(for_list) => {
            // `for (x in a to b)` counts like `for (x = a to b)`
            if let Some(Expression::BinaryOp {
                op: dreammaker::ast::BinaryOp::To,
                lhs,
                rhs,
            }) = &for_list.in_list
            {
                let range = dreammaker::ast::ForRangeStatement {
                    var_type: for_list.var_type.clone(),
                    name: for_list.name.clone(),
                    start: (**lhs).clone(),
                    end: (**rhs).clone(),
                    step: None,
                    block: for_list.block.clone(),
                };
                return emit_statement(compiler, &Statement::ForRange(Box::new(range)), line);
            }
            ctx(compiler).scopes.push(vec![]);
            let (var, declared) = if let Some(var_type) = &for_list.var_type {
                let ty = var_type.type_path.to_string();
                let slot = ctx(compiler).declare_typed_local(for_list.name.as_str(), ty.clone());
                compiler.emit_ins(Instruction::PushVal(Value::Null));
                compiler.emit_ins(Instruction::SetVar(Variable::Local(slot)));
                (EvalKind::Var(Variable::Local(slot)), ty)
            } else {
                let name_expr = Expression::from(Term::Ident(for_list.name.clone()));
                let ty = super::procs::declared_type_path(compiler, &name_expr).unwrap_or_default();
                (compiler.emit_find_var(for_list.name.clone())?, ty)
            };
            let var = lvalue_var(compiler, var)?;
            // DreamMaker reads the list afresh, not from what the cache held
            // before the loop
            compiler.cache = None;
            // view(), range() and friends have iterators of their own
            let kind = match &for_list.in_list {
                Some(list) => match iterator_kind(list) {
                    Some((kind, args)) => {
                        for arg in args.iter() {
                            emit_value(compiler, arg)?;
                        }
                        let wanted = if kind == 19 { 6 } else { 2 };
                        for _ in args.len().min(wanted)..wanted {
                            compiler.emit_ins(Instruction::PushVal(Value::Null));
                        }
                        kind
                    }
                    None => {
                        emit_value(compiler, list)?;
                        5
                    }
                },
                None => {
                    compiler.emit_ins(Instruction::GetVar(Variable::World));
                    5
                }
            };
            let nested = ctx(compiler).iterators > 0;
            // What the loop takes from the list: the `as` types, or else the
            // kind of the declared type, checked exactly with istype()
            let input = for_list.input_type.filter(|i| !i.is_empty()).or_else(|| {
                for_list
                    .var_type
                    .as_ref()
                    .map(|v| v.input_type)
                    .filter(|i| !i.is_empty())
            });
            let (mut filter, mut check) = match input {
                Some(input) => (crate::TypeFilter::from_bits_truncate(input.bits()), None),
                _ => loop_filter(compiler, &declared),
            };
            // All datums of a type, `for (var/datum/T/x)`: the iterator is given
            // the type to look for (`in world` goes over the world's atoms instead)
            if for_list.in_list.is_none() && filter.is_empty() && input.is_none() {
                if let Some(path) = check.take() {
                    compiler.emit_ins(Instruction::PushVal(Value::Path(path)));
                    filter = crate::TypeFilter::DATUM_INSTANCES;
                }
            }
            if nested {
                compiler.emit_ins(Instruction::IterPush);
            }
            compiler.emit_ins(Instruction::IterLoad(kind, filter));
            let head = compiler.new_label();
            let end = compiler.new_label();
            compiler.emit_loop_label(head.clone());
            compiler.emit_ins(Instruction::IterNext);
            compiler.emit_ins(Instruction::SetVar(var.clone()));
            compiler.emit_ins(Instruction::Jz(Label::Named(end.clone())));
            if let Some(path) = check {
                compiler.emit_ins(Instruction::GetVar(var));
                compiler.emit_ins(Instruction::PushVal(Value::Path(path)));
                compiler.emit_ins(Instruction::IsType);
                compiler.emit_ins(Instruction::Test);
                compiler.emit_ins(Instruction::JzLoop(Label::Named(head.clone())));
            }
            ctx(compiler).iterators += 1;
            // (only a nested loop pushed an iterator to drop)
            if nested {
                ctx(compiler).cleanups.push(Instruction::IterPop);
            }
            emit_loop_body(compiler, &for_list.block, &head, &end)?;
            ctx(compiler).iterators -= 1;
            if nested {
                ctx(compiler).cleanups.pop();
            }
            compiler.emit_ins(Instruction::JmpLoop(Label::Named(head)));
            compiler.emit_label(end);
            if nested {
                compiler.emit_ins(Instruction::IterPop);
            }
            ctx(compiler).scopes.pop();
            Ok(())
        }

        Statement::ForRange(for_range) => {
            ctx(compiler).scopes.push(vec![]);
            let var = if let Some(var_type) = &for_range.var_type {
                let ty = var_type.type_path.to_string();
                let slot = ctx(compiler).declare_typed_local(for_range.name.as_str(), ty);
                compiler.emit_ins(Instruction::PushVal(Value::Null));
                compiler.emit_ins(Instruction::SetVar(Variable::Local(slot)));
                EvalKind::Var(Variable::Local(slot))
            } else {
                compiler.emit_find_var(for_range.name.clone())?
            };
            let var = lvalue_var(compiler, var)?;
            emit_value(compiler, &for_range.start)?;
            emit_value(compiler, &for_range.end)?;
            let head = compiler.new_label();
            let end = compiler.new_label();
            match &for_range.step {
                Some(step) => {
                    emit_value(compiler, step)?;
                    compiler.emit_ins(Instruction::Check3Numbers);
                    compiler.emit_loop_label(head.clone());
                    compiler.emit_ins(Instruction::ForRangeStep(Label::Named(end.clone()), var));
                }
                None => {
                    compiler.emit_ins(Instruction::Check2Numbers);
                    compiler.emit_loop_label(head.clone());
                    compiler.emit_ins(Instruction::ForRange(Label::Named(end.clone()), var));
                }
            }
            let n = if for_range.step.is_some() { 3 } else { 2 };
            ctx(compiler).cleanups.push(Instruction::PopN(n));
            let res = emit_loop_body(compiler, &for_range.block, &head, &end);
            ctx(compiler).cleanups.pop();
            res?;
            compiler.emit_ins(Instruction::JmpLoop(Label::Named(head)));
            compiler.emit_label(end);
            compiler.emit_ins(Instruction::PopN(n));
            ctx(compiler).scopes.pop();
            Ok(())
        }

        Statement::Var(var) => emit_var_statement_then(compiler, var, false),
        Statement::Vars(vars) => {
            for var in vars {
                emit_var_statement_then(compiler, var, false)?;
            }
            Ok(())
        }

        Statement::Switch {
            input,
            cases,
            default,
        } => {
            emit_value(compiler, input)?;
            let end = compiler.new_label();
            let default_label = if default.is_some() {
                compiler.new_label()
            } else {
                end.clone()
            };
            let mut exact = vec![];
            let mut ranges = vec![];
            let mut arm_labels = vec![];
            for (values, _) in cases.iter() {
                let label = compiler.new_label();
                // DreamMaker lists one arm's values last to first
                for case in values.elem.iter().rev() {
                    match case {
                        dreammaker::ast::Case::Exact(e) => {
                            exact.push((constant_value(compiler, e)?, Label::Named(label.clone())))
                        }
                        dreammaker::ast::Case::Range(a, b) => ranges.push((
                            constant_value(compiler, a)?,
                            constant_value(compiler, b)?,
                            Label::Named(label.clone()),
                        )),
                    }
                }
                arm_labels.push(label);
            }
            if ranges.is_empty() {
                compiler.emit_ins(Instruction::Switch(crate::SwitchParams {
                    default: Label::Named(default_label.clone()),
                    cases: exact,
                }));
            } else {
                compiler.emit_ins(Instruction::SwitchRange(crate::SwitchRangeParams {
                    default: Label::Named(default_label.clone()),
                    cases: exact,
                    range_cases: ranges,
                }));
            }
            let count = cases.len();
            for (i, ((_, block), label)) in cases.iter().zip(arm_labels).enumerate() {
                compiler.emit_label(label);
                emit_block(compiler, block)?;
                if i + 1 < count || default.is_some() {
                    compiler.emit_ins(Instruction::Jmp(Label::Named(end.clone())));
                }
            }
            if let Some(block) = default {
                compiler.emit_label(default_label);
                emit_block(compiler, block)?;
            }
            compiler.emit_label(end);
            Ok(())
        }

        // Settings are metadata, not code
        Statement::Setting { .. } => Ok(()),

        Statement::Break(label) | Statement::Continue(label) => {
            let target = find_loop(compiler, label.as_ref().map(|l| l.as_str()))?;
            // Leaving inner loops drops their iterators and range bounds,
            // innermost first
            let live = if matches!(stmt, Statement::Break(_)) {
                target.break_cleanups
            } else {
                target.cleanups
            };
            let cleanups: Vec<Instruction> = ctx(compiler).cleanups[live..]
                .iter()
                .rev()
                .cloned()
                .collect();
            for ins in cleanups {
                compiler.emit_ins(ins);
            }
            // Inside a try block (wherever the loop is) DreamMaker leaves with the
            // try-ending jumps
            let in_try = ctx(compiler).try_depth > 0;
            compiler.emit_ins(match (matches!(stmt, Statement::Break(_)), in_try) {
                (true, false) => Instruction::Jmp(Label::Named(target.break_label)),
                (true, true) => Instruction::Catch(Label::Named(target.break_label)),
                (false, false) => Instruction::JmpLoop(Label::Named(target.continue_label)),
                (false, true) => Instruction::TryJmp(Label::Named(target.continue_label)),
            });
            Ok(())
        }

        Statement::Spawn { delay, block } => {
            match delay {
                Some(delay) => emit_value(compiler, delay)?,
                None => compiler.emit_ins(Instruction::PushInt(0)),
            }
            let after = compiler.new_label();
            compiler.emit_ins(Instruction::Spawn(Label::Named(after.clone())));
            // (The spawned code is still in its loops: a `continue` there
            // goes on with the loop in the new thread)
            let res = emit_block(compiler, block);
            res?;
            compiler.emit_ins(Instruction::End);
            compiler.emit_label(after);
            Ok(())
        }

        Statement::Throw(expr) => {
            emit_value(compiler, expr)?;
            compiler.emit_ins(Instruction::Throw);
            Ok(())
        }

        Statement::TryCatch {
            try_block,
            catch_params,
            catch_block,
        } => {
            let catch = compiler.new_label();
            let end = compiler.new_label();
            compiler.emit_ins(Instruction::Try(Label::Named(catch.clone())));
            ctx(compiler).try_depth += 1;
            let res = emit_block(compiler, try_block);
            ctx(compiler).try_depth -= 1;
            res?;
            compiler.emit_ins(Instruction::Catch(Label::Named(end.clone())));
            compiler.emit_label(catch);
            ctx(compiler).scopes.push(vec![]);
            match catch_params.first() {
                Some(param) => {
                    // `catch (var/exception/e)`: the path's last element is the name
                    let path = param.to_string();
                    let mut parts: Vec<&str> = path.split('/').filter(|p| !p.is_empty()).collect();
                    let name = parts.pop().unwrap_or("e").to_owned();
                    if parts.first() == Some(&"var") {
                        parts.remove(0);
                    }
                    let ty = if parts.is_empty() {
                        String::new()
                    } else {
                        format!("/{}", parts.join("/"))
                    };
                    let slot = ctx(compiler).declare_typed_local(&name, ty);
                    // cleared first like any new var, where DreamMaker does that
                    if !ctx(compiler).loops.is_empty() || ctx(compiler).clear_new_vars {
                        compiler.emit_ins(Instruction::PushVal(Value::Null));
                        compiler.emit_ins(Instruction::SetVar(Variable::Local(slot)));
                    }
                    compiler.emit_ins(Instruction::SetVar(Variable::Local(slot)));
                }
                None => compiler.emit_ins(Instruction::Pop),
            }
            let res = emit_block(compiler, catch_block);
            ctx(compiler).scopes.pop();
            res?;
            compiler.emit_label(end);
            Ok(())
        }

        Statement::Label { name, block } => {
            compiler.emit_label(format!("USER_{}", name));
            // (the first loop in the block, even after other statements)
            let is_loop = block.iter().any(|s| {
                matches!(
                    s.elem,
                    Statement::While { .. }
                        | Statement::DoWhile { .. }
                        | Statement::ForInfinite { .. }
                        | Statement::ForLoop { .. }
                        | Statement::ForList(_)
                        | Statement::ForKeyValue(_)
                        | Statement::ForRange(_)
                )
            });
            if is_loop {
                // The loop it labels takes the name, but `break name` leaves the
                // block, dropping the loop's iterator itself
                let end = compiler.new_label();
                let cleanups = ctx(compiler).cleanups.len();
                ctx(compiler).pending_loop_name =
                    Some((name.as_str().to_owned(), end.clone(), cleanups));
                let res = emit_block(compiler, block);
                ctx(compiler).pending_loop_name = None;
                res?;
                compiler.emit_label(end);
                return Ok(());
            }
            // A block: `break name` jumps past it
            let end = compiler.new_label();
            let cleanups = ctx(compiler).cleanups.len();
            ctx(compiler).loops.push(LoopLabels {
                name: Some(name.as_str().to_owned()),
                cleanups,
                break_cleanups: cleanups,
                continue_label: end.clone(),
                break_label: end.clone(),
                block: true,
            });
            let res = emit_block(compiler, block);
            ctx(compiler).loops.pop();
            res?;
            compiler.emit_label(end);
            Ok(())
        }

        // Jumping out of nested for-in loops drops their iterators
        Statement::Goto(name) => {
            let depth = ctx(compiler)
                .label_iterators
                .get(name.as_str())
                .copied()
                .unwrap_or(0);
            // (the outermost loop's iterator wasn't pushed)
            for _ in depth.max(1)..ctx(compiler).iterators {
                compiler.emit_ins(Instruction::IterPop);
            }
            // (in a try block, wherever the label is, with TryJmp)
            let label = Label::Named(format!("USER_{}", name));
            let in_try = ctx(compiler).try_depth > 0;
            compiler.emit_ins(if in_try {
                Instruction::TryJmp(label)
            } else {
                Instruction::JmpLoop(label)
            });
            Ok(())
        }

        Statement::ForInfinite { block } => {
            let head = compiler.new_label();
            let end = compiler.new_label();
            compiler.emit_loop_label(head.clone());
            emit_loop_body(compiler, block, &head, &end)?;
            compiler.emit_ins(Instruction::JmpLoop(Label::Named(head)));
            compiler.emit_label(end);
            Ok(())
        }

        Statement::ForKeyValue(kv) => {
            super::versions::require(
                compiler.byond,
                super::versions::BYOND_516,
                "for (k, v in list)",
            )?;
            ctx(compiler).scopes.push(vec![]);
            let (key, value) = if let Some(var_type) = &kv.var_type {
                let k = ctx(compiler)
                    .declare_typed_local(kv.key.as_str(), var_type.type_path.to_string());
                compiler.emit_ins(Instruction::PushVal(Value::Null));
                compiler.emit_ins(Instruction::SetVar(Variable::Local(k)));
                let v = ctx(compiler).declare_typed_local(kv.value.as_str(), String::new());
                compiler.emit_ins(Instruction::PushVal(Value::Null));
                compiler.emit_ins(Instruction::SetVar(Variable::Local(v)));
                (Variable::Local(k), Variable::Local(v))
            } else {
                let k = compiler.emit_find_var(kv.key.clone())?;
                let k = lvalue_var(compiler, k)?;
                let v = compiler.emit_find_var(kv.value.clone())?;
                let v = lvalue_var(compiler, v)?;
                (k, v)
            };
            compiler.cache = None;
            match &kv.in_list {
                Some(list) => emit_value(compiler, list)?,
                None => compiler.emit_ins(Instruction::GetVar(Variable::World)),
            }
            let nested = ctx(compiler).iterators > 0;
            if nested {
                compiler.emit_ins(Instruction::IterPush);
            }
            // The key is filtered like a for-in loop's var
            let input = kv.key_input_type.filter(|i| !i.is_empty()).or_else(|| {
                kv.var_type
                    .as_ref()
                    .map(|v| v.input_type)
                    .filter(|i| !i.is_empty())
            });
            let declared = kv
                .var_type
                .as_ref()
                .map(|v| v.type_path.to_string())
                .unwrap_or_default();
            let (filter, check) = match input {
                Some(input) => (crate::TypeFilter::from_bits_truncate(input.bits()), None),
                _ => loop_filter(compiler, &declared),
            };
            compiler.emit_ins(Instruction::IterLoad(20, filter));
            let head = compiler.new_label();
            let end = compiler.new_label();
            compiler.emit_loop_label(head.clone());
            compiler.emit_ins(Instruction::IterNext);
            compiler.emit_ins(Instruction::KeyValueIter(value));
            compiler.emit_ins(Instruction::SetVar(key.clone()));
            compiler.emit_ins(Instruction::Jz(Label::Named(end.clone())));
            if let Some(path) = check {
                compiler.emit_ins(Instruction::GetVar(key));
                compiler.emit_ins(Instruction::PushVal(Value::Path(path)));
                compiler.emit_ins(Instruction::IsType);
                compiler.emit_ins(Instruction::Test);
                compiler.emit_ins(Instruction::JzLoop(Label::Named(head.clone())));
            }
            ctx(compiler).iterators += 1;
            // (only a nested loop pushed an iterator to drop)
            if nested {
                ctx(compiler).cleanups.push(Instruction::IterPop);
            }
            emit_loop_body(compiler, &kv.block, &head, &end)?;
            ctx(compiler).iterators -= 1;
            if nested {
                ctx(compiler).cleanups.pop();
            }
            compiler.emit_ins(Instruction::JmpLoop(Label::Named(head)));
            compiler.emit_label(end);
            if nested {
                compiler.emit_ins(Instruction::IterPop);
            }
            ctx(compiler).scopes.pop();
            Ok(())
        }

        // DreamMaker clears the reference before deleting what it held, and
        // deleting src ends the proc
        Statement::Del(expr) => {
            emit_value(compiler, expr)?;
            let target = strip_parens(expr);
            if is_deletable_place(target) {
                compiler.emit_ins(Instruction::PushVal(Value::Null));
                let kind = compiler.emit_inner_expr(target.clone())?;
                if kind == EvalKind::ListRef {
                    compiler.emit_ins(Instruction::ListSet);
                } else {
                    let var = lvalue_var(compiler, kind)?;
                    compiler.emit_ins(Instruction::SetVar(var));
                }
            }
            compiler.emit_ins(Instruction::Del);
            if is_src(target) {
                compiler.emit_ins(Instruction::End);
            }
            Ok(())
        }

        Statement::Crash(expr) => {
            match expr {
                Some(expr) => emit_value(compiler, expr)?,
                None => compiler.emit_ins(Instruction::PushVal(Value::Null)),
            }
            compiler.emit_ins(Instruction::Crash);
            Ok(())
        }
    }
}

/// A switch case's value, which must be a compile-time constant.
fn constant_value(compiler: &mut Compiler, expr: &Expression) -> Result<Value, CompileError> {
    match super::fold::value(compiler, expr) {
        // (a case of -0 is 0)
        Some(super::fold::Constant::Number(n)) => return Ok(Value::Number(n + 0.0)),
        Some(super::fold::Constant::Text(t)) => {
            return Ok(Value::DMString(crate::operands::DMString(t)))
        }
        Some(super::fold::Constant::Null) => return Ok(Value::Null),
        Some(super::fold::Constant::Path(p)) => return Ok(Value::Path(p)),
        Some(super::fold::Constant::New) | None => {}
    }
    if let Expression::Base { term, follow } = strip_parens(expr) {
        if follow.is_empty() {
            match &term.elem {
                Term::Null => return Ok(Value::Null),
                Term::String(s) => return Ok(Value::DMString(super::strings::parse(s)?)),
                Term::Prefab(prefab) if prefab.vars.is_empty() => {
                    return Ok(Value::Path(prefab.path.to_string()))
                }
                Term::Resource(r) => return Ok(Value::Resource(r.to_string())),
                _ => {}
            }
        }
    }
    let _ = compiler;
    Err(CompileError::UnsupportedStatement(
        "switch case that isn't a simple constant",
    ))
}

fn emit_for_init(compiler: &mut Compiler, init: &Statement, line: u32) -> Result<(), CompileError> {
    match init {
        // `for (x, x < 10, x++)`: naming the variable does nothing
        Statement::Expr(Expression::Base { term, follow })
            if follow.is_empty() && matches!(term.elem, Term::Ident(_)) =>
        {
            Ok(())
        }
        Statement::Var(var) => emit_var_statement_then(compiler, var, true),
        Statement::Vars(vars) => {
            for var in vars {
                emit_var_statement_then(compiler, var, true)?;
            }
            Ok(())
        }
        other => emit_statement(compiler, other, line),
    }
}

/// A var statement, after which DreamMaker forgets a chain through fields if
/// the value was `new` of the var's own type (`var/L[] = new()`).
fn emit_var_statement_then(
    compiler: &mut Compiler,
    var: &VarStatement,
    null_first: bool,
) -> Result<(), CompileError> {
    emit_var_statement(compiler, var, null_first)?;
    if matches!(&var.value, Some(Expression::Base { term, follow })
        if follow.is_empty() && matches!(term.elem, Term::NewImplicit { .. }))
        && implicit_type_for(compiler, &var.var_type.type_path.to_string()).as_deref()
            == Some("/list")
    {
        compiler.end_assignment_statement();
    }
    Ok(())
}

fn emit_var_statement(
    compiler: &mut Compiler,
    var: &VarStatement,
    null_first: bool,
) -> Result<(), CompileError> {
    // `var/x = a in list(...)` is `var/x = a` with the list as x's `in` clause
    let in_clause;
    let var = match &var.value {
        Some(Expression::BinaryOp {
            op: dreammaker::ast::BinaryOp::In,
            lhs,
            ..
        }) => {
            let mut v = var.clone();
            v.value = Some((**lhs).clone());
            in_clause = v;
            &in_clause
        }
        _ => var,
    };
    // A local const is a global like a static local; a number or text is also
    // folded into constant expressions that use it
    if var.var_type.flags.is_const() {
        use dreammaker::constants::Constant as C;
        let value = match var
            .value
            .as_ref()
            .and_then(|v| super::fold::value(compiler, v))
        {
            Some(super::fold::Constant::Number(n)) => Some(C::Float(n)),
            Some(super::fold::Constant::Text(t))
                if !t.iter().any(|b| matches!(b, 0xFF | b'\\' | b'[')) =>
            {
                String::from_utf8(t).ok().map(|t| C::String(t.into()))
            }
            _ => None,
        };
        if let Some(value) = value {
            ctx(compiler)
                .local_consts
                .insert(var.name.as_str().to_owned(), value);
        }
    }
    // Static locals are globals, set up when the world starts
    if var.var_type.flags.is_static() || var.var_type.flags.is_const() {
        let ty = var.var_type.type_path.to_string();
        let value = match &var.value {
            None => Some(dreammaker::constants::Constant::Null(None)),
            Some(value) => {
                let nowhere = dreammaker::Location {
                    file: dreammaker::FileId::UNKNOWN,
                    line: 0,
                    column: 0,
                };
                value.simple_evaluate(nowhere).ok()
            }
        };
        ctx(compiler).declare_static(var.name.as_str(), ty, value, var.value.clone());
        return Ok(());
    }
    // The initializer can't see the variable it initializes
    let ty = var.var_type.type_path.to_string();
    compiler.implicit_type = implicit_type_for(compiler, &ty);
    let slot_var = |compiler: &mut Compiler| {
        let slot = ctx(compiler).declare_typed_local(var.name.as_str(), ty.clone());
        Variable::Local(slot)
    };
    if null_first {
        let local = slot_var(compiler);
        compiler.emit_ins(Instruction::PushVal(Value::Null));
        compiler.emit_ins(Instruction::SetVar(local.clone()));
        if let Some(value) = &var.value {
            emit_value(compiler, value)?;
            compiler.emit_ins(Instruction::SetVar(local));
        }
        return Ok(());
    }
    // `var/L[n]` is declared without a value and then given its list
    let sized_list = matches!(&var.value, Some(Expression::Base { term, .. })
        if term.location.file == dreammaker::FileId::UNKNOWN && matches!(term.elem, Term::NewPrefab { .. }));
    match &var.value {
        Some(value)
            if sized_list && (!ctx(compiler).loops.is_empty() || ctx(compiler).clear_new_vars) =>
        {
            compiler.implicit_type = None;
            let local = slot_var(compiler);
            compiler.emit_ins(Instruction::PushVal(Value::Null));
            compiler.emit_ins(Instruction::SetVar(local.clone()));
            emit_value(compiler, value)?;
            compiler.emit_ins(Instruction::SetVar(local));
        }
        Some(value) => {
            emit_value(compiler, value)?;
            compiler.implicit_type = None;
            let local = slot_var(compiler);
            compiler.emit_ins(Instruction::SetVar(local));
        }
        None => {
            compiler.implicit_type = None;
            let local = slot_var(compiler);
            // Inside a loop it's cleared on every pass
            if !ctx(compiler).loops.is_empty() || ctx(compiler).clear_new_vars {
                compiler.emit_ins(Instruction::PushVal(Value::Null));
                compiler.emit_ins(Instruction::SetVar(local));
            }
        }
    }
    Ok(())
}

/// The iterator kind a for-in loop over a builtin like view() uses, and its
/// arguments.
fn iterator_kind(list: &Expression) -> Option<(u32, &[Expression])> {
    let Expression::Base { term, follow } = strip_parens(list) else {
        return None;
    };
    if !follow.is_empty() {
        return None;
    }
    let Term::Call(name, args) = &term.elem else {
        return None;
    };
    let kind = match name.as_str() {
        "block" if args.len() == 2 => 6,
        // (with up to three coordinates left out, as nulls)
        "block" if (3..=6).contains(&args.len()) => 19,
        "view" => 7,
        "oview" => 8,
        "range" => 13,
        "orange" => 14,
        "viewers" => 15,
        "oviewers" => 16,
        "hearers" => 17,
        "ohearers" => 18,
        _ => return None,
    };
    if args.len() > 2 && kind != 19 {
        return None;
    }
    Some((kind, args))
}

/// The iterator filter for a loop variable declared as `ty`, and the type to
/// check items against when the filter alone doesn't pin it down.
fn loop_filter(compiler: &Compiler, ty: &str) -> (crate::TypeFilter, Option<String>) {
    use crate::TypeFilter as F;
    if ty.is_empty() {
        return (F::empty(), None);
    }
    // (`var/list/T/x` is checked as a T, or as a list if T isn't a type)
    let ty = match ty.strip_prefix("/list") {
        Some(rest) if rest.starts_with('/') => {
            if compiler
                .proc
                .as_ref()
                .and_then(|c| c.tree.find(rest))
                .is_some()
            {
                rest
            } else {
                "/list"
            }
        }
        _ => ty,
    };
    let path = compiler
        .proc
        .as_ref()
        .and_then(|c| c.tree.find(ty))
        .map(|t| t.get().path.clone())
        .unwrap_or_else(|| ty.to_owned());
    let under = |root: &str| path == root || path.starts_with(&format!("{}/", root));
    let filter = if under("/obj") {
        F::OBJ
    } else if under("/mob") {
        F::MOB
    } else if under("/turf") {
        F::TURF
    } else if under("/area") {
        F::AREA
    } else if under("/atom/movable") {
        F::MOB | F::OBJ
    } else if under("/atom") {
        F::MOB | F::OBJ | F::TURF | F::AREA
    } else {
        F::empty()
    };
    let exact = matches!(path.as_str(), "/mob" | "/turf" | "/area");
    (filter, if exact { None } else { Some(path) })
}

/// What a bare `new` makes for a variable declared as `ty`.
fn implicit_type_for(compiler: &Compiler, ty: &str) -> Option<String> {
    if ty.is_empty() {
        return None;
    }
    // (`list` anywhere in the path: `var/datum/d/list/x` is a list too)
    if is_list_path(ty) {
        return Some("/list".to_owned());
    }
    compiler
        .proc
        .as_ref()?
        .tree
        .find(ty)
        .map(|t| t.get().path.clone())
}

/// Whether a var's declared type makes it a list: `list` is one of its parts.
fn is_list_path(ty: &str) -> bool {
    ty.split('/').any(|part| part == "list")
}

/// `/list` when the expression is a variable declared as a list.
fn list_type_of(compiler: &Compiler, expr: &Expression) -> Option<String> {
    super::procs::declared_type_path(compiler, expr)
        .filter(|p| is_list_path(p))
        .map(|_| "/list".to_owned())
}

fn find_loop(compiler: &mut Compiler, name: Option<&str>) -> Result<LoopLabels, CompileError> {
    let loops = &ctx(compiler).loops;
    let found = match name {
        None => loops.iter().rev().find(|l| !l.block),
        Some(name) => loops.iter().rev().find(|l| l.name.as_deref() == Some(name)),
    };
    found.cloned().ok_or(CompileError::UnsupportedStatement(
        "break/continue outside a loop",
    ))
}

fn emit_loop_body(
    compiler: &mut Compiler,
    block: &Block,
    continue_label: &str,
    break_label: &str,
) -> Result<(), CompileError> {
    let pending = ctx(compiler).pending_loop_name.take();
    let cleanups = ctx(compiler).cleanups.len();
    let (name, break_label, break_cleanups) = match pending {
        Some((name, end, at)) => (Some(name), end, at),
        None => (None, break_label.to_owned(), cleanups),
    };
    ctx(compiler).loops.push(LoopLabels {
        name,
        cleanups,
        break_cleanups,
        continue_label: continue_label.to_owned(),
        break_label,
        block: false,
    });
    let res = emit_block(compiler, block);
    ctx(compiler).loops.pop();
    res
}

/// Whether the code from `start` uses the cache, calls a global proc or reads
/// a list element (so `x.y ||= v` saves the cache around v).
fn saves_cache(compiler: &Compiler, start: usize) -> bool {
    compiler.nodes[start..].iter().any(|n| {
        matches!(n, Node::Instruction(i, _) if super::procs::touches_cache(i)
            || matches!(i, Instruction::CallGlob(..) | Instruction::CallGlobalArgList(_) | Instruction::ListGet))
    })
}

/// Whether `value` would save the cache with nothing folded: it's compiled
/// that way and thrown away.
fn unfolded_saves_cache(compiler: &mut Compiler, value: &Expression) -> bool {
    unfolded_has(compiler, value, |compiler, start| {
        compiler.global_field || saves_cache(compiler, start)
    })
}

/// Whether `found` holds of `value`'s code (from the place it's given) with
/// nothing folded: it's compiled that way and thrown away.
pub(super) fn unfolded_has(
    compiler: &mut Compiler,
    value: &Expression,
    found: impl Fn(&Compiler, usize) -> bool,
) -> bool {
    let start = compiler.nodes.len();
    let saved = (
        compiler.label_count,
        compiler.cache.clone(),
        compiler.last_line,
        compiler.label_states.clone(),
        compiler.global_field,
        compiler.other_file,
        compiler.line_pending,
    );
    compiler.no_fold = true;
    compiler.global_field = false;
    let ok = emit_value(compiler, value).is_ok();
    compiler.no_fold = false;
    let saves = ok && found(compiler, start);
    compiler.nodes.truncate(start);
    (
        compiler.label_count,
        compiler.cache,
        compiler.last_line,
        compiler.label_states,
        compiler.global_field,
        compiler.other_file,
        compiler.line_pending,
    ) = saved;
    saves
}

/// An expression without the parentheses around it: `(x) = y` assigns to x.
fn unparenthesised(expr: &Expression) -> Expression {
    let mut expr = expr;
    while let Expression::Base { term, follow } = expr {
        match &term.elem {
            Term::Expr(inner) if follow.is_empty() => expr = inner,
            _ => break,
        }
    }
    expr.clone()
}

/// The writable variable an expression's result names.
fn lvalue_var(compiler: &mut Compiler, kind: EvalKind) -> Result<Variable, CompileError> {
    match kind {
        EvalKind::Var(var) if super::is_writable(&var) => Ok(var),
        EvalKind::Field(builder, field) => {
            Ok(builder.get_field(crate::operands::DMString(field.into())))
        }
        EvalKind::ListRef => {
            compiler.emit_ins(Instruction::SetVar(Variable::CacheKey));
            compiler.emit_ins(Instruction::SetVar(Variable::Cache));
            Ok(Variable::CacheIndex)
        }
        _ => Err(CompileError::ExpectedLValue),
    }
}

/// Whether `del` clears what it's given: a variable, a field (other than
/// builtin constants like `type`) or a list element.
fn is_deletable_place(expr: &Expression) -> bool {
    let Expression::Base { term, follow } = expr else {
        return false;
    };
    match follow.last().map(|f| &f.elem) {
        None => match &term.elem {
            Term::Ident(name) => name.as_str() != "world",
            Term::GlobalIdent(_) => true,
            _ => false,
        },
        Some(Follow::Field(
            dreammaker::ast::PropertyAccessKind::Dot | dreammaker::ast::PropertyAccessKind::Colon,
            name,
        )) => {
            !matches!(name.as_str(), "type" | "parent_type")
                && follow[..follow.len() - 1]
                    .iter()
                    .all(|f| matches!(f.elem, Follow::Field(..)))
        }
        Some(Follow::Index(dreammaker::ast::ListAccessKind::Normal, _)) => true,
        _ => false,
    }
}

fn is_src(expr: &Expression) -> bool {
    matches!(expr, Expression::Base { term, follow }
        if follow.is_empty() && matches!(&term.elem, Term::Ident(i) if i.as_str() == "src"))
}

fn strip_parens(expr: &Expression) -> &Expression {
    if let Expression::Base { term, follow } = expr {
        if follow.is_empty() {
            if let Term::Expr(inner) = &term.elem {
                return strip_parens(inner);
            }
        }
    }
    expr
}

/// Split safe-access targets into segments after each `?.`, plus an
/// optional final index. Segments can contain calls and list indexes.
pub(super) struct SafeChain {
    segments: Vec<Vec<Follow>>,
    last: Vec<Follow>,
    index: Option<Expression>,
}

/// What a write to a `?.` place does with the place.
pub(super) enum SafeWrite<'w> {
    /// Assign the value to it (the only thing a list element allows).
    Assign,
    /// The instructions that change it.
    Change(&'w dyn Fn(Variable) -> Vec<Instruction>),
    /// `||=` (or `&&=` when false) as a statement: the value is found and
    /// assigned only when the place's value doesn't decide it.
    Logical { or: bool },
}

fn is_safe_follow(f: &Follow) -> bool {
    use dreammaker::ast::{ListAccessKind as L, PropertyAccessKind as K};
    matches!(
        f,
        Follow::Field(K::SafeDot | K::SafeColon, _)
            | Follow::Call(K::SafeDot | K::SafeColon, _, _)
            | Follow::Index(L::Safe, _)
    )
}

/// `follow` starts at the first `?` access. None unless it's a place.
pub(super) fn safe_chain(follow: &[Follow]) -> Option<SafeChain> {
    use dreammaker::ast::{ListAccessKind as L, PropertyAccessKind as K};
    let mut segments: Vec<Vec<Follow>> = vec![];
    let mut index = None;
    for (i, f) in follow.iter().enumerate() {
        let last = i + 1 == follow.len();
        match f {
            Follow::Field(K::SafeDot | K::SafeColon, name) => {
                segments.push(vec![Follow::Field(K::Dot, name.clone())])
            }
            Follow::Call(K::SafeDot | K::SafeColon, name, args) => {
                segments.push(vec![Follow::Call(K::Dot, name.clone(), args.clone())])
            }
            Follow::Field(K::Dot | K::Colon, _) | Follow::Call(K::Dot | K::Colon, _, _) => {
                segments.last_mut()?.push(f.clone())
            }
            Follow::Index(L::Normal, e) if last => index = Some((**e).clone()),
            Follow::Index(L::Normal, _) => segments.last_mut()?.push(f.clone()),
            // `L?[k]`: the list itself is the last segment's place
            Follow::Index(L::Safe, e) if last => {
                segments.push(vec![]);
                index = Some((**e).clone())
            }
            Follow::Index(L::Safe, e) => segments.push(vec![Follow::Index(L::Normal, e.clone())]),
            _ => return None,
        }
    }
    let last = segments.pop()?;
    let calls = |s: &Vec<Follow>| s.iter().filter(|f| matches!(f, Follow::Call(..))).count();
    if segments.iter().any(|s| calls(s) > 1) || calls(&last) > 0 {
        return None;
    }
    Some(SafeChain {
        segments,
        last,
        index,
    })
}

/// `a?.b.c` as what comes before the first `?` and the chain after.
fn safe_place(expr: &Expression) -> Option<(Expression, SafeChain)> {
    let Expression::Base { term, follow } = expr else {
        return None;
    };
    let first = follow.iter().position(|f| is_safe_follow(&f.elem))?;
    let rest: Vec<Follow> = follow[first..].iter().map(|f| f.elem.clone()).collect();
    let chain = safe_chain(&rest)?;
    let base = Expression::Base {
        term: term.clone(),
        follow: follow[..first].to_vec().into_boxed_slice(),
    };
    Some((base, chain))
}

impl SafeChain {
    /// The last segment's field names, when it's all fields.
    fn fields(&self) -> Option<Vec<String>> {
        self.last
            .iter()
            .map(|f| match f {
                Follow::Field(_, name) => Some(name.to_string()),
                _ => None,
            })
            .collect()
    }

    /// Whether `write` can be done to the place, in a statement or for its
    /// value.
    pub fn writable(&self, write: &SafeWrite, value_form: bool) -> bool {
        match (self.fields(), write) {
            (Some(_), SafeWrite::Assign) => self.index.is_some(),
            (Some(_), SafeWrite::Change(_)) => true,
            // `a?.L[k].x`: the last segment is read before the field it ends
            // with is written
            (None, SafeWrite::Change(_)) => {
                !value_form
                    && self.index.is_none()
                    && matches!(self.last.last(), Some(Follow::Field(..)))
            }
            (None, SafeWrite::Assign) => false,
            (Some(_), SafeWrite::Logical { .. }) => !value_form,
            (None, SafeWrite::Logical { .. }) => false,
        }
    }
}

/// Write through a safe-access chain with its initial receiver on the stack.
/// Skip on null. `value_form` returns the assigned result or null.
/// `place` supplies static type information; `static_var` identifies
/// a globally stored target. The chain must be writable.
pub(super) fn emit_safe_chain_write(
    compiler: &mut Compiler,
    chain: SafeChain,
    place: Option<&Expression>,
    static_var: Option<Variable>,
    value: Option<&Expression>,
    write: SafeWrite,
    value_form: bool,
) -> Result<(), CompileError> {
    let fields = chain.fields();
    let jump = |end: &str| {
        let end = Label::Named(end.to_owned());
        if value_form {
            Instruction::SetCacheJmpIfNull(end)
        } else {
            Instruction::SetCachePopJmpIfNull(end)
        }
    };
    let mut ends = vec![];
    for segment in chain.segments.into_iter().chain([vec![]]) {
        let end = compiler.new_label();
        compiler.emit_ins(jump(&end));
        compiler.emit_ins(Instruction::PushCache);
        ends.push(end);
        if segment.is_empty() {
            break;
        }
        // The segment is read from the cache
        let start = compiler.nodes.len();
        let kind =
            super::follow::emit_typed(compiler, segment, EvalKind::Var(Variable::Cache), None)?;
        compiler.emit_move_to_stack(kind)?;
        // DM calls the proc on whatever the arguments left in the cache; we
        // put the receiver back first
        let call = compiler.nodes[start..]
            .iter()
            .position(|n| matches!(n, Node::Instruction(Instruction::Call(..), _)));
        if let Some(call) = call {
            let call = start + call;
            if compiler.nodes[start..call]
                .iter()
                .any(|n| matches!(n, Node::Instruction(i, _) if super::procs::touches_cache(i)))
            {
                compiler.warn(
                    "safe-call arguments overwrite the receiver cache in DreamMaker; dmasm calls the proc on its receiver",
                );
                compiler
                    .nodes
                    .insert(call, Node::Instruction(Instruction::PushCache, ()));
                compiler
                    .nodes
                    .insert(call, Node::Instruction(Instruction::PopCache, ()));
            }
        }
    }
    let implicit = place.and_then(|p| compiler.static_type_name(p));
    if let (Some(value), false) = (value, matches!(write, SafeWrite::Logical { .. })) {
        compiler.implicit_type = implicit.clone();
        emit_value(compiler, value)?;
        compiler.implicit_type = None;
    }
    let field_var = |fields: &[String]| {
        if let Some(var) = &static_var {
            return var.clone();
        }
        if fields.is_empty() {
            return Variable::Cache;
        }
        let mut builder = super::chain_builder::ChainBuilder::begin(Variable::Cache);
        for f in &fields[..fields.len() - 1] {
            builder.append(crate::operands::DMString(f.clone().into_bytes()));
        }
        builder.get_field(crate::operands::DMString(
            fields[fields.len() - 1].clone().into_bytes(),
        ))
    };
    match (fields, write, chain.index) {
        (None, SafeWrite::Change(write), None) => {
            let mut prefix = chain.last;
            let Some(Follow::Field(_, field)) = prefix.pop() else {
                unreachable!()
            };
            let kind =
                super::follow::emit_typed(compiler, prefix, EvalKind::Var(Variable::Cache), None)?;
            compiler.emit_move_to_stack(kind)?;
            compiler.emit_ins(Instruction::SetVar(Variable::Cache));
            for ins in write(field_var(&[field.to_string()])) {
                compiler.emit_ins(ins);
            }
            compiler.emit_ins(Instruction::PopCache);
            ends.pop().map(|end| compiler.emit_label(end));
        }
        (Some(fields), SafeWrite::Change(write), None) => {
            compiler.emit_ins(Instruction::PopCache);
            for ins in write(field_var(&fields)) {
                compiler.emit_ins(ins);
            }
            ends.pop().map(|end| compiler.emit_label(end));
        }
        (Some(fields), SafeWrite::Assign, Some(index)) => {
            compiler.emit_ins(Instruction::PopCache);
            if value_form {
                compiler.emit_ins(Instruction::PushTop);
            }
            compiler.emit_ins(Instruction::GetVar(field_var(&fields)));
            emit_value(compiler, &index)?;
            compiler.emit_ins(Instruction::ListSet);
            ends.pop().map(|end| compiler.emit_label(end));
        }
        (Some(fields), SafeWrite::Change(write), Some(index)) => {
            let restore_first = compiler.byond.at_least(super::versions::SAFE_ELEMENT_CACHE);
            if !restore_first {
                warn_list_from_cache(compiler);
            }
            if restore_first {
                compiler.emit_ins(Instruction::PopCache);
            }
            compiler.emit_ins(Instruction::GetVar(field_var(&fields)));
            emit_value(compiler, &index)?;
            if !restore_first {
                compiler.emit_ins(Instruction::PopCache);
            }
            compiler.emit_ins(Instruction::SetVar(Variable::CacheKey));
            compiler.emit_ins(Instruction::SetVar(Variable::Cache));
            for ins in write(Variable::CacheIndex) {
                compiler.emit_ins(ins);
            }
            ends.pop().map(|end| compiler.emit_label(end));
        }
        (Some(fields), SafeWrite::Logical { or }, index) => {
            if !compiler
                .byond
                .at_least(super::versions::SAFE_LOGICAL_ASSIGN)
            {
                compiler.warn(
                    "`?.` in `||=` or `&&=`: before 516.1673 DreamMaker leaves out its null tests (a runtime \
                 error on null, and the first object's var with two `?.`); dmasm keeps them",
                );
            }
            let restore_first =
                index.is_none() || compiler.byond.at_least(super::versions::SAFE_ELEMENT_CACHE);
            if !restore_first {
                warn_list_from_cache(compiler);
            }
            if restore_first {
                compiler.emit_ins(Instruction::PopCache);
            }
            let mut var = field_var(&fields);
            if let Some(index) = index {
                compiler.emit_ins(Instruction::GetVar(var));
                emit_value(compiler, &index)?;
                if !restore_first {
                    compiler.emit_ins(Instruction::PopCache);
                }
                compiler.emit_ins(Instruction::SetVar(Variable::CacheKey));
                compiler.emit_ins(Instruction::SetVar(Variable::Cache));
                var = Variable::CacheIndex;
            }
            compiler.emit_ins(Instruction::GetVar(var.clone()));
            compiler.emit_ins(Instruction::Test);
            let end = ends.pop().expect("a segment");
            compiler.emit_ins(if or {
                Instruction::Jnz(Label::Named(end.clone()))
            } else {
                Instruction::Jz(Label::Named(end.clone()))
            });
            // The place is kept around a value that changes the cache
            let start = compiler.nodes.len();
            compiler.implicit_type = implicit;
            emit_value(compiler, value.expect("a value"))?;
            compiler.implicit_type = None;
            if compiler.nodes[start..]
                .iter()
                .any(|n| matches!(n, Node::Instruction(i, _) if super::procs::touches_cache(i)))
            {
                compiler
                    .nodes
                    .insert(start, Node::Instruction(Instruction::PushCache, ()));
                compiler.emit_ins(Instruction::PopCache);
            }
            compiler.emit_ins(Instruction::SetVar(var));
            compiler.emit_label(end);
        }
        _ => unreachable!("not writable"),
    }
    for end in ends.into_iter().rev() {
        compiler.emit_ins(Instruction::PopCache);
        compiler.emit_label(end);
    }
    Ok(())
}

/// The global a place ending in a static var's field is (`a?.s`, or the list
/// in `a?.s[k]` when `indexed`).
pub(super) fn static_place(
    compiler: &Compiler,
    place: &Expression,
    indexed: bool,
) -> Option<Variable> {
    let Expression::Base { term, follow } = place else {
        return None;
    };
    let base = Expression::Base {
        term: term.clone(),
        follow: Default::default(),
    };
    let follow: Vec<Follow> = follow.iter().map(|f| f.elem.clone()).collect();
    static_member(
        compiler,
        super::procs::expr_type(compiler, &base),
        &follow,
        indexed,
    )
}

/// The same for the fields `follow` of a value of type `ty`.
pub(super) fn static_member(
    compiler: &Compiler,
    ty: Option<dreammaker::objtree::TypeRef>,
    follow: &[Follow],
    indexed: bool,
) -> Option<Variable> {
    let follow = &follow[..follow.len().checked_sub(indexed as usize)?];
    let (Follow::Field(_, name), holder) = (follow.last()?, &follow[..follow.len() - 1]) else {
        return None;
    };
    let mut ty = ty?;
    for f in holder {
        let Follow::Field(_, field) = f else {
            return None;
        };
        ty = super::procs::declared_type(
            compiler,
            &super::procs::member_var(ty, field.as_str())?.get().var_type,
        )?;
    }
    let decl = super::procs::member_var(ty, name.as_str())?;
    if !super::procs::is_global_member(&decl) {
        return None;
    }
    Some(Variable::Global(crate::operands::GlobalVar::named(
        name.as_str().as_bytes(),
        Some(decl.ty().get().path.clone()),
    )))
}

/// A write to the place `a?.b`. False if it can't be written this way.
pub(super) fn emit_safe_write(
    compiler: &mut Compiler,
    place: &Expression,
    value: Option<&Expression>,
    write: SafeWrite,
    value_form: bool,
) -> Result<bool, CompileError> {
    let Some((base, chain)) = safe_place(place) else {
        return Ok(false);
    };
    if !chain.writable(&write, value_form) {
        return Ok(false);
    }
    emit_value(compiler, &base)?;
    // A static var is written directly (`a?.n.s = x` doesn't read a.n)
    let static_var = static_place(compiler, place, chain.index.is_some());
    emit_safe_chain_write(
        compiler,
        chain,
        Some(place),
        static_var,
        value,
        write,
        value_form,
    )?;
    Ok(true)
}

fn emit_expr_statement(compiler: &mut Compiler, expr: &Expression) -> Result<(), CompileError> {
    // A type path on its own line does nothing at all
    if matches!(expr, Expression::Base { term, follow }
        if follow.is_empty() && matches!(&term.elem, Term::Prefab(p) if p.vars.is_empty()))
    {
        return Ok(());
    }
    // nor does a variable, a field of one or null (`x`, `(a.b)`)
    if let Expression::Base { term, follow } = strip_parens(expr) {
        let plain = matches!(term.elem, Term::Ident(_) | Term::Null)
            && follow.iter().all(|f| {
                matches!(
                    f.elem,
                    Follow::Field(
                        dreammaker::ast::PropertyAccessKind::Dot
                            | dreammaker::ast::PropertyAccessKind::Colon,
                        _
                    )
                )
            });
        if plain {
            return Ok(());
        }
    }
    emit_expr_statement_inner(compiler, expr)?;
    // Flag-only tests leave no stack value to pop. Preserve the separate
    // comparison sequence `Teq; Pop; GetFlag; Pop`.
    let n = compiler.nodes.len();
    if n >= 3
        && matches!(
            compiler.nodes[n - 1],
            Node::Instruction(Instruction::Pop, _)
        )
        && matches!(
            compiler.nodes[n - 2],
            Node::Instruction(Instruction::GetFlag, _)
        )
        && !matches!(
            compiler.nodes[n - 3],
            Node::Instruction(Instruction::Pop, _)
        )
    {
        compiler.nodes.truncate(n - 2);
    }
    // (`(x |= y)` counts too, and so does reading into x with `>>`)
    let assignment = match strip_parens(expr) {
        Expression::BinaryOp {
            op: dreammaker::ast::BinaryOp::RShift,
            ..
        } => true,
        Expression::AssignOp { lhs, .. } => !matches!(&**lhs, Expression::Base { follow, .. }
            if matches!(follow.last().map(|f| &f.elem), Some(Follow::Index(..)))),
        Expression::Base { follow, .. } => matches!(
            follow.last().map(|f| &f.elem),
            Some(Follow::Unary(
                UnaryOp::PreIncr | UnaryOp::PostIncr | UnaryOp::PreDecr | UnaryOp::PostDecr
            ))
        ),
        _ => false,
    };
    // (except `(x = y)` compiled as a value and popped; `(x |= y)` counts)
    let n = compiler.nodes.len();
    let value_set = n >= 2
        && matches!(
            compiler.nodes[n - 1],
            Node::Instruction(Instruction::Pop, _)
        )
        && matches!(
            compiler.nodes[n - 2],
            Node::Instruction(Instruction::SetVarExpr(_), _)
        );
    if assignment && !value_set {
        compiler.end_assignment_statement();
    }
    Ok(())
}

fn emit_expr_statement_inner(
    compiler: &mut Compiler,
    expr: &Expression,
) -> Result<(), CompileError> {
    match expr {
        // `x = a in L` is `(x = a) in L`, whose result DreamMaker leaves be
        Expression::BinaryOp {
            op: dreammaker::ast::BinaryOp::In,
            lhs,
            rhs,
        } if matches!(&**lhs, Expression::AssignOp { .. }) => {
            emit_value(compiler, rhs)?;
            emit_value(compiler, lhs)?;
            compiler.emit_ins(Instruction::IsIn(crate::operands::IsInParams::Value));
            Ok(())
        }

        Expression::AssignOp { op, lhs, rhs }
            if !matches!(op, AssignOp::AndAssign | AssignOp::OrAssign)
                && safe_place(lhs).is_some() =>
        {
            let op = *op;
            let element = safe_place(lhs).is_some_and(|(_, c)| c.index.is_some());
            let to_field = move |var: Variable| {
                vec![if op == AssignOp::Assign {
                    Instruction::SetVar(var)
                } else {
                    aug_instruction(op, var)
                }]
            };
            let write = if element && op == AssignOp::Assign {
                SafeWrite::Assign
            } else {
                SafeWrite::Change(&to_field)
            };
            if !emit_safe_write(compiler, lhs, Some(rhs), write, false)? {
                return Err(CompileError::ExpectedLValue);
            }
            Ok(())
        }

        // `a?.b ||= v`
        Expression::AssignOp {
            op: op @ (AssignOp::OrAssign | AssignOp::AndAssign),
            lhs,
            rhs,
        } if safe_place(lhs)
            .is_some_and(|(_, c)| c.writable(&SafeWrite::Logical { or: true }, false)) =>
        {
            let write = SafeWrite::Logical {
                or: *op == AssignOp::OrAssign,
            };
            emit_safe_write(compiler, lhs, Some(rhs), write, false)?;
            Ok(())
        }

        Expression::AssignOp { op, lhs, rhs }
            if !matches!(op, AssignOp::AndAssign | AssignOp::OrAssign) =>
        {
            if *op == AssignOp::Assign {
                compiler.implicit_type = compiler
                    .static_type_name(lhs)
                    .or_else(|| list_type_of(compiler, lhs));
            }
            emit_value(compiler, rhs)?;
            compiler.implicit_type = None;
            warn_pointer_index(compiler, lhs);
            // (no line for the place of `-=` and the like; `=` to a list
            // element gets one)
            compiler.place = *op != AssignOp::Assign;
            let kind = compiler.emit_inner_expr(unparenthesised(lhs))?;
            if *op == AssignOp::Assign {
                if kind == EvalKind::ListRef {
                    compiler.emit_ins(Instruction::ListSet);
                    return Ok(());
                }
                let var = lvalue_var(compiler, kind)?;
                compiler.emit_ins(Instruction::SetVar(var));
                return Ok(());
            }
            let var = lvalue_var(compiler, kind)?;
            compiler.emit_ins(aug_instruction(*op, var));
            Ok(())
        }

        Expression::Base { term, follow }
            if matches!(
                follow.last().map(|f| &f.elem),
                Some(Follow::Unary(
                    UnaryOp::PreIncr | UnaryOp::PostIncr | UnaryOp::PreDecr | UnaryOp::PostDecr
                ))
            ) =>
        {
            let Some(Follow::Unary(op)) = follow.last().map(|f| f.elem.clone()) else {
                unreachable!()
            };
            // (`(*p)++` steps what p points at)
            let inner = unparenthesised(&Expression::Base {
                term: term.clone(),
                follow: follow[..follow.len() - 1].to_vec().into_boxed_slice(),
            });
            let inc = matches!(op, UnaryOp::PreIncr | UnaryOp::PostIncr);
            let step = move |var: Variable| {
                vec![if inc {
                    Instruction::Inc(var)
                } else {
                    Instruction::Dec(var)
                }]
            };
            if emit_safe_write(compiler, &inner, None, SafeWrite::Change(&step), false)? {
                return Ok(());
            }
            let kind = compiler.emit_inner_expr(inner)?;
            let var = lvalue_var(compiler, kind)?;
            match op {
                UnaryOp::PreIncr | UnaryOp::PostIncr => compiler.emit_ins(Instruction::Inc(var)),
                _ => compiler.emit_ins(Instruction::Dec(var)),
            }
            Ok(())
        }

        // `x ||= y` as a statement: if (!x) x = y
        Expression::AssignOp {
            op: op @ (AssignOp::OrAssign | AssignOp::AndAssign),
            lhs,
            rhs,
        } => {
            let end = compiler.new_label();
            let jump = |end: &str| {
                if *op == AssignOp::OrAssign {
                    Instruction::Jnz(Label::Named(end.to_owned()))
                } else {
                    Instruction::Jz(Label::Named(end.to_owned()))
                }
            };
            // `x ||= new()` makes x's declared type
            let implicit = compiler
                .static_type_name(lhs)
                .or_else(|| list_type_of(compiler, lhs));
            // A list element or field is found once, and kept in the cache
            // (saved around the new value if that uses the cache too)
            let kind = compiler.emit_inner_expr((**lhs).clone())?;
            if matches!(kind, EvalKind::ListRef | EvalKind::Field(..)) {
                let field = matches!(kind, EvalKind::Field(..));
                let var = lvalue_var(compiler, kind)?;
                // Re-emit the field target unless it was cached before the test.
                // Do not reuse field chains loaded by the test when evaluating the RHS.
                let before = compiler.cache.clone();
                // (and as the test read it: `src.x` written out doesn't reuse a
                // cache a bare `x` loaded, for the assignment either)
                let explicit_src = compiler.explicit_src;
                compiler.emit_ins(Instruction::GetVar(var.clone()));
                if field && compiler.cache.as_ref().is_some_and(|h| h.len() > 1) {
                    compiler.cache = before.clone();
                }
                compiler.emit_ins(Instruction::Test);
                compiler.emit_ins(jump(&end));
                let start = compiler.nodes.len();
                compiler.implicit_type = implicit.clone();
                compiler.global_field = false;
                emit_value(compiler, rhs)?;
                compiler.implicit_type = None;
                // Use source-level cache dependencies, including global calls,
                // list indexes and subexpressions removed by constant folding.
                let saves = std::mem::take(&mut compiler.global_field)
                    || saves_cache(compiler, start)
                    || unfolded_saves_cache(compiler, rhs);
                // (a list element's key too, when the value indexes a list)
                let saves_key = !field
                    && compiler.nodes[start..]
                        .iter()
                        .any(|n| matches!(n, Node::Instruction(Instruction::ListGet, _)));
                if saves {
                    if saves_key {
                        compiler
                            .nodes
                            .insert(start, Node::Instruction(Instruction::PushCacheKey, ()));
                    }
                    compiler
                        .nodes
                        .insert(start, Node::Instruction(Instruction::PushCache, ()));
                    if saves_key {
                        compiler.emit_ins(Instruction::PopCacheKey);
                    }
                    compiler.emit_ins(Instruction::PopCache);
                }
                if field {
                    compiler.cache = before;
                    compiler.explicit_src = explicit_src;
                }
                compiler.emit_ins(Instruction::SetVar(var));
                compiler.emit_label(end);
                return Ok(());
            }
            let var = lvalue_var(compiler, kind)?;
            compiler.emit_ins(Instruction::GetVar(var.clone()));
            compiler.emit_ins(Instruction::Test);
            compiler.emit_ins(jump(&end));
            // (the value doesn't reuse a chain through fields)
            compiler.end_assignment_statement();
            compiler.implicit_type = implicit.clone();
            emit_value(compiler, rhs)?;
            compiler.implicit_type = None;
            compiler.emit_ins(Instruction::SetVar(var));
            compiler.emit_label(end);
            Ok(())
        }

        Expression::BinaryOp {
            op: dreammaker::ast::BinaryOp::LShift,
            lhs,
            rhs,
        } => emit_output(compiler, lhs, rhs),

        Expression::BinaryOp {
            op: dreammaker::ast::BinaryOp::RShift,
            lhs,
            rhs,
        } => emit_input(compiler, lhs, rhs),

        Expression::Base { term, follow }
            if follow.is_empty()
                && matches!(&term.elem, Term::Call(name, args)
                    if super::builtin_procs::emit_statement_builtin(compiler, name.as_str(), args)?) =>
        {
            Ok(())
        }

        other => {
            emit_value(compiler, other)?;
            // Statement calls use the statement opcode except for parenthesized
            // calls or receivers saved on the cache stack.
            let n = compiler.nodes.len();
            let parenthesized = matches!(other, Expression::Base { term, follow }
                if follow.is_empty() && matches!(term.elem, Term::Expr(_)));
            let keep_call = parenthesized
                || n >= 2
                    && matches!(
                        compiler.nodes[n - 2],
                        Node::Instruction(Instruction::PopCache, _)
                    );
            if let Some(Node::Instruction(ins @ Instruction::Call(..), _)) =
                compiler.nodes.last_mut().filter(|_| !keep_call)
            {
                if let Instruction::Call(var, count) = ins.clone() {
                    *ins = Instruction::CallStatement(var, count);
                }
            }
            // missile() leaves nothing (and can't be a value)
            if !matches!(
                compiler.nodes.last(),
                Some(Node::Instruction(Instruction::Missile, _))
            ) {
                compiler.emit_ins(Instruction::Pop);
            }
            Ok(())
        }
    }
}

/// Resolve the left operand of statement I/O. Savefile entries such as
/// `S["key"]` use Index.
fn emit_io_target(compiler: &mut Compiler, target: &Expression) -> Result<(), CompileError> {
    if let Expression::Base { term, follow } = target {
        if let Some(Follow::Index(dreammaker::ast::ListAccessKind::Normal, key)) =
            follow.last().map(|f| &f.elem)
        {
            let base = Expression::Base {
                term: term.clone(),
                follow: follow[..follow.len() - 1].to_vec().into_boxed_slice(),
            };
            emit_value(compiler, &base)?;
            emit_value(compiler, key)?;
            compiler.emit_ins(Instruction::Index);
            return Ok(());
        }
    }
    emit_value(compiler, target)
}

/// `source >> place` as a statement reads input into place.
fn emit_input(
    compiler: &mut Compiler,
    source: &Expression,
    place: &Expression,
) -> Result<(), CompileError> {
    emit_io_target(compiler, source)?;
    compiler.emit_ins(Instruction::Read);
    let kind = compiler.emit_inner_expr(place.clone())?;
    if kind == EvalKind::ListRef {
        compiler.emit_ins(Instruction::ListSet);
    } else {
        let var = lvalue_var(compiler, kind)?;
        compiler.emit_ins(Instruction::SetVar(var));
    }
    Ok(())
}

fn emit_output(
    compiler: &mut Compiler,
    target: &Expression,
    value: &Expression,
) -> Result<(), CompileError> {
    emit_io_target(compiler, target)?;
    if let Expression::Base { term, follow } = strip_parens(value) {
        if follow.is_empty() {
            match &term.elem {
                Term::Call(name, args) => {
                    let n = args.len();
                    let ins = match (name.as_str(), n) {
                        ("browse", 1) => Some(Instruction::Browse),
                        ("browse", 2) => Some(Instruction::BrowseOpt),
                        ("browse_rsc", 1 | 2) => Some(Instruction::BrowseRsc),
                        ("output", 2) => Some(Instruction::WinOutput),
                        ("link", 1) => Some(Instruction::Link),
                        ("ftp", 1 | 2) => Some(Instruction::OutputFtp),
                        ("run", 1) => Some(Instruction::OutputRun),
                        // x << load_resource(a, b): the target counts as an argument
                        ("load_resource", _) => Some(Instruction::LoadResource(n as u32 + 1)),
                        _ => None,
                    };
                    if let Some(ins) = ins {
                        for arg in args.iter() {
                            emit_value(compiler, arg)?;
                        }
                        let pad = match (name.as_str(), n) {
                            ("ftp", 1) | ("browse_rsc", 1) => 1,
                            _ => 0,
                        };
                        for _ in 0..pad {
                            compiler.emit_ins(Instruction::PushVal(Value::Null));
                        }
                        compiler.emit_ins(ins);
                        return Ok(());
                    }
                }
                Term::InterpString(..) => {
                    emit_value(compiler, value)?;
                    if let Some(Node::Instruction(Instruction::Format(pattern, count), _)) =
                        compiler.nodes.last().cloned()
                    {
                        compiler.nodes.pop();
                        compiler.emit_ins(Instruction::OutputFormat(pattern, count));
                        return Ok(());
                    }
                    compiler.emit_ins(Instruction::Output);
                    return Ok(());
                }
                _ => {}
            }
        }
    }
    emit_value(compiler, value)?;
    compiler.emit_ins(Instruction::Output);
    Ok(())
}

pub(super) fn aug_instruction(op: AssignOp, var: Variable) -> Instruction {
    match op {
        AssignOp::AddAssign => Instruction::AugAdd(var),
        AssignOp::SubAssign => Instruction::AugSub(var),
        AssignOp::MulAssign => Instruction::AugMul(var),
        AssignOp::DivAssign => Instruction::AugDiv(var),
        AssignOp::ModAssign => Instruction::AugMod(var),
        AssignOp::FloatModAssign => Instruction::AugFloatMod(var),
        AssignOp::AssignInto => Instruction::AssignInto(var),
        AssignOp::BitAndAssign => Instruction::AugBand(var),
        AssignOp::BitOrAssign => Instruction::AugBor(var),
        AssignOp::BitXorAssign => Instruction::AugXor(var),
        AssignOp::LShiftAssign => Instruction::AugLShift(var),
        AssignOp::RShiftAssign => Instruction::AugRShift(var),
        AssignOp::Assign | AssignOp::AndAssign | AssignOp::OrAssign => unreachable!(),
    }
}

/// `a?.b ||= v` (or `&&=`) as a value: a.b, or v when that's false and gets
/// assigned, or null when a is null. Only for a plain `a?.b.c` place.
pub(super) fn emit_safe_logical_assign(
    compiler: &mut Compiler,
    place: &Expression,
    rhs: &Expression,
    or: bool,
) -> Result<bool, CompileError> {
    let Some((base, chain)) = safe_place(place) else {
        return Ok(false);
    };
    let Some(fields) = chain.fields() else {
        return Ok(false);
    };
    if !chain.segments.is_empty() || chain.index.is_some() || fields.is_empty() {
        return Ok(false);
    }
    if !compiler
        .byond
        .at_least(super::versions::SAFE_LOGICAL_ASSIGN)
    {
        compiler.warn(
            "`?.` in `||=` or `&&=`: before 516.1673 DreamMaker leaves out its null tests (a runtime \
                 error on null, and the first object's var with two `?.`); dmasm keeps them",
        );
    }
    let mut builder = super::chain_builder::ChainBuilder::begin(Variable::Cache);
    for f in &fields[..fields.len() - 1] {
        builder.append(crate::operands::DMString(f.clone().into_bytes()));
    }
    let var = builder.get_field(crate::operands::DMString(
        fields[fields.len() - 1].clone().into_bytes(),
    ));
    emit_value(compiler, &base)?;
    let end = compiler.new_label();
    compiler.emit_ins(Instruction::SetCacheJmpIfNull(Label::Named(end.clone())));
    compiler.emit_ins(Instruction::PushCache);
    compiler.emit_ins(Instruction::PopCache);
    compiler.emit_ins(Instruction::GetVar(var.clone()));
    compiler.emit_ins(if or {
        Instruction::JmpOr(Label::Named(end.clone()))
    } else {
        Instruction::JmpAnd(Label::Named(end.clone()))
    });
    let start = compiler.nodes.len();
    compiler.implicit_type = compiler.static_type_name(place);
    emit_value(compiler, rhs)?;
    compiler.implicit_type = None;
    // (DM doesn't keep the place when v changes the cache; we do)
    if compiler.nodes[start..]
        .iter()
        .any(|n| matches!(n, Node::Instruction(i, _) if super::procs::touches_cache(i)))
    {
        compiler
            .nodes
            .insert(start, Node::Instruction(Instruction::PushCache, ()));
        compiler.emit_ins(Instruction::PopCache);
    }
    compiler.emit_ins(Instruction::SetVarExpr(var));
    compiler.emit_label(end);
    Ok(true)
}

/// A parameter default DreamMaker reads as a path when it's written without
/// spaces: a var or a field of one (`a.b`), divided by a number
fn path_division(default: &Expression) -> bool {
    let Expression::BinaryOp {
        op: dreammaker::ast::BinaryOp::Div,
        lhs,
        rhs,
    } = strip_parens(default)
    else {
        return false;
    };
    let name_chain = matches!(&**lhs, Expression::Base { term, follow }
        if matches!(term.elem, Term::Ident(_))
            && follow.iter().all(|f| matches!(f.elem, Follow::Field(dreammaker::ast::PropertyAccessKind::Dot, _))));
    let number = matches!(&**rhs, Expression::Base { term, follow }
        if matches!(term.elem, Term::Int(_) | Term::Float(_))
            && follow.iter().all(|f| matches!(f.elem, Follow::Unary(UnaryOp::Neg))));
    name_chain && number
}

/// Before 516.1685 the list of `a?.L[k] += v` is read from the cache, which
/// the value has changed when it uses the cache itself
fn warn_list_from_cache(compiler: &Compiler) {
    let since = compiler
        .nodes
        .iter()
        .rposition(|n| matches!(n, Node::Instruction(Instruction::PushCache, _)))
        .map_or(0, |i| i + 1);
    if compiler.nodes[since..]
        .iter()
        .any(|n| matches!(n, Node::Instruction(i, _) if super::procs::touches_cache(i)))
    {
        compiler.warn(
            "before 516.1685, safe list assignment resolves the list using the RHS cache value, matching DreamMaker",
        );
    }
}

/// `*p[i] = x`: from 516.1668 DreamMaker leaves the index out
pub(super) fn warn_pointer_index(compiler: &Compiler, place: &Expression) {
    if let Expression::Base { follow, .. } = strip_parens(place) {
        if compiler
            .byond
            .at_least(super::versions::POINTER_INDEX_DROPPED)
            && follow.iter().any(|f| matches!(f.elem, Follow::Index(..)))
            && follow
                .iter()
                .any(|f| matches!(f.elem, Follow::Unary(UnaryOp::Dereference)))
        {
            compiler.warn(
                "`*p[i] = x`: from 516.1668 DreamMaker leaves the index out; dmasm writes the indexed element",
            );
        }
    }
}
