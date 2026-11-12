use dreammaker::ast::{BinaryOp, Expression};

use crate::{
    compiler::{CompileError, Compiler, EvalKind},
    operands::{self, Label},
    Instruction,
};

/// Flatten a logical chain in evaluation order. The leftmost operand can be
/// parenthesized. Elsewhere, parentheses join the chain only with `rhs_parens`
/// (outside if/while conditions).
fn flatten(
    op: BinaryOp,
    expr: Expression,
    rhs_parens: bool,
    leftmost: bool,
    out: &mut Vec<Expression>,
) {
    use dreammaker::ast::Term;
    match expr {
        Expression::BinaryOp {
            op: inner,
            lhs,
            rhs,
        } if inner == op => {
            flatten(op, *lhs, rhs_parens, leftmost, out);
            flatten(op, *rhs, rhs_parens, false, out);
        }
        Expression::Base { term, follow }
            if (leftmost || rhs_parens)
                && follow.is_empty()
                && matches!(&term.elem, Term::Expr(_)) =>
        {
            let Term::Expr(inner) = term.elem else {
                unreachable!()
            };
            // Only a chain of the same operator merges; anything else stays one operand
            let mut probe = &*inner;
            while let Expression::Base { term, follow } = probe {
                match &term.elem {
                    Term::Expr(e) if follow.is_empty() => probe = e,
                    _ => break,
                }
            }
            if matches!(probe, Expression::BinaryOp { op: inner_op, .. } if *inner_op == op) {
                flatten(op, *inner, rhs_parens, leftmost, out);
            } else {
                out.push(Expression::Base {
                    term: Box::new(dreammaker::ast::Spanned::new(
                        term.location,
                        Term::Expr(inner),
                    )),
                    follow,
                });
            }
        }
        other => out.push(other),
    }
}

/// The expression inside parentheses, if `expr` is a parenthesized `op`.
fn paren_op(op: BinaryOp, expr: &Expression) -> Option<&Expression> {
    let mut inner = expr;
    let mut parens = false;
    while let Expression::Base { term, follow } = inner {
        match &term.elem {
            dreammaker::ast::Term::Expr(e) if follow.is_empty() => {
                inner = e;
                parens = true;
            }
            _ => break,
        }
    }
    (parens
        && matches!(
            inner,
            Expression::BinaryOp { op: inner_op, .. } if *inner_op == op
        ))
    .then_some(inner)
}

fn parenthesized(expr: Expression) -> Expression {
    // (not Expression::from, which drops the parentheses)
    Expression::Base {
        term: Box::new(dreammaker::ast::Spanned::invalid(
            dreammaker::ast::Term::Expr(Box::new(expr)),
        )),
        follow: Box::new([]),
    }
}

/// Match DreamMaker regrouping for +, *, &, | and ^:
/// `a + (b + c) + d` becomes `a + ((b + c) + d)`. Apply recursively
/// so grouped constants fold together.
fn regroup(op: BinaryOp, lhs: Expression, rhs: Expression) -> (Expression, Expression) {
    let lhs = match paren_op(op, &lhs) {
        Some(inner) => inner.clone(),
        None => lhs,
    };
    let lhs = match lhs {
        Expression::BinaryOp {
            op: o,
            lhs: a,
            rhs: b,
        } if o == op => {
            let (a, b) = regroup(op, *a, *b);
            Expression::BinaryOp {
                op,
                lhs: Box::new(a),
                rhs: Box::new(b),
            }
        }
        other => other,
    };
    match lhs {
        Expression::BinaryOp {
            op: o,
            lhs: a,
            rhs: group,
        } if o == op && paren_op(op, &group).is_some() => {
            let sum = Expression::BinaryOp {
                op,
                lhs: group,
                rhs: Box::new(rhs),
            };
            (*a, parenthesized(sum))
        }
        other => (other, rhs),
    }
}

/// `regroup` applied to every operation in an expression (not inside calls or
/// parentheses, which are regrouped on their own).
fn regroup_all(expr: Expression) -> Expression {
    match expr {
        Expression::BinaryOp { op, lhs, rhs } => {
            let (lhs, rhs) = if matches!(
                op,
                BinaryOp::Add
                    | BinaryOp::Mul
                    | BinaryOp::BitAnd
                    | BinaryOp::BitOr
                    | BinaryOp::BitXor
            ) {
                regroup(op, *lhs, *rhs)
            } else {
                (*lhs, *rhs)
            };
            Expression::BinaryOp {
                op,
                lhs: Box::new(regroup_all(lhs)),
                rhs: Box::new(regroup_all(rhs)),
            }
        }
        other => other,
    }
}

pub(super) fn emit(
    compiler: &mut Compiler<'_>,
    op: BinaryOp,
    lhs: Expression,
    rhs: Expression,
) -> Result<EvalKind, CompileError> {
    let mul_lhs = std::mem::take(&mut compiler.mul_lhs);
    let associative = matches!(
        op,
        BinaryOp::Add | BinaryOp::Mul | BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor
    );
    let root = std::mem::take(&mut compiler.regroup_chain);
    let (lhs, rhs) = if root && compiler.proc.is_some() {
        // (and so is every operation under it)
        let (lhs, rhs) = if associative {
            regroup(op, lhs, rhs)
        } else {
            (lhs, rhs)
        };
        (regroup_all(lhs), regroup_all(rhs))
    } else {
        (lhs, rhs)
    };
    let kind = match op {
        // Short circuiting logic ops
        BinaryOp::And | BinaryOp::Or => {
            // `a || b || c` short-circuits straight to the end of the chain, also
            // through parentheses: `(a || b) || (c || d)` is one chain
            let mut operands = vec![];
            let in_proc = compiler.proc.is_some();
            let rhs_parens = in_proc && !std::mem::take(&mut compiler.condition_chain);
            let chain = Expression::BinaryOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
            flatten(op, chain, rhs_parens, in_proc, &mut operands);

            // Fold short-circuit constants anywhere in the chain. Dropping
            // an operand also invalidates DreamMaker's cache knowledge.
            let mut forget = vec![false; operands.len()];
            if compiler.proc.is_some() {
                let mut i = 0;
                while i + 1 < operands.len() {
                    let Some(c) = super::fold::value(compiler, &operands[i]) else {
                        i += 1;
                        continue;
                    };
                    if c.truthy() == matches!(op, BinaryOp::Or) {
                        operands.truncate(i + 1);
                        forget.truncate(i + 1);
                        // (a number that's all that's left forgets it as any
                        // folded number does; text, null or a path has to be
                        // told to: `"a" || x`, `null && x`)
                        if i == 0 && !matches!(c, super::fold::Constant::Number(_)) {
                            forget[0] = true;
                        }
                    } else {
                        operands.remove(i);
                        forget.remove(i);
                        forget[i] = true;
                    }
                }
                if operands.len() == 1 {
                    if forget[0] {
                        compiler.cache = None;
                    }
                    let only = operands.pop().unwrap();
                    let kind = compiler.emit_expr(only)?;
                    return compiler.emit_move_to_stack(kind);
                }
            }

            let label = format!("LAB_{:0>4X}", compiler.label_count);
            compiler.label_count += 1;
            let count = operands.len();
            // Each operand can be the value assigned: `x = (locate() in L) || new`
            let implicit = compiler.implicit_type.take();
            for (i, operand) in operands.into_iter().enumerate() {
                if forget[i] {
                    compiler.cache = None;
                }
                compiler.implicit_type = implicit.clone();
                let kind = compiler.emit_expr(operand);
                compiler.implicit_type = None;
                let kind = kind?;
                compiler.emit_move_to_stack(kind)?;
                if i + 1 < count {
                    compiler.emit_ins(match op {
                        BinaryOp::And => Instruction::JmpAnd(Label::Named(label.clone())),
                        _ => Instruction::JmpOr(Label::Named(label.clone())),
                    });
                }
            }

            compiler.emit_label(label);
            EvalKind::Stack
        }

        // x ** 2 and x ** -1 have instructions of their own
        BinaryOp::Pow
            if compiler.proc.is_some()
                && matches!(super::fold::number(compiler, &rhs), Some(n) if n == 2.0 || n == -1.0) =>
        {
            let square = super::fold::number(compiler, &rhs) == Some(2.0);
            let lhs = compiler.emit_expr(lhs)?;
            compiler.emit_move_to_stack(lhs)?;
            compiler.emit_ins(if square {
                Instruction::PowSquare
            } else {
                Instruction::PowNegativeOne
            });
            EvalKind::Stack
        }

        // x * x squares x
        BinaryOp::Mul
            if compiler.proc.is_some()
                && !mul_lhs
                && same_variable(compiler, &lhs) == same_variable(compiler, &rhs)
                && is_plain_variable(&lhs) =>
        {
            let lhs = compiler.emit_expr(lhs)?;
            compiler.emit_move_to_stack(lhs)?;
            compiler.emit_ins(Instruction::PowSquare);
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
        | BinaryOp::LessOrGreater
        | BinaryOp::Equiv
        | BinaryOp::NotEquiv
        | BinaryOp::BitAnd
        | BinaryOp::BitXor
        | BinaryOp::BitOr
        | BinaryOp::LShift
        | BinaryOp::RShift
        | BinaryOp::FloatMod => {
            // A constant operand of `&`, `|` or `^` is stored as its 24 bits
            // (not of a shift)
            let bit_op = compiler.proc.is_some()
                && matches!(op, BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor);
            for (i, operand) in vec![lhs, rhs].into_iter().enumerate() {
                match super::fold::number(compiler, &operand) {
                    // (only on the left)
                    Some(n)
                        if bit_op
                            && i == 0
                            && !(compiler.expr_scope.is_some() && super::fold::wide(n)) =>
                    {
                        super::fold::warn_bits(compiler, n);
                        compiler.emit_number(super::fold::bits(n) as f32)
                    }
                    // A const on the left of `*` is its value, not its global
                    Some(n) if op == BinaryOp::Mul && i == 0 && compiler.proc.is_some() => {
                        compiler.emit_number(n)
                    }
                    _ => {
                        // `x * x * y` doesn't square x
                        compiler.mul_lhs = op == BinaryOp::Mul && i == 0;
                        let kind = compiler.emit_expr(operand)?;
                        compiler.mul_lhs = false;
                        compiler.emit_move_to_stack(kind)?;
                    }
                }
            }

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
                BinaryOp::LessOrGreater => compiler.emit_ins(Instruction::Spaceship),
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

/// Normalize operands for DreamMaker's `x * x` optimization.
/// Strip parentheses and normalize unshadowed `global.x` to `x`.
fn same_variable(compiler: &Compiler, expr: &Expression) -> Expression {
    use dreammaker::ast::{Follow, Term};
    let mut expr = expr;
    while let Expression::Base { term, follow } = expr {
        match &term.elem {
            Term::Expr(inner) if follow.is_empty() => expr = inner,
            _ => break,
        }
    }
    if let Expression::Base { term, follow } = expr {
        if let (Term::Ident(base), [first, rest @ ..]) = (&term.elem, &follow[..]) {
            if let Follow::Field(_, name) = &first.elem {
                // (the bare name is the global too)
                let global = matches!(
                    super::procs::find_var(compiler, name.as_str()),
                    Some(EvalKind::Var(crate::operands::Variable::Global(g))) if g.scope.as_deref() == Some("")
                );
                if base.as_str() == "global" && global {
                    return Expression::Base {
                        term: Box::new(dreammaker::ast::Spanned::new(
                            term.location,
                            Term::Ident(name.clone()),
                        )),
                        follow: rest.to_vec().into_boxed_slice(),
                    };
                }
            }
        }
    }
    expr.clone()
}

fn is_plain_variable(expr: &Expression) -> bool {
    use dreammaker::ast::{Follow, Term, UnaryOp};
    match expr {
        Expression::Base { term, follow } => {
            let base = match &term.elem {
                // (not with a fraction in it: DreamMaker doesn't see two equal
                // fractions as the same)
                Term::Ident(_) | Term::Int(_) => true,
                // (and text, null and type paths, but not embedded text)
                Term::String(_) | Term::Null | Term::Prefab(_) => true,
                Term::Expr(inner) => is_plain_variable(inner),
                _ => false,
            };
            base && follow
                .iter()
                .all(|f| matches!(f.elem, Follow::Field(..) | Follow::Unary(UnaryOp::Neg)))
        }
        Expression::BinaryOp { op, lhs, rhs } => {
            matches!(
                op,
                BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div
            ) && is_plain_variable(lhs)
                && is_plain_variable(rhs)
        }
        _ => false,
    }
}
