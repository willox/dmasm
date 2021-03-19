use dreammaker::ast::Expression;

use crate::compiler::*;
use crate::Instruction;

pub(super) fn emit(compiler: &mut Compiler<'_>, term: Term) -> Result<EvalKind, CompileError> {
    match term {
        // Nested expression, probably something in brackets
        Term::Expr(expr) => compiler.emit_expr(*expr),

        // Simple stack pushes
        Term::Null => {
            compiler.emit_ins(Instruction::PushVal(Value::Null));
            Ok(EvalKind::Stack)
        }
        Term::Int(i) => {
            compiler.emit_ins(Instruction::PushInt(i));
            Ok(EvalKind::Stack)
        }
        Term::Float(f) => {
            compiler.emit_ins(Instruction::PushVal(Value::Number(f)));
            Ok(EvalKind::Stack)
        }
        Term::String(str) => {
            compiler.emit_ins(Instruction::PushVal(Value::DMString(DMString(str.into()))));
            Ok(EvalKind::Stack)
        }

        // Identifiers. These could be params or globals.
        Term::Ident(ident) => Ok(compiler.emit_find_var(ident)),

        // Resources
        Term::Resource(resource) => {
            compiler.emit_ins(Instruction::PushVal(Value::Resource(resource)));
            Ok(EvalKind::Stack)
        }

        // Type paths: We don't support the anonymous kind with variable declarations.
        Term::Prefab(prefab) => {
            if !prefab.vars.is_empty() {
                return Err(CompileError::UnsupportedPrefabWithVars);
            }

            let mut path = String::new();

            // TODO: Relative stuff
            for (op, part) in prefab.path {
                use std::fmt::Write;
                write!(&mut path, "{}{}", op, part).unwrap();
            }

            compiler.emit_ins(Instruction::PushVal(Value::Path(path)));
            Ok(EvalKind::Stack)
        }

        Term::Call(ident, args) => {
            // If any of the arguments are a Expression:AssignOp, byond does _crazy_ not-so-well defined things.
            // We can implement this later...
            if args
                .iter()
                .any(|x| matches!(x, Expression::AssignOp { .. }))
            {
                return Err(CompileError::NamedArgumentsNotImplemented);
            }

            match builtin_procs::emit(compiler, &ident, &args)? {
                // Handled by builtin_procs
                Some(kind) => Ok(kind),

                // We've got to call a proc
                None => {
                    let arg_count = args.len() as u32;

                    // Bring all arguments onto the stack
                    for arg in args {
                        let expr = compiler.emit_expr(arg)?;
                        compiler.emit_move_to_stack(expr)?;
                    }

                    // We're treating all Term::Call expressions as global calls
                    compiler.emit_ins(Instruction::CallGlob(
                        arg_count,
                        operands::Proc(format!("/proc/{}", ident)),
                    ));

                    Ok(EvalKind::Stack)
                }
            }
        }

        other => Err(CompileError::UnsupportedExpressionTerm(other)),
    }
}
