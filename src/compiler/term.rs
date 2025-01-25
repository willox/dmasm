use std::convert::TryInto;

use dreammaker::ast::*;
use operands::PickProbParams;

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
            compiler.emit_ins(Instruction::PushVal(Value::DMString(strings::parse(&str)?)));
            Ok(EvalKind::Stack)
        }

        // Identifiers. These could be params or globals.
        Term::Ident(ident) => Ok(compiler.emit_find_var(ident)),
        Term::GlobalIdent(ident) => Ok(EvalKind::Var(Variable::Global(DMString(
            ident.as_str().into(),
        )))),

        // Resources
        Term::Resource(resource) => {
            compiler.emit_ins(Instruction::PushVal(Value::Resource(resource)));
            Ok(EvalKind::Stack)
        }

        // as() stuff. We might eventually change this to its own EvalKind so statements can act on them better.
        Term::As(val) => {
            compiler.emit_ins(Instruction::PushInt(val.bits() as i32));
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

        Term::Call(ident, args) | Term::GlobalCall(ident, args) => {
            let args_vec = args.into_vec();
            match builtin_procs::emit(compiler, &ident, &args_vec)? {
                // Handled by builtin_procs
                Some(kind) => Ok(kind),

                // We've got to call a proc
                None => {
                    let arg_count = args_vec.len() as u32;

                    match args::emit(compiler, args::ArgsContext::Proc, args_vec)? {
                        args::ArgsResult::Normal => {
                            // We're treating all Term::Call expressions as global calls
                            compiler.emit_ins(Instruction::CallGlob(
                                arg_count,
                                operands::Proc(format!("/proc/{}", ident)),
                            ));
                        }

                        args::ArgsResult::Assoc => {
                            compiler.emit_ins(Instruction::NewAssocList(arg_count));
                            compiler.emit_ins(Instruction::CallGlobalArgList(operands::Proc(
                                format!("/proc/{}", ident),
                            )));
                        }

                        args::ArgsResult::ArgList => {
                            compiler.emit_ins(Instruction::CallGlobalArgList(operands::Proc(
                                format!("/proc/{}", ident),
                            )));
                        }
                    }

                    Ok(EvalKind::Stack)
                }
            }
        }

        Term::DynamicCall(lhs, rhs) => {
            let lhs_len = lhs.len();
            let rhs_len = rhs.len();

            if lhs.is_empty() {
                return Err(CompileError::MissingArgument {
                    proc: "call".to_owned(),
                    index: 1,
                });
            }

            if lhs_len > 2 {
                return Err(CompileError::TooManyArguments {
                    proc: "call".to_owned(),
                    expected: 2,
                });
            }

            // Push LHS
            for expr in lhs {
                let kind = compiler.emit_expr(expr)?;
                compiler.emit_move_to_stack(kind)?;
            }

            // Push RHS
            match args::emit(compiler, args::ArgsContext::List, rhs.into_vec())? {
                args::ArgsResult::Normal => match lhs_len {
                    1 => compiler.emit_ins(Instruction::CallPath(rhs_len as u32)),
                    2 => compiler.emit_ins(Instruction::CallName(rhs_len as u32)),

                    _ => unreachable!(),
                },

                args::ArgsResult::Assoc => {
                    compiler.emit_ins(Instruction::NewAssocList(rhs_len as u32));

                    match lhs_len {
                        1 => compiler.emit_ins(Instruction::CallPathArgList),
                        2 => compiler.emit_ins(Instruction::CallNameArgList),

                        _ => unreachable!(),
                    }
                }

                args::ArgsResult::ArgList => match lhs_len {
                    1 => compiler.emit_ins(Instruction::CallPathArgList),
                    2 => compiler.emit_ins(Instruction::CallNameArgList),

                    _ => unreachable!(),
                },
            }

            Ok(EvalKind::Stack)
        }

        Term::SelfCall { .. } | Term::ParentCall { .. } => {
            // Can't implement these until we compile full procs
            // Well, maybe we could
            return Err(CompileError::UnsupportedRelativeCall);
        }

        Term::NewImplicit { .. } => Err(CompileError::UnsupportedImplicitNew),
        Term::NewMiniExpr { expr, args } => {
            let var = compiler.emit_find_var(expr.ident.into());
            let follows: Vec<Follow> = expr
                .fields
                .into_iter()
                .map(|f: &Field| f.clone().into())
                .collect();

            let kind = follow::emit(compiler, follows, var)?;
            compiler.emit_move_to_stack(kind)?;

            emit_new(compiler, args)
        }
        Term::NewPrefab { prefab, args } => {
            if !prefab.vars.is_empty() {
                return Err(CompileError::UnsupportedPrefabWithVars);
            }

            let path = format!("{}", FormatTypePath(&prefab.path));
            let typeval = operands::Value::Path(path);
            compiler.emit_ins(Instruction::PushVal(typeval));

            emit_new(compiler, args)
        }

        Term::Locate { args, in_list } => {
            let args_len = args.len();

            args::emit_normal(compiler, args::ArgsContext::Proc, args.into_vec())?;

            match args_len {
                // locate()
                0 => return Err(CompileError::UnsupportedImplicitLocate),

                // locate(ref|type)
                1 if in_list.is_none() => {
                    compiler.emit_ins(Instruction::LocateRef);
                }

                // locate(type) in container
                1 if in_list.is_some() => {
                    let kind = compiler.emit_expr(*in_list.unwrap())?;
                    compiler.emit_move_to_stack(kind)?;

                    compiler.emit_ins(Instruction::LocateType);
                }

                // locate(X, Y, Z)
                3 => {
                    compiler.emit_ins(Instruction::LocatePos);
                }

                _ => return Err(CompileError::InvalidLocateArgs),
            }

            Ok(EvalKind::Stack)
        }

        Term::Pick(args) => {
            match args.len() {
                // prob()
                0 => {
                    return Err(CompileError::MissingArgument {
                        proc: "pick".to_owned(),
                        index: 1,
                    })
                }

                // prob(L)
                1 => {
                    let (lhs, rhs) = args.into_vec().pop().unwrap();

                    if lhs.is_some() {
                        return Err(CompileError::UnexpectedProbability);
                    }

                    let kind = compiler.emit_expr(rhs)?;
                    compiler.emit_move_to_stack(kind)?;
                    compiler.emit_ins(Instruction::Pick);
                }

                // prob(x, y; z, ...)
                _ => {
                    let label_end = format!("LAB_{:0>4X}", compiler.label_count);
                    compiler.label_count += 1;

                    let mut branches = vec![];

                    for (lhs, rhs) in args {
                        match lhs {
                            Some(lhs) => {
                                let kind = compiler.emit_expr(lhs)?;
                                compiler.emit_move_to_stack(kind)?;
                            }

                            None => compiler.emit_ins(Instruction::PushInt(100)),
                        }

                        branches.push((format!("LAB_{:0>4X}", compiler.label_count), rhs));
                        compiler.label_count += 1;
                    }

                    compiler.emit_ins(Instruction::PickProb(PickProbParams {
                        cases: branches
                            .iter()
                            .map(|(label, _)| Label(label.to_owned()))
                            .collect(),
                    }));

                    for (label, expr) in branches {
                        compiler.emit_label(label);

                        let kind = compiler.emit_expr(expr)?;
                        compiler.emit_move_to_stack(kind)?;

                        compiler.emit_ins(Instruction::Jmp(Label(label_end.clone())));
                    }

                    compiler.emit_label(label_end);
                }
            }

            Ok(EvalKind::Stack)
        }

        Term::List(args) => {
            let arg_count = args.len();

            match args::emit(compiler, args::ArgsContext::List, args.into_vec())? {
                args::ArgsResult::Normal => {
                    compiler.emit_ins(Instruction::NewList(arg_count as u32));
                }

                args::ArgsResult::Assoc => {
                    compiler.emit_ins(Instruction::NewAssocList(arg_count as u32));
                }

                args::ArgsResult::ArgList => return Err(CompileError::UnexpectedArgList),
            }

            Ok(EvalKind::Stack)
        }

        Term::ExternalCall {
            library_name,
            function_name,
            args,
        } => {
            compiler.emit_expr(*library_name)?;
            compiler.emit_expr(*function_name)?;

            let args_vec = args.into_vec();
            let arg_amount: u32 = args_vec.len().try_into().unwrap();
            args::emit_normal(compiler, args::ArgsContext::Proc, args_vec)?;
            compiler.emit_ins(Instruction::CallLib(arg_amount));

            Ok(EvalKind::Stack)
        }

        Term::InterpString(_, _) => Err(CompileError::UnsupportedStringInterpolation),
        Term::Input {
            args: _,
            input_type: _,
            in_list: _,
        } => Err(CompileError::UnsupportedInput),
        Term::__TYPE__ => Err(CompileError::UnsupportedCompilerMacro {
            name: "__TYPE__".to_string(),
        }),
        Term::__IMPLIED_TYPE__ => Err(CompileError::UnsupportedCompilerMacro {
            name: "__IMPLIED_TYPE__".to_string(),
        }),
        Term::__PROC__ => Err(CompileError::UnsupportedCompilerMacro {
            name: "__PROC__".to_string(),
        }),
    }
}

// Assuming the type to create will always be on the stack
fn emit_new(
    compiler: &mut Compiler<'_>,
    args: Option<Box<[Expression]>>,
) -> Result<EvalKind, CompileError> {
    let args = args.unwrap_or_else(|| Box::new([])).into_vec();
    let arg_count = args.len() as u32;

    match args::emit(compiler, args::ArgsContext::Proc, args)? {
        args::ArgsResult::Normal => {
            compiler.emit_ins(Instruction::New(arg_count));
        }

        args::ArgsResult::Assoc => {
            compiler.emit_ins(Instruction::NewAssocList(arg_count));
            compiler.emit_ins(Instruction::NewArgList);
        }

        args::ArgsResult::ArgList => {
            compiler.emit_ins(Instruction::NewArgList);
        }
    }

    Ok(EvalKind::Stack)
}
