use dreammaker::ast::ListAccessKind;

use crate::compiler::*;
use crate::Instruction;

pub(super) fn emit(
    compiler: &mut Compiler,
    follow: Vec<Follow>,
    kind: EvalKind,
) -> Result<EvalKind, CompileError> {
    let mut kind = kind;

    // sequential field accessors (example: a.b.c) get buffered into a single operation
    // TODO: Move this state and the commit function into a struct!
    let mut field_buffer = vec![];

    for sub_expr in follow {
        match sub_expr {
            Follow::Field(access_kind, ident) => {
                // We currently treat `.` and `:` as the same
                // TODO: Should we type check?
                match access_kind {
                    PropertyAccessKind::Dot
                    | PropertyAccessKind::Colon
                    | PropertyAccessKind::Scope => {
                        field_buffer.push(ident.into());
                    }

                    PropertyAccessKind::SafeDot | PropertyAccessKind::SafeColon => {
                        kind = commit_field_buffer(compiler, kind, &mut field_buffer)?;
                        compiler.emit_move_to_stack(kind)?;

                        let short_circuit = compiler.short_circuit();
                        compiler.emit_ins(Instruction::SetCacheJmpIfNull(Label(short_circuit)));

                        kind = EvalKind::Field(ChainBuilder::begin(Variable::Cache), ident.into());
                    }
                }
            }

            Follow::Index(access_kind, expr) => {
                kind = commit_field_buffer(compiler, kind, &mut field_buffer)?;

                match access_kind {
                    ListAccessKind::Normal => {
                        // Move base to the stack
                        compiler.emit_move_to_stack(kind)?;

                        // Move inner expression to stack
                        let expr = compiler.emit_expr(*expr)?;
                        compiler.emit_move_to_stack(expr)?;

                        kind = EvalKind::ListRef;
                    }

                    ListAccessKind::Safe => {
                        // Move base to the stack
                        compiler.emit_move_to_stack(kind)?;

                        // Short-circuit if base is null
                        // TODO: Can we do this without using cache?
                        let short_circuit = compiler.short_circuit();
                        compiler.emit_ins(Instruction::SetCacheJmpIfNull(Label(short_circuit)));
                        compiler.emit_ins(Instruction::GetVar(Variable::Cache));

                        // Move inner expression to stack
                        let expr = compiler.emit_expr(*expr)?;
                        compiler.emit_move_to_stack(expr)?;

                        kind = EvalKind::ListRef;
                    }
                }
            }

            Follow::Call(index_kind, ident, args) => {
                let arg_count = args.len() as u32;

                match index_kind {
                    // Global call syntax `global.f()`
                    PropertyAccessKind::Dot
                    | PropertyAccessKind::Colon
                    | PropertyAccessKind::Scope
                        if matches!(kind, EvalKind::Global) && field_buffer.is_empty() =>
                    {
                        match args::emit(compiler, args::ArgsContext::Proc, args.into_vec())? {
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
                    }

                    // We just treat these as the same
                    // TODO: Should we type check?
                    PropertyAccessKind::Dot
                    | PropertyAccessKind::Colon
                    | PropertyAccessKind::Scope => {
                        // TODO: Can emit much cleaner code when no params
                        kind = commit_field_buffer(compiler, kind, &mut field_buffer)?;
                        compiler.emit_move_to_stack(kind)?;

                        // We'll need our src after pushing the parameters
                        compiler.emit_ins(Instruction::SetVar(Variable::Cache));
                        compiler.emit_ins(Instruction::PushCache);

                        match args::emit(compiler, args::ArgsContext::Proc, args.into_vec())? {
                            args::ArgsResult::Normal => {
                                compiler.emit_ins(Instruction::PopCache);

                                compiler.emit_ins(Instruction::Call(
                                    Variable::DynamicProc(DMString(ident.as_str().into())),
                                    arg_count,
                                ));
                            }

                            args::ArgsResult::Assoc => {
                                compiler.emit_ins(Instruction::NewAssocList(arg_count));

                                compiler.emit_ins(Instruction::PopCache);

                                compiler.emit_ins(Instruction::Call(
                                    Variable::DynamicProc(DMString(ident.as_str().into())),
                                    65535, // TODO: remove hardcoded value
                                ));
                            }

                            args::ArgsResult::ArgList => {
                                compiler.emit_ins(Instruction::PopCache);

                                compiler.emit_ins(Instruction::Call(
                                    Variable::DynamicProc(DMString(ident.as_str().into())),
                                    65535, // TODO: remove hardcoded value
                                ));
                            }
                        }
                    }

                    PropertyAccessKind::SafeDot | PropertyAccessKind::SafeColon => {
                        // TODO: Can emit much cleaner code when no params
                        kind = commit_field_buffer(compiler, kind, &mut field_buffer)?;
                        compiler.emit_move_to_stack(kind)?;

                        let short_circuit = compiler.short_circuit();
                        compiler.emit_ins(Instruction::SetCacheJmpIfNull(Label(short_circuit)));

                        // We'll need our src after pushing the parameters
                        compiler.emit_ins(Instruction::PushCache);

                        match args::emit(compiler, args::ArgsContext::Proc, args.into_vec())? {
                            args::ArgsResult::Normal => {
                                compiler.emit_ins(Instruction::PopCache);

                                compiler.emit_ins(Instruction::Call(
                                    Variable::DynamicProc(DMString(ident.as_str().into())),
                                    arg_count,
                                ));
                            }

                            args::ArgsResult::Assoc => {
                                compiler.emit_ins(Instruction::NewAssocList(arg_count));

                                compiler.emit_ins(Instruction::PopCache);

                                compiler.emit_ins(Instruction::Call(
                                    Variable::DynamicProc(DMString(ident.as_str().into())),
                                    65535, // TODO: remove hardcoded value
                                ));
                            }

                            args::ArgsResult::ArgList => {
                                compiler.emit_ins(Instruction::PopCache);

                                compiler.emit_ins(Instruction::Call(
                                    Variable::DynamicProc(DMString(ident.as_str().into())),
                                    65535, // TODO: remove hardcoded value
                                ));
                            }
                        }
                    }
                }

                kind = EvalKind::Stack;
            }

            Follow::Unary(op) => {
                kind = commit_field_buffer(compiler, kind, &mut field_buffer)?;
                kind = unary::emit(compiler, vec![op], kind)?;
            }

            Follow::ProcReference(ident) | Follow::StaticField(ident) => {
                compiler.emit_ins(Instruction::GetVar(Variable::Initial(Box::new(
                    Variable::Global(DMString(ident.as_str().into())),
                ))));
            }
        }
    }

    kind = commit_field_buffer(compiler, kind, &mut field_buffer)?;
    Ok(kind)
}

fn commit_field_buffer(
    compiler: &mut Compiler,
    kind: EvalKind,
    field_chain: &mut Vec<String>,
) -> Result<EvalKind, CompileError> {
    if field_chain.is_empty() {
        return Ok(kind);
    }

    // We need a ChainBuilder
    // TODO: Lots of repeated code from emit_move_to_chain_builder. We should call that for the non-specialized ones.
    let mut builder = match kind {
        EvalKind::Stack => {
            compiler.emit_ins(Instruction::SetVar(Variable::Cache));
            ChainBuilder::begin(Variable::Cache)
        }

        EvalKind::ListRef => {
            compiler.emit_ins(Instruction::ListGet);
            compiler.emit_ins(Instruction::SetVar(Variable::Cache));
            ChainBuilder::begin(Variable::Cache)
        }

        EvalKind::Range => return Err(CompileError::UnexpectedRange),
        EvalKind::ArgList => return Err(CompileError::UnexpectedArgList),

        // Bit hacky.
        EvalKind::Global => {
            let name = field_chain.remove(0);
            let var = Variable::Global(DMString(name.into()));
            return commit_field_buffer(compiler, EvalKind::Var(var), field_chain);
        }

        EvalKind::Field(mut builder, field) => {
            builder.append(DMString(field.into()));
            builder
        }

        EvalKind::Var(var) => ChainBuilder::begin(var),
    };

    let last_field = field_chain.pop().unwrap();

    for field in field_chain.iter() {
        builder.append(DMString(field.clone().into()));
    }

    field_chain.clear();
    Ok(EvalKind::Field(builder, last_field))
}
