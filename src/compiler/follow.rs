use crate::compiler::*;
use crate::Instruction;

pub(super) fn emit(
    compiler: &mut Compiler,
    follow: Vec<Spanned<Follow>>,
    kind: EvalKind,
) -> Result<EvalKind, CompileError> {
    let mut kind = kind;

    // sequential field accessors (example: a.b.c) get buffered into a single operand
    // TODO: Move this state and the commit function into a struct!
    let mut field_buffer = vec![];
    let mut skip_label = None;

    for sub_expr in follow {
        match sub_expr.elem {
            Follow::Field(index_kind, ident) => {
                match index_kind {
                    // We just treat these as the same
                    // TODO: Should we type check?
                    IndexKind::Dot | IndexKind::Colon => {
                        field_buffer.push(ident);
                    }

                    // We just treat these as the same
                    // TODO: Should we type check?
                    // TODO: Generates kind of badly compared to BYOND.
                    IndexKind::SafeDot | IndexKind::SafeColon => {
                        kind = commit_field_buffer(
                            compiler,
                            kind,
                            &mut field_buffer,
                            &mut skip_label,
                        )?;
                        compiler.emit_move_to_stack(kind)?;

                        let label = format!("LAB_{:0>4X}", compiler.label_count);
                        compiler.label_count += 1;

                        compiler.emit_ins(Instruction::SetCacheJmpIfNull(Label(label.clone())));
                        compiler.emit_ins(Instruction::GetVar(Variable::Cache));

                        assert!(skip_label.is_none());
                        field_buffer.push(ident);
                        skip_label = Some(label);

                        kind = EvalKind::Stack;
                    }
                }
            }

            Follow::Index(expr) => {
                kind = commit_field_buffer(compiler, kind, &mut field_buffer, &mut skip_label)?;

                // Move base to the stack
                compiler.emit_move_to_stack(kind)?;

                // Handle inner expression and move it to the stack
                match compiler.emit_expr(*expr)? {
                    EvalKind::Stack => {}

                    EvalKind::ListRef => {
                        compiler.emit_ins(Instruction::ListGet);
                    }

                    EvalKind::Range => return Err(CompileError::UnexpectedRange),
                    EvalKind::Global => return Err(CompileError::UnexpectedGlobal),

                    EvalKind::Var(var) => {
                        compiler.emit_ins(Instruction::GetVar(var));
                    }

                    EvalKind::Field(builder, field) => {
                        let var = builder.get_field(DMString(field.into()));
                        compiler.emit_ins(Instruction::GetVar(var));
                    }
                }

                kind = EvalKind::ListRef;
            }

            Follow::Call(index_kind, ident, args) => {
                // If any of the arguments are a Expression:AssignOp, byond does _crazy_ not-so-well defined things.
                // We can implement this later...
                if args
                    .iter()
                    .any(|x| matches!(x, Expression::AssignOp { .. }))
                {
                    return Err(CompileError::NamedArgumentsNotImplemented);
                }

                match index_kind {
                    // Global call syntax `global.f()`
                    IndexKind::Dot | IndexKind::Colon
                        if matches!(kind, EvalKind::Global) && field_buffer.is_empty() =>
                    {
                        assert!(skip_label.is_none());

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
                    }

                    // We just treat these as the same
                    // TODO: Should we type check?
                    IndexKind::Dot | IndexKind::Colon => {
                        let arg_count = args.len() as u32;

                        // TODO: Can emit much cleaner code when no params
                        kind = commit_field_buffer(
                            compiler,
                            kind,
                            &mut field_buffer,
                            &mut skip_label,
                        )?;
                        compiler.emit_move_to_stack(kind)?;

                        // We'll need our src after pushing the parameters
                        compiler.emit_ins(Instruction::SetVar(Variable::Cache));
                        compiler.emit_ins(Instruction::PushCache);

                        // Push args to the stack
                        for arg in args {
                            let arg = compiler.emit_expr(arg)?;
                            compiler.emit_move_to_stack(arg)?;
                        }

                        compiler.emit_ins(Instruction::PopCache);

                        // Move base to the stack
                        compiler.emit_ins(Instruction::Call(
                            Variable::DynamicProc(DMString(ident.into())),
                            arg_count,
                        ));
                    }

                    IndexKind::SafeDot | IndexKind::SafeColon => {
                        let args_count = args.len() as u32;

                        // TODO: Can emit much cleaner code when no params
                        kind = commit_field_buffer(
                            compiler,
                            kind,
                            &mut field_buffer,
                            &mut skip_label,
                        )?;
                        compiler.emit_move_to_stack(kind)?;

                        let label = format!("LAB_{:0>4X}", compiler.label_count);
                        compiler.label_count += 1;

                        // We'll need our src after pushing the parameters
                        compiler.emit_ins(Instruction::SetCacheJmpIfNull(Label(label.clone())));
                        compiler.emit_ins(Instruction::PushCache);

                        // Push args to the stack
                        for arg in args {
                            let arg = compiler.emit_expr(arg)?;
                            compiler.emit_move_to_stack(arg)?;
                        }

                        compiler.emit_ins(Instruction::PopCache);

                        // Move base to the stack
                        compiler.emit_ins(Instruction::Call(
                            Variable::DynamicProc(DMString(ident.into())),
                            args_count,
                        ));

                        compiler.emit_label(label);
                    }
                }

                kind = EvalKind::Stack;
            }
        }
    }

    kind = commit_field_buffer(compiler, kind, &mut field_buffer, &mut skip_label)?;
    Ok(kind)
}

fn commit_field_buffer(
    compiler: &mut Compiler,
    kind: EvalKind,
    field_chain: &mut Vec<String>,
    skip_label: &mut Option<String>,
) -> Result<EvalKind, CompileError> {
    if field_chain.is_empty() {
        assert!(skip_label.is_none());
        return Ok(kind);
    }

    // We need a value
    let mut builder = match kind {
        EvalKind::Stack => {
            compiler.emit_ins(Instruction::SetVar(Variable::Cache));
            VariableChainBuilder::begin(Variable::Cache)
        }

        EvalKind::ListRef => {
            compiler.emit_ins(Instruction::ListGet);
            compiler.emit_ins(Instruction::SetVar(Variable::Cache));
            VariableChainBuilder::begin(Variable::Cache)
        }

        EvalKind::Range => return Err(CompileError::UnexpectedRange),

        // Bit hacky.
        EvalKind::Global => {
            let name = field_chain.remove(0);
            let var = Variable::Global(DMString(name.into()));
            return commit_field_buffer(compiler, EvalKind::Var(var), field_chain, skip_label);
        }

        EvalKind::Field(mut builder, field) => {
            builder.append(DMString(field.into()));
            builder
        }

        EvalKind::Var(var) => VariableChainBuilder::begin(var),
    };

    let last = field_chain.pop().unwrap();

    for field in field_chain.iter() {
        builder.append(DMString(field.clone().into()));
    }

    let kind = EvalKind::Field(builder, last);

    // If we had a skip label, we have to go on to the stack
    let kind = match skip_label {
        Some(skip_label) => {
            compiler.emit_move_to_stack(kind)?;
            compiler.emit_label(skip_label.clone());
            EvalKind::Stack
        }

        None => kind,
    };

    field_chain.clear();
    *skip_label = None;
    Ok(kind)
}
