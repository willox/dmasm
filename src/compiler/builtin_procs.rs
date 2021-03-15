use dreammaker::ast::Expression;

use crate::compiler::*;

pub(super) fn eval(
    compiler: &mut Compiler<'_>,
    name: &str,
    args: &Vec<Expression>,
) -> Result<Option<EvalKind>, CompileError> {
    let arg_count = args.len() as u32;

    match name {
        "url_encode" => {
            if !(1..=2).contains(&arg_count) {
                return Err(CompileError::IncorrectArgCount(name.to_owned()));
            }

            // TODO: ugly clones
            let text = compiler.emit_expr(args[0].clone())?;
            compiler.emit_move_to_stack(text);

            if let Some(format) = args.get(1) {
                let format = compiler.emit_expr(format.clone())?;
                compiler.emit_move_to_stack(format);
            } else {
                compiler.emit_ins(Instruction::PushVal(Value::Null));
            }

            compiler.emit_ins(Instruction::UrlEncode);
            Ok(Some(EvalKind::Stack))
        }

        "initial" => {
            if arg_count != 1 {
                return Err(CompileError::IncorrectArgCount(name.to_owned()));
            }

            let var = match compiler.emit_expr(args[0].clone())? {
                EvalKind::Field(builder, field) => {
                    builder.get_initial_field(DMString(field.into()))
                }

                _ => return Err(CompileError::ExpectedFieldReference),
            };

            // The chain builder can't handle the kind of var we have, so move the reuslt to the stack
            compiler.emit_ins(Instruction::GetVar(var));
            Ok(Some(EvalKind::Stack))
        }

        _ => Ok(None),
    }
}
