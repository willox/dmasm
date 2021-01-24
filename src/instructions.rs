use crate::operands::Operand;
use crate::parser;
use crate::{
    assembler::{AssembleEnv, Assembler},
    disassembler::{DebugData, DisassembleEnv, DisassembleError, Disassembler},
    operands::*,
};
use std::fmt;

#[allow(unused)]
macro_rules! instructions {
    ( $(
        $opcode:literal = $name:ident
        $( ( $(
            $operand_name:ident: $operand_type:tt
        ),* $(,)? ) )?
    ),* $(,)? ) => {
        #[derive(PartialEq, Debug)]
        pub enum Instruction {
            $(
                $name$( ( $( $operand_type, )* ) )?,
            )*
        }

        impl Instruction {
            pub fn assemble<'a, E: AssembleEnv>(&'a self, asm: &mut Assembler<'a, E>) {
                match self {
                    $(
                        Self::$name$( ( $( $operand_name, )* ) )? => {
                            asm.emit($opcode);
                            $( $( $operand_name.assemble(asm); )* )?
                        }
                    )*
                }
            }

            pub fn disassemble<'a, E: DisassembleEnv>(
                dism: &mut Disassembler<'a, E>,
            ) -> Result<(Self, DebugData<'a>), DisassembleError> {
                let offset = dism.current_offset;

                let ins = match dism.read_u32()? {
                    $(
                        $opcode => {
                            Self::$name$( ( $( $operand_type::disassemble(dism)?, )* ) )?
                        }
                    )*

                    opcode => return Err(DisassembleError::UnknownOpcode { offset, opcode }),
                };

                let range_start = offset as usize;
                let range_end = dism.current_offset as usize;

                Ok((
                    ins,
                    DebugData {
                        offset,
                        bytecode: &dism.bytecode[range_start..range_end],
                    },
                ))
            }

            pub fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(
                        Self::$name$( ( $( $operand_name, )* ) )? => {
                            write!(f, stringify!($name))?;
                            $( $(
                                write!(f, " ")?;
                                $operand_name.serialize(f)?;
                            )* )?
                        }
                    )*
                }

                Ok(())
            }

            pub fn deserialize<'b, E: 'b>(i: &'b str) -> nom::IResult<&str, Self, E>
            where
                E: nom::error::ParseError<&'b str>
                    + nom::error::FromExternalError<&'b str, std::num::ParseIntError>,
            {
                let (i, name) = parser::whitespace(parser::parse_identifier)(i)?;

                let (i, instruction) = match name {
                    $(
                        stringify!($name) => {
                            $( $(
                                let (i, $operand_name) = parser::whitespace($operand_type::deserialize)(i)?;
                            )* )?
                            (i, Self::$name$( ( $(
                                $operand_name,
                             )* ))? )
                        },
                    )*

                    // TODO: Real error
                    other => panic!("unknown instruction {}", other),
                };

                Ok((i, instruction))
            }
        }
    };
}

instructions! {
    0x00 = End,
    0x02 = Format(fmt: DMString, arg_count: u32),
    0x0F = Jmp(dst: Label),
    0x03 = Output,
    0x84 = DbgFile(file: DMString),
    0x85 = DbgLine(line: u32),
    0x50 = PushInt(value: i32),
    0x12 = Ret,
    0x33 = GetVar(var: Variable),
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.serialize(f)
    }
}
