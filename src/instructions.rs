use crate::{assembler::{AssembleEnv, Assembler}, disassembler::{DebugData, DisassembleEnv, DisassembleError, Disassembler}, opcodes, operands::*};
use crate::operands::Operand;

#[derive(PartialEq, Debug)]
pub enum Instruction {
    End,
    Format(DMString, u32),
    Jmp(Label),
    Output,
    DbgFile(DMString),
    DbgLine(u32),
    PushInt(i32),
    Ret,
    GetVar(Variable),
}

impl Instruction {
    pub fn assemble<'a, E: AssembleEnv>(&'a self, asm: &mut Assembler<'a, E>) {
        match self {
            Instruction::End => {
                asm.emit(opcodes::End);
            }

            Instruction::Format(string, arg_count) => {
                asm.emit(opcodes::Format);
                string.assemble(asm);
                arg_count.assemble(asm);
            }

            Instruction::Output => {
                asm.emit(opcodes::Output);
            }

            Instruction::Jmp(dst) => {
                asm.emit(opcodes::Jmp);
                dst.assemble(asm);
            }

            Instruction::DbgFile(path) => {
                asm.emit(opcodes::DbgFile);
                path.assemble(asm);
            }

            Instruction::DbgLine(line) => {
                asm.emit(opcodes::DbgLine);
                line.assemble(asm);
            }

            Instruction::PushInt(val) => {
                asm.emit(opcodes::PushInt);
                val.assemble(asm);
            }

            Instruction::Ret => {
                asm.emit(opcodes::Ret);
            }

            Instruction::GetVar(var) => {
                asm.emit(opcodes::GetVar);
                var.assemble(asm);
            }
        }
    }

    pub fn disassemble<'a, E: DisassembleEnv>(dism: &mut Disassembler<'a, E>) -> Result<(Self, DebugData<'a>), DisassembleError> {
        let offset = dism.current_offset;

        let ins = match dism.read_u32()? {
            opcodes::End => Instruction::End,
            opcodes::Format => Instruction::Format(DMString::disassemble(dism)?, u32::disassemble(dism)?),
            opcodes::Output => Instruction::Output,
            opcodes::Jmp => Instruction::Jmp(Label::disassemble(dism)?),
            opcodes::DbgFile => Instruction::DbgFile(DMString::disassemble(dism)?),
            opcodes::DbgLine => Instruction::DbgLine(u32::disassemble(dism)?),
            opcodes::PushInt => Instruction::PushInt(i32::disassemble(dism)?),
            opcodes::Ret => Instruction::Ret,
            opcodes::GetVar => Instruction::GetVar(Variable::disassemble(dism)?),

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
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::End => write!(f, "End"),
            Self::Format(string, arg_count) => write!(f, "Format {} {}", string, arg_count),
            Self::Jmp(label) => write!(f, "Jmp {}", label),
            Self::Output => write!(f, "Output"),
            Self::DbgFile(path) => write!(f, "DbgFile {}", path),
            Self::DbgLine(line) => write!(f, "DbgLine {}", line),
            Self::PushInt(v) => write!(f, "PushInt {}", v),
            Self::Ret => write!(f, "Ret"),
            Self::GetVar(var) => write!(f, "GetVar {}", var),
        }
    }
}
