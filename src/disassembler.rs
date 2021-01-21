use crate::opcodes;
use crate::access_modifiers;
use crate::operands::*;
use crate::Instruction;
use crate::Node;

use std::collections::HashSet;

pub trait Environment {
    /// Converts a dm string identifier into a rust string
    fn get_string(&mut self, index: u32) -> Option<String>;
    fn get_variable_name(&mut self, index: u32) -> Option<String>;
    fn get_proc_name(&mut self, index: u32) -> Option<String>;
}

#[derive(Debug, PartialEq)]
pub enum DisassembleError {
    UnexpectedEnd,
    UnknownOpcode { offset: u32, opcode: u32 },
    InvalidOffset { offset: u32, dst: u32 },
    InvalidString { offset: u32, id: u32 },
    InvalidVariableName { offset: u32, id: u32 },
    InvalidProc { offset: u32, id: u32 },
    UnknownAccessModifier { offset: u32, value: u32 },
    UnknownFieldAccessModifier { offset: u32, value: u32 },
    Todo,
}

#[derive(Debug, PartialEq)]
pub struct DebugData<'a> {
    pub offset: u32,
    pub bytecode: &'a [u32],
}

pub fn disassemble<'a, E: Environment>(
    bytecode: &'a [u32],
    env: &'a mut E,
) -> Result<Vec<Node<DebugData<'a>>>, DisassembleError> {
    let mut state = State::new(bytecode, env);
    let mut instructions = vec![];

    // TODO: Move me
    while !state.finished() {
        let (ins, dbg) = state.read_instruction()?;
        instructions.push((ins, dbg));
    }

    let mut nodes = vec![];

    for (ins, dbg) in instructions {
        if state.indirection_destinations.contains(&dbg.offset) {
            nodes.push(Node::Label(format!("LAB_{:0>4X}", dbg.offset)));
        }

        nodes.push(Node::Instruction(ins, dbg));
    }

    Ok(nodes)
}

struct State<'a, E: Environment> {
    bytecode: &'a [u32],
    current_offset: u32,
    indirection_destinations: HashSet<u32>,
    env: &'a mut E,
}

impl<'a, E: Environment> State<'a, E> {
    fn new(bytecode: &'a [u32], env: &'a mut E) -> Self {
        Self {
            bytecode,
            current_offset: 0,
            indirection_destinations: HashSet::new(),
            env,
        }
    }

    fn finished(&self) -> bool {
        self.current_offset as usize == self.bytecode.len()
    }

    fn peek_u32(&mut self) -> Option<u32> {
        self.bytecode.get(self.current_offset as usize).map(|x| *x)
    }

    fn read_u32(&mut self) -> Result<u32, DisassembleError> {
        let val = self
            .bytecode
            .get(self.current_offset as usize)
            .map(|x| *x)
            .ok_or(DisassembleError::UnexpectedEnd);
        self.current_offset += 1;
        val
    }

    fn read_i32(&mut self) -> Result<i32, DisassembleError> {
        unsafe { Ok(std::mem::transmute(self.read_u32()?)) }
    }

    fn read_label(&mut self) -> Result<Label, DisassembleError> {
        let offset = self.read_u32()?;

        if offset as usize >= self.bytecode.len() {
            return Err(DisassembleError::InvalidOffset {
                offset: self.current_offset - 1,
                dst: offset,
            });
        }

        self.indirection_destinations.insert(offset);
        Ok(Label(format!("LAB_{:0>4X}", offset)))
    }

    fn read_dm_string(&mut self) -> Result<DMString, DisassembleError> {
        let id = self.read_u32()?;
        let string = self
            .env
            .get_string(id)
            .ok_or(DisassembleError::InvalidString {
                offset: self.current_offset - 1,
                id,
            })?;

        Ok(DMString(string))
    }

    fn read_variable_name(&mut self) -> Result<DMString, DisassembleError> {
        let id = self.read_u32()?;
        let string = self
            .env
            .get_variable_name(id)
            .ok_or(DisassembleError::InvalidVariableName {
                offset: self.current_offset - 1,
                id,
            })?;

        Ok(DMString(string))
    }

    fn read_proc(&mut self) -> Result<Proc, DisassembleError> {
        let id = self.read_u32()?;
        let string = self
            .env
            .get_proc_name(id)
            .ok_or(DisassembleError::InvalidProc {
                offset: self.current_offset - 1,
                id,
            })?;

        Ok(Proc(string))
    }

    fn read_variable_fields(&mut self) -> Result<Variable, DisassembleError> {
        // This is either a string-ref or an AccessModifier
        let param = self.peek_u32()
            .ok_or(DisassembleError::UnexpectedEnd)?;
        let lhs;
        let mut fields = vec![];

        if access_modifiers::is_access_modifier(param) {
            lhs = Box::new(self.read_variable()?);
        } else {
            lhs = Box::new(Variable::Cache);
            fields.push(self.read_dm_string()?);
        }

        loop {
            let param = self.read_u32()?;

            // The last value is a string
            if !access_modifiers::is_access_modifier(param) {
                fields.push(self.read_dm_string()?);
                return Ok(Variable::Field(lhs, fields));
            }

            match param {
                access_modifiers::Field => {
                    // HACK: We need to rethink this whole fn
                    let param = self.peek_u32();
                    if param == Some(access_modifiers::Global) {
                        self.read_u32()?;
                        fields.push(self.read_variable_name()?);
                        continue;
                    }

                    fields.push(self.read_dm_string()?);
                }

                // Initial is always last (i think,) so just grab the last string and ret
                access_modifiers::Initial => {
                    fields.push(self.read_dm_string()?);
                    return Ok(Variable::Initial(lhs, fields));
                }

                access_modifiers::Proc | access_modifiers::Proc2 => {
                    let proc = self.read_proc()?;
                    return Ok(Variable::StaticProcField(lhs, fields, proc))
                }

                access_modifiers::SrcProc | access_modifiers::SrcProc2 => {
                    let proc = self.read_dm_string()?;
                    return Ok(Variable::RuntimeProcField(lhs, fields, proc))
                }

                other => return Err(DisassembleError::UnknownFieldAccessModifier {
                    offset: self.current_offset - 1,
                    value: other,
                })
            }
        }
    }

    // TODO
    fn read_variable(&mut self) -> Result<Variable, DisassembleError> {
        // This is either a string-ref or an AccessModifier
        let param = self.read_u32()?;

        if !access_modifiers::is_access_modifier(param) {
            let cache = Box::new(Variable::Cache);
            let field = self.read_dm_string()?;
            return Ok(Variable::Field(cache, vec![field]));
        }

        let var = match param {
            access_modifiers::Null => Variable::Null,
            access_modifiers::World => Variable::World,
            access_modifiers::Usr => Variable::Usr,
            access_modifiers::Src => Variable::Src,
            access_modifiers::Args => Variable::Args,
            access_modifiers::Dot => Variable::Dot,
            access_modifiers::Cache => Variable::Cache,
            access_modifiers::Cache2 => Variable::Cache2,
            access_modifiers::Cache3 => Variable::Cache3,
            access_modifiers::Arg => Variable::Arg(self.read_u32()?),
            access_modifiers::Local => Variable::Local(self.read_u32()?),
            access_modifiers::Global => Variable::Global(self.read_variable_name()?),
            access_modifiers::Field => self.read_variable_fields()?,

            other => return Err(DisassembleError::UnknownAccessModifier {
                offset: self.current_offset - 1,
                value: other,
            })
        };

        Ok(var)
    }

    fn read_instruction(&mut self) -> Result<(Instruction, DebugData<'a>), DisassembleError> {
        let offset = self.current_offset;

        let ins = match self.read_u32()? {
            opcodes::End => Instruction::End,
            opcodes::Format => Instruction::Format(self.read_dm_string()?, self.read_u32()?),
            opcodes::Output => Instruction::Output,
            opcodes::Jmp => Instruction::Jmp(self.read_label()?),
            opcodes::DbgFile => Instruction::DbgFile(self.read_dm_string()?),
            opcodes::DbgLine => Instruction::DbgLine(self.read_u32()?),
            opcodes::PushInt => Instruction::PushInt(self.read_i32()?),
            opcodes::Ret => Instruction::Ret,
            opcodes::GetVar => Instruction::GetVar(self.read_variable()?),

            opcode => return Err(DisassembleError::UnknownOpcode { offset, opcode }),
        };

        let range_start = offset as usize;
        let range_end = self.current_offset as usize;

        Ok((
            ins,
            DebugData {
                offset,
                bytecode: &self.bytecode[range_start..range_end],
            },
        ))
    }
}
