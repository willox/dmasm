use crate::Instruction;
use crate::Node;

use std::collections::HashSet;

pub trait DisassembleEnv {
    fn get_string(&mut self, index: u32) -> Option<String>;
    fn get_variable_name(&mut self, index: u32) -> Option<String>;
    fn get_proc_name(&mut self, index: u32) -> Option<String>;
    fn value_to_string(&mut self, tag: u32, data: u32) -> Option<String>;
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
    UnknownRangeParams { offset: u32, value: u32 },
    UnknownIsInOperand { offset: u32, value: u32 },
    UnknownValue { offset: u32, tag: u32 },
    Todo,
}

#[derive(Debug, PartialEq)]
pub struct DebugData<'a> {
    pub offset: u32,
    pub bytecode: &'a [u32],
}

pub fn disassemble<'a, E: DisassembleEnv>(
    bytecode: &'a [u32],
    env: &'a mut E,
) -> (Vec<Node<DebugData<'a>>>, Option<DisassembleError>) {
    let mut state = Disassembler::new(bytecode, env);
    let mut instructions = vec![];
    let mut err = None;

    // TODO: Move me
    loop {
        match Instruction::disassemble(&mut state) {
            Ok((ins, dbg)) => {
                instructions.push((ins, dbg));
            },

            Err(e) => {
                err = Some(e);
                break;
            }
        }

        if state.finished() {
            break;
        }
    }

    let mut nodes = vec![];

    for (ins, dbg) in instructions {
        if state.indirection_destinations.contains(&dbg.offset) {
            nodes.push(Node::Label(format!("LAB_{:0>4X}", dbg.offset)));
        }

        nodes.push(Node::Instruction(ins, dbg));
    }

    (nodes, err)
}

pub struct Disassembler<'a, E: DisassembleEnv> {
    pub bytecode: &'a [u32],
    pub current_offset: u32,
    indirection_destinations: HashSet<u32>,
    pub env: &'a mut E,
}

impl<'a, E: DisassembleEnv> Disassembler<'a, E> {
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

    pub fn peek_u32(&mut self) -> Option<u32> {
        self.bytecode.get(self.current_offset as usize).map(|x| *x)
    }

    pub fn read_u32(&mut self) -> Result<u32, DisassembleError> {
        let val = self
            .bytecode
            .get(self.current_offset as usize)
            .map(|x| *x)
            .ok_or(DisassembleError::UnexpectedEnd);
        self.current_offset += 1;
        val
    }

    pub fn read_i32(&mut self) -> Result<i32, DisassembleError> {
        unsafe { Ok(std::mem::transmute(self.read_u32()?)) }
    }

    pub fn reserve_destination(&mut self, offset: u32) {
        self.indirection_destinations.insert(offset);
    }
}
