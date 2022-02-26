use crate::Instruction;
use crate::Node;

use std::collections::HashSet;

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
    UnknownTypeFilter { offset: u32, value: u32 },
    Todo,
}

#[derive(Debug, PartialEq)]
pub struct DebugData<'a> {
    pub offset: u32,
    pub bytecode: &'a [u32],
}

pub fn disassemble<'a>(
    bytecode: &'a [u32],
) -> (Vec<Node<DebugData<'a>>>, Option<DisassembleError>) {
    let mut state = Disassembler::new(bytecode);
    let mut instructions = vec![];
    let mut err = None;

    // TODO: Move me
    loop {
        match Instruction::disassemble(&mut state) {
            Ok((ins, dbg)) => {
                instructions.push((ins, dbg));
            }

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
        nodes.push(Node(ins, dbg));
    }

    (nodes, err)
}

pub struct Disassembler<'a> {
    pub bytecode: &'a [u32],
    pub current_offset: u32,
}

impl<'a> Disassembler<'a> {
    fn new(bytecode: &'a [u32]) -> Self {
        Self {
            bytecode,
            current_offset: 0,
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
}
