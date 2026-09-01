mod access_modifiers;
pub mod assembler;
mod bytecode;
pub mod compiler;
pub mod disassembler;
mod instructions;
mod list_operands;
mod operands;
mod operands_deserialize;
mod parser;

pub use bytecode::{scan, BytecodeError, InstructionSpan, LabelOperand, ScannedBytecode};
pub use disassembler::DebugData;
pub use instructions::{Instruction, Opcode};
pub use list_operands::TypeFilter;
pub use operands::{
    DMString, IsInParams, Label, PickProbParams, PickSwitchParams, Proc, RangeParams, SwitchParams,
    SwitchRangeParams, Value, Variable,
};
use std::fmt::Write;

#[derive(PartialEq, Clone, Debug)]
pub enum Node<D = ()> {
    Comment(String),
    Label(Label),
    Instruction(Instruction, D),
}

impl<D> Node<D> {
    pub fn strip_debug_data(self) -> Node {
        match self {
            Self::Comment(str) => Node::Comment(str),
            Self::Label(label) => Node::Label(label),
            Self::Instruction(ins, _debug) => Node::Instruction(ins, ()),
        }
    }
}

impl<D> std::fmt::Display for Node<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comment(text) => writeln!(f, ";{}", text),
            Self::Label(label) => writeln!(f, "{}:", label),
            Self::Instruction(ins, _) => {
                ins.serialize(f)?;
                writeln!(f)
            }
        }
    }
}

fn write_raw_words(buf: &mut String, words: &[u32]) {
    for word in words {
        write!(buf, " {:0>8X}", word).unwrap();
    }
}

/// Formats disassembly with offsets and raw bytecode.
pub fn format_disassembly(nodes: &[Node<DebugData>], cursor: Option<u32>) -> String {
    let mut buf = String::new();

    for node in nodes {
        match node {
            Node::Instruction(ins, dbg) => {
                let mut chunks = dbg.bytecode.chunks(3);
                let first = chunks.next().expect("instructions contain an opcode");

                let prefix = match cursor {
                    Some(offset)
                        if offset >= dbg.offset
                            && offset < (dbg.offset + dbg.bytecode.len() as u32) =>
                    {
                        '>'
                    }
                    _ => ' ',
                };

                write!(&mut buf, "{} {:0>4X}:", prefix, dbg.offset).unwrap();
                write_raw_words(&mut buf, first);
                write!(&mut buf, "{:width$}", "", width = 28 - first.len() * 9).unwrap();
                writeln!(&mut buf, " {}", ins).unwrap();

                for chunk in chunks {
                    write!(&mut buf, "       ").unwrap();
                    write_raw_words(&mut buf, chunk);
                    writeln!(&mut buf).unwrap();
                }
            }

            other => write!(&mut buf, "{}", other).unwrap(),
        }
    }

    buf
}

pub fn format<D>(nodes: &[Node<D>]) -> String {
    let mut out = String::new();

    for node in nodes {
        write!(&mut out, "{}", node).unwrap()
    }

    out
}

#[cfg(test)]
pub(crate) struct TestAssembleEnv;

#[cfg(test)]
impl assembler::AssembleEnv for TestAssembleEnv {
    fn get_string_index(&mut self, _data: &[u8]) -> Option<u32> {
        Some(1337)
    }

    fn get_variable_name_index(&mut self, _name: &[u8]) -> Option<u32> {
        Some(1338)
    }

    fn get_proc_index(&mut self, _path: &str) -> Option<u32> {
        Some(1339)
    }

    fn get_type(&mut self, _path: &str) -> Option<(u8, u32)> {
        Some((0x09, 0x01))
    }
}

#[test]
fn formats_multi_line_bytecode_without_intermediate_strings() {
    let bytecode = [0x60, 0x2A, 0x3F80, 0];
    let nodes = [Node::Instruction(
        Instruction::PushVal(Value::Number(1.0)),
        DebugData {
            offset: 0,
            bytecode: &bytecode,
        },
    )];

    assert_eq!(
        format_disassembly(&nodes, None),
        "  0000: 00000060 0000002A 00003F80  PushVal 1\n        00000000\n"
    );
}
