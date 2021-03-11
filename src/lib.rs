#![allow(dead_code)]

mod access_modifiers;
pub mod assembler;
pub mod disassembler;
// pub mod builder;
pub mod compiler;
mod instructions;
mod list_operands;
mod operands;
mod operands_deserialize;
mod parser;

pub use compiler::CompileData;
pub use disassembler::DebugData;
pub use instructions::Instruction;

use std::fmt::Write;

struct Proc {}

#[derive(PartialEq, Debug)]
pub enum Node<D = ()> {
    Comment(String),
    Label(String),
    Instruction(Instruction, D),
}

impl<D> Node<D> {
    pub fn strip_debug_data(self) -> Node {
        match self {
            Self::Comment(str) => Node::Comment(str),
            Self::Label(str) => Node::Label(str),
            Self::Instruction(ins, _debug) => Node::Instruction(ins, ()),
        }
    }
}

impl<D> std::fmt::Display for Node<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comment(text) => writeln!(f, ";{}", text),
            Self::Label(name) => writeln!(f, "{}:", name),
            Self::Instruction(ins, _) => {
                ins.serialize(f)?;
                write!(f, "\n")
            }
        }
    }
}

/// Formats the output of the disassembler into a human readable format including offsets, bytecode, and assembly.
pub fn format_disassembly(nodes: &[Node<DebugData>], cursor: Option<u32>) -> String {
    let mut buf = String::new();

    for node in nodes {
        match node {
            Node::Instruction(ins, dbg) => {
                let mut raw_lines = vec![];

                for chunk in dbg.bytecode.chunks(3) {
                    let mut line = String::new();
                    for code in chunk {
                        write!(&mut line, " {:0>8X}", code).unwrap();
                    }
                    raw_lines.push(line);
                }

                let prefix = match cursor {
                    Some(offset)
                        if offset >= dbg.offset
                            && offset < (dbg.offset + dbg.bytecode.len() as u32) =>
                    {
                        '>'
                    }
                    _ => ' ',
                };

                writeln!(
                    &mut buf,
                    "{} {:0>4X}:{:28} {}",
                    prefix, dbg.offset, raw_lines[0], ins
                )
                .unwrap();

                for line in &raw_lines[1..] {
                    writeln!(&mut buf, "       {}", line).unwrap();
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

pub(crate) struct TestAssembleEnv;
struct TestDisassembleEnv;

impl assembler::AssembleEnv for TestAssembleEnv {
    fn get_string_index(&mut self, _data: &[u8]) -> u32 {
        1337
    }

    fn get_variable_name_index(&mut self, _name: &[u8]) -> u32 {
        1338
    }
}

impl disassembler::DisassembleEnv for TestDisassembleEnv {
    fn get_string_data(&mut self, index: u32) -> Option<Vec<u8>> {
        Some(format!("(Test String for {})", index).into_bytes())
    }

    fn get_variable_name(&mut self, index: u32) -> Option<Vec<u8>> {
        Some(format!("var_{}", index).into_bytes())
    }

    fn get_proc_name(&mut self, index: u32) -> Option<String> {
        Some(format!("/proc/func{}", index))
    }

    fn value_to_string_data(&mut self, tag: u32, data: u32) -> Option<Vec<u8>> {
        Some(format!("/datum/type{}/data{}", tag, data).into_bytes())
    }
}

#[test]
fn test_assemble() {
    return;
    let nodes = parser::parse(
        r#"
DbgFile "main.dm"
DbgLine 7
PushInt 5
Ret
End
    "#,
    )
    .unwrap();

    let bytecode = assembler::assemble(&nodes, &mut TestAssembleEnv).unwrap();

    let mut env = TestDisassembleEnv;
    let (nodes, _error) = disassembler::disassemble(&bytecode, &mut env);

    println!("{}", format_disassembly(&nodes, Some(4)));
}
