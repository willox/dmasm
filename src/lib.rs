#![allow(dead_code)]

mod access_modifiers;
pub mod assembler;
pub mod disassembler;
// pub mod builder;
mod instructions;
mod operands;
mod operands_deserialize;
pub mod parser;
mod list_operands;

use instructions::Instruction;

use std::fmt::Write;

struct Proc {}

#[derive(PartialEq, Debug)]
pub enum Node<D = ()> {
    Comment(String),
    Label(String),
    Instruction(Instruction, D),
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
pub fn format_disassembly(nodes: &[Node<disassembler::DebugData>], cursor: Option<u32>) -> String {
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
        writeln!(&mut out, "{}", node).unwrap()
    }

    out
}

struct TestAssembleEnv;
struct TestDisassembleEnv;

impl assembler::AssembleEnv for TestAssembleEnv {
    fn get_string_index(&mut self, _string: &str) -> u32 {
        1337
    }
}

impl disassembler::DisassembleEnv for TestDisassembleEnv {
    fn get_string(&mut self, index: u32) -> Option<String> {
        Some(format!("(Test String for {})", index))
    }

    fn get_variable_name(&mut self, index: u32) -> Option<String> {
        Some(format!("var_{}", index))
    }

    fn get_proc_name(&mut self, index: u32) -> Option<String> {
        Some(format!("/proc/func{}", index))
    }

    fn value_to_string(&mut self, tag: u32, data: u32) -> Option<String> {
        Some(format!("/datum/type{}/data{}", tag, data))
    }
}

#[test]
fn test_assemble() {
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

    let bytecode = assembler::assemble(&nodes, &mut TestAssembleEnv);

    let mut env = TestDisassembleEnv;
    let disassembled = disassembler::disassemble(&bytecode, &mut env).unwrap();

    println!("{}", format_disassembly(&disassembled, Some(4)));
}
