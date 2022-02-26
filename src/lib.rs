#![allow(dead_code)]

mod access_modifiers;
pub mod disassembler;
mod instructions;
mod list_operands;
mod operands;

pub use disassembler::DebugData;
pub use instructions::Instruction;

use std::fmt::Write;

struct Proc {}

#[derive(PartialEq, Clone, Debug)]
pub struct Node<D = ()>(pub Instruction, pub D);

impl<D> Node<D> {
    pub fn strip_debug_data(self) -> Node {
        Node(self.0, ())
    }
}

impl<D> std::fmt::Display for Node<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.serialize(f)?;
        write!(f, "\n")
    }
}

/// Formats the output of the disassembler into a human readable format including offsets, bytecode, and assembly.
pub fn format_disassembly(nodes: &[Node<DebugData>], cursor: Option<u32>) -> String {
    let mut buf = String::new();

    for node in nodes {
        let (ins, dbg) = (&node.0, &node.1);

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

    buf
}

pub fn format<D>(nodes: &[Node<D>]) -> String {
    let mut out = String::new();

    for node in nodes {
        write!(&mut out, "{}", node).unwrap()
    }

    out
}

