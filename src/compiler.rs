use crate::Node;
use crate::Instruction;
use std::collections::HashMap;

pub fn compile(nodes: &[Node]) -> Vec<u32> {
    let mut bytecode = vec![];
    let mut jump_destinations = HashMap::new();
    let mut jump_sources = vec![];

    for node in nodes {
        match node {
            Node::Label(identifier) => {
                jump_destinations.insert(identifier, bytecode.len() as u32);
            }

            Node::Comment(_) => (),

            Node::Instruction(ins) => {

                match ins {
                    Instruction::End => {
                        bytecode.push(0x00);
                    }

                    Instruction::Jmp(dst) => {
                        bytecode.push(0x0F);
                        jump_sources.push((bytecode.len(), &dst.0));
                        bytecode.push(0x00);
                    }
                }

            }
        }
    }

    for src in jump_sources {
        bytecode[src.0] = jump_destinations[src.1];
    }

    bytecode
}
