use crate::opcodes;
use crate::Instruction;
use crate::Node;
use std::collections::HashMap;

pub trait Environment {
    /// Converts a rust string into the correct string identifier for the destination context
    fn get_string_index(&mut self, string: &str) -> u32;
}

pub fn assemble<E: Environment>(nodes: &[Node], env: &mut E) -> Vec<u32> {
    let mut bytecode = vec![];
    let mut jump_destinations = HashMap::new();
    let mut jump_sources = vec![];

    for node in nodes {
        match node {
            Node::Label(identifier) => {
                jump_destinations.insert(identifier, bytecode.len() as u32);
            }

            Node::Comment(_) => (),

            Node::Instruction(ins, _) => match ins {
                Instruction::End => {
                    bytecode.push(opcodes::End);
                }

                Instruction::Format(string, arg_count) => {
                    bytecode.push(opcodes::Format);
                    bytecode.push(env.get_string_index(&string.0));
                    bytecode.push(*arg_count);
                }

                Instruction::Output => {
                    bytecode.push(opcodes::Output);
                }

                Instruction::Jmp(dst) => {
                    bytecode.push(opcodes::Jmp);
                    jump_sources.push((bytecode.len(), &dst.0));
                    bytecode.push(0xC0C0C0C0);
                }

                Instruction::DbgFile(path) => {
                    bytecode.push(opcodes::DbgFile);
                    bytecode.push(env.get_string_index(&path.0));
                }

                Instruction::DbgLine(line) => {
                    bytecode.push(opcodes::DbgLine);
                    bytecode.push(*line);
                }

                Instruction::PushInt(val) => {
                    bytecode.push(opcodes::PushInt);
                    bytecode.push(unsafe { std::mem::transmute(*val) });
                }

                Instruction::Ret => {
                    bytecode.push(opcodes::Ret);
                }

                Instruction::GetVar(_) => {
                    bytecode.push(opcodes::GetVar);
                    bytecode.push(0x00); // AAAAAAA
                }
            },
        }
    }

    for src in jump_sources {
        bytecode[src.0] = jump_destinations[src.1];
    }

    bytecode
}
