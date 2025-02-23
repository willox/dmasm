use crate::{operands, Node};
use std::collections::HashMap;

pub trait AssembleEnv {
    /// Converts a rust string into the correct string identifier for the destination context
    fn get_string_index(&mut self, string: &[u8]) -> Option<u32>;
    fn get_variable_name_index(&mut self, name: &[u8]) -> Option<u32>;
    fn get_proc_index(&mut self, path: &str) -> Option<u32>;
    fn get_type(&mut self, path: &str) -> Option<(u8, u32)>;
}

#[derive(Debug, PartialEq)]
pub enum AssembleError {
    UnsupportedValue(operands::Value),
    ProcNotFound(String),
    InvalidVariableName,
    TypeNotFound(String),
}

pub struct Assembler<'a, E: AssembleEnv> {
    nodes: &'a [Node],
    bytecode: Vec<u32>,
    jump_destinations: HashMap<String, u32>,
    jump_sources: Vec<(usize, String)>,
    pub env: &'a mut E,
}

impl<'a, E: AssembleEnv> Assembler<'a, E> {
    fn new(nodes: &'a [Node], env: &'a mut E) -> Self {
        Assembler {
            nodes,
            bytecode: vec![],
            jump_destinations: HashMap::new(),
            jump_sources: vec![],
            env,
        }
    }

    pub fn emit(&mut self, code: u32) {
        self.bytecode.push(code);
    }

    pub fn emit_label_operand(&mut self, name: &str) {
        self.jump_sources
            .push((self.bytecode.len(), name.to_owned()));
        self.emit(0xC0C0C0C0);
    }
}

pub fn assemble<E: AssembleEnv>(nodes: &[Node], env: &mut E) -> Result<Vec<u32>, AssembleError> {
    let mut state = Assembler::new(nodes, env);

    for node in nodes {
        match node {
            Node::Label(identifier) => {
                state
                    .jump_destinations
                    .insert(identifier.clone(), state.bytecode.len() as u32);
            }

            Node::Comment(_) => (),

            Node::Instruction(ins, _) => ins.assemble(&mut state)?,
        }
    }

    for src in state.jump_sources {
        state.bytecode[src.0] = state.jump_destinations[&src.1];
    }

    Ok(state.bytecode)
}
