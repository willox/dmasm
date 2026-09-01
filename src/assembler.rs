use crate::{operands, Label, Node};
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
    StringNotFound(Vec<u8>),
    VariableNameNotFound(Vec<u8>),
    TypeNotFound(String),
    LabelNotFound(String),
}

pub struct Assembler<'a, E: AssembleEnv> {
    bytecode: Vec<u32>,
    jump_destinations: HashMap<&'a str, u32>,
    jump_sources: Vec<(usize, &'a str)>,
    pub env: &'a mut E,
}

impl<'a, E: AssembleEnv> Assembler<'a, E> {
    fn new(env: &'a mut E) -> Self {
        Assembler {
            bytecode: vec![],
            jump_destinations: HashMap::new(),
            jump_sources: vec![],
            env,
        }
    }

    pub fn emit(&mut self, code: u32) {
        self.bytecode.push(code);
    }

    pub fn emit_label_operand(&mut self, name: &'a str) {
        self.jump_sources.push((self.bytecode.len(), name));
        self.emit(0xC0C0C0C0);
    }
}

pub fn assemble<'a, E: AssembleEnv>(
    nodes: &'a [Node],
    env: &'a mut E,
) -> Result<Vec<u32>, AssembleError> {
    let mut state = Assembler::new(env);

    for node in nodes {
        match node {
            Node::Label(Label::Named(identifier)) => {
                state
                    .jump_destinations
                    .insert(identifier, state.bytecode.len() as u32);
            }

            Node::Label(Label::Offset(_)) => {}

            Node::Comment(_) => (),

            Node::Instruction(ins, _) => ins.assemble(&mut state)?,
        }
    }

    for (offset, name) in state.jump_sources {
        let destination = state
            .jump_destinations
            .get(name)
            .copied()
            .ok_or_else(|| AssembleError::LabelNotFound(name.to_owned()))?;
        state.bytecode[offset] = destination;
    }

    Ok(state.bytecode)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{DMString, Instruction, Label};

    struct MissingEnv;

    impl AssembleEnv for MissingEnv {
        fn get_string_index(&mut self, _string: &[u8]) -> Option<u32> {
            None
        }

        fn get_variable_name_index(&mut self, _name: &[u8]) -> Option<u32> {
            None
        }

        fn get_proc_index(&mut self, _path: &str) -> Option<u32> {
            None
        }

        fn get_type(&mut self, _path: &str) -> Option<(u8, u32)> {
            None
        }
    }

    #[test]
    fn assembles_named_and_numeric_labels() {
        let nodes = [
            Node::Instruction(Instruction::Jmp(Label::Named("end".into())), ()),
            Node::Instruction(Instruction::Jmp(Label::Offset(4)), ()),
            Node::Label(Label::Named("end".into())),
            Node::Instruction(Instruction::End, ()),
        ];

        assert_eq!(
            assemble(&nodes, &mut crate::TestAssembleEnv).unwrap(),
            vec![
                crate::Opcode::Jmp.word(),
                4,
                crate::Opcode::Jmp.word(),
                4,
                crate::Opcode::End.word(),
            ]
        );
    }

    #[test]
    fn reports_missing_assembly_inputs() {
        let missing_label = [Node::Instruction(
            Instruction::Jmp(Label::Named("missing".into())),
            (),
        )];
        assert_eq!(
            assemble(&missing_label, &mut MissingEnv),
            Err(AssembleError::LabelNotFound("missing".into()))
        );

        let missing_string = [Node::Instruction(
            Instruction::DbgFile(DMString(b"missing.dm".to_vec())),
            (),
        )];
        assert_eq!(
            assemble(&missing_string, &mut MissingEnv),
            Err(AssembleError::StringNotFound(b"missing.dm".to_vec()))
        );
    }
}
