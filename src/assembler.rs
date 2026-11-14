use crate::{Label, Node};
use std::collections::HashMap;

pub trait AssembleEnv {
    /// Converts a rust string into the correct string identifier for the destination context
    fn get_string_index(&mut self, string: &[u8]) -> Option<u32>;
    fn get_variable_name_index(&mut self, name: &[u8]) -> Option<u32>;
    fn get_proc_index(&mut self, path: &str) -> Option<u32>;
    fn get_type(&mut self, path: &str) -> Option<(u8, u32)>;

    /// Resolve a global or static variable ID by name and optional scope.
    /// Use `""` for globals, a declaring type path for type statics, and
    /// `"proc:/path/of/proc"` for proc statics resolved from existing bytecode.
    fn get_global_index(&mut self, name: &[u8], _scope: Option<&str>) -> Option<u32> {
        self.get_variable_name_index(name)
    }

    /// Finds the id of a resource (a single-quoted file like `'icon.dmi'`) in the destination context
    fn get_resource_index(&mut self, _path: &str) -> Option<u32> {
        None
    }

    /// Find a prefab instance of `path` initialized by assembled `init` code.
    fn get_instance(&mut self, _path: &str, _init: &[u32]) -> Option<u32> {
        None
    }
}

#[derive(Debug, PartialEq)]
pub enum AssembleError {
    ProcNotFound(String),
    StringNotFound(Vec<u8>),
    VariableNameNotFound(Vec<u8>),
    TypeNotFound(String),
    ResourceNotFound(String),
    InstanceNotFound(String),
    LabelNotFound(String),
}

pub struct Assembler<'a, E: AssembleEnv> {
    bytecode: Vec<u32>,
    jump_destinations: HashMap<&'a str, u32>,
    jump_sources: Vec<(usize, &'a str)>,
    offset_destinations: HashMap<u32, u32>,
    offset_sources: Vec<(usize, u32)>,
    pub env: &'a mut E,
}

impl<'a, E: AssembleEnv> Assembler<'a, E> {
    fn new(env: &'a mut E) -> Self {
        Assembler {
            bytecode: vec![],
            jump_destinations: HashMap::new(),
            jump_sources: vec![],
            offset_destinations: HashMap::new(),
            offset_sources: vec![],
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

    pub fn emit_offset_label_operand(&mut self, target: u32) {
        self.offset_sources.push((self.bytecode.len(), target));
        self.emit(target);
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

            Node::Label(Label::Offset(offset)) => {
                state
                    .offset_destinations
                    .insert(*offset, state.bytecode.len() as u32);
            }

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

    for (source, target) in state.offset_sources {
        if let Some(destination) = state.offset_destinations.get(&target) {
            state.bytecode[source] = *destination;
        }
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

    impl crate::disassembler::DisassembleEnv for MissingEnv {
        fn get_string_data(&mut self, _index: u32) -> Option<Vec<u8>> {
            None
        }

        fn get_variable_name(&mut self, _index: u32) -> Option<Vec<u8>> {
            None
        }

        fn get_proc_name(&mut self, _index: u32) -> Option<String> {
            None
        }

        fn value_to_string_data(&mut self, _tag: u32, _data: u32) -> Option<Vec<u8>> {
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
    fn relocates_offset_labels() {
        let bytecode = [
            crate::Opcode::Jmp.word(),
            4,
            crate::Opcode::PushInt.word(),
            7,
            crate::Opcode::End.word(),
        ];
        let (nodes, error) = crate::disassembler::disassemble(&bytecode, &mut MissingEnv);
        assert_eq!(error, None);

        let mut nodes = nodes
            .into_iter()
            .map(Node::strip_debug_data)
            .collect::<Vec<_>>();
        let target = nodes
            .iter()
            .position(|node| matches!(node, Node::Label(Label::Offset(4))))
            .unwrap();
        nodes.insert(target, Node::Instruction(Instruction::PushInt(8), ()));

        assert_eq!(
            assemble(&nodes, &mut crate::TestAssembleEnv).unwrap(),
            vec![
                crate::Opcode::Jmp.word(),
                6,
                crate::Opcode::PushInt.word(),
                7,
                crate::Opcode::PushInt.word(),
                8,
                crate::Opcode::End.word(),
            ]
        );
    }

    // With an env that knows no strings: read as a field name, either word
    // would fail the lookup instead of decoding.
    #[test]
    fn caller_and_callee_round_trip() {
        use crate::Variable;

        let bytecode = [
            crate::Opcode::GetVar.word(),
            0xFFF0,
            crate::Opcode::GetVar.word(),
            0xFFF1,
        ];
        let (nodes, error) = crate::disassembler::disassemble(&bytecode, &mut MissingEnv);
        assert_eq!(error, None);

        let nodes = nodes
            .into_iter()
            .map(Node::strip_debug_data)
            .collect::<Vec<_>>();
        assert_eq!(
            nodes,
            [
                Node::Instruction(Instruction::GetVar(Variable::Caller), ()),
                Node::Instruction(Instruction::GetVar(Variable::Callee), ()),
            ]
        );
        assert_eq!(
            assemble(&nodes, &mut crate::TestAssembleEnv).unwrap(),
            bytecode
        );
    }

    // Big worlds have more than 0xFFCD strings, so some field names get ids
    // that look like access modifiers
    struct ModifierRangeStringEnv;

    impl AssembleEnv for ModifierRangeStringEnv {
        fn get_string_index(&mut self, string: &[u8]) -> Option<u32> {
            match string {
                b"in_range" => Some(crate::access_modifiers::StaticVerb),
                _ => Some(1337),
            }
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

    impl crate::disassembler::DisassembleEnv for ModifierRangeStringEnv {
        fn get_string_data(&mut self, index: u32) -> Option<Vec<u8>> {
            match index {
                crate::access_modifiers::StaticVerb => Some(b"in_range".to_vec()),
                1337 => Some(b"normal".to_vec()),
                _ => None,
            }
        }

        fn get_variable_name(&mut self, _index: u32) -> Option<Vec<u8>> {
            None
        }

        fn get_proc_name(&mut self, _index: u32) -> Option<String> {
            None
        }

        fn value_to_string_data(&mut self, _tag: u32, _data: u32) -> Option<Vec<u8>> {
            None
        }
    }

    #[test]
    fn flags_field_ids_in_the_access_modifier_range() {
        use crate::access_modifiers;
        use crate::operands::Variable;

        let field = |name: &str| {
            Instruction::GetVar(Variable::SetCache(
                Box::new(Variable::Src),
                Box::new(Variable::Field(DMString(name.as_bytes().to_vec()))),
            ))
        };
        let nodes = [
            Node::Instruction(field("in_range"), ()),
            Node::Instruction(field("normal"), ()),
        ];

        let bytecode = assemble(&nodes, &mut ModifierRangeStringEnv).unwrap();
        assert_eq!(
            bytecode,
            vec![
                crate::Opcode::GetVar.word(),
                access_modifiers::SetCache,
                access_modifiers::Src,
                0x1000_0000 | access_modifiers::StaticVerb,
                crate::Opcode::GetVar.word(),
                access_modifiers::SetCache,
                access_modifiers::Src,
                1337,
            ]
        );

        let (disassembled, error) =
            crate::disassembler::disassemble(&bytecode, &mut ModifierRangeStringEnv);
        assert_eq!(error, None);
        let disassembled = disassembled
            .into_iter()
            .map(Node::strip_debug_data)
            .collect::<Vec<_>>();
        assert_eq!(disassembled, nodes);
    }

    // Knows one string, one resource and one type, both ways round
    struct OperandEnv;

    impl AssembleEnv for OperandEnv {
        fn get_string_index(&mut self, string: &[u8]) -> Option<u32> {
            (string == b"case").then_some(0x15)
        }

        fn get_variable_name_index(&mut self, _name: &[u8]) -> Option<u32> {
            None
        }

        fn get_proc_index(&mut self, _path: &str) -> Option<u32> {
            None
        }

        fn get_type(&mut self, path: &str) -> Option<(u8, u32)> {
            (path == "/obj").then_some((0x09, 0x02))
        }

        fn get_resource_index(&mut self, path: &str) -> Option<u32> {
            (path == "icon.dmi").then_some(0x01_0203)
        }
    }

    impl crate::disassembler::DisassembleEnv for OperandEnv {
        fn get_string_data(&mut self, index: u32) -> Option<Vec<u8>> {
            (index == 0x15).then(|| b"case".to_vec())
        }

        fn get_variable_name(&mut self, _index: u32) -> Option<Vec<u8>> {
            None
        }

        fn get_proc_name(&mut self, _index: u32) -> Option<String> {
            None
        }

        fn value_to_string_data(&mut self, tag: u32, data: u32) -> Option<Vec<u8>> {
            match (tag, data) {
                (0x09, 0x02) => Some(b"/obj".to_vec()),
                (0x0C, 0x01_0203) => Some(b"icon.dmi".to_vec()),
                _ => None,
            }
        }
    }

    fn assert_round_trips(bytecode: &[u32]) -> Vec<Node> {
        let (nodes, error) = crate::disassembler::disassemble(bytecode, &mut OperandEnv);
        assert_eq!(error, None);

        let nodes = nodes
            .into_iter()
            .map(Node::strip_debug_data)
            .collect::<Vec<_>>();
        assert_eq!(assemble(&nodes, &mut OperandEnv).unwrap(), bytecode);
        nodes
    }

    #[test]
    fn switch_operands_round_trip() {
        use crate::Opcode;

        // switch on 1, "case" and /obj, then default
        let switch = [
            Opcode::Switch.word(),
            3,
            0x2A,
            0x3F80,
            0x0000,
            9,
            0x06,
            0x15,
            9,
            0x09,
            0x02,
            9,
            9,
            Opcode::End.word(),
        ];
        let nodes = assert_round_trips(&switch);
        assert!(matches!(
            &nodes[0],
            Node::Instruction(Instruction::Switch(params), ()) if params.cases.len() == 3
        ));

        // pick(50; a, 50; b)
        let pick_switch = [
            Opcode::PickSwitch.word(),
            2,
            0x7FFF,
            7,
            0xFFFF,
            7,
            7,
            Opcode::End.word(),
        ];
        assert_round_trips(&pick_switch);

        // if(1 to 2), if("case"), else
        let switch_range = [
            Opcode::SwitchRange.word(),
            1,
            0x2A,
            0x3F80,
            0x0000,
            0x2A,
            0x4000,
            0x0000,
            13,
            1,
            0x06,
            0x15,
            13,
            13,
            Opcode::End.word(),
        ];
        assert_round_trips(&switch_range);
    }

    #[test]
    fn is_in_with_a_step_round_trips() {
        // x in 1 to 10 step 2: the bounds, the step and x, then IsIn 0x0C
        let bytecode = [
            crate::Opcode::PushInt.word(),
            1,
            crate::Opcode::PushInt.word(),
            10,
            crate::Opcode::PushInt.word(),
            2,
            crate::Opcode::IsIn.word(),
            0x0C,
            crate::Opcode::End.word(),
        ];
        let nodes = assert_round_trips(&bytecode);
        assert!(matches!(
            &nodes[3],
            Node::Instruction(
                Instruction::IsIn(crate::operands::IsInParams::RangeStep),
                ()
            )
        ));
    }

    #[test]
    fn iter_load_round_trips() {
        let filter = crate::TypeFilter::MOB | crate::TypeFilter::OBJ;
        let bytecode = [crate::Opcode::IterLoad.word(), 1, filter.bits()];
        assert_round_trips(&bytecode);
    }

    #[test]
    fn resource_and_file_values_round_trip() {
        use crate::{operands::Value, Opcode};

        let bytecode = [
            Opcode::PushVal.word(),
            0x010C,
            0x0203,
            Opcode::PushVal.word(),
            0x27,
            0x00,
        ];
        let nodes = assert_round_trips(&bytecode);
        assert_eq!(
            nodes,
            [
                Node::Instruction(Instruction::PushVal(Value::Resource("icon.dmi".into())), ()),
                Node::Instruction(Instruction::PushVal(Value::File), ()),
            ]
        );

        // An env that can't find resources says which one
        let missing = [Node::Instruction(
            Instruction::PushVal(Value::Resource("missing.dmi".into())),
            (),
        )];
        assert_eq!(
            assemble(&missing, &mut MissingEnv),
            Err(AssembleError::ResourceNotFound("missing.dmi".into()))
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
