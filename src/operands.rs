use crate::{
    assembler::{AssembleEnv, Assembler},
    disassembler::{DisassembleEnv, DisassembleError, Disassembler},
};
use std::fmt;

pub trait Operand: Sized {
    fn assemble<E: AssembleEnv>(&self, asm: &mut Assembler<E>);
    fn disassemble<E: DisassembleEnv>(dism: &mut Disassembler<E>)
        -> Result<Self, DisassembleError>;

    fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

// This is a separate trait just so that the large amount of nom code can live in operands_deserialize
pub trait OperandDeserialize: Sized {
    fn deserialize<'a, E>(i: &'a str) -> nom::IResult<&str, Self, E>
    where
        E: nom::error::ParseError<&'a str>
            + nom::error::FromExternalError<&'a str, std::num::ParseIntError>;
}

//
// u32
//
impl Operand for u32 {
    fn assemble<E: AssembleEnv>(&self, asm: &mut Assembler<E>) {
        asm.emit(*self);
    }

    fn disassemble<E: DisassembleEnv>(
        dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        dism.read_u32()
    }

    fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", *self)
    }
}

//
// i32
//
impl Operand for i32 {
    fn assemble<E: AssembleEnv>(&self, asm: &mut Assembler<E>) {
        asm.emit(unsafe { std::mem::transmute(*self) });
    }

    fn disassemble<E: DisassembleEnv>(
        dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        dism.read_i32()
    }

    fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", *self)
    }
}

//
// Label
//
#[derive(PartialEq, Debug)]
pub struct Label(pub String);

impl Operand for Label {
    fn assemble<E: AssembleEnv>(&self, asm: &mut Assembler<E>) {
        asm.emit_label_operand(&self.0)
    }

    fn disassemble<E: DisassembleEnv>(
        dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        let offset = dism.read_u32()?;

        // TODO: Move to output
        dism.reserve_destination(offset);

        // TODO: This label naming scheme is duplicated into output stage
        Ok(Self(format!("LAB_{:0>4X}", offset)))
    }

    fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

//
// Proc
//
#[derive(PartialEq, Debug)]
pub struct Proc(pub String);

impl Operand for Proc {
    fn assemble<E: AssembleEnv>(&self, _asm: &mut Assembler<E>) {
        panic!("TODO");
    }

    fn disassemble<E: DisassembleEnv>(
        dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        let id = dism.read_u32()?;
        let string = dism
            .env
            .get_proc_name(id)
            .ok_or(DisassembleError::InvalidProc {
                offset: dism.current_offset - 1,
                id,
            })?;

        Ok(Proc(string))
    }

    fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

//
// DMString
//
#[derive(PartialEq, Debug)]
pub struct DMString(pub String);

impl Operand for DMString {
    fn assemble<E: AssembleEnv>(&self, asm: &mut Assembler<E>) {
        let index = asm.env.get_string_index(&self.0);
        asm.emit(index);
    }

    fn disassemble<E: DisassembleEnv>(
        dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        let id = dism.read_u32()?;
        let string = dism
            .env
            .get_string(id)
            .ok_or(DisassembleError::InvalidString {
                offset: dism.current_offset - 1,
                id,
            })?;

        Ok(DMString(string))
    }

    // TODO: Formatting
    fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

//
// Variable
//
#[derive(PartialEq, Debug)]
pub enum Variable {
    Null,
    World,
    Usr,
    Src,
    Args,
    Dot,
    Cache,
    Cache2,
    Cache3,
    Unk1,
    Unk2,
    CurrentProc,
    Arg(u32),
    Local(u32),
    Global(DMString),
    Field(Box<Variable>, Vec<DMString>),
    Initial(Box<Variable>, Vec<DMString>),
    StaticProcField(Box<Variable>, Vec<DMString>, Proc),
    RuntimeProcField(Box<Variable>, Vec<DMString>, DMString),
}

impl Operand for Variable {
    fn assemble<E: AssembleEnv>(&self, _asm: &mut Assembler<E>) {
        panic!("TODO")
    }

    fn disassemble<E: DisassembleEnv>(
        dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        use crate::access_modifiers;

        pub fn read_variable_name<E: DisassembleEnv>(
            dism: &mut Disassembler<E>,
        ) -> Result<DMString, DisassembleError> {
            let id = dism.read_u32()?;
            let string =
                dism.env
                    .get_variable_name(id)
                    .ok_or(DisassembleError::InvalidVariableName {
                        offset: dism.current_offset - 1,
                        id,
                    })?;

            Ok(DMString(string))
        }

        // Inner function used for when we encounter a field accessor
        fn read_variable_fields<E: DisassembleEnv>(
            dism: &mut Disassembler<E>,
        ) -> Result<Variable, DisassembleError> {
            // This is either a string-ref or an AccessModifier
            let param = dism.peek_u32().ok_or(DisassembleError::UnexpectedEnd)?;
            let lhs;
            let mut fields = vec![];

            if access_modifiers::is_access_modifier(param) {
                lhs = Box::new(Variable::disassemble(dism)?);
            } else {
                lhs = Box::new(Variable::Cache);
                fields.push(DMString::disassemble(dism)?);
            }

            loop {
                let param = dism.read_u32()?;

                // The last value is a string
                if !access_modifiers::is_access_modifier(param) {
                    fields.push(DMString::disassemble(dism)?);
                    return Ok(Variable::Field(lhs, fields));
                }

                match param {
                    access_modifiers::Field => {
                        // HACK: We need to rethink this whole fn
                        let param = dism.peek_u32();
                        if param == Some(access_modifiers::Global) {
                            dism.read_u32()?;
                            fields.push(read_variable_name(dism)?);
                            continue;
                        }

                        fields.push(DMString::disassemble(dism)?);
                    }

                    // The other modifiers rest are always last, I think! So they return.
                    access_modifiers::Initial => {
                        fields.push(DMString::disassemble(dism)?);
                        return Ok(Variable::Initial(lhs, fields));
                    }

                    access_modifiers::Proc | access_modifiers::Proc2 => {
                        let proc = Proc::disassemble(dism)?;
                        return Ok(Variable::StaticProcField(lhs, fields, proc));
                    }

                    access_modifiers::SrcProc | access_modifiers::SrcProc2 => {
                        let proc = DMString::disassemble(dism)?;
                        return Ok(Variable::RuntimeProcField(lhs, fields, proc));
                    }

                    other => {
                        return Err(DisassembleError::UnknownFieldAccessModifier {
                            offset: dism.current_offset - 1,
                            value: other,
                        })
                    }
                }
            }
        }

        // This is either a string-ref or an AccessModifier
        let param = dism.read_u32()?;

        if !access_modifiers::is_access_modifier(param) {
            let cache = Box::new(Variable::Cache);
            let field = DMString::disassemble(dism)?;
            return Ok(Variable::Field(cache, vec![field]));
        }

        let var = match param {
            access_modifiers::Null => Variable::Null,
            access_modifiers::World => Variable::World,
            access_modifiers::Usr => Variable::Usr,
            access_modifiers::Src => Variable::Src,
            access_modifiers::Args => Variable::Args,
            access_modifiers::Dot => Variable::Dot,
            access_modifiers::Cache => Variable::Cache,
            access_modifiers::Cache2 => Variable::Cache2,
            access_modifiers::Cache3 => Variable::Cache3,
            access_modifiers::Arg => Variable::Arg(dism.read_u32()?),
            access_modifiers::Local => Variable::Local(dism.read_u32()?),
            access_modifiers::Global => Variable::Global(read_variable_name(dism)?),
            access_modifiers::Field => read_variable_fields(dism)?,

            other => {
                return Err(DisassembleError::UnknownAccessModifier {
                    offset: dism.current_offset - 1,
                    value: other,
                })
            }
        };

        Ok(var)
    }

    fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variable::Null => write!(f, "null"),
            Variable::World => write!(f, "world"),
            Variable::Usr => write!(f, "usr"),
            Variable::Src => write!(f, "src"),
            Variable::Args => write!(f, "args"),
            Variable::Dot => write!(f, "dot"),
            Variable::Cache => write!(f, "cache"),
            Variable::Cache2 => write!(f, "cache2"),
            Variable::Cache3 => write!(f, "cache3"),
            Variable::Unk1 => write!(f, "unk1"),
            Variable::Unk2 => write!(f, "unk2"),
            Variable::CurrentProc => write!(f, "dotdot"),
            Variable::Arg(x) => write!(f, "arg({})", x),
            Variable::Local(x) => write!(f, "local({})", x),
            Variable::Global(name) => {
                write!(f, "global(")?;
                name.serialize(f)?;
                write!(f, ")")
            }
            Variable::Field(var, fields) => write!(
                f,
                "field({:?} {:?})",
                **var,
                fields
                    .iter()
                    .map(|x| x.0.clone())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            Variable::Initial(var, fields) => write!(
                f,
                "initial({:?} {:?})",
                **var,
                fields
                    .iter()
                    .map(|x| x.0.clone())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            Variable::StaticProcField(var, fields, name) => write!(
                f,
                "static_proc({:?} {:?} {:?})",
                **var,
                fields
                    .iter()
                    .map(|x| x.0.clone())
                    .collect::<Vec<String>>()
                    .join(" "),
                name
            ),
            Variable::RuntimeProcField(var, fields, name) => write!(
                f,
                "runtime_proc({:?} {:?} {:?})",
                **var,
                fields
                    .iter()
                    .map(|x| x.0.clone())
                    .collect::<Vec<String>>()
                    .join(" "),
                name
            ),
        }
    }
}
