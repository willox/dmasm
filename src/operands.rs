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
// f32
// Only a partial implementation because the type is only used for serialization/deserialization
// TODO: Split the behaviour into two traits?
//
impl Operand for f32 {
    fn assemble<E: AssembleEnv>(&self, _asm: &mut Assembler<E>) {
        unimplemented!()
    }

    fn disassemble<E: DisassembleEnv>(
        _dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        unimplemented!()
    }

    fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:.}", *self)
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
// RangeParams
// This one's a bit odd. Range and ORange seem to always be followed by 0xAE.
// This might actually be a combination of two instructions - but it doesn't really matter for our purposes.
// (TODO: Use the debugger to single-step over this and know for sure.)
//
#[derive(PartialEq, Debug)]
pub struct RangeParams;

impl Operand for RangeParams {
    fn assemble<E: AssembleEnv>(&self, asm: &mut Assembler<E>) {
        asm.emit(0xAE);
    }

    fn disassemble<E: DisassembleEnv>(
        dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        let param = dism.read_u32()?;

        if param != 0xAE {
            return Err(DisassembleError::UnknownRangeParams {
                offset: dism.current_offset - 1,
                value: param,
            })
        }

        Ok(RangeParams)
    }

    fn serialize(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        // It's nothing! This works, right?
        Ok(())
    }
}

//
// IsInParams
//
#[derive(PartialEq, Debug)]
pub enum IsInParams {
    Range,
    Value,
}

impl Operand for IsInParams {
    fn assemble<E: AssembleEnv>(&self, asm: &mut Assembler<E>) {
        match self {
            Self::Range => asm.emit(0x0B),
            Self::Value => asm.emit(0x05),
        }
    }

    fn disassemble<E: DisassembleEnv>(
        dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        let param = dism.read_u32()?;

        let res = match param {
            0x0B => Self::Range,
            0x05 => Self::Value,
            other => return Err(DisassembleError::UnknownIsInOperand {
                offset: dism.current_offset - 1,
                value: other,
            }),
        };

        Ok(res)
    }

    fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Range => write!(f, "Range"),
            Self::Value => write!(f, "Value"),
        }
    }
}

//
// SwitchParams
//
#[derive(PartialEq, Debug)]
pub struct SwitchParams {
    pub default: Label,
    pub cases: Vec<(Value, Label)>,
}

impl Operand for SwitchParams {
    fn assemble<E: AssembleEnv>(&self, _asm: &mut Assembler<E>) {
        unimplemented!();
    }

    fn disassemble<E: DisassembleEnv>(
        dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        let mut cases = vec![];

        for _ in 0..dism.read_u32()? {
            cases.push((
                Value::disassemble(dism)?,
                Label::disassemble(dism)?,
            ));
        }

        Ok(Self {
            default: Label::disassemble(dism)?,
            cases,
        })
    }

    fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "default => ")?;
        self.default.serialize(f)?;
        write!(f, ", ")?;

        for case in &self.cases {
            case.0.serialize(f)?;
            write!(f, " => ")?;
            case.1.serialize(f)?;
            write!(f, ", ")?;
        }

        Ok(())
    }
}

//
// PickSwitchParams
//
#[derive(PartialEq, Debug)]
pub struct PickSwitchParams {
    pub default: Label,
    pub cases: Vec<(u32, Label)>,
}

impl Operand for PickSwitchParams {
    fn assemble<E: AssembleEnv>(&self, _asm: &mut Assembler<E>) {
        unimplemented!();
    }

    fn disassemble<E: DisassembleEnv>(
        dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        let mut cases = vec![];

        for _ in 0..dism.read_u32()? {
            cases.push((
                u32::disassemble(dism)?,
                Label::disassemble(dism)?,
            ));
        }

        Ok(Self {
            default: Label::disassemble(dism)?,
            cases,
        })
    }

    fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: Could change our cases to the original DM prob() values
        write!(f, "default => ")?;
        self.default.serialize(f)?;
        write!(f, ", ")?;

        for case in &self.cases {
            case.0.serialize(f)?;
            write!(f, " => ")?;
            case.1.serialize(f)?;
            write!(f, ", ")?;
        }

        Ok(())
    }
}

//
// SwitchRangeParams
//
#[derive(PartialEq, Debug)]
pub struct SwitchRangeParams {
    pub default: Label,
    pub cases: Vec<(Value, Label)>,
    pub range_cases: Vec<(Value, Value, Label)>,
}

impl Operand for SwitchRangeParams {
    fn assemble<E: AssembleEnv>(&self, _asm: &mut Assembler<E>) {
        unimplemented!();
    }

    fn disassemble<E: DisassembleEnv>(
        dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        let mut range_cases = vec![];
        let mut cases = vec![];

        for _ in 0..dism.read_u32()? {
            range_cases.push((
                Value::disassemble(dism)?,
                Value::disassemble(dism)?,
                Label::disassemble(dism)?,
            ));
        }

        for _ in 0..dism.read_u32()? {
            cases.push((
                Value::disassemble(dism)?,
                Label::disassemble(dism)?,
            ));
        }

        Ok(Self {
            default: Label::disassemble(dism)?,
            cases,
            range_cases,
        })
    }

    fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "default => ")?;
        self.default.serialize(f)?;
        write!(f, ", ")?;

        for case in &self.cases {
            case.0.serialize(f)?;
            write!(f, " => ")?;
            case.1.serialize(f)?;
            write!(f, ", ")?;
        }

        for range_case in &self.range_cases {
            write!(f, "(")?;
            range_case.0.serialize(f)?;
            write!(f, " to ")?;
            range_case.1.serialize(f)?;
            write!(f, ") => ")?;
            range_case.2.serialize(f)?;
            write!(f, ", ")?;
        }

        Ok(())
    }
}

//
// PickProbParams
//
#[derive(PartialEq, Debug)]
pub struct PickProbParams {
    pub cases: Vec<Label>,
}

impl Operand for PickProbParams {
    fn assemble<E: AssembleEnv>(&self, _asm: &mut Assembler<E>) {
        unimplemented!();
    }

    fn disassemble<E: DisassembleEnv>(
        dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        let mut cases = vec![];

        for _ in 0..dism.read_u32()? {
            cases.push(
                Label::disassemble(dism)?);
        }

        Ok(Self {
            cases,
        })
    }

    fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for case in &self.cases {
            case.serialize(f)?;
            write!(f, ", ")?;
        }

        Ok(())
    }
}

//
// Value
//
#[derive(PartialEq, Debug)]
pub enum Value {
    Null,
    Number(f32),
    DMString(DMString),
    Path(String),
    Resource(String),
    File,
    Raw { tag: u8, data: u32 },
    /*/
    DatumPath(DMString),
    ClientPath,
    ProcPath(DMString),
    Resource(DMString),
    TurfPath(DMString),
    ObjPath(DMString),
    File(DMString),
    MobPath(DMString),
    ImagePath(DMString),
    */
}

impl Operand for Value {
    fn assemble<E: AssembleEnv>(&self, asm: &mut Assembler<E>) {
        // TODO: WRONG!
        match self {
            Self::Null => {
                asm.emit(0x00);
                asm.emit(0x00);
            }
            Self::Raw { tag, data } => {
                asm.emit(*tag as u32);
                asm.emit(*data);
            }
            Self::DMString(value) => {
                asm.emit(0x06);
                value.assemble(asm);
            }

            Self::Number(value) => {
                asm.emit(0x2A);
                // Numbers store their data portion in the lower 16-bits of two operands
                // TODO: test code
                let bits = value.to_bits();
                asm.emit((bits >> 16) & 0xFFFF);
                asm.emit(bits & 0xFFFF);
            }

            Self::Path(..) | Self::Resource(..) | Self::File => {
                // TODO: This _will_ bite me in the ass, implement assemble errors asap
                asm.emit(0x00);
                asm.emit(0x00);
            }
        }
    }

    fn disassemble<E: DisassembleEnv>(
        dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        let offset = dism.current_offset;

        let tag = dism.read_u32()?;
        let data = (tag & 0xFF00) << 8 | dism.read_u32()?;
        let tag = tag & 0xFF;

        // Number is a special snowflake
        let value = match tag {
            0x00 if data == 0 => Self::Null,

            // This one's a bit dodgy. We can't use DMString::disassemble because our bytes are split apart
            0x06 => Self::DMString(DMString(
                dism.env
                    .get_string(data)
                    .ok_or(DisassembleError::InvalidString { offset, id: data })?,
            )),

            0x2A => {
                // Numbers store their data portion in the lower 16-bits of two operands
                let upper_bits = data;
                let lower_bits = dism.read_u32()?;
                Self::Number(f32::from_bits((upper_bits << 16) | lower_bits))
            }

            0x20 | 0x3B | 0x24 | 0x26 | 0x0A | 0x0B | 0x28 | 0x09 | 0x08 | 0x3F => Self::Path(
                dism.env
                    .value_to_string(tag, data)
                    .ok_or(DisassembleError::UnknownValue { offset, tag })?,
            ),

            0x0C => Self::Resource(
                dism.env
                    .value_to_string(tag, data)
                    .ok_or(DisassembleError::UnknownValue { offset, tag })?,
            ),

            0x27 if data == 0 => Self::File,

            0x29 => {
                Self::Raw { tag: tag as u8, data }
            }

            _ => {
                return Err(DisassembleError::UnknownValue { offset, tag });
            }
        };

        Ok(value)
    }

    // TODO: Formatting
    fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Value::Number(value) => value.serialize(f),
            Value::DMString(value) => value.serialize(f),
            Value::Path(value) => write!(f, "{}", value),
            Value::Resource(value) => write!(f, "'{}'", value),
            Value::File => write!(f, "/file"),
            Value::Raw {tag, data} => write!(f, "ref({:X}{:08X})", tag, data)
        }
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
                let param = dism.peek_u32().ok_or(DisassembleError::UnexpectedEnd)?;

                // The last value is a variable
                if !access_modifiers::is_access_modifier(param) {
                    fields.push(DMString::disassemble(dism)?);
                    return Ok(Variable::Field(lhs, fields));
                }

                match dism.read_u32()? {
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
        let param = dism.peek_u32().ok_or(DisassembleError::UnexpectedEnd)?;

        if !access_modifiers::is_access_modifier(param) {
            let cache = Box::new(Variable::Cache);
            let field = DMString::disassemble(dism)?;
            return Ok(Variable::Field(cache, vec![field]));
        }

        let var = match dism.read_u32()? {
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

            access_modifiers::Initial => Variable::Initial(Box::new(Variable::Cache), vec![DMString::disassemble(dism)?]),

            access_modifiers::Proc | access_modifiers::Proc2 => Variable::StaticProcField(Box::new(Variable::Cache), vec![], Proc::disassemble(dism)?),
            access_modifiers::SrcProc | access_modifiers::SrcProc2 => Variable::RuntimeProcField(Box::new(Variable::Cache), vec![], DMString::disassemble(dism)?),

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
            Variable::Arg(x) => {
                write!(f, "arg(")?;
                x.serialize(f)?;
                write!(f, ")")
            }
            Variable::Local(x) => {
                write!(f, "local(")?;
                x.serialize(f)?;
                write!(f, ")")
            }
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
