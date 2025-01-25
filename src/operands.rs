use crate::{
    assembler::{AssembleEnv, AssembleError, Assembler},
    disassembler::{DisassembleEnv, DisassembleError, Disassembler},
};
use std::fmt;

pub trait Operand: Sized {
    fn assemble<E: AssembleEnv>(&self, asm: &mut Assembler<E>) -> Result<(), AssembleError>;
    fn disassemble<E: DisassembleEnv>(dism: &mut Disassembler<E>)
        -> Result<Self, DisassembleError>;

    fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

// This is a separate trait just so that the large amount of nom code can live in operands_deserialize
pub trait OperandDeserialize: Sized {
    fn deserialize<'a, E>(i: &'a str) -> nom::IResult<&'a str, Self, E>
    where
        E: nom::error::ParseError<&'a str>
            + nom::error::FromExternalError<&'a str, std::num::ParseIntError>;
}

//
// u32
//
impl Operand for u32 {
    fn assemble<E: AssembleEnv>(&self, asm: &mut Assembler<E>) -> Result<(), AssembleError> {
        asm.emit(*self);
        Ok(())
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
    fn assemble<E: AssembleEnv>(&self, asm: &mut Assembler<E>) -> Result<(), AssembleError> {
        asm.emit(unsafe { std::mem::transmute::<i32, u32>(*self) });
        Ok(())
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
    fn assemble<E: AssembleEnv>(&self, _asm: &mut Assembler<E>) -> Result<(), AssembleError> {
        unreachable!()
    }

    fn disassemble<E: DisassembleEnv>(
        _dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        unreachable!()
    }

    fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:.}", *self)
    }
}

//
// Label
//
#[derive(PartialEq, Debug, Clone)]
pub struct Label(pub String);

impl Operand for Label {
    fn assemble<E: AssembleEnv>(&self, asm: &mut Assembler<E>) -> Result<(), AssembleError> {
        asm.emit_label_operand(&self.0);
        Ok(())
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
#[derive(PartialEq, Debug, Clone)]
pub struct Proc(pub String);

impl Operand for Proc {
    fn assemble<E: AssembleEnv>(&self, asm: &mut Assembler<E>) -> Result<(), AssembleError> {
        let idx = asm
            .env
            .get_proc_index(&self.0)
            .ok_or(AssembleError::ProcNotFound(self.0.to_owned()))?;
        asm.emit(idx);
        Ok(())
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
#[derive(PartialEq, Debug, Clone)]
pub struct DMString(pub Vec<u8>);

impl DMString {
    fn get_string_index<E: AssembleEnv>(&self, asm: &mut Assembler<E>) -> u32 {
        asm.env.get_string_index(&self.0).unwrap()
    }
}

impl Operand for DMString {
    fn assemble<E: AssembleEnv>(&self, asm: &mut Assembler<E>) -> Result<(), AssembleError> {
        let idx = self.get_string_index(asm);
        asm.emit(idx);
        Ok(())
    }

    fn disassemble<E: DisassembleEnv>(
        dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        let id = dism.read_u32()?;
        let data = dism
            .env
            .get_string_data(id)
            .ok_or(DisassembleError::InvalidString {
                offset: dism.current_offset - 1,
                id,
            })?;

        Ok(DMString(data))
    }

    // TODO: Formatting
    fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut format = vec![];
        let mut iter = self.0.iter();

        while let Some(byte_ref) = iter.next() {
            let byte = *byte_ref;
            if byte == 0xFF {
                // NOTE: Doesn't hold state for formatting, so some strings relying on are a little off
                format.extend_from_slice(match iter.next() {
                    None => break,
                    Some(1) | Some(2) | Some(3) | Some(4) => b"[]",
                    Some(5) => b"[]\\th",
                    Some(6) => b"\\a",
                    Some(7) => b"\\A",
                    Some(8) => b"\\the",
                    Some(9) => b"\\The",
                    Some(10) => b"\\he",
                    Some(11) => b"\\He",
                    Some(12) => b"\\his",
                    Some(13) => b"\\His",
                    Some(14) => b"\\hers",
                    Some(15) => b"\\Hers",
                    Some(16) => b"\\him ",
                    Some(17) => b"\\himself",
                    Some(18) => b"\\... ",
                    Some(19) => b"\\n",
                    Some(20) => b"\\s ",
                    Some(21) => b"\\proper ",
                    Some(22) => b"\\improper ",
                    Some(23) => b"\\bold ",
                    Some(24) => b"\\italic ",
                    Some(25) => b"\\underline ",
                    Some(26) => b"\\strike ",
                    Some(27) => b"\\font",
                    Some(28) => b"\\color",
                    Some(29) => b"\\font",
                    Some(30) => b"\\color",
                    Some(31) => b"\\red ",
                    Some(32) => b"\\green ",
                    Some(33) => b"\\blue ",
                    Some(34) => b"\\black ",
                    Some(35) => b"\\white ",
                    Some(36) => b"\\yellow ",
                    Some(37) => b"\\cyan ",
                    Some(38) => b"\\magenta ",
                    Some(39) => b"\\beep ",
                    Some(40) => b"\\link",
                    Some(41) => b" \\link",
                    Some(42) => b"\\ref[]",
                    Some(43) => b"\\icon[]",
                    Some(44) => b"\\roman[]",
                    Some(45) => b"\\Roman[]",
                    Some(_) => b"[UNKNONWN FORMAT SPECIFIER]",
                });
                continue;
            }

            if byte == b'\n' {
                format.extend_from_slice(b"\\n");
                continue;
            }

            if byte == b'\r' {
                format.extend_from_slice(b"\\r");
                continue;
            }

            // Escape \[]"" chars
            if byte == b'\\' || byte == b'[' || byte == b']' || byte == b'"' {
                format.push(b'\\');
            }

            format.push(byte);
        }

        write!(f, "\"{}\"", String::from_utf8_lossy(&format))
    }
}

//
// RangeParams
// This one's a bit odd. Range and ORange seem to always be followed by 0xAE.
// This might actually be a combination of two instructions - but it doesn't really matter for our purposes.
// (TODO: Use the debugger to single-step over this and know for sure.)
//
#[derive(PartialEq, Clone, Debug)]
pub struct RangeParams;

impl Operand for RangeParams {
    fn assemble<E: AssembleEnv>(&self, asm: &mut Assembler<E>) -> Result<(), AssembleError> {
        asm.emit(0xAE);
        Ok(())
    }

    fn disassemble<E: DisassembleEnv>(
        dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        let param = dism.read_u32()?;

        if param != 0xAE {
            return Err(DisassembleError::UnknownRangeParams {
                offset: dism.current_offset - 1,
                value: param,
            });
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
#[derive(PartialEq, Clone, Debug)]
pub enum IsInParams {
    Range,
    Value,
}

impl Operand for IsInParams {
    fn assemble<E: AssembleEnv>(&self, asm: &mut Assembler<E>) -> Result<(), AssembleError> {
        match self {
            Self::Range => asm.emit(0x0B),
            Self::Value => asm.emit(0x05),
        }

        Ok(())
    }

    fn disassemble<E: DisassembleEnv>(
        dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        let param = dism.read_u32()?;

        let res = match param {
            0x0B => Self::Range,
            0x05 => Self::Value,
            other => {
                return Err(DisassembleError::UnknownIsInOperand {
                    offset: dism.current_offset - 1,
                    value: other,
                })
            }
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
#[derive(PartialEq, Clone, Debug)]
pub struct SwitchParams {
    pub default: Label,
    pub cases: Vec<(Value, Label)>,
}

impl Operand for SwitchParams {
    fn assemble<E: AssembleEnv>(&self, _asm: &mut Assembler<E>) -> Result<(), AssembleError> {
        unimplemented!();
    }

    fn disassemble<E: DisassembleEnv>(
        dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        let mut cases = vec![];

        for _ in 0..dism.read_u32()? {
            cases.push((Value::disassemble(dism)?, Label::disassemble(dism)?));
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
#[derive(PartialEq, Clone, Debug)]
pub struct PickSwitchParams {
    pub default: Label,
    pub cases: Vec<(u32, Label)>,
}

impl Operand for PickSwitchParams {
    fn assemble<E: AssembleEnv>(&self, _asm: &mut Assembler<E>) -> Result<(), AssembleError> {
        unimplemented!();
    }

    fn disassemble<E: DisassembleEnv>(
        dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        let mut cases = vec![];

        for _ in 0..dism.read_u32()? {
            cases.push((u32::disassemble(dism)?, Label::disassemble(dism)?));
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
#[derive(PartialEq, Clone, Debug)]
pub struct SwitchRangeParams {
    pub default: Label,
    pub cases: Vec<(Value, Label)>,
    pub range_cases: Vec<(Value, Value, Label)>,
}

impl Operand for SwitchRangeParams {
    fn assemble<E: AssembleEnv>(&self, _asm: &mut Assembler<E>) -> Result<(), AssembleError> {
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
            cases.push((Value::disassemble(dism)?, Label::disassemble(dism)?));
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
#[derive(PartialEq, Clone, Debug)]
pub struct PickProbParams {
    pub cases: Vec<Label>,
}

impl Operand for PickProbParams {
    fn assemble<E: AssembleEnv>(&self, asm: &mut Assembler<E>) -> Result<(), AssembleError> {
        asm.emit(self.cases.len() as u32);

        for case in &self.cases {
            case.assemble(asm)?;
        }

        Ok(())
    }

    fn disassemble<E: DisassembleEnv>(
        dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        let mut cases = vec![];

        for _ in 0..dism.read_u32()? {
            cases.push(Label::disassemble(dism)?);
        }

        Ok(Self { cases })
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
#[derive(PartialEq, Debug, Clone)]
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
    fn assemble<E: AssembleEnv>(&self, asm: &mut Assembler<E>) -> Result<(), AssembleError> {
        let (tag, data): (u8, u32) = match self {
            Self::Null => (0x00, 0x00),
            Self::Raw { tag, data } => (*tag, *data),
            Self::DMString(value) => (0x06, value.get_string_index(asm)),

            // Numbers are a special case. They use an extra operand.
            Self::Number(num) => {
                let bits = num.to_bits();
                asm.emit(0x2A);
                asm.emit((bits >> 16) & 0xFFFF);
                asm.emit(bits & 0xFFFF);
                return Ok(());
            }

            Self::Path(path) => match asm.env.get_type(path) {
                Some(t) => t,
                None => return Err(AssembleError::TypeNotFound(path.clone())),
            },

            // TODO: This _will_ bite me in the ass, implement assemble errors asap
            other => return Err(AssembleError::UnsupportedValue(other.clone())),
        };

        // The top 8 bits of data live in tag
        asm.emit((tag as u32) | ((data & 0xFF0000) >> 8));
        asm.emit(data & 0xFFFF);
        Ok(())
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
                    .get_string_data(data)
                    .ok_or(DisassembleError::InvalidString { offset, id: data })?,
            )),

            0x2A => {
                // Numbers store their data portion in the lower 16-bits of two operands
                let upper_bits = data;
                let lower_bits = dism.read_u32()?;
                Self::Number(f32::from_bits((upper_bits << 16) | lower_bits))
            }

            0x20 | 0x3B | 0x24 | 0x26 | 0x0A | 0x0B | 0x28 | 0x09 | 0x08 | 0x3F => Self::Path(
                String::from_utf8(
                    dism.env
                        .value_to_string_data(tag, data)
                        .ok_or(DisassembleError::UnknownValue { offset, tag })?,
                )
                .map_err(|_| DisassembleError::UnknownValue { offset, tag })?,
            ),

            0x0C => Self::Resource(
                String::from_utf8(
                    dism.env
                        .value_to_string_data(tag, data)
                        .ok_or(DisassembleError::UnknownValue { offset, tag })?,
                )
                .map_err(|_| DisassembleError::UnknownValue { offset, tag })?,
            ),

            0x27 if data == 0 => Self::File,

            0x29 => Self::Raw {
                tag: tag as u8,
                data,
            },

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
            Value::Raw { tag, data } => write!(f, "ref({:X}{:08X})", tag, data),
        }
    }
}

//
// Variable
//
#[derive(PartialEq, Debug, Clone)]
pub enum Variable {
    Null,
    World,
    Usr,
    Src,
    Args,
    Dot,
    Cache,
    CacheKey,
    CacheIndex,
    Arg(u32),
    Local(u32),
    Global(DMString),
    SetCache(Box<Variable>, Box<Variable>),
    Initial(Box<Variable>),
    IsSaved(Box<Variable>),
    Field(DMString),
    //Initial(Box<Variable>, Vec<DMString>),
    StaticVerb(Proc),
    DynamicVerb(DMString),
    StaticProc(Proc),
    DynamicProc(DMString),
    PtrRef(Box<Variable>),
    PtrDeref(Box<Variable>),
    //RuntimeProcField(Box<Variable>, Vec<DMString>, DMString),
}

impl Operand for Variable {
    fn assemble<E: AssembleEnv>(&self, asm: &mut Assembler<E>) -> Result<(), AssembleError> {
        use crate::access_modifiers;

        fn write_variable_name<E: AssembleEnv>(
            asm: &mut Assembler<E>,
            name: &DMString,
        ) -> Result<(), AssembleError> {
            let id = asm
                .env
                .get_variable_name_index(&name.0)
                .ok_or(AssembleError::InvalidVariableName)?;
            asm.emit(id);
            Ok(())
        }

        match self {
            Variable::Null => asm.emit(access_modifiers::Null),
            Variable::World => asm.emit(access_modifiers::World),
            Variable::Usr => asm.emit(access_modifiers::Usr),
            Variable::Src => asm.emit(access_modifiers::Src),
            Variable::Args => asm.emit(access_modifiers::Args),
            Variable::Dot => asm.emit(access_modifiers::Dot),
            Variable::Cache => asm.emit(access_modifiers::Cache),
            Variable::CacheKey => asm.emit(access_modifiers::CacheKey),
            Variable::CacheIndex => asm.emit(access_modifiers::CacheIndex),
            Variable::Field(name) => name.assemble(asm)?,
            Variable::Arg(idx) => {
                asm.emit(access_modifiers::Arg);
                asm.emit(*idx);
            }
            Variable::Local(idx) => {
                asm.emit(access_modifiers::Local);
                asm.emit(*idx);
            }
            Variable::Global(name) => {
                asm.emit(access_modifiers::Global);
                write_variable_name(asm, name)?;
            }
            Variable::SetCache(lhs, rhs) => {
                asm.emit(access_modifiers::SetCache);
                lhs.assemble(asm)?;
                rhs.assemble(asm)?;
            }
            Variable::Initial(rhs) => {
                asm.emit(access_modifiers::Initial);
                rhs.assemble(asm)?;
            }
            Variable::IsSaved(rhs) => {
                asm.emit(access_modifiers::IsSaved);
                rhs.assemble(asm)?;
            }
            Variable::DynamicProc(name) => {
                asm.emit(access_modifiers::DynamicProc);

                // TODO: Improve
                let mut name = name.clone();
                for character in &mut name.0 {
                    if *character == b'_' {
                        *character = b' ';
                    }
                }

                name.assemble(asm)?;
            }
            Variable::DynamicVerb(name) => {
                asm.emit(access_modifiers::DynamicVerb);

                // TODO: Improve
                let mut name = name.clone();
                for character in &mut name.0 {
                    if *character == b'_' {
                        *character = b' ';
                    }
                }

                name.assemble(asm)?;
            }
            Variable::StaticProc(proc) => {
                asm.emit(access_modifiers::StaticProc);
                proc.assemble(asm)?;
            }
            Variable::StaticVerb(proc) => {
                asm.emit(access_modifiers::StaticVerb);
                proc.assemble(asm)?;
            }
            Variable::PtrRef(var) => {
                asm.emit(access_modifiers::PtrRef);
                var.assemble(asm)?;
            }
            Variable::PtrDeref(var) => {
                asm.emit(access_modifiers::PtrDeref);
                var.assemble(asm)?;
            }
        }

        Ok(())
    }

    fn disassemble<E: DisassembleEnv>(
        dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        use crate::access_modifiers;

        fn read_variable_name<E: DisassembleEnv>(
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

        // This is either a string-ref or an AccessModifier
        let param = dism.peek_u32().ok_or(DisassembleError::UnexpectedEnd)?;

        if !access_modifiers::is_access_modifier(param) {
            return Ok(Variable::Field(DMString::disassemble(dism)?));
        }

        let var = match dism.read_u32()? {
            access_modifiers::Null => Variable::Null,
            access_modifiers::World => Variable::World,
            access_modifiers::Usr => Variable::Usr,
            access_modifiers::Src => Variable::Src,
            access_modifiers::Args => Variable::Args,
            access_modifiers::Dot => Variable::Dot,
            access_modifiers::Cache => Variable::Cache,
            access_modifiers::CacheKey => Variable::CacheKey,
            access_modifiers::CacheIndex => Variable::CacheIndex,
            access_modifiers::Arg => Variable::Arg(dism.read_u32()?),
            access_modifiers::Local => Variable::Local(dism.read_u32()?),
            access_modifiers::Global => Variable::Global(read_variable_name(dism)?),
            access_modifiers::SetCache => Variable::SetCache(
                Box::new(Variable::disassemble(dism)?),
                Box::new(Variable::disassemble(dism)?),
            ),
            access_modifiers::Initial => Variable::Initial(Box::new(Variable::disassemble(dism)?)),
            access_modifiers::IsSaved => Variable::IsSaved(Box::new(Variable::disassemble(dism)?)),

            access_modifiers::DynamicProc => Variable::DynamicProc(DMString::disassemble(dism)?),
            access_modifiers::DynamicVerb => Variable::DynamicVerb(DMString::disassemble(dism)?),
            access_modifiers::StaticProc => Variable::StaticProc(Proc::disassemble(dism)?),
            access_modifiers::StaticVerb => Variable::StaticVerb(Proc::disassemble(dism)?),
            access_modifiers::PtrRef => Variable::PtrRef(Box::new(Variable::disassemble(dism)?)),

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
            Variable::CacheKey => write!(f, "cache_key"),
            Variable::CacheIndex => write!(f, "cache[cache_key]"),
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
            Variable::Field(name) => {
                write!(f, "cache[")?;
                name.serialize(f)?;
                write!(f, "]")
            }
            Variable::SetCache(var, var2) => {
                write!(f, "cache = ")?;
                var.serialize(f)?;
                write!(f, "; ")?;
                var2.serialize(f)
            }
            Variable::Initial(var) => {
                write!(f, "initial(")?;
                var.serialize(f)?;
                write!(f, ")")
            }
            Variable::IsSaved(var) => {
                write!(f, "issaved(")?;
                var.serialize(f)?;
                write!(f, ")")
            }
            Variable::StaticVerb(proc) => {
                write!(f, "static_verb(")?;
                proc.serialize(f)?;
                write!(f, ")")
            }
            Variable::DynamicVerb(proc) => {
                write!(f, "dynamic_verb(")?;
                proc.serialize(f)?;
                write!(f, ")")
            }
            Variable::StaticProc(proc) => {
                write!(f, "static_proc(")?;
                proc.serialize(f)?;
                write!(f, ")")
            }
            Variable::DynamicProc(proc) => {
                write!(f, "dynamic_proc(")?;
                proc.serialize(f)?;
                write!(f, ")")
            }
            Variable::PtrRef(var) => {
                write!(f, "&")?;
                var.serialize(f)
            }
            Variable::PtrDeref(var) => {
                write!(f, "*")?;
                var.serialize(f)
            }
        }
    }
}
