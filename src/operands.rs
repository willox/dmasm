use crate::{
    disassembler::{DisassembleError, Disassembler},
};
use std::fmt;

pub trait Operand: Sized {
    fn disassemble(dism: &mut Disassembler)
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
    fn disassemble(
        dism: &mut Disassembler,
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
    fn disassemble(
        dism: &mut Disassembler,
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
    fn disassemble(
        _dism: &mut Disassembler,
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
pub struct Label(pub u32);

impl Operand for Label {
    fn disassemble(
        dism: &mut Disassembler,
    ) -> Result<Self, DisassembleError> {
        let offset = dism.read_u32()?;

        // TODO: This label naming scheme is duplicated into output stage
        Ok(Self(offset))
    }

    fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

//
// Proc
//
#[derive(PartialEq, Debug, Clone)]
pub struct Proc(pub u32);

impl Operand for Proc {
    fn disassemble(
        dism: &mut Disassembler,
    ) -> Result<Self, DisassembleError> {
        Ok(Proc(dism.read_u32()?))
    }

    fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

//
// DMString
//
#[derive(PartialEq, Debug, Clone)]
pub struct DMString(pub u32);

impl Operand for DMString {
    fn disassemble(
        dism: &mut Disassembler,
    ) -> Result<Self, DisassembleError> {
        Ok(DMString(dism.read_u32()?))
    }

    fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "DMString({})", self.0)
    }
}

//
// DMVariableName
//
#[derive(PartialEq, Debug, Clone)]
pub struct DMVariableName(pub u32);

impl Operand for DMVariableName {
    fn disassemble(
        dism: &mut Disassembler,
    ) -> Result<Self, DisassembleError> {
        Ok(DMVariableName(dism.read_u32()?))
    }

    fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "DMVariable({})", self.0)
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
    fn disassemble(
        dism: &mut Disassembler,
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
    fn disassemble(
        dism: &mut Disassembler,
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
    fn disassemble(
        dism: &mut Disassembler,
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
    fn disassemble(
        dism: &mut Disassembler,
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
    fn disassemble(
        dism: &mut Disassembler,
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
    fn disassemble(
        dism: &mut Disassembler,
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
    Path(u8, u32),
    Resource(u32),
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
    fn disassemble(
        dism: &mut Disassembler,
    ) -> Result<Self, DisassembleError> {
        let offset = dism.current_offset;

        let tag = dism.read_u32()?;
        let data = (tag & 0xFF00) << 8 | dism.read_u32()?;
        let tag = tag & 0xFF;

        let value = match tag {
            0x00 if data == 0 => Self::Null,

            0x06 => Self::DMString(DMString(data)),

            0x2A => {
                // Numbers store their data portion in the lower 16-bits of two operands
                let upper_bits = data;
                let lower_bits = dism.read_u32()?;
                Self::Number(f32::from_bits((upper_bits << 16) | lower_bits))
            }

            0x20 | 0x3B | 0x24 | 0x26 | 0x0A | 0x0B | 0x28 | 0x09 | 0x08 | 0x3F => Self::Path(tag as u8, data),

            0x0C => Self::Resource(data),

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
            Value::Path(tag, data) => write!(f, "Path({:X}{:08X})", tag, data),
            Value::Resource(value) => write!(f, "Resource({})", value),
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
    Global(DMVariableName),
    SetCache(Box<Variable>, Box<Variable>),
    Initial(Box<Variable>),
    IsSaved(Box<Variable>),
    Field(DMString),
    //Initial(Box<Variable>, Vec<DMString>),
    StaticVerb(Proc),
    DynamicVerb(DMString),
    StaticProc(Proc),
    DynamicProc(DMString),
    //RuntimeProcField(Box<Variable>, Vec<DMString>, DMString),
}

impl Operand for Variable {
    fn disassemble(
        dism: &mut Disassembler,
    ) -> Result<Self, DisassembleError> {
        use crate::access_modifiers;

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
            access_modifiers::Global => Variable::Global(DMVariableName::disassemble(dism)?),
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
        }
    }
}
