use std::fmt;

use crate::{
    assembler::{AssembleEnv, Assembler},
    disassembler::{DisassembleEnv, DisassembleError, Disassembler},
};

use crate::operands::Operand;

use bitflags::bitflags;

pub enum Iterator {
    Contents,
    Block,
    View,
    OView,

}

pub static OVIEW: u32 = 0x08;
pub static VIEW: u32 = 0x07;
pub static BLOCK: u32 = 0x06;
pub static CONTENTS: u32 = 0x05;

bitflags! {
    pub struct TypeFilter: u32 {
        const MOB = 0x01;
        const OBJ = 0x02;
        const TEXT = 0x04;
        const NUM = 0x08;
        const FILE = 0x10;
        const TURF = 0x20;
        const KEY = 0x40;
        const NULL = 0x80;
        const AREA = 0x100;
        const ICON = 0x200;
        const SOUND = 0x400;
        const MESSAGE = 0x800;
        const ANYTHING = 0x1000;
        const DATUM_INSTANCES = 0x4000;
        const PASSWORD = 0x8000;
        const COMMAND_TEXT = 0x10000;
        const COLOR = 0x20000;
    }
}

impl Operand for TypeFilter {
    fn assemble<E: AssembleEnv>(&self, _asm: &mut Assembler<E>) {
        unimplemented!()
    }

    fn disassemble<E: DisassembleEnv>(
        dism: &mut Disassembler<E>,
    ) -> Result<Self, DisassembleError> {
        let bits = dism.read_u32()?;

        let filter = match Self::from_bits(bits) {
            Some(filter) => filter,

            None => return Err(DisassembleError::UnknownTypeFilter {
                offset: dism.current_offset - 1,
                value: bits,
            }),
        };

        Ok(filter)
    }

    fn serialize(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;

        if self.contains(Self::MOB) {
            write!(f, "mob | ")?;
        }

        if self.contains(Self::OBJ) {
            write!(f, "obj | ")?;
        }

        if self.contains(Self::TEXT) {
            write!(f, "text | ")?;
        }

        if self.contains(Self::NUM) {
            write!(f, "num | ")?;
        }

        if self.contains(Self::FILE) {
            write!(f, "file | ")?;
        }

        if self.contains(Self::TURF) {
            write!(f, "turf | ")?;
        }

        if self.contains(Self::KEY) {
            write!(f, "key | ")?;
        }

        if self.contains(Self::NULL) {
            write!(f, "null | ")?;
        }

        if self.contains(Self::AREA) {
            write!(f, "area | ")?;
        }

        if self.contains(Self::ICON) {
            write!(f, "icon | ")?;
        }

        if self.contains(Self::SOUND) {
            write!(f, "sound | ")?;
        }

        if self.contains(Self::MESSAGE) {
            write!(f, "message | ")?;
        }

        if self.contains(Self::ANYTHING) {
            write!(f, "anything | ")?;
        }

        if self.contains(Self::DATUM_INSTANCES) {
            write!(f, "datum_instances | ")?;
        }

        if self.contains(Self::PASSWORD) {
            write!(f, "password | ")?;
        }

        if self.contains(Self::COMMAND_TEXT) {
            write!(f, "command_text | ")?;
        }

        if self.contains(Self::COLOR) {
            write!(f, "color | ")?;
        }

        write!(f, ")")
    }
}
