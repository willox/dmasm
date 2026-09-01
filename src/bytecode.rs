use crate::{access_modifiers, instructions::Opcode};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LabelOperand {
    pub word_offset: u32,
    pub target: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstructionSpan<'a> {
    pub offset: u32,
    pub opcode: Opcode,
    pub bytecode: &'a [u32],
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScannedBytecode<'a> {
    pub instructions: Vec<InstructionSpan<'a>>,
    pub label_operands: Vec<LabelOperand>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BytecodeError {
    UnexpectedEnd { offset: u32 },
    UnknownOpcode { offset: u32, opcode: u32 },
    UnknownAccessModifier { offset: u32, value: u32 },
}

pub fn scan(bytecode: &[u32]) -> Result<ScannedBytecode<'_>, BytecodeError> {
    let mut cursor = Cursor::new(bytecode);
    let mut instructions = Vec::with_capacity(bytecode.len() / 2);
    let mut label_operands = Vec::new();

    while !cursor.finished() {
        let start = cursor.offset();
        let word = cursor.read()?;
        let opcode = Opcode::from_word(word).ok_or(BytecodeError::UnknownOpcode {
            offset: start,
            opcode: word,
        })?;

        opcode.scan_operands(&mut cursor, &mut label_operands)?;

        let end = cursor.offset() as usize;
        instructions.push(InstructionSpan {
            offset: start,
            opcode,
            bytecode: &bytecode[start as usize..end],
        });
    }

    Ok(ScannedBytecode {
        instructions,
        label_operands,
    })
}

pub(crate) struct Cursor<'a> {
    bytecode: &'a [u32],
    offset: usize,
}

impl<'a> Cursor<'a> {
    fn new(bytecode: &'a [u32]) -> Self {
        Self {
            bytecode,
            offset: 0,
        }
    }

    fn finished(&self) -> bool {
        self.offset >= self.bytecode.len()
    }

    fn offset(&self) -> u32 {
        self.offset as u32
    }

    fn read(&mut self) -> Result<u32, BytecodeError> {
        let offset = self.offset();
        let word = self
            .bytecode
            .get(self.offset)
            .copied()
            .ok_or(BytecodeError::UnexpectedEnd { offset })?;
        self.offset += 1;
        Ok(word)
    }

    fn peek(&self) -> Result<u32, BytecodeError> {
        self.bytecode
            .get(self.offset)
            .copied()
            .ok_or(BytecodeError::UnexpectedEnd {
                offset: self.offset(),
            })
    }

    pub(crate) fn skip(&mut self, words: usize) -> Result<(), BytecodeError> {
        let Some(end) = self.offset.checked_add(words) else {
            return Err(BytecodeError::UnexpectedEnd {
                offset: self.bytecode.len() as u32,
            });
        };

        if end > self.bytecode.len() {
            return Err(BytecodeError::UnexpectedEnd {
                offset: self.bytecode.len() as u32,
            });
        }

        self.offset = end;
        Ok(())
    }
}

macro_rules! scan_operand {
    ($cursor:expr, $labels:expr, Label) => {
        crate::bytecode::scan_label($cursor, $labels)?
    };
    ($cursor:expr, $labels:expr, Value) => {
        crate::bytecode::scan_value($cursor)?
    };
    ($cursor:expr, $labels:expr, Variable) => {
        crate::bytecode::scan_variable($cursor)?
    };
    ($cursor:expr, $labels:expr, SwitchParams) => {
        crate::bytecode::scan_switch($cursor, $labels)?
    };
    ($cursor:expr, $labels:expr, PickSwitchParams) => {
        crate::bytecode::scan_pick_switch($cursor, $labels)?
    };
    ($cursor:expr, $labels:expr, SwitchRangeParams) => {
        crate::bytecode::scan_switch_range($cursor, $labels)?
    };
    ($cursor:expr, $labels:expr, PickProbParams) => {
        crate::bytecode::scan_pick_prob($cursor, $labels)?
    };
    ($cursor:expr, $labels:expr, u32) => {
        $cursor.skip(1)?
    };
    ($cursor:expr, $labels:expr, i32) => {
        $cursor.skip(1)?
    };
    ($cursor:expr, $labels:expr, DMString) => {
        $cursor.skip(1)?
    };
    ($cursor:expr, $labels:expr, Proc) => {
        $cursor.skip(1)?
    };
    ($cursor:expr, $labels:expr, TypeFilter) => {
        $cursor.skip(1)?
    };
    ($cursor:expr, $labels:expr, RangeParams) => {
        $cursor.skip(1)?
    };
    ($cursor:expr, $labels:expr, IsInParams) => {
        $cursor.skip(1)?
    };
}

pub(crate) use scan_operand;

pub(crate) fn scan_label(
    cursor: &mut Cursor<'_>,
    labels: &mut Vec<LabelOperand>,
) -> Result<(), BytecodeError> {
    let word_offset = cursor.offset();
    let target = cursor.read()?;
    labels.push(LabelOperand {
        word_offset,
        target,
    });
    Ok(())
}

pub(crate) fn scan_value(cursor: &mut Cursor<'_>) -> Result<(), BytecodeError> {
    let tag = cursor.read()? & 0xFF;
    cursor.skip(1)?;
    if tag == 0x2A {
        cursor.skip(1)?;
    }
    Ok(())
}

pub(crate) fn scan_switch(
    cursor: &mut Cursor<'_>,
    labels: &mut Vec<LabelOperand>,
) -> Result<(), BytecodeError> {
    let count = cursor.read()?;
    for _ in 0..count {
        scan_value(cursor)?;
        scan_label(cursor, labels)?;
    }
    scan_label(cursor, labels)
}

pub(crate) fn scan_pick_switch(
    cursor: &mut Cursor<'_>,
    labels: &mut Vec<LabelOperand>,
) -> Result<(), BytecodeError> {
    let count = cursor.read()?;
    for _ in 0..count {
        cursor.skip(1)?;
        scan_label(cursor, labels)?;
    }
    scan_label(cursor, labels)
}

pub(crate) fn scan_switch_range(
    cursor: &mut Cursor<'_>,
    labels: &mut Vec<LabelOperand>,
) -> Result<(), BytecodeError> {
    let range_count = cursor.read()?;
    for _ in 0..range_count {
        scan_value(cursor)?;
        scan_value(cursor)?;
        scan_label(cursor, labels)?;
    }

    let exact_count = cursor.read()?;
    for _ in 0..exact_count {
        scan_value(cursor)?;
        scan_label(cursor, labels)?;
    }

    scan_label(cursor, labels)
}

pub(crate) fn scan_pick_prob(
    cursor: &mut Cursor<'_>,
    labels: &mut Vec<LabelOperand>,
) -> Result<(), BytecodeError> {
    let count = cursor.read()?;
    for _ in 0..count {
        scan_label(cursor, labels)?;
    }
    Ok(())
}

pub(crate) fn scan_variable(cursor: &mut Cursor<'_>) -> Result<(), BytecodeError> {
    let offset = cursor.offset();
    let param = cursor.peek()?;

    if !access_modifiers::is_access_modifier(param) {
        cursor.skip(1)?;
        return Ok(());
    }

    match cursor.read()? {
        access_modifiers::Null
        | access_modifiers::World
        | access_modifiers::Usr
        | access_modifiers::Src
        | access_modifiers::Args
        | access_modifiers::Dot
        | access_modifiers::Cache
        | access_modifiers::CacheKey
        | access_modifiers::CacheIndex => {}

        access_modifiers::Arg
        | access_modifiers::Local
        | access_modifiers::Global
        | access_modifiers::DynamicProc
        | access_modifiers::DynamicVerb
        | access_modifiers::StaticProc
        | access_modifiers::StaticVerb => {
            cursor.skip(1)?;
        }

        access_modifiers::SetCache => {
            scan_variable(cursor)?;
            scan_variable(cursor)?;
        }

        access_modifiers::Initial
        | access_modifiers::IsSaved
        | access_modifiers::PtrRef
        | access_modifiers::PtrDeref => {
            scan_variable(cursor)?;
        }

        value => return Err(BytecodeError::UnknownAccessModifier { offset, value }),
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scans_newly_mapped_opcodes() {
        let bytecode = [
            Opcode::PopN.word(),
            3,
            Opcode::Sin2.word(),
            Opcode::Cos2.word(),
            Opcode::Tan2.word(),
        ];
        let scanned = scan(&bytecode).unwrap();

        assert_eq!(
            scanned
                .instructions
                .iter()
                .map(|span| (span.offset, span.opcode))
                .collect::<Vec<_>>(),
            vec![
                (0, Opcode::PopN),
                (2, Opcode::Sin2),
                (3, Opcode::Cos2),
                (4, Opcode::Tan2),
            ]
        );
    }

    #[test]
    fn rejects_truncated_length_changing_operands() {
        assert_eq!(
            scan(&[Opcode::PushVal.word(), 0x2A, 0]),
            Err(BytecodeError::UnexpectedEnd { offset: 3 })
        );
        assert_eq!(
            scan(&[Opcode::GetVar.word(), access_modifiers::SetCache]),
            Err(BytecodeError::UnexpectedEnd { offset: 2 })
        );
        assert_eq!(
            scan(&[Opcode::GetVar.word(), 0xFFD1]),
            Err(BytecodeError::UnknownAccessModifier {
                offset: 1,
                value: 0xFFD1,
            })
        );
    }

    #[test]
    fn scans_variable_length_label_operands() {
        let bytecode = [
            Opcode::Switch.word(),
            2,
            0,
            0,
            20,
            0x2A,
            0x3F80,
            0,
            22,
            24,
            Opcode::PickSwitch.word(),
            2,
            10,
            20,
            30,
            40,
            60,
            Opcode::SwitchRange.word(),
            1,
            0,
            0,
            0,
            0,
            20,
            1,
            0,
            0,
            24,
            28,
            Opcode::PickProb.word(),
            3,
            8,
            10,
            12,
            Opcode::End.word(),
        ];
        let scanned = scan(&bytecode).unwrap();

        assert_eq!(
            scanned
                .instructions
                .iter()
                .map(|span| (span.offset, span.opcode))
                .collect::<Vec<_>>(),
            vec![
                (0, Opcode::Switch),
                (10, Opcode::PickSwitch),
                (17, Opcode::SwitchRange),
                (29, Opcode::PickProb),
                (34, Opcode::End),
            ]
        );
        assert_eq!(
            scanned.label_operands,
            vec![
                LabelOperand {
                    word_offset: 4,
                    target: 20,
                },
                LabelOperand {
                    word_offset: 8,
                    target: 22,
                },
                LabelOperand {
                    word_offset: 9,
                    target: 24,
                },
                LabelOperand {
                    word_offset: 13,
                    target: 20,
                },
                LabelOperand {
                    word_offset: 15,
                    target: 40,
                },
                LabelOperand {
                    word_offset: 16,
                    target: 60,
                },
                LabelOperand {
                    word_offset: 23,
                    target: 20,
                },
                LabelOperand {
                    word_offset: 27,
                    target: 24,
                },
                LabelOperand {
                    word_offset: 28,
                    target: 28,
                },
                LabelOperand {
                    word_offset: 31,
                    target: 8,
                },
                LabelOperand {
                    word_offset: 32,
                    target: 10,
                },
                LabelOperand {
                    word_offset: 33,
                    target: 12,
                },
            ]
        );
    }
}
