use crate::operands::*;

#[derive(PartialEq, Debug)]
pub enum Instruction {
    End,
    Jmp(Label),
    Output,
}
