mod instructions;
mod operands;
mod parser;

use instructions::Instruction;

struct Proc {}

#[derive(PartialEq, Debug)]
pub enum Node {
    Comment(String),
    Label(String),
    Instruction(Instruction),
}
