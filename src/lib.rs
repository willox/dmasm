mod instructions;
mod operands;
mod parser;
mod compiler;

use instructions::Instruction;

struct Proc {}

#[derive(PartialEq, Debug)]
pub enum Node {
    Comment(String),
    Label(String),
    Instruction(Instruction),
}

#[test]
fn test_compile() {
    let nodes = parser::parse(r#"
Jmp SKIP
START:
; Do a loop
Jmp START
SKIP:
End
    "#);

    let bytecode = compiler::compile(&nodes);

    println!("{:?}", bytecode);

}
