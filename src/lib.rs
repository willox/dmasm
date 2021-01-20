mod compiler;
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

#[test]
fn test_compile() {
    let nodes = match parser::parse(
        r#"
Jmp SKIP
START:

Jmp START
SKIP:
End
    "#,
    ) {
        Ok(x) => x,
        Err(e) => panic!(e),
    };

    let bytecode = compiler::compile(&nodes);
    // [15, 4, 15, 2, 0]

    println!("{:?}", bytecode);
}
