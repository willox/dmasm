// Prints dmasm's compile_expr output for each line of stdin. For comparing
// compiler output across changes.
use std::io::BufRead;

fn main() {
    for line in std::io::stdin().lock().lines() {
        let line = line.unwrap();
        if line.trim().is_empty() {
            continue;
        }
        println!("== {line}");
        match dmasm::compiler::compile_expr(&line, &["a", "b"]) {
            Ok(nodes) => print!("{}", dmasm::format(&nodes)),
            Err(e) => println!("error: {e}"),
        }
    }
}
