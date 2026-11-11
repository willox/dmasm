//! Finding the global vars a proc's code uses.

use dmasm::compiler::{compile_proc, ProcSource};
use dmasm::{Node, Variable};

#[test]
fn finds_globals_and_static_locals() {
    let code = "var/g = 1\n/proc/f()\n\tvar/static/s = 0\n\ts++\n\treturn g + s\n";
    let ctx = dreammaker::Context::default();
    let pp = dreammaker::Preprocessor::from_buffer(&ctx, "test.dm".into(), code);
    let mut parser = dreammaker::Parser::new(&ctx, pp);
    parser.enable_procs();
    let tree = parser.parse_object_tree();
    let ty = tree.root();
    let value = ty.get().procs.get("f").unwrap().value.last().unwrap();
    let source = ProcSource {
        tree: &tree,
        ty,
        name: "f",
        value,
        file: "test.dm",
        debug_info: false,
        index: None,
        byond: dmasm::ByondVersion::LATEST,
        file_names: None,
    };
    let compiled = compile_proc(&source).unwrap();

    let mut found: Vec<(String, Option<String>)> = compiled
        .nodes
        .iter()
        .filter_map(|n| match n {
            Node::Instruction(ins, _) => Some(ins.variables()),
            _ => None,
        })
        .flatten()
        .flat_map(Variable::globals)
        .map(|g| {
            (
                String::from_utf8_lossy(&g.name.0).into_owned(),
                g.scope.clone(),
            )
        })
        .collect();
    found.sort();
    found.dedup();
    assert_eq!(
        found,
        [
            ("g".to_owned(), Some(String::new())),
            ("s".to_owned(), Some("proc:/proc/f#0".to_owned())),
        ]
    );
}
