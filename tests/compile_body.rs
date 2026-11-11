//! Debugger proc-body compilation and frame variable writeback.

use dmasm::compiler::{compile_body_in, is_body, CompileError, ExprGlobal, ExprScope};
use dmasm::ByondVersion;

/// Compile with src variable hp, src proc bump and global ticks.
/// The globals parameter represents global.vars; other parameters
/// represent the selected frame.
fn body(code: &str) -> Result<(Vec<String>, Vec<String>), CompileError> {
    let scope = ExprScope {
        has_src_var: &|name| name == "hp",
        has_src_proc: &|name| name == "bump",
        global: &|name| (name == "ticks").then_some(ExprGlobal::Listed),
        globals: "globals",
    };
    let compiled = compile_body_in(
        code,
        &[".", "src", "by", "globals"],
        Some(&scope),
        ByondVersion::LATEST,
    )?;
    let text = dmasm::format(&compiled.nodes)
        .lines()
        .map(|l| l.trim().to_owned())
        .filter(|l| !l.is_empty())
        .collect();
    Ok((text, compiled.locals))
}

/// The lines that return the result and the four params
const RETURN: [&str; 6] = [
    "GetVar arg(0)",
    "GetVar arg(1)",
    "GetVar arg(2)",
    "GetVar arg(3)",
    "NewList 5",
    "Ret",
];

#[track_caller]
fn assert_has(code: &[String], lines: &[&str]) {
    assert!(
        code.windows(lines.len()).any(|w| w == lines),
        "expected\n{}\nin\n{}",
        lines.join("\n"),
        code.join("\n")
    );
}

#[test]
fn what_is_a_body() {
    for code in [
        "var/x = 1",
        "x = 1; x",
        "x\ny",
        "return 5",
        "for(var/i in 1 to 3) x += i",
        "if(x) 1",
        "var/n = 0; for(var/i in 1 to 3) n += i; n",
        "  var/x = 1\n  x",
    ] {
        assert!(is_body(code), "{:?} should be a body", code);
    }
    // Keep single expressions and invalid bodies on the expression path
    // so the expression compiler supplies diagnostics.
    for code in [
        "x",
        "x;",
        "x = 1",
        "f(x)",
        "x + 1\n",
        "",
        "1 +",
        "x x",
        "\"(a = b)\"",
    ] {
        assert!(!is_body(code), "{:?} shouldn't be a body", code);
    }
}

#[test]
fn a_last_statement_that_is_an_expression_is_the_result() {
    let (code, locals) = body("var/n = 0; for(var/i in 1 to 3) n += i; n").unwrap();
    assert_eq!(locals, ["n", "i"]);
    let mut lines = vec!["GetVar local(0)"];
    lines.extend_from_slice(&RETURN[1..]);
    // (`n` then the params, after the result)
    assert_has(
        &code,
        &[
            "GetVar local(0)",
            "GetVar arg(0)",
            "GetVar arg(1)",
            "GetVar arg(2)",
            "GetVar arg(3)",
            "NewList 5",
            "Ret",
        ],
    );
}

#[test]
fn every_way_out_returns_the_params() {
    let (code, _) = body("if(by)\n\treturn 1\nby = 2").unwrap();
    assert_has(
        &code,
        &[
            "PushInt 1",
            "GetVar arg(0)",
            "GetVar arg(1)",
            "GetVar arg(2)",
            "GetVar arg(3)",
            "NewList 5",
            "Ret",
        ],
    );
    // the last statement, an assignment, is the result
    assert_has(&code, &["PushInt 2", "SetVarExpr arg(2)"]);
    // Fallthrough and bare return use the frame's dot value.
    let (code, _) = body("var/x = 1\nreturn").unwrap();
    assert_has(&code, &RETURN);
    assert!(!code.iter().any(|l| l == "End"), "{}", code.join("\n"));
}

#[test]
fn names_are_the_body_s_then_the_frame_s_then_src_s_then_globals() {
    let (code, _) = body("var/x = hp + by + ticks\nx").unwrap();
    assert_has(&code, &["GetVar cache = arg(1); cache[\"hp\"]"]);
    assert_has(&code, &["GetVar arg(2)"]);
    assert_has(&code, &["GetVar arg(3)", "PushVal \"ticks\"", "ListGet"]);
    // a bare call is src's proc when it has one
    let (code, _) = body("var/x = bump(1)\nx").unwrap();
    assert!(
        code.iter()
            .any(|l| l.contains("arg(1)") && l.starts_with("Call")),
        "{}",
        code.join("\n")
    );
    let error = body("var/x = 1\nx = nowhere").unwrap_err();
    assert_eq!(error.to_string(), "line 2: undefined var: nowhere");
}

#[test]
fn errors_say_the_line_in_the_code() {
    let error = body("var/x = 1\nx +").unwrap_err();
    assert!(matches!(error, CompileError::ParseError(_)), "{:?}", error);
    assert!(
        error.to_string().starts_with("parser error: 2:"),
        "{}",
        error
    );
}

#[test]
fn a_pointer_and_a_type_s_var() {
    let (code, _) = body("var/x = 5\n*(&x)").unwrap();
    assert_has(
        &code,
        &["GetVar &local(0)", "SetVar cache_key", "GetVar *cache_key"],
    );
    // Without project types, resolve :: at runtime.
    let (code, _) = body("var/x = /obj/item::throwforce\nx").unwrap();
    assert_has(
        &code,
        &[
            "PushVal /obj/item",
            "SetVar cache",
            "GetVar initial(cache[\"throwforce\"])",
        ],
    );
    let (code, _) = body("var/x = initial(/obj::name)\nx").unwrap();
    assert_has(
        &code,
        &[
            "PushVal /obj",
            "SetVar cache",
            "GetVar initial(cache[\"name\"])",
        ],
    );
}

#[test]
fn the_debug_info_names_lines_in_the_code() {
    let (code, _) = body("var/x = 1\n\nx++\nx").unwrap();
    assert_eq!(code[0], "DbgFile \"<dmasm expression>\"");
    assert_has(&code, &["DbgLine 1"]);
    assert_has(&code, &["DbgLine 3"]);
}
