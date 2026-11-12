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
    // Resolve bare calls on src by name, since its runtime type may supply a verb.
    let (code, _) = body("var/x = bump(1)\nx").unwrap();
    assert_has(&code, &["GetVar arg(1)", "PushVal \"bump\""]);
    assert!(
        code.iter().any(|l| l.starts_with("CallName")),
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

#[test]
fn a_call_the_builtin_types_dont_have_goes_by_name() {
    // Call by name to find a world verb absent from the builtin-only parse.
    let (code, _) = body("var/mob/M = new\nM.plain_verb()\nM:examinate(M)").expect("compiles");
    let joined = code.join("\n");
    assert!(joined.contains("CallName 0"), "{}", joined);
    assert!(joined.contains("CallName 1"), "{}", joined);
    assert!(!joined.contains("dynamic_proc"), "{}", joined);
    // Known builtin calls keep their existing compilation.
    let (code, _) = body("var/mob/M = new\nM.Move(null)").expect("compiles");
    let joined = code.join("\n");
    assert!(!joined.contains("CallName"), "{}", joined);
}

#[test]
fn an_implicit_new_of_a_world_type_makes_the_declared_type() {
    // Assembly resolves world types absent from the builtin-only parse.
    let (code, _) = body("var/datum/apiModel/Error/m = new\nm").expect("compiles");
    assert!(
        code.iter().any(|l| l == "PushVal /datum/apiModel/Error"),
        "{}",
        code.join("\n")
    );
}

#[test]
fn a_body_may_be_indented_with_spaces() {
    let tabs = body("var/n = 0\nfor(var/i in 1 to 3)\n\tn++\nn").unwrap();
    for code in [
        "var/n = 0\nfor(var/i in 1 to 3)\n    n++\nn",
        "var/n = 0\nfor(var/i in 1 to 3)\n  n++\nn",
        // Also accept a body pasted with common leading indentation.
        "    var/n = 0\n    for(var/i in 1 to 3)\n        n++\n    n",
    ] {
        assert!(is_body(code), "{:?} should be a body", code);
        assert_eq!(body(code).unwrap(), tabs, "{:?}", code);
    }
    // Nested blocks indented with spaces.
    let (code, _) = body("var/n = 0\nfor(var/i in 1 to 3)\n    if(i > 1)\n        n++\nn").unwrap();
    assert_has(&code, &["Inc local(0)"]);
    // Error columns must refer to the original input.
    for (code, at) in [
        ("var/x = 1\nif(x)\n    x x", "3:7:"),
        ("var/x = 1\nif(x)\n\tx x", "3:4:"),
        ("  var/x = 1\n  if(x)\n      x x", "3:9:"),
    ] {
        let error = body(code).unwrap_err().to_string();
        assert!(
            error.starts_with(&format!("parser error: {} error: got `x`", at)),
            "{:?}: {}",
            code,
            error
        );
    }
}

#[test]
fn global_vars_is_the_globals_list() {
    let (code, _) = body("var/n = length(global.vars)\nn").unwrap();
    assert_has(&code, &["GetVar arg(3)", "Length", "SetVar local(0)"]);
}
