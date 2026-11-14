//! Compare small-proc compilation against observed DreamMaker 516 bytecode.

use dmasm::compiler::{compile_proc, ProcSource};

/// Compiles proc `name` of type `ty` ("" for global procs) from `code`, and
/// returns its instructions one per line, without debug info.
fn compile(code: &str, ty: &str, name: &str) -> String {
    let ctx = dreammaker::Context::default();
    let pp = dreammaker::Preprocessor::from_buffer(&ctx, "test.dm".into(), code.to_owned());
    let mut parser = dreammaker::Parser::new(&ctx, pp);
    parser.enable_procs();
    let tree = parser.parse_object_tree();
    let ty = if ty.is_empty() {
        tree.root()
    } else {
        tree.find(ty).expect("type")
    };
    let value = ty
        .get()
        .procs
        .get(name)
        .expect("proc")
        .value
        .last()
        .expect("proc value");
    let source = ProcSource {
        tree: &tree,
        ty,
        name,
        value,
        file: "test.dm",
        debug_info: false,
        index: None,
        byond: dmasm::ByondVersion::LATEST,
        file_names: None,
    };
    let compiled = compile_proc(&source).expect("compiles");
    dmasm::format(&compiled.nodes)
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty())
        .collect::<Vec<_>>()
        .join("\n")
}

/// Asserts that `lines` appear in `code` consecutively.
#[track_caller]
fn assert_has(code: &str, lines: &[&str]) {
    let want = lines.join("\n");
    assert!(code.contains(&want), "expected\n{}\nin\n{}", want, code);
}

#[test]
fn findlasttext_defaults() {
    let code = compile("/proc/f(a)\n\treturn findlasttext(a, \"/\")\n", "", "f");
    assert_has(
        &code,
        &["PushVal \"/\"", "PushInt 0", "PushInt 1", "FindLastText"],
    );
}

#[test]
fn empty_argument_is_pushed_null() {
    let code = compile("/proc/g(x, y, z)\n/proc/f(a, b)\n\tg(a, , b)\n", "", "f");
    assert_has(&code, &["GetVar arg(0)", "PushVal null", "GetVar arg(1)"]);
}

#[test]
fn const_alone_is_its_global_but_folds_in_expressions() {
    let src = "var/const/RC = 20\n/proc/f()\n\tworld << RC\n\tworld << RC + 1\n\tworld << NORTH\n";
    let code = compile(src, "", "f");
    assert_has(&code, &["GetVar world", "GetVar global(\"RC\")", "Output"]);
    assert_has(&code, &["GetVar world", "PushInt 21", "Output"]);
    assert_has(&code, &["GetVar world", "PushInt 1", "Output"]);
}

#[test]
fn static_after_double_colon_chain() {
    let src = "/datum/a\n\tvar/datum/b/b\n/datum/b\n\tvar/const/C = 3\nvar/datum/a/A\n/proc/f()\n\treturn A::b::C\n";
    let code = compile(src, "", "f");
    assert_has(&code, &["GetVar global(\"C\")", "Ret"]);
    assert!(!code.contains("global(\"A\")"), "{}", code);
}

#[test]
fn del_clears_its_target() {
    let src = "/datum/q/proc/f()\n\tvar/datum/x = new\n\tdel(x)\n\tdel src\n";
    let code = compile(src, "/datum/q", "f");
    assert_has(
        &code,
        &["GetVar local(0)", "PushVal null", "SetVar local(0)", "Del"],
    );
    assert_has(
        &code,
        &["GetVar src", "PushVal null", "SetVar src", "Del", "End"],
    );
}

#[test]
fn vars_after_a_switch_start_null() {
    let src =
        "/proc/f(a)\n\tvar/x\n\tswitch(a)\n\t\tif(1)\n\t\t\treturn\n\tvar/y\n\treturn list(x, y)\n";
    let code = compile(src, "", "f");
    assert_eq!(code.matches("PushVal null").count(), 1, "{}", code);
    assert_has(&code, &["PushVal null", "SetVar local(1)"]);
}

#[test]
fn alist_makes_an_alist() {
    let code = compile("/proc/f(a)\n\treturn alist(1 = a)\n", "", "f");
    assert_has(&code, &["PushInt 1", "GetVar arg(0)", "NewAlist 1"]);
}

#[test]
fn input_from_builtin_sources() {
    let src = "/proc/f(a, L)\n\t. = input(a) as num\n\t. = input(a) as mob in world\n\t. = input(a) in L\n";
    let code = compile(src, "", "f");
    assert_has(&code, &["Input 8 0 0", "PromptCheck"]);
    assert_has(&code, &["Input 1 127 16", "PromptCheck"]);
    assert_has(
        &code,
        &[
            "GetVar arg(1)",
            "GetVar arg(0)",
            "PushVal null",
            "PushVal null",
            "PushVal null",
            "Input 0 0 64",
        ],
    );
}

#[test]
fn safe_access_assignment() {
    let src = "/datum/q\n\tvar/n\n/proc/f(datum/q/a, x)\n\ta?.n = x\n";
    let code = compile(src, "", "f");
    assert_has(
        &code,
        &[
            "GetVar arg(0)",
            "SetCachePopJmpIfNull LAB_0001",
            "PushCache",
            "GetVar arg(1)",
            "PopCache",
            "SetVar cache[\"n\"]",
        ],
    );
}

#[test]
fn or_assign_field_saves_cache_around_value() {
    let src = "/datum/q\n\tvar/x\n\tvar/y\n/proc/f(datum/q/o)\n\to.y ||= o.x\n";
    let code = compile(src, "", "f");
    assert_has(
        &code,
        &[
            "Test",
            "Jnz LAB_0000",
            "PushCache",
            "GetVar cache[\"x\"]",
            "PopCache",
        ],
    );
}

#[test]
fn sized_list_var() {
    let code = compile("/proc/f(n)\n\tvar/list/L[n]\n\treturn L\n", "", "f");
    assert_has(&code, &["GetVar arg(0)", "EmptyList", "SetVar local(0)"]);
}

#[test]
fn verbs_are_called_by_display_name() {
    let src = "/mob/verb/stop_pulling()\n\tset name = \"Stop Pulling\"\n/proc/f(mob/m)\n\tm.stop_pulling()\n";
    let code = compile(src, "", "f");
    assert!(code.contains("dynamic_verb(\"Stop Pulling\")"), "{}", code);
}

#[test]
fn savefile_input_statement() {
    let code = compile("/proc/f(savefile/S, x)\n\tS[\"k\"] >> x\n", "", "f");
    assert_has(
        &code,
        &[
            "GetVar arg(0)",
            "PushVal \"k\"",
            "Index",
            "Read",
            "SetVar arg(1)",
        ],
    );
}

#[test]
fn same_named_static_locals_in_one_proc_are_different_globals() {
    let src = "/proc/f(a)\n\tif(a)\n\t\tvar/static/x = 1\n\t\tworld << x\n\telse\n\t\tvar/static/x = 2\n\t\tworld << x\n";
    let ctx = dreammaker::Context::default();
    let pp = dreammaker::Preprocessor::from_buffer(&ctx, "test.dm".into(), src.to_owned());
    let mut parser = dreammaker::Parser::new(&ctx, pp);
    parser.enable_procs();
    let tree = parser.parse_object_tree();
    let value = tree
        .root()
        .get()
        .procs
        .get("f")
        .unwrap()
        .value
        .last()
        .unwrap();
    let source = ProcSource {
        tree: &tree,
        ty: tree.root(),
        name: "f",
        value,
        file: "test.dm",
        debug_info: false,
        index: None,
        byond: dmasm::ByondVersion::LATEST,
        file_names: None,
    };
    let compiled = compile_proc(&source).unwrap();
    let scopes: Vec<Option<String>> = compiled
        .nodes
        .iter()
        .filter_map(|n| match n {
            dmasm::Node::Instruction(dmasm::Instruction::GetVar(dmasm::Variable::Global(g)), _) => {
                Some(g.scope.clone())
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        scopes,
        vec![
            Some("proc:/proc/f#0".to_owned()),
            Some("proc:/proc/f#1".to_owned())
        ]
    );
}

/// Calls can invalidate cached field chains; reload after non-receiver calls.
#[test]
fn chain_reloaded_after_call() {
    let code = compile(
        "/datum/var/datum/m\n/datum/var/list/L\n/datum/proc/g()\n/proc/f(datum/a)\n\ta.m.g()\n\tfor (var/x in a.m.L)\n\t\treturn x\n",
        "",
        "f",
    );
    assert_has(
        &code,
        &["GetVar cache = arg(0); cache = cache[\"m\"]; cache[\"L\"]"],
    );
}

/// `/type::proc()` is the type's own override.
#[test]
fn type_proc_reference_override() {
    let code = compile(
        "/atom/proc/p()\n/obj/a/p()\n/obj/a/b\n/proc/f()\n\treturn /obj/a/b::p()\n",
        "",
        "f",
    );
    assert_has(&code, &["PushVal /obj/a/p"]);
}

/// `var/x = a in list(...)` is `var/x = a`: the list is x's `in` clause.
#[test]
fn var_in_clause() {
    let code = compile(
        "/proc/f(a)\n\tvar/x = a in list(1, 2)\n\treturn x\n",
        "",
        "f",
    );
    assert_has(&code, &["GetVar arg(0)", "SetVar local(0)"]);
}

/// Out-of-range conversion produces x86 integer sentinel 0x80000000,
/// so ~(65536 ** 2) becomes 0xFFFFFF.
#[test]
fn bit_not_of_huge_number() {
    let code = compile("/proc/f()\n\treturn ~(65536 ** 2)\n", "", "f");
    assert_has(&code, &["PushVal 16777215"]);
}

/// Builtin vars like `type` are declared const but differ per object: never
/// folded, so `loc?.type || "x"` keeps its first operand.
#[test]
fn builtin_const_not_folded() {
    let code = compile(
        "/atom/proc/f()\n\treturn loc?.type || \"x\"\n",
        "/atom",
        "f",
    );
    assert!(code.contains("SetCacheJmpIfNull"), "{}", code);
}

/// Receiver calls retain the cached old object until an assignment
/// statement or non-receiver call invalidates the chain.
#[test]
fn chain_kept_after_call() {
    let code = compile(
        "/datum/var/datum/m\n/datum/var/x\n/datum/proc/g()\n/proc/f(datum/a)\n\ta.m.g()\n\ta.m.g()\n\t. = a.m.x\n\t. = a.m.x\n",
        "",
        "f",
    );
    assert_has(
        &code,
        &[
            "CallStatement cache = arg(0); cache = cache[\"m\"]; dynamic_proc(\"g\") 0",
            "Pop",
            "CallStatement dynamic_proc(\"g\") 0",
            "Pop",
            "GetVar cache[\"x\"]",
            "SetVar dot",
            "GetVar cache = arg(0); cache = cache[\"m\"]; cache[\"x\"]",
        ],
    );
}

#[test]
fn safe_src_is_not_an_explicit_src_chain() {
    // `src?.x` leaves nothing for the next chain to treat as written with `src.`
    let code = compile(
        "/mob/var/mob/m\n/mob/var/x\n/mob/var/y\n/mob/var/list/t\n/mob/proc/f()\n\tif(src?.t)\n\t\treturn 0\n\tvar/g = m.x\n\treturn m.y\n",
        "/mob",
        "f",
    );
    assert_has(
        &code,
        &[
            "GetVar cache = src; cache = cache[\"m\"]; cache[\"x\"]",
            "SetVar local(0)",
            "GetVar cache[\"y\"]",
        ],
    );
}

#[test]
fn a_logical_operator_folded_to_its_first_value_forgets_the_cache() {
    let code = |value: &str| {
        compile(
            &format!("/datum/var/datum/m\n/datum/var/x\n/datum/var/y\n/proc/f(datum/a, b)\n\tif(a.m.x)\n\t\tvar/c = ({value})\n\t\treturn a.m.y\n"),
            "",
            "f",
        )
    };
    // A plain value: the cache has a.m, and a.m.y is read from there
    for kept in ["5", "\"s\"", "b"] {
        assert_has(&code(kept), &["GetVar cache[\"y\"]", "Ret"]);
    }
    // A `&&` or `||`, and one that's folded away to its first value too: the
    // cache is set again
    for forgotten in [
        "1 || b",
        "\"s\" || b",
        "\"\" && b",
        "null && b",
        "/datum || 1",
    ] {
        assert_has(
            &code(forgotten),
            &[
                "GetVar cache = arg(0); cache = cache[\"m\"]; cache[\"y\"]",
                "Ret",
            ],
        );
    }
}

/// DreamMaker drops an empty initializer before a semicolon, so
/// for (; a < 5; a++) initializes with a < 5 and tests a++.
#[test]
fn a_for_with_an_empty_first_part_is_read_past_it() {
    let code = |head: &str| {
        compile(
            &format!("/proc/f(a, b)\n\tfor ({head})\n\t\tb++\n\treturn b\n"),
            "",
            "f",
        )
    };
    let shifted = code("; a < 5; a++");
    assert_has(&shifted, &["GetVar arg(0)", "PushInt 5", "Tl", "Pop"]);
    assert_has(&shifted, &["PostInc arg(0)", "Test", "Jz"]);
    // Comma separators preserve the empty initializer.
    let plain = code(", a < 5, a++");
    assert_has(&plain, &["GetVar arg(0)", "PushInt 5", "Tl", "Test", "Jz"]);
    assert!(plain.contains("\nInc arg(0)\nJmpLoop"), "{plain}");
}

/// `global.name` and `::name` of a const fold as the name alone does.
#[test]
fn a_global_const_named_through_global_folds() {
    let code = |body: &str| {
        compile(
            &format!(
                "var/const/gc = 7\n/datum/d\n\tvar/const/gc = 9\n/datum/d/proc/f(a)\n\t{body}\n"
            ),
            "/datum/d",
            "f",
        )
    };
    assert_has(&code("return global.gc + 1"), &["PushInt 8", "Ret"]);
    assert_has(&code("return ::gc + 1"), &["PushInt 8", "Ret"]);
    assert_has(&code("return gc + 1"), &["PushInt 10", "Ret"]);
    assert_has(&code("return global.gc ? a : 2"), &["GetVar arg(0)", "Ret"]);
    // alone it's read from the global
    assert_has(&code("return global.gc"), &["GetVar global(\"gc\")", "Ret"]);
    assert!(code("switch (a)\n\t\tif (global.gc)\n\t\t\treturn 1").contains("Switch default => "));
}

/// `new` for a var with `list` anywhere in its type makes a list.
#[test]
fn an_implicit_new_for_a_list_of_a_type() {
    let code = compile(
        "/datum/d/var/datum/d/list/dl\n/proc/f(datum/d/o)\n\tvar/datum/d/list/v = new\n\tvar/datum/d/list/w = new(3)\n\to.dl = new\n\treturn v\n",
        "",
        "f",
    );
    assert_has(&code, &["PushVal /list", "New 0", "SetVar local(0)"]);
    assert_has(
        &code,
        &["PushVal /list", "PushInt 3", "New 1", "SetVar local(1)"],
    );
    assert_has(
        &code,
        &[
            "PushVal /list",
            "New 0",
            "SetVar cache = arg(0); cache[\"dl\"]",
        ],
    );
}

/// Statement-specific builtin forms.
#[test]
fn rarer_forms_of_builtins_on_their_own_line() {
    let code = |body: &str| {
        compile(
            &format!("/proc/f(a, b, c)\n\t{body}\n\treturn a\n"),
            "",
            "f",
        )
    };
    // statpanel(Panel, Value) has no name
    assert_has(
        &code("statpanel(a, b)"),
        &[
            "GetVar arg(0)",
            "PushVal null",
            "GetVar arg(1)",
            "StatPanel",
            "GetVar arg(0)",
            "Ret",
        ],
    );
    assert_has(
        &code("statpanel(a, b, c)"),
        &[
            "GetVar arg(0)",
            "GetVar arg(1)",
            "GetVar arg(2)",
            "StatPanel",
            "GetVar arg(0)",
            "Ret",
        ],
    );
    // outside `<<` each argument is worked out and dropped
    assert_has(
        &code("output(a, b)"),
        &[
            "GetVar arg(0)",
            "Pop",
            "GetVar arg(1)",
            "Pop",
            "GetVar arg(0)",
            "Ret",
        ],
    );
    // anything after animate()'s object that isn't named stops the animation
    assert_has(
        &code("animate(a, b, c)"),
        &[
            "GetVar arg(0)",
            "NullAnimate",
            "Pop",
            "GetVar arg(0)",
            "Ret",
        ],
    );
}

/// Standalone variable reads emit no code. Flag-only tests need no Pop.
#[test]
fn a_line_that_does_nothing() {
    let code = |body: &str| {
        compile(
            &format!("/datum/var/x\n/proc/f(datum/a, b)\n\t{body}\n\treturn b\n"),
            "",
            "f",
        )
    };
    for nothing in ["a", "(a)", "a.x", "usr", "null"] {
        assert_eq!(code(nothing).trim(), "GetVar arg(1)\nRet\nEnd", "{nothing}");
    }
    assert_has(
        &code("isarea(a)"),
        &["GetVar arg(0)", "IsArea", "GetVar arg(1)", "Ret"],
    );
    // a comparison keeps its drops
    assert_has(
        &code("a == b"),
        &["Teq", "Pop", "GetFlag", "Pop", "GetVar arg(1)", "Ret"],
    );
    // and a call is still made
    assert_has(
        &code("isnull(a)"),
        &["GetVar arg(0)", "IsNull", "Pop", "GetVar arg(1)", "Ret"],
    );
}

/// `x * x` is a square however x is written.
#[test]
fn a_variable_times_itself_is_squared() {
    let code = |expr: &str| compile(&format!("var/g\n/proc/f(a)\n\treturn {expr}\n"), "", "f");
    assert_has(&code("a * (a)"), &["GetVar arg(0)", "PowSquare", "Ret"]);
    assert_has(
        &code("g * global.g"),
        &["GetVar global(\"g\")", "PowSquare", "Ret"],
    );
    assert_has(
        &code("global.g * g"),
        &["GetVar global(\"g\")", "PowSquare", "Ret"],
    );
    // (a local of the name isn't the global)
    let shadowed = compile(
        "var/g\n/proc/f(a)\n\tvar/g = a\n\treturn g * global.g\n",
        "",
        "f",
    );
    assert!(!shadowed.contains("PowSquare"), "{}", shadowed);
}

/// Zero-weight pick entries are omitted from the switch; -0 cases equal 0.
#[test]
fn a_pick_of_weight_0_and_a_case_of_minus_0() {
    let code = compile(
        "/proc/f(a, b)\n\treturn pick(1; a, 0; b, 2; \"c\")\n",
        "",
        "f",
    );
    // one case (a's third of the draw) and the default, with b's value still written
    let table = code.lines().find(|l| l.starts_with("PickSwitch")).unwrap();
    assert_eq!(table.matches("=>").count(), 2, "{}", table);
    assert!(table.contains("21845 =>"), "{}", table);
    assert!(code.contains("GetVar arg(1)"), "{}", code);
    // A final zero-weight entry would jump outside the proc in DreamMaker.
    // Emit a compatibility warning and use a valid weighted entry.
    let source = "/proc/f(a, b)\n\treturn pick(1; a, 0; b)\n";
    let code = compile(source, "", "f");
    let table = code.lines().find(|l| l.starts_with("PickSwitch")).unwrap();
    assert_eq!(table.matches("=>").count(), 1, "{}", table);
    let target = table
        .split("default => ")
        .nth(1)
        .unwrap()
        .trim_end_matches([',', ' ']);
    assert_has(&code, &[&format!("{target}:"), "GetVar arg(0)"]);
    let warnings = warnings_for(source, dmasm::ByondVersion::LATEST);
    assert!(
        warnings.iter().any(|w| w.contains("zero-weight entry")),
        "{:?}",
        warnings
    );
    let code = compile(
        "/proc/f(a)\n\tswitch (a)\n\t\tif (-0)\n\t\t\treturn 1\n\treturn 2\n",
        "",
        "f",
    );
    let table = code.lines().find(|l| l.starts_with("Switch")).unwrap();
    assert!(
        table.contains(" 0 =>") && !table.contains("-0"),
        "{}",
        table
    );
}

/// A parameter's `x = a in L` has the default a.
#[test]
fn a_parameter_default_stops_at_in() {
    let code = compile(
        "/proc/f(a, b = a in list(1), c = (a in list(1)))\n\treturn b\n",
        "",
        "f",
    );
    assert_has(&code, &["GetVar arg(0)", "SetVar arg(1)"]);
    // in parentheses it's the test
    assert!(code.contains("IsIn"), "{code}");
}

/// Compound assignments used as values invalidate cached field chains.
#[test]
fn a_chain_through_fields_isnt_reused_after_a_change_as_a_value() {
    let code = |body: &str| {
        compile(
            &format!("/datum/var/datum/m\n/datum/var/x\n/proc/f(datum/a, b)\n\t{body}\n"),
            "",
            "f",
        )
    };
    let whole = "cache = arg(0); cache = cache[\"m\"]; cache[\"x\"]";
    assert_has(
        &code("return a.m.x++ + a.m.x"),
        &[
            &format!("PostInc {whole}"),
            &format!("GetVar {whole}"),
            "Add",
        ],
    );
    assert_has(
        &code("return (a.m.x += b) + a.m.x"),
        &["PushEval", &format!("GetVar {whole}"), "Add"],
    );
    assert_has(
        &code("return (a.m.x ||= b)"),
        &[
            &format!("GetVar {whole}"),
            "JmpOr LAB_0002",
            "GetVar arg(1)",
            &format!("SetVarExpr {whole}"),
        ],
    );
    // a plain assignment as a value does reuse it
    assert_has(
        &code("return a.m.x + (a.m.x = b)"),
        &["GetVar arg(1)", "SetVarExpr cache[\"x\"]", "Add"],
    );
}

/// DreamMaker writes the field of `x.f ||= v` the way it read it for the test.
#[test]
fn a_logical_assignment_writes_its_field_as_it_read_it() {
    let code = |body: &str| {
        compile(
            &format!("/datum/d\n\tvar/x\n\tvar/y\n/datum/d/proc/f(b)\n\t{body}\n"),
            "/datum/d",
            "f",
        )
    };
    // The cache has src from the statement before: read from it, written to it
    assert_has(
        &code("src.y = 1\n\tsrc.x &&= b"),
        &[
            "GetVar cache[\"x\"]",
            "Test",
            "Jz LAB_0001",
            "GetVar arg(0)",
            "SetVar cache[\"x\"]",
        ],
    );
    // A bare `y` loaded it, which `src.x` written out doesn't read from, or
    // write to
    assert_has(
        &code("y = 1\n\tsrc.x &&= b"),
        &[
            "GetVar cache = src; cache[\"x\"]",
            "Test",
            "Jz LAB_0001",
            "GetVar arg(0)",
            "SetVar cache = src; cache[\"x\"]",
        ],
    );
    assert_has(
        &code("src.y = 1\n\tx ||= b"),
        &[
            "GetVar cache = src; cache[\"x\"]",
            "Test",
            "Jnz LAB_0001",
            "GetVar arg(0)",
            "SetVar cache = src; cache[\"x\"]",
        ],
    );
}

#[test]
fn dropped_constant_forgets_cache() {
    // `0 || a.m` is `a.m`, but DreamMaker forgets what the cache held
    let code = compile(
        "/datum/var/datum/m\n/datum/var/x\n/datum/var/y\n/proc/f(datum/a)\n\tif(a.m.x)\n\t\treturn (0 || a.m.y)\n",
        "",
        "f",
    );
    assert_has(
        &code,
        &[
            "GetVar cache = arg(0); cache = cache[\"m\"]; cache[\"y\"]",
            "Ret",
        ],
    );
}

#[test]
fn datum_loop_in_world_goes_over_atoms() {
    // `in world` is the world's atoms, checked for the type; without it, all datums
    let code = compile(
        "/datum/foo\n/proc/f()\n\tfor (var/datum/foo/x in world)\n\t\tworld.log << x\n\tfor (var/datum/foo/y)\n\t\tworld.log << y\n",
        "",
        "f",
    );
    assert_has(&code, &["GetVar world", "IterLoad 5 ()"]);
    assert_has(
        &code,
        &["PushVal /datum/foo", "IterLoad 5 (datum_instances | )"],
    );
}

#[test]
fn text_squared_is_pow_square() {
    // DreamMaker squares text, null and paths too, but not embedded text
    let code = compile(
        "/proc/f(x)\n\t. = \"a\" * \"a\"\n\treturn \"[x]\" * \"[x]\"\n",
        "",
        "f",
    );
    assert_has(&code, &["PushVal \"a\"", "PowSquare"]);
    assert_has(&code, &["Format \"[]\" 1", "Mul"]);
}

#[test]
fn bit_ops_fold_like_dreammaker() {
    // ~ takes a u32 (negative is 0, over 2**32 is all ones); & | ^ << >> clamp to i32
    let code = compile(
        "/proc/f()\n\t. = ~(65536**3)\n\t. = ~(2**31+256)\n\t. = ~(-1)\n\t. = (2**33) & 16777215\n\t. = (-5) | 0\n\treturn (2**31+256) >> 8\n",
        "",
        "f",
    );
    assert_has(
        &code,
        &[
            "PushInt 0",
            "SetVar dot",
            "PushVal 16776959",
            "SetVar dot",
            "PushVal 16777215",
            "SetVar dot",
            "PushVal 16777215",
            "SetVar dot",
            "PushVal 16777211",
            "SetVar dot",
            "PushInt 65535",
        ],
    );
}

#[test]
fn cmptext_with_more_than_two() {
    let code = compile(
        "/proc/f()\n\treturn cmptextEx(\"a\", \"A\", \"b\")\n",
        "",
        "f",
    );
    assert_has(
        &code,
        &[
            "PushVal \"a\"",
            "PushVal \"A\"",
            "Teq",
            "Jz LAB_0001",
            "PushVal \"b\"",
            "Teq",
            "LAB_0001:",
            "Pop",
            "GetFlag",
        ],
    );
    let code = compile("/proc/f(a)\n\treturn cmptext(a, \"A\", \"b\")\n", "", "f");
    assert_has(
        &code,
        &["CmpText", "Jz LAB_0001", "PushVal \"b\"", "CmpText"],
    );
}

#[test]
fn call_ext_of_a_loaded_function() {
    let src = "/proc/f()\n\tvar/h = load_ext(\"lib.so\", \"func\")\n\treturn call_ext(h)(1, 2)\n";
    let code = compile(src, "", "f");
    assert_has(
        &code,
        &["PushVal \"lib.so\"", "PushVal \"func\"", "LoadExt"],
    );
    assert_has(
        &code,
        &[
            "GetVar local(0)",
            "PushInt 1",
            "PushInt 2",
            "CallExtLoaded 2",
        ],
    );
}

#[test]
fn type_macro_in_a_global_proc_is_null() {
    let code = compile("/proc/f()\n\treturn __TYPE__\n", "", "f");
    assert_has(&code, &["PushVal null", "Ret"]);
}

#[test]
fn missile_leaves_nothing() {
    let code = compile("/proc/f()\n\tvar/atom/a\n\tmissile(/obj, a, a)\n", "", "f");
    assert_has(&code, &["GetVar local(0)", "Missile", "End"]);
}

#[test]
fn proc_argument_keys_worked_out() {
    let src = "/proc/g(...)\n/proc/f()\n\tvar/a = 0\n\treturn g(a + 1 = 2, 7)\n";
    let code = compile(src, "", "f");
    assert_has(
        &code,
        &[
            "Add",
            "PushInt 2",
            "PushInt 2",
            "PushInt 7",
            "NewAssocList 2",
            "CallGlobalArgList /proc/g",
        ],
    );
    // A safe access is an assignment, passed as it is
    let src =
        "/datum/t\n\tvar/b\n/proc/g(...)\n/proc/f()\n\tvar/datum/t/a = new\n\treturn g(a?.b = 1)\n";
    let code = compile(src, "", "f");
    assert_has(
        &code,
        &["SetVarExpr cache[\"b\"]", "LAB_0004:", "CallGlob 1 /proc/g"],
    );
}

#[test]
fn switch_with_no_cases() {
    let code = compile("/proc/f(a)\n\tswitch(a)\n\treturn 1\n", "", "f");
    assert_has(
        &code,
        &[
            "GetVar arg(0)",
            "Switch default => LAB_0001,",
            "LAB_0001:",
            "PushInt 1",
            "Ret",
        ],
    );
}

#[test]
fn parenthesised_assignment_is_a_list_key() {
    // Parentheses preserve key/value syntax in list constructors,
    // but make an assignment expression in proc calls.
    let code = compile("/proc/f(a, b, c)\n\treturn list(a, (b = 1), c)\n", "", "f");
    assert_has(
        &code,
        &[
            "PushInt 1",
            "GetVar arg(0)",
            "GetVar arg(1)",
            "PushInt 1",
            "PushInt 3",
            "GetVar arg(2)",
            "NewAssocList 3",
        ],
    );
    let code = compile("/proc/f(a, b, c)\n\treturn list((\"x\" = 1))\n", "", "f");
    assert_has(&code, &["PushVal \"x\"", "PushInt 1", "NewAssocList 1"]);
    let code = compile(
        "/proc/g(a, b)\n\treturn b\n/proc/f(a, b, c)\n\treturn g(a, (b = 1))\n",
        "",
        "f",
    );
    assert_has(
        &code,
        &[
            "GetVar arg(0)",
            "PushInt 1",
            "SetVarExpr arg(1)",
            "CallGlob 2 /proc/g",
        ],
    );
}

#[test]
fn writing_through_a_pointer() {
    let code = compile("/proc/f(a)\n\tvar/p = &a\n\t*p = 2\n\treturn a\n", "", "f");
    assert_has(&code, &["PushInt 2", "SetVar *local(0)", "GetVar arg(0)"]);
    let code = compile("/proc/f(a)\n\tvar/p = &a\n\t*p += 2\n\treturn a\n", "", "f");
    assert_has(&code, &["PushInt 2", "AugAdd *local(0)", "GetVar arg(0)"]);
    // as a value, the value is copied first
    let code = compile("/proc/f(a)\n\tvar/p = &a\n\treturn (*p = 3)\n", "", "f");
    assert_has(&code, &["PushInt 3", "PushTop", "SetVar *local(0)", "Ret"]);
    let code = compile(
        "/proc/f(a)\n\tvar/p = &a\n\t(*p)++\n\t(*p) = 6\n\treturn a\n",
        "",
        "f",
    );
    assert_has(&code, &["Inc *local(0)", "PushInt 6", "SetVar *local(0)"]);
}

#[test]
fn pointer_to_a_field() {
    // the & goes on the field, after finding its holder
    let src =
        "/datum/t\n\tvar/v = 1\n/proc/f()\n\tvar/datum/t/T = new\n\tvar/p = &T.v\n\treturn *p\n";
    assert_has(
        &compile(src, "", "f"),
        &["GetVar cache = local(0); &cache[\"v\"]", "SetVar local(1)"],
    );
    let src = "/datum/t\n\tvar/v = 1\n\tproc/m()\n\t\tvar/p = &src.v\n\t\treturn *p\n";
    assert_has(
        &compile(src, "/datum/t", "m"),
        &["GetVar cache = src; &cache[\"v\"]", "SetVar local(0)"],
    );
    // `&(T.v)` is the same; `*(&T.v)` reads the pointer it makes
    let src = "/datum/t\n\tvar/v = 1\n/proc/f()\n\tvar/datum/t/T = new\n\tvar/p = &(T.v)\n\treturn *(&T.v)\n";
    assert_has(
        &compile(src, "", "f"),
        &[
            "GetVar cache = local(0); &cache[\"v\"]",
            "SetVar local(1)",
            "GetVar &cache[\"v\"]",
            "SetVar cache_key",
            "GetVar *cache_key",
        ],
    );
    assert_has(
        &compile("/proc/f(a)\n\tvar/p = &a\n\treturn *(p)\n", "", "f"),
        &["GetVar *local(0)", "Ret"],
    );
}

#[test]
fn new_without_arguments_is_true_in_a_test() {
    // Constant constructors can be folded away in conditions.
    let src = "/datum/t\n/proc/f(a, b)\n\treturn (new /datum/t ? a : b)\n";
    assert_eq!(compile(src, "", "f"), "GetVar arg(0)\nRet\nEnd");
    let src = "/datum/t\n/proc/f(a, b)\n\treturn !(new /datum/t()) || (new /datum/t && a)\n";
    assert_eq!(compile(src, "", "f"), "GetVar arg(0)\nRet\nEnd");
    // Value contexts and nonconstant arguments still allocate.
    let src = "/datum/t\n/proc/f(a, b)\n\treturn (new /datum/t || a)\n";
    assert_has(
        &compile(src, "", "f"),
        &["PushVal /datum/t", "New 0", "Ret"],
    );
    let src = "/datum/t\n/proc/f(a, b)\n\treturn (new /datum/t(a) ? a : b)\n";
    assert_has(
        &compile(src, "", "f"),
        &["PushVal /datum/t", "GetVar arg(0)", "New 1", "Test"],
    );
}

#[test]
fn writing_a_static_var_through_safe_access() {
    let src = "/datum/t\n\tvar/static/s = 2\n\tvar/datum/t/n\n/proc/f(datum/t/a, b)\n\ta?.s = b\n\ta?.n.s += b\n\tb = a?.s++\n";
    assert_has(
        &compile(src, "", "f"),
        &[
            "GetVar arg(0)",
            "SetCachePopJmpIfNull LAB_0001",
            "PushCache",
            "GetVar arg(1)",
            "PopCache",
            "SetVar global(\"s\")",
            "LAB_0001:",
            "GetVar arg(0)",
            "SetCachePopJmpIfNull LAB_0004",
            "PushCache",
            "GetVar arg(1)",
            "PopCache",
            "AugAdd global(\"s\")",
            "LAB_0004:",
            "GetVar arg(0)",
            "SetCacheJmpIfNull LAB_0007",
            "PushCache",
            "PopCache",
            "PostInc global(\"s\")",
        ],
    );
}

#[test]
fn logical_assignment_through_safe_access() {
    let src =
        "/datum/t\n\tvar/v\n\tvar/datum/t/n\n/proc/f(datum/t/a, b)\n\ta?.n?.v ||= b\n\treturn a\n";
    assert_eq!(
        compile(src, "", "f"),
        [
            "GetVar arg(0)",
            "SetCachePopJmpIfNull LAB_0001",
            "PushCache",
            "GetVar cache[\"n\"]",
            "SetCachePopJmpIfNull LAB_0002",
            "PushCache",
            "PopCache",
            "GetVar cache[\"v\"]",
            "Test",
            "Jnz LAB_0002",
            "GetVar arg(1)",
            "SetVar cache[\"v\"]",
            "LAB_0002:",
            "PopCache",
            "LAB_0001:",
            "GetVar arg(0)",
            "Ret",
            "End",
        ]
        .join("\n")
    );
    // a value that uses the cache keeps the place around it
    let src = "/datum/t\n\tvar/v\n\tvar/datum/t/n\n/proc/f(datum/t/a, b)\n\ta?.v &&= a.n\n";
    assert_has(
        &compile(src, "", "f"),
        &[
            "Jz LAB_0001",
            "PushCache",
            "GetVar cache = arg(0); cache[\"n\"]",
            "PopCache",
            "SetVar cache[\"v\"]",
        ],
    );
}

#[test]
fn or_assign_saves_the_cache_for_some_values() {
    let types =
        "var/gx = 1\n/datum/t\n\tvar/n = 1\n\tvar/m = 2\n\tvar/static/st = 3\n\tvar/const/k = 4\n";
    let value = |v: &str| {
        compile(
            &format!("{types}/proc/f(datum/t/o, list/L)\n\to.n ||= {v}\n"),
            "",
            "f",
        )
    };
    // Static/const reads through . and global.x do not require saving the cache.
    for v in ["o.st", "o.k", "global.gx", "gx"] {
        assert!(!value(v).contains("PushCache"), "{v}: {}", value(v));
    }
    // Fields, list indexes, global calls and :: can require a cache save.
    for v in ["o.m", "L[1]", "/datum/t::st", "o::st"] {
        assert!(value(v).contains("PushCache"), "{v}: {}", value(v));
    }
}

#[test]
fn safe_access_to_a_const_is_not_folded() {
    // `a?.k` is null when a is, so it isn't the constant
    let src =
        "/datum/t\n\tvar/const/k = 256\n/proc/f(datum/t/a)\n\tif (a?.k) return 1\n\treturn 2\n";
    assert_has(
        &compile(src, "", "f"),
        &[
            "GetVar arg(0)",
            "SetCacheJmpIfNull LAB_0003",
            "GetVar global(\"k\")",
            "LAB_0003:",
            "Test",
        ],
    );
}

#[test]
fn hex_escapes_are_characters() {
    // `\xe9` is é, stored as UTF-8
    let code = compile("/proc/f()\n\treturn \"\\xe9t\\x41\"\n", "", "f");
    assert_has(&code, &["PushVal \"étA\"", "Ret"]);
}

#[test]
fn hashes_of_text_fold() {
    let code = compile("/proc/f()\n\treturn md5(\"x\")\n", "", "f");
    assert_has(
        &code,
        &["PushVal \"9dd4e461268c8034f5c8564e155c67a6\"", "Ret"],
    );
    let code = compile(
        "/proc/f()\n\treturn sha1(\"k\" + \"UPPER lower\")\n",
        "",
        "f",
    );
    assert_has(
        &code,
        &[
            "PushVal \"7806b65f635265368f10c2abc91db9eae1753a54\"",
            "Ret",
        ],
    );
    // the text as UTF-8
    let code = compile("/proc/f()\n\treturn md5(\"\\xff\")\n", "", "f");
    assert_has(
        &code,
        &["PushVal \"f3f7437e8c1f303fc6742d9368b36cf6\"", "Ret"],
    );
    // not of a number
    let code = compile("/proc/f()\n\treturn md5(1)\n", "", "f");
    assert_has(&code, &["PushInt 1", "Md5"]);
}

#[test]
fn assert_gives_numbers_as_written() {
    let code = compile("/proc/f(n)\n\tASSERT(n > 1e3 + 0.50)\n", "", "f");
    assert!(code.contains("PushVal \"n > 1e3 + 0.50\""), "{code}");
}

#[test]
fn folded_var_defaults_have_their_escapes() {
    let src = "/datum/t\n\tvar/v = \"q\\\"q\"\n\tvar/const/k = \"a\\tb\"\n/proc/f()\n\treturn (\"a\" + /datum/t::v) + /datum/t::k\n";
    let code = compile(src, "", "f");
    assert_has(&code, &["PushVal \"aq\\\"qa\tb\"", "Ret"]);
}

#[test]
fn constant_lists_are_true_in_a_test() {
    // Constant list/alist/newlist/new constructors are truthy without allocation.
    for v in [
        "list(1, \"k\" = 2)",
        "list()",
        "new /list(2)",
        "newlist(/datum)",
        "list(k = new /datum)",
    ] {
        let code = compile(&format!("/proc/f(a, b)\n\treturn ({v} ? a : b)\n"), "", "f");
        assert_eq!(code, "GetVar arg(0)\nRet\nEnd", "{v}");
    }
    // Nonconstant constructors still run before testing.
    let code = compile("/proc/f(a, b)\n\treturn (list(a) ? a : b)\n", "", "f");
    assert_has(&code, &["GetVar arg(0)", "NewList 1", "Test"]);
}

#[test]
fn a_const_chosen_by_a_constant_test_is_read_alone() {
    // `7 && K` is K, read from its global, but `1 && 2` is 2
    let types = "/datum/t\n\tvar/const/k = 1000\n";
    for v in [
        "(7 && /datum/t::k)",
        "(/datum/t::k || \"abc\")",
        "(1 ? /datum/t::k : 2)",
    ] {
        let code = compile(&format!("{types}/proc/f()\n\treturn {v}\n"), "", "f");
        assert_eq!(code, "GetVar global(\"k\")\nRet\nEnd", "{v}");
    }
    let code = compile(
        &format!("{types}/proc/f()\n\treturn (0 || /datum/t::k + 1)\n"),
        "",
        "f",
    );
    assert_eq!(code, "PushInt 1001\nRet\nEnd");
}

#[test]
fn goto_in_a_try_block_uses_try_jmp() {
    let src = "/proc/f(a)\n\ttry\n\t\tif (a) goto l1\n\t\ta = 2\n\t\tl1:\n\t\ta += 1\n\tcatch\n\t\tif (a) goto l2\n\t\tl2:\n\treturn a\n";
    let code = compile(src, "", "f");
    assert!(code.contains("TryJmp USER_l1"), "{code}");
    assert!(code.contains("JmpLoop USER_l2"), "{code}");
}

#[test]
fn type_and_proc_macros_fold_in_a_test() {
    let code = compile(
        "/datum/t/proc/f(a, b)\n\treturn (__TYPE__ ? a : b)\n",
        "/datum/t",
        "f",
    );
    assert_eq!(code, "GetVar arg(0)\nRet\nEnd");
    let code = compile("/proc/f(a, b)\n\treturn (__TYPE__ ? a : b)\n", "", "f");
    assert_eq!(code, "GetVar arg(1)\nRet\nEnd");
    let code = compile("/proc/f(a, b)\n\treturn (__PROC__ ? a : b)\n", "", "f");
    assert_eq!(code, "GetVar arg(0)\nRet\nEnd");
}

#[test]
fn call_arguments_are_a_procs() {
    // `(n = 1)` is an assignment passed in its place
    let code = compile(
        "/proc/f(o, n)\n\treturn call(o, \"f\")((n = 1), 2)\n",
        "",
        "f",
    );
    assert_has(
        &code,
        &[
            "PushVal \"f\"",
            "PushInt 1",
            "SetVarExpr arg(1)",
            "PushInt 2",
            "CallName 2",
        ],
    );
    // (with a named argument, as in any call, it's n's value as a key)
    let code = compile(
        "/proc/f(o, n)\n\treturn call(o, \"f\")((n = 1), x = 2)\n",
        "",
        "f",
    );
    assert_has(
        &code,
        &[
            "GetVar arg(1)",
            "PushInt 1",
            "PushVal \"x\"",
            "PushInt 2",
            "NewAssocList 2",
            "CallNameArgList",
        ],
    );
}

#[test]
fn a_false_else_if_leaves_its_jump() {
    let code = compile(
        "/proc/f(a, b)\n\tif (a) b = 1\n\telse if (0) b = 2\n\treturn b\n",
        "",
        "f",
    );
    assert_has(&code, &["SetVar arg(1)", "Jmp LAB_0000", "LAB_0001:"]);
    // not when it comes before a live arm
    let code = compile(
        "/proc/f(a, b)\n\tif (0) b = 1\n\telse if (a) b = 2\n\treturn b\n",
        "",
        "f",
    );
    assert!(!code.contains("Jmp "), "{code}");
}

#[test]
fn or_assign_goes_by_the_source_of_a_folded_value() {
    // `0 && o.m` folds to 0, but reads a field in the source
    let types = "/datum/t\n\tvar/n = 1\n\tvar/m = 2\n\tvar/static/st = 3\n";
    let code = compile(
        &format!("{types}/proc/f(datum/t/o)\n\to.n &&= (0 && o.m)\n"),
        "",
        "f",
    );
    assert_has(&code, &["PushCache", "PushInt 0", "PopCache"]);
    // a static read isn't one
    let code = compile(
        &format!("{types}/proc/f(datum/t/o)\n\to.n &&= (0 && o.st)\n"),
        "",
        "f",
    );
    assert!(!code.contains("PushCache"), "{code}");
}

#[test]
fn a_dropped_if_forgets_the_cache() {
    let src = "/datum/t\n\tvar/list/l = list(1)\n/proc/f(datum/t/o, a)\n\to.l.Remove(a)\n\tif (0)\n\t\ta = 2\n\treturn o.l.len\n";
    assert_has(
        &compile(src, "", "f"),
        &[
            "Pop",
            "GetVar cache = arg(0); cache = cache[\"l\"]; cache[\"len\"]",
        ],
    );
}

#[test]
fn folding_and_or_forgets_the_cache() {
    let types = "/datum/t\n\tvar/a = 1\n\tvar/list/l\n";
    let code = compile(
        &format!("{types}/proc/f(datum/t/p, n)\n\tp.l = list()\n\tn = (1 || 2)\n\treturn p.a\n"),
        "",
        "f",
    );
    assert_has(
        &code,
        &["SetVar arg(1)", "GetVar cache = arg(0); cache[\"a\"]"],
    );
    let code = compile(
        &format!("{types}/proc/f(datum/t/p, n)\n\tp.l = list()\n\tn = (1 ? 2 : 3)\n\treturn p.a\n"),
        "",
        "f",
    );
    assert_has(&code, &["SetVar arg(1)", "GetVar cache[\"a\"]"]);
}

#[test]
fn an_if_decided_when_compiled_forgets_the_cache_after_it() {
    let src = "/datum/t\n\tvar/a = 1\n\tvar/b = 2\n/proc/f(datum/t/p, n)\n\tif (1) n = p.a\n\treturn p.b\n";
    assert_has(
        &compile(src, "", "f"),
        &["SetVar arg(1)", "GetVar cache = arg(0); cache[\"b\"]"],
    );
}

#[test]
fn initial_of_a_static_or_const_is_its_globals() {
    let types = "/datum/t\n\tvar/n = 1\n\tvar/static/s = 2\n\tvar/const/k = 3\n\tvar/datum/t/o\n";
    let code = compile(
        &format!("{types}/proc/f(datum/t/a)\n\treturn initial(a.o.s) + initial(a.k)\n"),
        "",
        "f",
    );
    assert_has(
        &code,
        &[
            "GetVar initial(global(\"s\"))",
            "GetVar initial(global(\"k\"))",
            "Add",
        ],
    );
    // through ?., later ones are saved around
    let code = compile(
        &format!("{types}/proc/f(datum/t/a)\n\treturn initial(a?.o?.n)\n"),
        "",
        "f",
    );
    assert_has(
        &code,
        &[
            "GetVar arg(0)",
            "SetVar cache",
            "PushCache",
            "GetVar cache[\"o\"]",
            "SetVar cache",
            "PopCache",
            "GetVar initial(cache[\"n\"])",
        ],
    );
}

/// Compile src proc f with debug info; map external files to other.dm.
fn compile_with_lines(src: &str) -> Vec<String> {
    let ctx = dreammaker::Context::default();
    let pp = dreammaker::Preprocessor::from_buffer(&ctx, "test.dm".into(), src.to_owned());
    let mut parser = dreammaker::Parser::new(&ctx, pp);
    parser.enable_procs();
    let tree = parser.parse_object_tree();
    let value = tree
        .root()
        .get()
        .procs
        .get("f")
        .unwrap()
        .value
        .last()
        .unwrap();
    let source = ProcSource {
        tree: &tree,
        ty: tree.root(),
        name: "f",
        value,
        file: "test.dm",
        debug_info: true,
        index: None,
        byond: dmasm::ByondVersion::LATEST,
        file_names: Some(&|_| "other.dm".to_owned()),
    };
    let code = dmasm::format(&compile_proc(&source).unwrap().nodes);
    code.lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with("LAB_"))
        .map(str::to_owned)
        .collect()
}

#[test]
fn a_type_scope_default_records_where_it_is_written() {
    // (same file: its line, and the statement's line still counts)
    let code =
        compile_with_lines("/datum/u\n\tvar/x = 3\n/proc/f(a)\n\ta = /datum/u::x\n\treturn a\n");
    assert_eq!(
        code,
        [
            "DbgFile \"test.dm\"",
            "DbgLine 4",
            "DbgLine 2",
            "PushInt 3",
            "SetVar arg(0)",
            "DbgLine 5",
            "GetVar arg(0)",
            "Ret",
            "End"
        ]
    );
}

#[test]
fn only_compound_terms_record_a_later_line() {
    // Only compound terms record a new line after an embedded default;
    // variables, fields and literals do not.
    let code = compile_with_lines("/proc/g()\n/proc/f(datum/a)\n\ta = list(1,\n\t\ta,\n\t\ta.type,\n\t\t-1,\n\t\t1 + 2,\n\t\t\"[a]\",\n\t\tg())\n");
    let lines: Vec<&str> = code
        .iter()
        .map(String::as_str)
        .filter(|l| l.starts_with("DbgLine"))
        .collect();
    assert_eq!(lines, ["DbgLine 3", "DbgLine 7", "DbgLine 8", "DbgLine 9"]);
}

#[test]
fn after_a_default_the_next_compound_term_records_its_line() {
    // Record the proc's line after a later or external default, even
    // when || can skip the restoring expression.
    let code = compile_with_lines(
        "/proc/f(a)\n\tif (text(\"[]\", /datum/u::x) || \"[a]\")\n\t\ta = 1\n/datum/u\n\tvar/x = 3\n",
    );
    let at = code.iter().position(|l| l.starts_with("JmpOr")).unwrap();
    assert_eq!(
        code[at + 1..at + 3],
        ["DbgLine 2", "GetVar arg(0)"],
        "{code:?}"
    );
    let code = compile_with_lines(
        "/proc/f(a)\n\ta = list(/datum/u::x, a, a.type)\n/datum/u\n\tvar/x = 3\n",
    );
    assert_eq!(
        code.iter().filter(|l| l.starts_with("DbgLine")).count(),
        2,
        "{code:?}"
    );
}

#[test]
fn locate_in_a_list_in_another() {
    // `locate(X) in a in b` is `(locate(X) in a) in b`
    let code = compile(
        "/datum/t\n/proc/f(a, b)\n\treturn locate(/datum/t) in a in b\n",
        "",
        "f",
    );
    assert_eq!(
        code,
        "GetVar arg(1)\nPushVal /datum/t\nGetVar arg(0)\nLocateType\nIsIn Value\nGetFlag\nRet\nEnd"
    );
}

#[test]
fn for_loop_separators_can_be_mixed() {
    // DreamMaker takes `for(a; b, c)` and `for(a, b; c)` as `for(a; b; c)` (tgstation has both)
    let body = |head: &str| {
        compile(
            &format!("/proc/f(n)\n\tvar/i\n\tfor({head})\n\t\tworld.log << i\n"),
            "",
            "f",
        )
    };
    let want = body("i = 1; i <= n; i++");
    assert!(want.contains("Inc local(0)"), "{want}");
    assert_eq!(body("i = 1; i <= n, i++"), want);
    assert_eq!(body("i = 1, i <= n; i++"), want);
}

#[test]
fn assert_keeps_raw_strings_as_written() {
    // `#x` writes a raw string the way the source does
    let code = compile(
        "/proc/f(a)\n\tASSERT(a == @{\"brace\"} || a == @'one')\n",
        "",
        "f",
    );
    assert!(
        code.contains(r#"PushVal "a == @{\"brace\"} || a == @'one'""#),
        "{code}"
    );
}

#[test]
fn the_place_of_an_augmented_assignment_records_no_line() {
    // Compound assignment targets suppress their own debug line,
    // unlike plain assignment targets.
    let lines = |code: Vec<String>| {
        code.into_iter()
            .filter(|l| l.starts_with("DbgLine"))
            .collect::<Vec<_>>()
    };
    let src =
        |op: &str| format!("/proc/f(list/L)\n\tL[\"b\"] {op} /datum/u::x\n/datum/u\n\tvar/x = 3\n");
    assert_eq!(
        lines(compile_with_lines(&src("-="))),
        ["DbgLine 2", "DbgLine 4"]
    );
    assert_eq!(
        lines(compile_with_lines(&src("="))),
        ["DbgLine 2", "DbgLine 4", "DbgLine 2"]
    );
}

#[test]
fn a_folded_default_from_another_file_still_names_the_file_again() {
    // A folded external default still requires restoring the proc's
    // file at the next recorded line. The test maps external files
    // to other.dm.
    let ctx = dreammaker::Context::default();
    let mut pp = dreammaker::Preprocessor::from_buffer(
        &ctx,
        "test.dm".into(),
        "/proc/f(a)\n\ta = /datum/u::x + 1\n\ta = 2\n".to_owned(),
    );
    pp.push_file(
        "other.dm".into(),
        std::io::Cursor::new(b"/datum/u\n\tvar/x = 3\n".to_vec()),
    )
    .unwrap();
    let mut parser = dreammaker::Parser::new(&ctx, pp);
    parser.enable_procs();
    let tree = parser.parse_object_tree();
    let value = tree
        .root()
        .get()
        .procs
        .get("f")
        .unwrap()
        .value
        .last()
        .unwrap();
    let source = ProcSource {
        tree: &tree,
        ty: tree.root(),
        name: "f",
        value,
        file: "test.dm",
        debug_info: true,
        index: None,
        byond: dmasm::ByondVersion::LATEST,
        file_names: Some(&|_| "other.dm".to_owned()),
    };
    let code = dmasm::format(&compile_proc(&source).unwrap().nodes);
    let code: Vec<&str> = code
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty())
        .collect();
    assert_eq!(
        code,
        [
            "DbgFile \"test.dm\"",
            "DbgLine 2",
            "PushInt 4",
            "SetVar arg(0)",
            "DbgFile \"test.dm\"",
            "DbgLine 3",
            "PushInt 2",
            "SetVar arg(0)",
            "End"
        ]
    );
}

/// Debug records for two assignments, with `/datum/u::x` defined in another file.
fn records_with_default_from_other_file(expr: &str) -> Vec<String> {
    let ctx = dreammaker::Context::default();
    let mut pp = dreammaker::Preprocessor::from_buffer(
        &ctx,
        "test.dm".into(),
        format!("/proc/f(a)\n\ta = {expr}\n\ta = 2\n"),
    );
    pp.push_file(
        "other.dm".into(),
        std::io::Cursor::new(b"/datum/u\n\tvar/x = -4\n".to_vec()),
    )
    .unwrap();
    let mut parser = dreammaker::Parser::new(&ctx, pp);
    parser.enable_procs();
    let tree = parser.parse_object_tree();
    let value = tree
        .root()
        .get()
        .procs
        .get("f")
        .unwrap()
        .value
        .last()
        .unwrap();
    let source = ProcSource {
        tree: &tree,
        ty: tree.root(),
        name: "f",
        value,
        file: "test.dm",
        debug_info: true,
        index: None,
        byond: dmasm::ByondVersion::LATEST,
        file_names: Some(&|_| "other.dm".to_owned()),
    };
    dmasm::format(&compile_proc(&source).unwrap().nodes)
        .lines()
        .map(str::trim)
        .filter(|l| l.starts_with("Dbg"))
        .map(str::to_owned)
        .collect()
}

/// Folded-away operands must not change the debug file. Live operands
/// referencing defaults in another file must restore the proc's file.
#[test]
fn a_default_from_another_file_in_an_operand_folded_away_names_nothing() {
    let files = |expr: &str| {
        records_with_default_from_other_file(expr)
            .iter()
            .filter(|l| l.starts_with("DbgFile"))
            .count()
    };
    for expr in [
        "(1 ? -1 : /datum/u::x)",
        "(0 ? /datum/u::x : 2)",
        "(1 || /datum/u::x)",
        "(0 && /datum/u::x)",
        "(/datum/u::x && 0)",
        "((~(\"A,b,c\" ? -1 : /datum/u::x)) || 3)",
        "min(1, (1 ? 2 : /datum/u::x))",
    ] {
        assert_eq!(files(expr), 1, "{expr}");
    }
    for expr in [
        "/datum/u::x + 1",
        "(/datum/u::x ? 1 : 2)",
        "(/datum/u::x || 3)",
    ] {
        assert!(files(expr) > 1, "{expr}");
    }
}

/// The compiler folds angles and colors as Linux DreamMaker does and warns
/// where Windows folding differs. At runtime, DreamDaemon uses Linux angle
/// results on both platforms; color results follow the local platform.
#[test]
fn a_fold_that_windows_has_differently_is_warned_about() {
    let latest = dmasm::ByondVersion::LATEST;
    let warnings =
        |expr: &str, byond| warnings_for(&format!("/proc/f(a)\n\treturn {expr}\n"), byond);
    let warned = |expr: &str, what: &str| {
        let w = warnings(expr, latest);
        assert!(w.len() == 1 && w[0].contains(what), "{expr}: {w:?}");
    };
    let quiet = |expr: &str, byond| {
        let w = warnings(expr, byond);
        assert!(w.is_empty(), "{expr} on {byond:?}: {w:?}");
    };
    // Inverse-trig result differs by one f32 rounding step.
    warned(
        "arctan(2)",
        "arctan(2) is folded to 63.43495, matching Linux DreamMaker and runtime evaluation",
    );
    warned("arctan(2)", "Windows DreamMaker folds it to 63.434948");
    warned("arccos(0.7)", "Windows DreamMaker folds it to 45.572998");
    warned("arctan(1, 2)", "arctan(1, 2) is folded to 63.43495");
    warned("arctan(16777215)", "folds it to 90.");
    quiet("arctan(1)", latest);
    quiet("arcsin(0.5)", latest);
    quiet("arctan(a)", latest);
    // (before 515.1644 Linux's was Windows')
    quiet("arctan(2)", dmasm::ByondVersion::new(515, 1637));
    // Color channel near a half-integer boundary.
    warned(
        "rgb(0, 60, 25, space = COLORSPACE_HSV)",
        "rgb() is folded to #401919, matching Linux DreamMaker",
    );
    warned(
        "rgb(h = 180, s = 60, l = 25)",
        "Windows may round a channel differently",
    );
    quiet("rgb(0, 60, 26, space = COLORSPACE_HSV)", latest);
    quiet("rgb(127.5, 2, 3)", latest);
    quiet("rgb(0, 60, a, space = COLORSPACE_HSV)", latest);
    // Before 515.1647, boundary colors remain runtime expressions.
    quiet(
        "rgb(0, 60, 25, space = COLORSPACE_HSV)",
        dmasm::ByondVersion::new(515, 1637),
    );
}

/// The warnings for global proc f of `code`, compiled for `byond`
fn warnings_for(code: &str, byond: dmasm::ByondVersion) -> Vec<String> {
    let ctx = dreammaker::Context::default();
    let pp = dreammaker::Preprocessor::from_buffer(&ctx, "test.dm".into(), code.to_owned());
    let mut parser = dreammaker::Parser::new(&ctx, pp);
    parser.enable_procs();
    let tree = parser.parse_object_tree();
    let value = tree
        .root()
        .get()
        .procs
        .get("f")
        .unwrap()
        .value
        .last()
        .unwrap();
    let source = ProcSource {
        tree: &tree,
        ty: tree.root(),
        name: "f",
        value,
        file: "test.dm",
        debug_info: false,
        index: None,
        byond,
        file_names: None,
    };
    compile_proc(&source).unwrap().warnings
}

#[test]
fn deliberate_differences_and_copied_quirks_are_warned_about() {
    let latest = dmasm::ByondVersion::LATEST;
    let v = |build| dmasm::ByondVersion::new(516, build);
    let warned = |code: &str, byond, what: &str| {
        let w = warnings_for(code, byond);
        assert!(
            w.iter().any(|w| w.contains(what)),
            "{code:?} on {byond:?}: {w:?}"
        );
    };
    let quiet = |code: &str, byond| {
        let w = warnings_for(code, byond);
        assert!(w.is_empty(), "{code:?} on {byond:?}: {w:?}");
    };
    let types = "/datum/t\n\tvar/n = 1\n\tvar/list/L = list(1)\n\tvar/datum/t/o\n\tproc/g(x)\n";
    warned(
        "/proc/f(t = world.fps/10)\n\treturn t\n",
        latest,
        "reads it as a path",
    );
    quiet("/proc/f(t = 1/10)\n\treturn t\n", latest);
    warned(
        &format!("{types}/proc/f(datum/t/a, datum/t/b)\n\ta?.g(b?.n)?.n = 1\n"),
        latest,
        "on its receiver",
    );
    warned(
        &format!("{types}/proc/f(datum/t/a)\n\ta?.n ||= 1\n"),
        v(1672),
        "before 516.1673",
    );
    quiet(
        &format!("{types}/proc/f(datum/t/a)\n\ta?.n ||= 1\n"),
        v(1673),
    );
    let pointer = "/proc/f()\n\tvar/list/L = list(1, 2)\n\tvar/p = &L\n\t*p[1] = 5\n";
    warned(pointer, v(1668), "leaves the index out");
    quiet(pointer, v(1667));
    warned("/proc/f(x)\n\treturn (-1e10) ^ x\n", latest, "32-bit range");
    warned(
        "/proc/f()\n\treturn isnan(1)\n",
        v(1659),
        "the previous test flag",
    );
    quiet("/proc/f()\n\treturn isnan(1)\n", v(1672));
    warned(
        &format!("{types}/proc/f(datum/t/a, datum/t/b)\n\ta?.L[1] += b.n\n"),
        v(1684),
        "before 516.1685",
    );
    quiet(
        &format!("{types}/proc/f(datum/t/a, datum/t/b)\n\ta?.L[1] += b.n\n"),
        v(1685),
    );
    quiet(
        &format!("{types}/proc/f(datum/t/a)\n\ta?.L[1] += 2\n"),
        v(1684),
    );
}

#[test]
fn initial_of_a_static_field() {
    // initial(x::v) remains a runtime read even when x::v alone folds.
    // Static variables still resolve to their global slots.
    let src = "/obj/item\n\tvar/throwforce = 7\n\tvar/static/sv = 3\n\tvar/obj/item/other\n\
        /proc/a()\n\treturn initial(/obj/item::throwforce)\n\
        /proc/b()\n\treturn initial(/obj/item::sv)\n\
        /proc/c()\n\tvar/obj/item/I = new\n\treturn initial(I.other::throwforce)\n\
        /obj/item/proc/m()\n\treturn initial(src::throwforce)\n";
    assert_has(
        &compile(src, "", "a"),
        &[
            "PushVal /obj/item",
            "SetVar cache",
            "GetVar initial(cache[\"throwforce\"])",
            "Ret",
        ],
    );
    assert_has(
        &compile(src, "", "b"),
        &[
            "PushVal /obj/item",
            "SetVar cache",
            "GetVar initial(global(\"sv\"))",
            "Ret",
        ],
    );
    assert_has(
        &compile(src, "", "c"),
        &[
            "GetVar cache = local(0); cache[\"other\"]",
            "SetVar cache",
            "GetVar initial(cache[\"throwforce\"])",
        ],
    );
    assert_has(
        &compile(src, "/obj/item", "m"),
        &[
            "GetVar src",
            "SetVar cache",
            "GetVar initial(cache[\"throwforce\"])",
        ],
    );
}

#[test]
fn a_name_alone_on_its_line_that_a_goto_names_is_a_label() {
    // DreamMaker accepts both bare-name and `name:` labels.
    let src = "/proc/f()\n\tvar/i = 0\n\tagain\n\ti++\n\tif (i < 5)\n\t\tgoto again\n\treturn i\n";
    let code = compile(src, "", "f");
    assert_eq!(
        code,
        "PushInt 0\nSetVar local(0)\nUSER_again:\nInc local(0)\nGetVar local(0)\nPushInt 5\nTl\nTest\nJz LAB_0002\nJmpLoop USER_again\nLAB_0002:\nLAB_0001:\nGetVar local(0)\nRet\nEnd"
    );
    // Jumping out of a for-in loop must release its iterator.
    let src = "/proc/f(L)\n\tout\n\tfor (var/x in L)\n\t\tif (x) goto out\n";
    let code = compile(src, "", "f");
    assert!(code.contains("USER_out:"), "{code}");
    // Untargeted names remain expression statements.
    let code = compile("/proc/f(a)\n\ta\n\treturn 1\n", "", "f");
    assert!(!code.contains("USER_"), "{code}");
}

#[test]
fn a_proc_of_the_world_s_never_calls_by_name() {
    // World procs retain typed compilation; only debugger code falls back to call-by-name.
    let src = "/mob/proc/known()\n/proc/f(mob/M, x)\n\tM.known()\n\tM.unknown()\n\tx:unknown()\n";
    let code = compile(src, "", "f");
    assert!(!code.contains("CallName"), "{code}");
    assert!(code.contains("dynamic_proc(\"known\")"), "{code}");
    assert!(code.contains("dynamic_proc(\"unknown\")"), "{code}");
}

/// The DbgLines of `f` compiled from `src`.
fn dbg_lines(src: &str) -> Vec<String> {
    compile_with_lines(src)
        .into_iter()
        .filter(|l| l.starts_with("DbgLine"))
        .collect()
}

#[test]
fn the_first_line_is_the_first_with_anything_on_it() {
    // First-line rules checked against DreamMaker 515.1637 and 516.1688.
    let empty_macro = "#define EMPTY\n/proc/f()\n\tEMPTY\n\tvar/x\n\treturn x\n";
    assert_eq!(
        dbg_lines(empty_macro),
        ["DbgLine 3", "DbgLine 4", "DbgLine 5"]
    );
    let semicolon = "/proc/f()\n\t;\n\treturn 1\n";
    assert_eq!(dbg_lines(semicolon), ["DbgLine 2", "DbgLine 3"]);
    let doc_comment = "/proc/f()\n\t/// doc\n\treturn 1\n";
    assert_eq!(dbg_lines(doc_comment), ["DbgLine 3"]);
    // A multiline macro call starts at its name.
    let call_macro = "#define CALL(a, b) world.log << list(a, b)\n/proc/f()\n\tCALL(1,\n\t\t2)\n";
    assert_eq!(dbg_lines(call_macro), ["DbgLine 3", "DbgLine 4"]);
    // An empty body uses the next nonempty line.
    let empty = "/proc/f()\n\t// nothing\n\n/proc/g()\n";
    assert_eq!(dbg_lines(empty), ["DbgLine 4"]);
    let before_a_type = "/proc/f()\n\n/datum\n\tvar/v\n";
    assert_eq!(dbg_lines(before_a_type), ["DbgLine 3"]);
}

#[test]
fn a_return_type_records_the_header_after_the_first_line() {
    let src = "/proc/f() as num\n\t// comment\n\treturn 1\n";
    assert_eq!(dbg_lines(src), ["DbgLine 3", "DbgLine 1", "DbgLine 3"]);
}

#[test]
fn lines_read_on_are_recorded_as_dreammaker_does() {
    // Lookahead records checked against DreamMaker 515.1637 and 516.1688.
    let lines = |src: &str| dbg_lines(&format!("/proc/f(a, b)\n{src}"));
    // past text with an embedded expression: the next line
    assert_eq!(
        lines("\tf(1)\n\tvar/s = \"[a]\"\n\treturn s\n"),
        [
            "DbgLine 2",
            "DbgLine 3",
            "DbgLine 4",
            "DbgLine 3",
            "DbgLine 4"
        ]
    );
    assert_eq!(
        lines("\tif(a)\n\t\treturn \"[a]\"\n\treturn 1\n"),
        ["DbgLine 2", "DbgLine 4", "DbgLine 3", "DbgLine 4"]
    );
    // the value of `for (k, v in L)`: the loop's body
    assert_eq!(
        lines("\tfor(var/k, v in a)\n\t\tf(v)\n"),
        ["DbgLine 2", "DbgLine 3", "DbgLine 2", "DbgLine 3"]
    );
    // where a call with named arguments, a condition or text ends
    assert_eq!(lines("\tf(1,\n\t\tb = 2)\n"), ["DbgLine 2", "DbgLine 3"]);
    assert_eq!(
        lines("\tif(f(1,\n\t\t2))\n\t\tf(3)\n"),
        ["DbgLine 2", "DbgLine 3", "DbgLine 2", "DbgLine 4"]
    );
    assert_eq!(lines("\ta = {\"x\n[a]\ny\"}\n"), ["DbgLine 2", "DbgLine 4"]);
    // an operator on a later line
    assert_eq!(lines("\ta = a \\\n\t\t+ 1\n"), ["DbgLine 2", "DbgLine 3"]);
    assert_eq!(
        lines("\ta = f(1,\n\t\t2) + 3\n"),
        ["DbgLine 2", "DbgLine 3", "DbgLine 2"]
    );
}
