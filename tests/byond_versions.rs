//! Compare version-specific output with bytecode observed from the same
//! source in each target DreamMaker build.

use dmasm::compiler::{compile_proc, ProcSource};
use dmasm::ByondVersion;

const B515_1637: ByondVersion = ByondVersion::new(515, 1637);
const B515_1647: ByondVersion = ByondVersion::new(515, 1647);
const B516_1659: ByondVersion = ByondVersion::new(516, 1659);
const B516_1688: ByondVersion = ByondVersion::new(516, 1688);

/// Compile global proc f for `byond`, returning instructions or an error.
fn compile_for(byond: ByondVersion, code: &str) -> Result<String, String> {
    let ctx = dreammaker::Context::default();
    byond.read_as(&ctx);
    let pp = dreammaker::Preprocessor::from_buffer(&ctx, "test.dm".into(), code.to_owned());
    let mut parser = dreammaker::Parser::new(&ctx, pp);
    parser.enable_procs();
    let tree = parser.parse_object_tree();
    let value = tree
        .root()
        .get()
        .procs
        .get("f")
        .expect("proc")
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
    let compiled = compile_proc(&source).map_err(|e| e.to_string())?;
    Ok(dmasm::format(&compiled.nodes)
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty())
        .collect::<Vec<_>>()
        .join("\n"))
}

/// `return EXPR` for `byond`, without the `Ret` and `End`.
#[track_caller]
fn ret(byond: ByondVersion, expr: &str) -> String {
    let code = compile_for(byond, &format!("/proc/f(a)\n\treturn {expr}\n")).unwrap();
    code.strip_suffix("Ret\nEnd")
        .expect("ends in Ret End")
        .trim()
        .to_owned()
}

#[test]
fn trig_instructions() {
    for (f, old, new) in [
        ("sin", "Sin", "Sin2"),
        ("cos", "Cos", "Cos2"),
        ("tan", "Tan", "Tan2"),
    ] {
        assert_eq!(
            ret(B515_1647, &format!("{f}(a)")),
            format!("GetVar arg(0)\n{old}")
        );
        assert_eq!(
            ret(B516_1659, &format!("{f}(a)")),
            format!("GetVar arg(0)\n{new}")
        );
    }
}

#[test]
fn rounding_folds_from_516() {
    assert_eq!(ret(B515_1647, "ceil(2.5)"), "PushVal 2.5\nCeil");
    assert_eq!(ret(B515_1647, "floor(2.5)"), "PushVal 2.5\nRound");
    assert_eq!(ret(B515_1647, "round(-2.5)"), "PushVal -2.5\nRound");
    assert_eq!(
        ret(B515_1647, "round(2.5, 2)"),
        "PushVal 2.5\nPushInt 2\nRoundN"
    );
    assert_eq!(ret(B515_1647, "trunc(-2.5)"), "PushVal -2.5\nTrunc");
    assert_eq!(ret(B515_1647, "fract(-2.5)"), "PushVal -2.5\nFract");
    assert_eq!(ret(B516_1659, "ceil(2.5)"), "PushInt 3");
    assert_eq!(ret(B516_1659, "round(2.5, 2)"), "PushInt 2");
    assert_eq!(ret(B516_1659, "fract(-2.5)"), "PushVal -0.5");
}

#[test]
fn nan_tests_fold_from_516() {
    assert_eq!(ret(B515_1647, "isnan(1)"), "PushInt 1\nIsNaN\nGetFlag");
    // 516.1659 still reads the flag after the folded test
    assert_eq!(ret(B516_1659, "isnan(1)"), "PushInt 0\nGetFlag");
    assert_eq!(
        ret(B516_1659, "-(isnan(31))"),
        "PushInt 0\nGetFlag\nUnaryNeg"
    );
    assert_eq!(ret(B516_1659, "isinf(1e39)"), "PushInt 1\nGetFlag");
    assert_eq!(ret(B516_1688, "isnan(1)"), "PushInt 0");
    assert_eq!(ret(ByondVersion::new(516, 1670), "isnan(1)"), "PushInt 0");
    assert_eq!(
        ret(ByondVersion::new(516, 1671), "isnan(1)"),
        "PushInt 0\nGetFlag"
    );
    assert_eq!(ret(B516_1688, "!isnan(1)"), "PushInt 1");
    assert_eq!(ret(B516_1688, "isinf(-1e39)"), "PushInt 1");
    // not a number: left to run
    assert_eq!(
        ret(B516_1688, "isinf(/obj)"),
        "PushVal /obj\nIsInf\nGetFlag"
    );
}

#[test]
fn sin_and_cos_of_quarter_turns_are_exact_from_516() {
    assert_eq!(
        ret(B515_1647, "sin(180)"),
        "PushVal 0.00000000000000012246469"
    );
    assert_eq!(ret(B516_1659, "sin(180)"), "PushInt 0");
    assert_eq!(ret(B516_1659, "cos(270)"), "PushInt 0");
    assert_eq!(ret(B515_1647, "cos(1e10)"), "PushVal 0.1736482");
    assert_eq!(ret(B516_1659, "cos(1e10)"), "PushVal 0.17364818");
    assert_eq!(ret(B516_1688, "sin(-731.272)"), "PushVal -0.19546643");
}

#[test]
fn inverse_trig_rounds_radians_from_515_1647() {
    assert_eq!(ret(B515_1637, "arctan(16777215)"), "PushInt 90");
    assert_eq!(
        ret(ByondVersion::new(515, 1643), "arctan(16777215)"),
        "PushInt 90"
    );
    // From 515.1644 DreamMaker on Linux rounds the radians to single precision
    // first (Windows' doesn't, which is warned about)
    assert_eq!(
        ret(ByondVersion::new(515, 1644), "arctan(16777215)"),
        "PushVal 89.99999"
    );
    assert_eq!(ret(B515_1637, "arctan(2)"), "PushVal 63.434948");
    assert_eq!(ret(B516_1688, "arctan(2)"), "PushVal 63.43495");
    assert_eq!(ret(B516_1688, "arctan(1, 2)"), "PushVal 63.43495");
    assert_eq!(ret(B516_1688, "arctan(3, -4)"), "PushVal -53.1301");
    // a y of -0 counts as 0
    assert_eq!(ret(B516_1688, "arctan(-1, -(0))"), "PushInt 180");
    assert_eq!(ret(B516_1688, "arctan(-1, 0 * -1)"), "PushInt 180");
    assert_eq!(ret(B516_1688, "arctan(-1, -0.5)"), "PushVal -153.43495");
    assert_eq!(ret(B515_1637, "arccos(0.7071)"), "PushVal 45.000553");
    assert_eq!(ret(B516_1688, "arccos(0.7071)"), "PushVal 45.00055");
    assert_eq!(ret(B516_1688, "arcsin(0.5)"), "PushInt 30");
    // out of their domain: left to run
    assert_eq!(ret(B516_1688, "arcsin(2)"), "PushInt 2\nArcSin");
}

#[test]
fn tan_folds_as_each_build_does() {
    assert_eq!(ret(B515_1647, "tan(90)"), "PushVal 16331240000000000");
    // 516 works on the angle's quarter turn and the rest of it, in single
    // precision from the tangent on
    assert_eq!(ret(B516_1688, "tan(30)"), "PushVal 0.57735026");
    assert_eq!(ret(B516_1688, "tan(60)"), "PushVal 1.7320509");
    assert_eq!(ret(B516_1688, "tan(45)"), "PushInt 1");
    assert_eq!(ret(B516_1688, "tan(89)"), "PushVal 57.28996");
    assert_eq!(ret(B516_1688, "tan(90)"), "PushVal -inf");
    assert_eq!(ret(B516_1688, "tan(270)"), "PushVal inf");
    assert_eq!(ret(B516_1688, "tan(-90)"), "PushVal inf");
    assert_eq!(ret(B516_1688, "tan(-180)"), "PushInt 0");
    assert_eq!(ret(B516_1688, "tan(1e10)"), "PushVal -5.671282");
}

/// `\ref`, `\icon` and `\roman` belong to the next embedded value, wherever
/// in the text they're written.
#[test]
fn a_macro_that_takes_a_value_goes_to_the_next_one() {
    let text = |expr: &str| ret(B516_1688, expr);
    assert_eq!(text(r#""\ref[a]""#), "GetVar arg(0)\nFormat \"\\ref[]\" 1");
    assert_eq!(
        text(r#""x\ref y[a]""#),
        "GetVar arg(0)\nFormat \"x y\\ref[]\" 1"
    );
    assert_eq!(
        text(r#""\roman x [a]""#),
        "GetVar arg(0)\nFormat \" x \\roman[]\" 1"
    );
    // with no value after it, it's dropped
    assert_eq!(text(r#""x \ref""#), "PushVal \"x \"");
    assert_eq!(text(r#""[a]\roman""#), "GetVar arg(0)\nFormat \"[]\" 1");
    // the first of two has the value, and an article before or after has it
    assert_eq!(
        text(r#""\ref\icon[a]""#),
        "GetVar arg(0)\nFormat \"\\ref[]\" 1"
    );
    assert_eq!(
        text(r#""\the \ref[a]""#),
        "GetVar arg(0)\nFormat \"\\the[]\" 1"
    );
    // \UXXXXXX is a character
    assert_eq!(text(r#""\U01F600""#), "PushVal \"\u{1F600}\"");
}

/// The old colour and style macros switch off when they come again.
#[test]
fn a_colour_or_style_macro_again_ends_it() {
    let text = |t: &str| ret(B516_1688, &format!("\"{t}\""));
    // the colour in force again ends colour (code 30, shown as \color), and
    // takes the space before it instead of the one after
    assert_eq!(text(r"\red x \red y"), r#"PushVal "\red x\color y""#);
    assert_eq!(text(r"\red x\red y"), r#"PushVal "\red x\color y""#);
    // another colour takes over, and after colour has ended one starts again
    assert_eq!(text(r"\red x \blue y"), r#"PushVal "\red x\blue  y""#);
    assert_eq!(
        text(r"\red x \red y \red z"),
        r#"PushVal "\red x\color y \red z""#
    );
    // a style is ended by its own macro, with the same code
    assert_eq!(text(r"\bold x \bold y"), r#"PushVal "\bold x\bold  y""#);
    assert_eq!(text(r"\bold x \italic y"), r#"PushVal "\bold x \italic y""#);
    assert_eq!(
        text(r"\italic x \italic y"),
        r#"PushVal "\italic x\italic  y""#
    );
}

#[test]
fn json_decode_takes_flags() {
    for byond in [B515_1637, B516_1688] {
        assert_eq!(ret(byond, "json_decode(a)"), "GetVar arg(0)\nJsonDecode");
        assert_eq!(
            ret(byond, "json_decode(a, 1)"),
            "GetVar arg(0)\nPushInt 1\nJsonDecodeFlags 2"
        );
    }
}

#[test]
fn text2ascii_char_of_a_constant_folds() {
    for byond in [B515_1637, B516_1688] {
        assert_eq!(ret(byond, "text2ascii_char(\"abc\")"), "PushInt 97");
        assert_eq!(ret(byond, "text2ascii_char(\"é\")"), "PushInt 233");
        assert_eq!(ret(byond, "text2ascii(\"é\")"), "PushInt 233");
    }
}

/// `rgb2num()` of a constant colour is a list DreamMaker knows when compiling.
#[test]
fn rgb2num_of_a_constant_colour() {
    for byond in [B515_1637, B516_1688] {
        let v = |expr: &str| ret(byond, expr);
        assert_eq!(
            v("rgb2num(\"#102030\")"),
            "PushInt 16\nPushInt 32\nPushInt 48\nNewList 3"
        );
        assert_eq!(
            v("rgb2num(\"#10203040\")"),
            "PushInt 16\nPushInt 32\nPushInt 48\nPushInt 64\nNewList 4"
        );
        // a digit for a channel is that digit twice
        assert_eq!(
            v("rgb2num(\"#123\")"),
            "PushInt 17\nPushInt 34\nPushInt 51\nNewList 3"
        );
        assert_eq!(
            v("rgb2num(\"#1234\")"),
            "PushInt 17\nPushInt 34\nPushInt 51\nPushInt 68\nNewList 4"
        );
        // names, whatever their case; null is white
        assert_eq!(
            v("rgb2num(\"Green\")"),
            "PushInt 0\nPushInt 192\nPushInt 0\nNewList 3"
        );
        assert_eq!(
            v("rgb2num(\"none\")"),
            "PushInt 0\nPushInt 0\nPushInt 0\nPushInt 0\nNewList 4"
        );
        assert_eq!(
            v("rgb2num(null)"),
            "PushInt 255\nPushInt 255\nPushInt 255\nNewList 3"
        );
        // a space that isn't HSV, HSL or HCY is RGB
        assert_eq!(
            v("rgb2num(\"#102030\", COLORSPACE_RGB)"),
            "PushInt 16\nPushInt 32\nPushInt 48\nNewList 3"
        );
        assert_eq!(
            v("rgb2num(\"#102030\", 5)"),
            "PushInt 16\nPushInt 32\nPushInt 48\nNewList 3"
        );
        // in another space the numbers differ by who works them out: left to run
        assert_eq!(
            v("rgb2num(\"#102030\", COLORSPACE_HSV)"),
            "PushVal \"#102030\"\nPushInt 1\nRgb2Num"
        );
        // what isn't a colour runs
        assert_eq!(
            v("rgb2num(\"#GGGGGG\")"),
            "PushVal \"#GGGGGG\"\nPushInt 0\nRgb2Num"
        );
        assert_eq!(
            v("rgb2num(\"pink\")"),
            "PushVal \"pink\"\nPushInt 0\nRgb2Num"
        );
        // where it's tested it's true, and the list isn't made (in any space)
        assert_eq!(v("rgb2num(\"#102030\") ? a : 2"), "GetVar arg(0)");
        assert_eq!(v("rgb2num(\"red\", COLORSPACE_HSV) && a"), "GetVar arg(0)");
        assert_eq!(v("!rgb2num(\"#123\")"), "PushInt 0");
    }
}

/// Rarer ways to call a builtin, as DreamMaker writes them.
#[test]
fn rarer_forms_of_builtins() {
    let v = |expr: &str| ret(B516_1688, expr);
    // block() with a z, and with some of the second corner: the rest is null
    assert_eq!(
        v("block(a, 2, 3)"),
        "GetVar arg(0)\nPushInt 2\nPushInt 3\nPushVal null\nPushVal null\nPushVal null\nBlockXYZ"
    );
    assert_eq!(
        v("block(a, 2, 3, 4)"),
        "GetVar arg(0)\nPushInt 2\nPushInt 3\nPushInt 4\nPushVal null\nPushVal null\nBlockXYZ"
    );
    // with nothing, of null
    assert_eq!(v("time2text()"), "PushVal null\nPushVal null\nTime2Text");
    assert_eq!(v("round()"), "PushVal null\nRound");
    // outside `<<` these are their first argument; the others are worked out and dropped
    assert_eq!(v("browse(a)"), "GetVar arg(0)");
    assert_eq!(
        v("browse(a, 1, 2)"),
        "GetVar arg(0)\nPushInt 1\nPop\nPushInt 2\nPop"
    );
    assert_eq!(v("output(a, 1)"), "GetVar arg(0)\nPushInt 1\nPop");
    assert_eq!(v("load_resource(a)"), "GetVar arg(0)");
    // as values
    assert_eq!(
        v("shutdown(a)"),
        "GetVar arg(0)\nPushVal null\nShutdownArgs"
    );
    assert_eq!(v("statpanel(a)"), "GetVar arg(0)\nStatPanelCheck\nGetFlag");
    assert_eq!(v("animate(a)"), "GetVar arg(0)\nNullAnimate");
    assert_eq!(
        v("animate(a, time = 5)"),
        "PushInt 1\nGetVar arg(0)\nPushVal \"time\"\nPushInt 5\nNewAssocList 2\nAnimate"
    );
    // what comes after animate()'s object without a name isn't even worked out
    assert_eq!(v("animate(a, a + 1, 3)"), "GetVar arg(0)\nNullAnimate");
    // initial() of an argument, src or `.`
    assert_eq!(v("initial(a)"), "GetVar initial(arg(0))");
    assert_eq!(v("initial(src)"), "GetVar initial(src)");
    assert_eq!(v("initial(.)"), "GetVar initial(dot)");
    // a key with no value in alist() has null
    assert_eq!(
        v("alist(a, \"k\" = 2)"),
        "GetVar arg(0)\nPushVal null\nPushVal \"k\"\nPushInt 2\nNewAlist 2"
    );
    assert_eq!(
        v("alist(1, 2)"),
        "PushInt 1\nPushVal null\nPushInt 2\nPushVal null\nNewAlist 2"
    );
}

/// Before 515.1644 an integer literal beyond 2147483647 is 2147483647.
#[test]
fn an_integer_literal_beyond_an_i32_before_515_1644() {
    for byond in [B515_1637, ByondVersion::new(515, 1643)] {
        assert_eq!(
            ret(byond, "a + 3000000000"),
            "GetVar arg(0)\nPushVal 2147483600\nAdd"
        );
        assert_eq!(
            ret(byond, "a + 0xFFFFFFFF"),
            "GetVar arg(0)\nPushVal 2147483600\nAdd"
        );
        assert_eq!(
            ret(byond, "a + -3000000000"),
            "GetVar arg(0)\nPushVal -2147483600\nAdd"
        );
        // written with a point or an exponent it's the number
        assert_eq!(
            ret(byond, "a + 3e9"),
            "GetVar arg(0)\nPushVal 3000000000\nAdd"
        );
        assert_eq!(
            ret(byond, "a + 3000000000.0"),
            "GetVar arg(0)\nPushVal 3000000000\nAdd"
        );
    }
    for byond in [ByondVersion::new(515, 1644), B515_1647, B516_1688] {
        assert_eq!(
            ret(byond, "a + 3000000000"),
            "GetVar arg(0)\nPushVal 3000000000\nAdd"
        );
    }
}

/// A hexadecimal literal beyond an i32 is the number it is.
#[test]
fn a_hexadecimal_literal_beyond_an_i32() {
    assert_eq!(
        ret(B516_1688, "a + 0xFFFFFFFF"),
        "GetVar arg(0)\nPushVal 4294967300\nAdd"
    );
    assert_eq!(
        ret(B516_1688, "a + 0x80000000"),
        "GetVar arg(0)\nPushVal 2147483600\nAdd"
    );
    assert_eq!(
        ret(B516_1688, "a + 0x7FFFFFFF"),
        "GetVar arg(0)\nPushVal 2147483600\nAdd"
    );
    assert_eq!(ret(B515_1647, "0xDEADBEEF"), "PushVal 3735928600");
}

/// Folds at the edges, as DreamMaker has them on every build.
#[test]
fn folds_at_the_edges() {
    for byond in [B515_1637, B516_1688] {
        let folds = |expr: &str, to: &str| assert_eq!(ret(byond, expr), to, "{expr}");
        // A shift takes the low five bits of its count, which is 65535 at most
        folds("(1 << 32)", "PushInt 1");
        folds("(1 << 33)", "PushInt 2");
        folds("(1 << 31)", "PushInt 0");
        folds("(16777215 << 36)", "PushVal 16777200");
        folds("(255 >> 36)", "PushInt 15");
        folds("(-1 >> 36)", "PushVal 1048575");
        folds("(1 << 65505)", "PushInt 2");
        folds("(1 << 65537)", "PushInt 0");
        folds("(255 >> 131073)", "PushInt 0");
        folds("(1 << -1)", "PushInt 1");
        // Beyond the i32 range a side of % is the i32 furthest that way
        folds("(3e9 % 7)", "PushInt 1");
        folds("(-1e30 % 7)", "PushVal -1");
        folds("(3e9 % 3e9)", "PushInt 0");
        folds("(2147483648 % 7)", "PushVal -2");
        folds("(7 % 3e9)", "PushInt 7");
        // Zero raised to a negative power returns zero, except exponent -1.
        folds("(0 ** -7)", "PushInt 0");
        folds("(0 ** -0.5)", "PushInt 0");
        folds("(0 ** -1)", "PushVal inf");
        folds("(2 ** -1)", "PushVal 0.5");
        folds("arctan(0, 0)", "PushInt 0");
        // Shifts involving text or null return zero. Dividing a number by
        // text returns the original number.
        folds("(\"a\" << 2)", "PushInt 0");
        folds("(null << 2)", "PushInt 0");
        folds("(5 >> \"a\")", "PushInt 5");
        folds("(5 / \"a\")", "PushInt 5");
        folds("(6 / null)", "PushInt 6");
        folds("tan(1.#INF)", "PushVal NaN");
        // NaN does not trigger a domain error.
        folds("sqrt(1.#IND)", "PushVal NaN");
        folds("arcsin(1.#IND)", "PushVal NaN");
        folds("arccos(1.#INF - 1.#INF)", "PushVal NaN");
    }
    // (515 doesn't fold fract)
    assert_eq!(ret(B516_1688, "fract(1.#INF)"), "PushInt 0");
}

#[test]
fn float_mod_folds() {
    assert_eq!(ret(B516_1688, "10 %% 3"), "PushInt 1");
    assert_eq!(ret(B516_1688, "7 %% 0.1"), "PushVal 0.0999999");
    assert_eq!(ret(B515_1637, "1e30 %% 1e10"), "PushVal 6688855000");
}

#[test]
fn builtins_515_lacks_are_calls() {
    let code = "/proc/sign(x)\n\treturn x\n/proc/f(a)\n\treturn sign(a)\n";
    assert_eq!(
        compile_for(B515_1647, code).unwrap(),
        "GetVar arg(0)\nCallGlob 1 /proc/sign\nRet\nEnd"
    );
    assert_eq!(
        compile_for(B516_1659, code).unwrap(),
        "GetVar arg(0)\nSign\nRet\nEnd"
    );
}

#[test]
fn language_515_lacks_is_refused() {
    let err = compile_for(B515_1647, "/proc/f(a)\n\treturn alist(a = 1)\n").unwrap_err();
    assert!(
        err.contains("BYOND 515.1647 does not support alist()"),
        "{err}"
    );
    let err = compile_for(
        B515_1647,
        "/proc/f(a)\n\tfor (var/k, v in a)\n\t\treturn k\n",
    )
    .unwrap_err();
    assert!(err.contains("does not support for (k, v in list)"), "{err}");
    let err = compile_for(
        ByondVersion::new(515, 1643),
        "/proc/f(a)\n\treturn __PROC__\n",
    )
    .unwrap_err();
    assert!(err.contains("does not support __PROC__"), "{err}");
    assert!(compile_for(
        ByondVersion::new(515, 1644),
        "/proc/f(a)\n\treturn __PROC__\n"
    )
    .is_ok());
}

#[test]
fn safe_element_changes_restore_the_cache_first_from_516_1685() {
    let code = "/datum/t\n\tvar/list/l = list(1)\n/proc/f(datum/t/a, b)\n\ta?.l[1] += b\n\ta?.l[1] ||= b\n";
    let new = compile_for(ByondVersion::new(516, 1685), code).unwrap();
    assert!(
        new.contains(
            "PushCache\nGetVar arg(1)\nPopCache\nGetVar cache[\"l\"]\nPushInt 1\nSetVar cache_key"
        ),
        "{new}"
    );
    assert!(
        new.contains("PushCache\nPopCache\nGetVar cache[\"l\"]\nPushInt 1\nSetVar cache_key"),
        "{new}"
    );
    for byond in [ByondVersion::new(516, 1684), ByondVersion::new(515, 1637)] {
        let old = compile_for(byond, code).unwrap();
        assert!(old.contains("PushCache\nGetVar arg(1)\nGetVar cache[\"l\"]\nPushInt 1\nPopCache\nSetVar cache_key"), "{old}");
        assert!(
            old.contains("PushCache\nGetVar cache[\"l\"]\nPushInt 1\nPopCache\nSetVar cache_key"),
            "{old}"
        );
    }
}

#[test]
fn bit_not_folds() {
    let byond = ByondVersion::LATEST;
    // prefix operators apply innermost first
    assert_eq!(ret(byond, "~-1"), "PushVal 16777215");
    assert_eq!(ret(byond, "-~1"), "PushVal -16777214");
    assert_eq!(ret(byond, "~-3.25"), "PushVal 16777215");
    // text and null are 0
    assert_eq!(ret(byond, "~\"x\""), "PushVal 16777215");
    assert_eq!(ret(byond, "~null"), "PushVal 16777215");
    assert_eq!(ret(byond, "~256"), "PushVal 16776959");
}

#[test]
fn text2ascii_of_empty_text_folds() {
    for byond in [ByondVersion::new(515, 1637), ByondVersion::LATEST] {
        assert_eq!(ret(byond, "text2ascii(\"\")"), "PushInt 0");
        assert_eq!(ret(byond, "text2ascii(\"x\")"), "PushInt 120");
    }
}

#[test]
fn log_folds_in_double_precision_before_515_1644() {
    assert_eq!(
        ret(ByondVersion::new(515, 1643), "log(1.5, 255.519)"),
        "PushVal 13.671452"
    );
    assert_eq!(
        ret(ByondVersion::new(515, 1644), "log(1.5, 255.519)"),
        "PushVal 13.6714525"
    );
}

#[test]
fn proc_macro_in_call_before_515_1644() {
    // Before 515.1644, __PROC__ is accepted only in call(src, __PROC__)
    // and nameof(__PROC__).
    let b = ByondVersion::new(515, 1637);
    let code = compile_for(b, "/proc/f(a)\n\tcall(a, __PROC__)(a)\n").unwrap();
    assert!(code.contains("PushVal /proc/f"), "{code}");
    assert_eq!(ret(b, "nameof(__PROC__)"), "PushVal \"f\"");
    assert!(compile_for(b, "/proc/f(a)\n\treturn __PROC__\n").is_err());
}

#[test]
fn a_colour_of_constants_is_its_text_in_any_colour_space() {
    let text = |expr: &str| ret(B516_1688, expr);
    // Truncate RGB and alpha channels to integers.
    assert_eq!(text("rgb(1.9, 2.5, 300, 127.9)"), "PushVal \"#0102ff7f\"");
    assert_eq!(
        text("rgb(120, 100, 50, space = COLORSPACE_RGB)"),
        "PushVal \"#786432\""
    );
    // an alpha that's null (or text) is 0
    assert_eq!(text("rgb(1, 2, 3, null)"), "PushVal \"#01020300\"");
    assert_eq!(
        text("rgb(h = 120, s = 100, v = 60, a = null)"),
        "PushVal \"#00990000\""
    );
    // Clamp HSV and HSL percentage channels to [0, 100].
    assert_eq!(text("rgb(h = 120, s = 100, v = 60)"), "PushVal \"#009900\"");
    assert_eq!(
        text("rgb(359, 60, 60, 200, COLORSPACE_HSV)"),
        "PushVal \"#993d3fc8\""
    );
    assert_eq!(text("rgb(h = -30, s = 150, v = 64)"), "PushVal \"#a30052\"");
    assert_eq!(text("rgb(l = 13, h = 351, s = 180)"), "PushVal \"#42000a\"");
    // HCY percentage channels are unclamped; green uses weight 0.557.
    assert_eq!(text("rgb(h = 120, c = 100, y = 50)"), "PushVal \"#00e500\"");
    assert_eq!(
        text("rgb(50, 300, 92, space = COLORSPACE_HCY)"),
        "PushVal \"#fffd25\""
    );
    assert_eq!(text("rgb(h = -27, y = -5, c = 255)"), "PushVal \"#001400\"");
    assert_eq!(
        text("rgb(h = 120, s = 100, l = 60) + \"x\""),
        "PushVal \"#33ff33x\""
    );
    // 515 works with an HSV saturation beyond 100 as it is
    assert_eq!(
        ret(B515_1647, "rgb(h = 90, s = 100.4, v = 64)"),
        "PushVal \"#51a300\""
    );
    assert_eq!(
        ret(B516_1659, "rgb(h = 90, s = 100.4, v = 64)"),
        "PushVal \"#52a300\""
    );
    // Near half-integer channels expose Linux f32 normalization and rounding.
    // Warn because Windows may round to the other integer.
    assert_eq!(
        text("rgb(0, 60, 25, space = COLORSPACE_HSV)"),
        "PushVal \"#401919\""
    );
    assert_eq!(
        text("rgb(200, 100, 75, space = COLORSPACE_HSV)"),
        "PushVal \"#0080bf\""
    );
    assert_eq!(
        text("rgb(0, 0, 70, space = COLORSPACE_HSV)"),
        "PushVal \"#b3b3b3\""
    );
    assert_eq!(
        text("rgb(180, 60, 25, space = COLORSPACE_HSL)"),
        "PushVal \"#196666\""
    );
    assert_eq!(
        text("rgb(200, 80, 50, space = COLORSPACE_HSL)"),
        "PushVal \"#19a2e6\""
    );
    assert_eq!(
        text("rgb(0, 60, 25, space = COLORSPACE_HCY)"),
        "PushVal \"#991919\""
    );
    assert_eq!(
        ret(B515_1647, "rgb(330, 60, 75, space = COLORSPACE_HCY)"),
        "PushVal \"#e6aac8\""
    );
    assert_eq!(
        ret(B515_1647, "rgb(158, 53, 70, 64, COLORSPACE_HSV)"),
        "PushVal \"#54b39040\""
    );
    // Leave boundary colors for runtime before 515.1647.
    assert!(ret(B515_1637, "rgb(158, 53, 70, 64, COLORSPACE_HSV)").ends_with("RgbEx"));
    assert!(ret(B515_1637, "rgb(0, 0, 70, space = COLORSPACE_HSV)").ends_with("RgbEx"));
    assert_eq!(
        ret(B515_1637, "rgb(0, 0, 71, space = COLORSPACE_HSV)"),
        "PushVal \"#b5b5b5\""
    );
    // What isn't constant runs
    assert!(text("rgb(a, 2, 3)").ends_with("Rgb"));
    assert!(text("rgb(h = a, s = 2, v = 3)").ends_with("RgbEx"));
}

#[test]
fn a_gradient_written_out_is_read_at_an_index() {
    // gradient(list(...), index): the list is the gradient
    assert_eq!(
        ret(B516_1688, "gradient(list(\"#000000\", \"#ffffff\"), a)"),
        "PushVal \"#000000\"\nPushVal \"#ffffff\"\nNewList 2\nGetVar arg(0)\nGradientIndex"
    );
    // A list variable is an ordinary gradient argument.
    assert_eq!(
        ret(B516_1688, "gradient(a, 0.5)"),
        "GetVar arg(0)\nPushVal 0.5\nNewList 2\nGradient"
    );
    // and arglist()'s list is all of them
    assert_eq!(
        ret(B516_1688, "gradient(arglist(a))"),
        "GetVar arg(0)\nGradient"
    );
}

#[test]
fn a_comparison_a_logical_operator_folds_to_is_tested_as_a_value() {
    let test = |condition: &str| {
        compile_for(
            B516_1688,
            &format!("/proc/f(a, b)\n\tif ({condition})\n\t\treturn 1\n"),
        )
        .unwrap()
    };
    // Written alone, a comparison leaves its result in the flag
    assert!(
        test("a == b").contains("Teq\nPop\nJz"),
        "{}",
        test("a == b")
    );
    assert!(test("(a == b)").contains("Teq\nPop\nJz"));
    // A comparison remaining after logical folding is fetched and retested.
    for condition in [
        "0 || (a == b)",
        "1 && (a == b)",
        "(0 || a == b)",
        "\"\" || (a == b)",
    ] {
        assert!(
            test(condition).contains("Teq\nPop\nGetFlag\nTest\nJz"),
            "{}: {}",
            condition,
            test(condition)
        );
    }
}

#[test]
fn a_logical_assignment_saves_the_cache_by_its_values_source() {
    let code = |body: &str| {
        compile_for(
            B516_1688,
            &format!("/datum/d\n\tvar/x\n/proc/f(datum/d/a, list/L, b)\n\t{body}\n"),
        )
        .unwrap()
    };
    // `null ? L.len : 2` folds to 2, but L.len is there in the source
    let partly = code("a.x ||= (b ? (null ? L.len : 2) : 3)");
    assert!(
        partly.contains("PushCache") && partly.contains("PopCache"),
        "{partly}"
    );
    let side = code("a.x &&= (0 ? L.len : b)");
    assert!(
        side.contains("PushCache\nGetVar arg(2)\nPopCache"),
        "{side}"
    );
    let value = code("return (a.x ||= (b ? (null ? L.len : 2) : 3))");
    assert!(
        value.contains("PushCache") && value.contains("PopCache\nSetVarExpr"),
        "{value}"
    );
    // a value that has no use for the cache in its source doesn't
    assert!(!code("a.x ||= (b ? 2 : 3)").contains("PushCache"));
}
