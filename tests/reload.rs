//! Source snapshot comparison and incremental reload tests.

use std::collections::HashSet;
use std::path::PathBuf;

use dmasm::reload::{
    diff, Changes, ConstValue, InList, NewVar, ParseOptions, ProcKey, ProcSettings, Snapshot,
};
use dmasm::{Instruction, Node};

/// A codebase in a temporary directory: `world.dme` including `code.dm`.
struct World {
    dir: PathBuf,
}

impl World {
    fn new(name: &str, code: &str) -> World {
        let dir =
            std::env::temp_dir().join(format!("dmasm-reload-{}-{}", name, std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(dir.join("code")).unwrap();
        std::fs::write(dir.join("world.dme"), "#include \"code/code.dm\"\n").unwrap();
        let world = World { dir };
        world.write(code);
        world
    }

    fn write(&self, code: &str) {
        std::fs::write(self.dir.join("code/code.dm"), code).unwrap();
    }

    fn parse(&self) -> Snapshot {
        Snapshot::parse(&self.dir.join("world.dme"), &ParseOptions::default()).unwrap()
    }
}

impl Drop for World {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.dir);
    }
}

/// `changes`, for where a test has a binding of that name.
fn changes_of(name: &str, before: &str, after: &str) -> Changes {
    changes(name, before, after)
}

/// The changes from `before` to `after`, with code.dm as the edited file.
fn changes(name: &str, before: &str, after: &str) -> Changes {
    let world = World::new(name, before);
    let old = world.parse();
    world.write(after);
    let new = world.parse();
    let edited: HashSet<PathBuf> = vec![PathBuf::from("code/code.dm")].into_iter().collect();
    diff(&old, &new, Some(&edited))
}

fn key(path: &str, override_id: u32) -> ProcKey {
    ProcKey {
        path: path.to_owned(),
        override_id,
    }
}

fn reloaded(changes: &Changes) -> Vec<ProcKey> {
    assert!(changes.refused.is_empty(), "refused: {:?}", changes.refused);
    changes.reloads.iter().map(|r| r.proc.clone()).collect()
}

#[test]
fn nothing_changed() {
    let code = "/proc/a()\n\treturn 1\n";
    assert!(changes("same", code, code).is_empty());
}

#[test]
fn a_changed_body_is_compiled() {
    let changes = changes(
        "body",
        "/proc/a()\n\treturn 1\n/proc/b()\n\treturn 2\n",
        "/proc/a()\n\treturn 1\n/proc/b()\n\tvar/x = 3\n\treturn x\n",
    );
    assert_eq!(reloaded(&changes), [key("/b", 0)]);
    assert!(changes.line_moves.is_empty());
    let reload = &changes.reloads[0];
    assert_eq!(reload.locals, ["x"]);
    // With debug info, named by the file's path from the .dme
    assert_eq!(
        reload.nodes.first(),
        Some(&Node::Instruction(
            Instruction::DbgFile(dmasm::DMString(b"code/code.dm".to_vec())),
            ()
        ))
    );
}

/// Nested .dme includes can use non-project-relative debug names,
/// e.g. _std/tgs.dm becomes tgs.dm.
#[test]
fn files_of_an_included_environment_are_named_as_it_names_them() {
    let world = World::new("names", "/proc/a()\n\treturn 1\n");
    let dir = &world.dir;
    std::fs::create_dir_all(dir.join("lib/sub")).unwrap();
    std::fs::write(
        dir.join("world.dme"),
        "#include \"code/code.dm\"\n#include \"lib\\lib.dme\"\n",
    )
    .unwrap();
    std::fs::write(
        dir.join("lib/lib.dme"),
        "#include \"std.dm\"\n#include \"sub\\more.dm\"\n",
    )
    .unwrap();
    std::fs::write(dir.join("lib/std.dm"), "/proc/b()\n\treturn 2\n").unwrap();
    std::fs::write(dir.join("lib/sub/more.dm"), "/proc/c()\n\treturn 3\n").unwrap();
    let old = world.parse();

    let file_of = |snapshot: &Snapshot, proc: &str| match snapshot
        .compile(&key(proc, 0))
        .unwrap()
        .nodes
        .first()
    {
        Some(Node::Instruction(Instruction::DbgFile(name), ())) => {
            String::from_utf8(name.0.clone()).unwrap()
        }
        other => panic!("{proc} starts with {other:?}"),
    };
    assert_eq!(file_of(&old, "/a"), "code/code.dm");
    assert_eq!(file_of(&old, "/b"), "std.dm");
    assert_eq!(file_of(&old, "/c"), "sub/more.dm");
    // Diagnostic source paths remain project-relative.
    assert_eq!(old.proc_file(&key("/b", 0)).as_deref(), Some("lib/std.dm"));

    // Reloaded bytecode retains the original debug source name.
    std::fs::write(dir.join("lib/std.dm"), "/proc/b()\n\treturn 4\n").unwrap();
    let changes = diff(&old, &world.parse(), None);
    assert_eq!(reloaded(&changes), [key("/b", 0)]);
    assert_eq!(
        changes.reloads[0].nodes.first(),
        Some(&Node::Instruction(
            Instruction::DbgFile(dmasm::DMString(b"std.dm".to_vec())),
            ()
        ))
    );
}

#[test]
fn a_file_included_earlier_moves_no_defaults() {
    // An earlier include changes file IDs, not default locations.
    let world = World::new(
        "renumbered",
        "/obj/thing\n\tname = \"thing\"\n\n/proc/a()\n\treturn /obj/thing::name\n",
    );
    let old = world.parse();
    std::fs::write(world.dir.join("code/first.dm"), "/proc/b()\n\treturn 2\n").unwrap();
    std::fs::write(
        world.dir.join("world.dme"),
        "#include \"code/first.dm\"\n#include \"code/code.dm\"\n",
    )
    .unwrap();
    let edited: HashSet<PathBuf> = ["world.dme", "code/first.dm"]
        .iter()
        .map(PathBuf::from)
        .collect();
    let changes = diff(&old, &world.parse(), Some(&edited));
    assert!(
        changes.defaults_moved.is_empty(),
        "{:?}",
        changes.defaults_moved
    );
    assert!(reloaded(&changes).is_empty(), "{:?}", reloaded(&changes));
}

#[test]
fn procs_below_an_edit_move_lines() {
    let changes = changes(
        "lines",
        "/proc/a()\n\treturn 1\n\n/proc/b()\n\tworld.log << 1\n\treturn 2\n",
        "/proc/a()\n\tworld.log << 0\n\treturn 1\n\n/proc/b()\n\tworld.log << 1\n\treturn 2\n",
    );
    assert_eq!(reloaded(&changes), [key("/a", 0)]);
    assert_eq!(changes.line_moves.len(), 1);
    assert_eq!(changes.line_moves[0].proc, key("/b", 0));
    assert_eq!(changes.line_moves[0].lines, [(5, 6), (6, 7)]);
}

#[test]
fn types_and_overrides_are_named_like_auxtools_does() {
    let before = "/datum/thing/proc/f()\n\treturn 1\n/datum/thing/f()\n\treturn ..() + 1\n/datum/thing/verb/v()\n\treturn 3\n";
    let after = "/datum/thing/proc/f()\n\treturn 1\n/datum/thing/f()\n\treturn ..() + 2\n/datum/thing/verb/v()\n\treturn 4\n";
    let changes = changes("overrides", before, after);
    let mut keys = reloaded(&changes);
    keys.sort();
    assert_eq!(keys, [key("/datum/thing/f", 1), key("/datum/thing/v", 0)]);
}

#[test]
fn a_define_change_recompiles_its_users() {
    let mut keys = reloaded(&changes(
        "define",
        "#define N 1\n/proc/a()\n\treturn N\n/proc/b()\n\treturn 2\n",
        "#define N 5\n/proc/a()\n\treturn N\n/proc/b()\n\treturn 2\n",
    ));
    keys.sort();
    assert_eq!(keys, [key("/a", 0)]);
}

#[test]
fn a_const_change_recompiles_its_users() {
    let before = "var/const/LIMIT = 3\n/datum/thing\n\tvar/const/SIZE = 2\n/proc/a()\n\treturn LIMIT\n/proc/b()\n\treturn 2\n/datum/thing/proc/c()\n\treturn SIZE * 2\n/datum/thing/proc/d()\n\treturn \"SIZE\"\n";
    let after = before
        .replace("LIMIT = 3", "LIMIT = 4")
        .replace("SIZE = 2", "SIZE = 5");
    let changes = changes("const", before, &after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    assert_eq!(changes.consts, ["LIMIT", "SIZE"]);
    // Both are globals, set to their new values now
    let set: Vec<(&str, &str, Option<ConstValue>)> = changes
        .redeclared
        .iter()
        .map(|r| (r.type_path.as_str(), r.var.as_str(), r.value.clone()))
        .collect();
    assert_eq!(
        set,
        [
            ("", "LIMIT", Some(ConstValue::Number(4.0))),
            ("/datum/thing", "SIZE", Some(ConstValue::Number(5.0)))
        ]
    );
    // d names SIZE only in a string, so its code is the same
    assert_eq!(reloaded(&changes), [key("/datum/thing/c", 0)]);
    // Recompiled procs already have current debug lines; do not also
    // apply a line-only update that would restore old bytecode.
    let changes = changes_of(
        "const-moved",
        before,
        &after.replace("/datum/thing\n", "/datum/thing\n\n"),
    );
    assert_eq!(reloaded(&changes), [key("/datum/thing/c", 0)]);
    let moved: Vec<ProcKey> = changes.line_moves.iter().map(|m| m.proc.clone()).collect();
    assert_eq!(
        moved,
        [key("/a", 0), key("/b", 0), key("/datum/thing/d", 0)]
    );
    // (no "only after a restart" warning for a const)
    assert!(changes.warnings.is_empty(), "{:?}", changes.warnings);
}

#[test]
fn a_new_const_is_a_global() {
    let before = "/datum/thing
	var/x = 1
/datum/thing/sub
";
    let after = "/datum/thing
	var/x = 1
	var/const/K = 7
/datum/thing/sub
/datum/other
	var/const/J = 2
";
    let changes = changes("new-const", before, after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    let new: Vec<(&str, &str, u32)> = changes
        .new_vars
        .iter()
        .map(|v| (v.type_path.as_str(), v.var.as_str(), v.flags))
        .collect();
    assert_eq!(new, [("/datum/other", "J", 3), ("/datum/thing", "K", 3)]);
    // not a default in the types' var tables
    assert!(
        changes.var_defaults.is_empty(),
        "{:?}",
        changes.var_defaults
    );
}

#[test]
fn a_var_declared_with_another_type_recompiles_its_users() {
    let before = "/datum/holder\n\tvar/list/items = null\n/datum/holder/proc/count()\n\treturn items.len\n/datum/holder/proc/other()\n\treturn 1\n";
    let after = before.replace("var/list/items", "var/items");
    let changes = changes("retyped", before, &after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    assert_eq!(changes.retyped, ["items"]);
    // Whatever count compiles to now, other is left alone
    assert!(!reloaded(&changes).contains(&key("/datum/holder/other", 0)));
}

#[test]
fn vars_declared_another_way() {
    let base = "/datum/a\n\tvar/x = 1\n/datum/a/b\n/proc/f()\n\tvar/datum/a/a = new\n\treturn a.x\n/datum/a/proc/g()\n\treturn x\n";
    let redeclared = |changes: &Changes| -> Vec<(String, u32, Option<ConstValue>)> {
        assert!(changes.refused.is_empty(), "{:?}", changes.refused);
        changes
            .redeclared
            .iter()
            .map(|r| (r.var.clone(), r.flags, r.value.clone()))
            .collect()
    };

    // Converting to static moves the value into a global slot and
    // recompiles reads to use it.
    let changes = changes_of(
        "to-static",
        base,
        &base.replace("var/x = 1", "var/static/x = 2"),
    );
    assert_eq!(
        redeclared(&changes),
        [("x".to_owned(), 1, Some(ConstValue::Number(2.0)))]
    );
    assert_eq!(reloaded(&changes), [key("/datum/a/g", 0), key("/f", 0)]);
    assert!(changes.var_defaults.is_empty());

    let changes = changes_of(
        "to-const",
        base,
        &base.replace("var/x = 1", "var/const/x = 1"),
    );
    assert_eq!(
        redeclared(&changes),
        [("x".to_owned(), 3, Some(ConstValue::Number(1.0)))]
    );
    assert_eq!(reloaded(&changes), [key("/datum/a/g", 0), key("/f", 0)]);

    // Converting back restores per-type defaults, including subtype overrides.
    let before = base.replace("var/x = 1", "var/static/x = 1");
    let after = base.replace("/datum/a/b\n", "/datum/a/b\n\tx = 5\n");
    let changes = changes_of("from-static", &before, &after);
    assert!(changes.line_moves.is_empty(), "{:?}", changes.line_moves);
    assert_eq!(redeclared(&changes), [("x".to_owned(), 0, None)]);
    assert_eq!(reloaded(&changes), [key("/datum/a/g", 0), key("/f", 0)]);
    let mut defaults: Vec<(ConstValue, Vec<String>)> = changes
        .var_defaults
        .iter()
        .map(|d| (d.value.clone(), d.types.clone()))
        .collect();
    defaults.sort_by_key(|d| d.1.clone());
    defaults.dedup();
    assert_eq!(
        defaults,
        [
            (ConstValue::Number(1.0), vec!["/datum/a".to_owned()]),
            (ConstValue::Number(5.0), vec!["/datum/a/b".to_owned()])
        ]
    );

    // tmp changes flags only; existing static values are preserved.
    let changes = changes_of("to-tmp", base, &base.replace("var/x = 1", "var/tmp/x = 1"));
    assert_eq!(redeclared(&changes), [("x".to_owned(), 4, None)]);
    assert!(reloaded(&changes).is_empty());
    let changes = changes_of(
        "static-tmp",
        &before,
        &before.replace("var/static/x", "var/static/tmp/x"),
    );
    assert_eq!(redeclared(&changes), [("x".to_owned(), 5, None)]);

    // `as` is only for compiling
    let changes = changes_of("as", base, &base.replace("var/x = 1", "var/x = 1 as num"));
    assert!(redeclared(&changes).is_empty());
}

#[test]
fn type_macro_in_an_inherited_default_is_the_declaring_type() {
    let before = "/datum/a\n/datum/a/b\n";
    let after = "/datum/a\n\tvar/list/l = list(__TYPE__)\n/datum/a/b\n";
    let changes = changes("type-macro", before, after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    assert_eq!(changes.init_procs.len(), 2);
    for init in &changes.init_procs {
        let code = init.code.as_ref().unwrap();
        assert!(
            code.nodes.iter().any(|n| matches!(n, Node::Instruction(Instruction::PushVal(v), _) if format!("{:?}", v) == "Path(\"/datum/a\")")),
            "{}: {:?}",
            init.type_path,
            code.nodes
        );
    }
}

#[test]
fn spaceman_dmm_is_not_defined() {
    let code = |n: u32| {
        format!("/proc/a()\n#ifdef SPACEMAN_DMM\n\treturn 0\n#else\n\treturn {n}\n#endif\n")
    };
    let changes = changes("spaceman", &code(1), &code(2));
    assert_eq!(reloaded(&changes), [key("/a", 0)]);
    let code = dmasm::format(&changes.reloads[0].nodes);
    assert!(code.contains("PushInt 2"), "{}", code);
}

#[test]
fn default_arguments_can_change() {
    let changes = changes(
        "default-args",
        "/proc/a(x = 1)\n\treturn x\n",
        "/proc/a(x = 2)\n\treturn x\n",
    );
    assert_eq!(reloaded(&changes), [key("/a", 0)]);
}

fn refusal(name: &str, before: &str, after: &str) -> String {
    let changes = changes(name, before, after);
    assert!(
        !changes.refused.is_empty(),
        "nothing refused: {:?}",
        changes
    );
    changes.refused.join("\n")
}

#[test]
fn refuses_what_a_reload_cant_do() {
    let base = "/datum/thing\n\tvar/a = 1\n\tvar/list/l = list()\n/datum/thing/proc/f(x)\n\tset waitfor = FALSE\n\treturn x\n/obj/verb/v()\n\treturn\n";
    let cases = [
        (
            "new-type-builtin",
            format!("{base}/mob/other\n\tckey = \"someone\"\n"),
            "cannot set built-in variable ckey",
        ),
        (
            "setting",
            base.replace(
                "/obj/verb/v()\n\treturn",
                "/obj/verb/v()\n\tset src in usr.loc.contents\n\treturn",
            ),
            "unsupported `set src` expression",
        ),
        (
            "parse-error",
            base.replace("return x", "return (x"),
            "parse error",
        ),
    ];
    for (name, after, why) in cases.iter() {
        let refused = refusal(name, base, after);
        assert!(
            refused.contains(*why),
            "{}: expected {:?} in {:?}",
            name,
            why,
            refused
        );
    }
}

#[test]
fn builtin_var_defaults() {
    let before = "/obj/thing\n\tname = \"a\"\n/obj/thing/sub\n/obj/thing/own\n\tpixel_x = 2\n";
    let after = before.replace("\"a\"", "\"b\"").replace(
        "/obj/thing\n",
        "/obj/thing\n\tpixel_x = 5\n\tgender = \"male\"\n",
    );
    let changes = changes("builtin", before, &after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    let mut defaults: Vec<(String, ConstValue, Vec<String>)> = changes
        .builtin_defaults
        .iter()
        .map(|d| (d.var.clone(), d.value.clone(), d.types.clone()))
        .collect();
    defaults.sort_by(|a, b| a.0.cmp(&b.0));
    let both = vec!["/obj/thing".to_owned(), "/obj/thing/sub".to_owned()];
    let all = vec![
        "/obj/thing".to_owned(),
        "/obj/thing/own".to_owned(),
        "/obj/thing/sub".to_owned(),
    ];
    let sorted = |mut v: Vec<String>| {
        v.sort();
        v
    };
    let defaults: Vec<_> = defaults
        .into_iter()
        .map(|(var, value, types)| (var, value, sorted(types)))
        .collect();
    assert_eq!(
        defaults,
        [
            (
                "gender".to_owned(),
                ConstValue::String("male".to_owned()),
                all.clone()
            ),
            (
                "name".to_owned(),
                ConstValue::String("b".to_owned()),
                all.clone()
            ),
            ("pixel_x".to_owned(), ConstValue::Number(5.0), both),
            // (text is the first letter of the name, where it isn't set)
            ("text".to_owned(), ConstValue::String("b".to_owned()), all),
        ]
    );
    assert!(changes.var_defaults.is_empty());

    // Ones a reload can't set
    let refused = refusal("builtin-mob", "/mob/a\n", "/mob/a\n\tckey = \"someone\"\n");
    assert!(refused.contains("built-in default"), "{}", refused);
}

#[test]
fn new_procs_are_compiled() {
    let before = "/datum/thing/proc/f()\n\treturn 1\n/datum/thing/sub\n";
    let after = "/datum/thing/proc/f()\n\treturn 1\n/datum/thing/sub\n/proc/g()\n\treturn 2\n/datum/thing/proc/h(a, obj/b, c as num)\n\treturn a\n/datum/thing/sub/f()\n\treturn ..() + 1\n";
    let changes = changes("new-procs", before, after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    let mut new: Vec<(String, String, bool, String)> = changes
        .new_procs
        .iter()
        .map(|p| {
            (
                p.type_path.clone(),
                p.name.clone(),
                p.declared,
                p.code.proc.path.clone(),
            )
        })
        .collect();
    new.sort();
    assert_eq!(
        new,
        [
            ("".to_owned(), "g".to_owned(), true, "/g".to_owned()),
            (
                "/datum/thing".to_owned(),
                "h".to_owned(),
                true,
                "/datum/thing/h".to_owned()
            ),
            (
                "/datum/thing/sub".to_owned(),
                "f".to_owned(),
                false,
                "/datum/thing/sub/f".to_owned()
            ),
        ]
    );
    let h = changes.new_procs.iter().find(|p| p.name == "h").unwrap();
    let params: Vec<(String, u32)> = h
        .code
        .params
        .iter()
        .map(|p| (p.name.clone(), p.input))
        .collect();
    assert_eq!(
        params,
        [
            ("a".to_owned(), 0),
            ("b".to_owned(), 2),
            ("c".to_owned(), 8)
        ]
    );
}

#[test]
fn removed_procs_and_vars_stay() {
    let before =
        "/datum/thing\n\tvar/a = 1\n/datum/thing/proc/f()\n\treturn 1\n/proc/g()\n\treturn f()\n";
    let after = "/datum/thing\n/proc/g()\n\treturn 2\n";
    let changes = changes("removed", before, after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    let warnings = changes.warnings.join("\n");
    assert!(warnings.contains("var a was removed"), "{}", warnings);
    assert_eq!(changes.removed_procs.len(), 1);
    assert_eq!(changes.removed_procs[0].proc, key("/datum/thing/f", 0));
    assert_eq!(reloaded(&changes), [key("/g", 0)]);
}

#[test]
fn a_removed_var_a_subtype_overrode_stays() {
    // The subtype's override goes with the var: nothing to set there
    let before = "/datum/thing\n\tvar/a = 1\n/datum/thing/sub\n\ta = 2\n";
    let after = "/datum/thing\n/datum/thing/sub\n";
    let changes = changes("removed-override", before, after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    assert!(
        changes.warnings.join("\n").contains("var a was removed"),
        "{:?}",
        changes.warnings
    );
    assert!(
        changes.var_defaults.is_empty(),
        "{:?}",
        changes.var_defaults
    );
}

#[test]
fn removed_overrides_are_unlinked() {
    let before = "/datum/a/b\n\tvar/x = 1\n/datum/a/proc/f()\n\treturn 1\n/datum/a/b/f()\n\treturn 2\n/datum/a/b/f()\n\treturn 3\n/proc/g()\n\treturn 1\n";
    let changes = changes(
        "removed-last",
        before,
        &before.replace("/datum/a/b/f()\n\treturn 3\n", ""),
    );
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    let removed: Vec<(ProcKey, &str)> = changes
        .removed_procs
        .iter()
        .map(|r| (r.proc.clone(), r.type_path.as_str()))
        .collect();
    assert_eq!(removed, [(key("/datum/a/b/f", 1), "/datum/a/b")]);

    let changes = changes_of(
        "removed-both",
        before,
        &before.replace(
            "/datum/a/b/f()\n\treturn 2\n/datum/a/b/f()\n\treturn 3\n",
            "",
        ),
    );
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    assert_eq!(changes.removed_procs.len(), 2);

    // Remove an earlier definition and renumber the remaining overrides.
    let changes = changes_of(
        "removed-first",
        before,
        &before.replace("/datum/a/b/f()\n\treturn 2\n", ""),
    );
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    let removed: Vec<ProcKey> = changes
        .removed_procs
        .iter()
        .map(|r| r.proc.clone())
        .collect();
    assert_eq!(removed, [key("/datum/a/b/f", 0)]);
    assert!(reloaded(&changes).is_empty(), "{:?}", reloaded(&changes));

    // A global proc's definitions are only taken out of the numbering
    let changes = changes_of(
        "removed-global",
        before,
        &before.replace("/proc/g()\n\treturn 1\n", ""),
    );
    let removed: Vec<(ProcKey, &str)> = changes
        .removed_procs
        .iter()
        .map(|r| (r.proc.clone(), r.type_path.as_str()))
        .collect();
    assert_eq!(removed, [(key("/g", 0), "")]);
    assert!(changes
        .warnings
        .join("\n")
        .contains("/g: a definition was removed"));
}

#[test]
fn definitions_come_among_the_others() {
    let before =
        "/datum/a/proc/f()\n\treturn 1\n/datum/a/f()\n\treturn 2\n/datum/a/f()\n\treturn 3\n";

    // Insert a definition between existing ones. Requests still identify
    // existing procs by their old runtime indexes.
    let after = before.replace(
        "/datum/a/f()\n\treturn 2\n",
        "/datum/a/f()\n\treturn 9\n/datum/a/f()\n\treturn 2\n",
    );
    let changes = changes_of("inserted", before, &after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    let added: Vec<ProcKey> = changes
        .new_procs
        .iter()
        .map(|p| p.code.proc.clone())
        .collect();
    assert_eq!(added, [key("/datum/a/f", 1)]);
    assert!(changes.removed_procs.is_empty());
    assert!(reloaded(&changes).is_empty(), "{:?}", reloaded(&changes));
    let moved: Vec<ProcKey> = changes.line_moves.iter().map(|m| m.proc.clone()).collect();
    assert_eq!(moved, [key("/datum/a/f", 1), key("/datum/a/f", 2)]);

    // With a change to one after it, which the world knows by its old number
    let changes = changes_of(
        "inserted-and-changed",
        before,
        &after.replace("return 3", "return 4"),
    );
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    let added: Vec<ProcKey> = changes
        .new_procs
        .iter()
        .map(|p| p.code.proc.clone())
        .collect();
    assert_eq!(added, [key("/datum/a/f", 1)]);
    assert_eq!(reloaded(&changes), [key("/datum/a/f", 2)]);

    // Alignment prefers changing later definitions and appending one
    // over deleting and recreating several entries.
    let after = before.replace("/datum/a/f()\n\treturn 2\n", "").replace(
        "/datum/a/f()\n\treturn 3\n",
        "/datum/a/f()\n\treturn 3\n/datum/a/f()\n\treturn 5\n/datum/a/f()\n\treturn 6\n",
    );
    let changes = changes_of("swapped", before, &after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    assert!(changes.removed_procs.is_empty());
    let added: Vec<ProcKey> = changes
        .new_procs
        .iter()
        .map(|p| p.code.proc.clone())
        .collect();
    assert_eq!(added, [key("/datum/a/f", 3)]);
    assert_eq!(
        reloaded(&changes),
        [key("/datum/a/f", 1), key("/datum/a/f", 2)]
    );

    let cases = [
        (
            "verb-between",
            "/obj/verb/v()\n\treturn 1\n/obj/v()\n\treturn 2\n",
            "/obj/v()",
            "/obj/v",
            true,
        ),
        (
            "world-between",
            "/world/New()\n\treturn 1\n/world/New()\n\treturn 2\n",
            "/world/New()",
            "/world/New",
            false,
        ),
        (
            "global-between",
            "/proc/g()\n\treturn 1\n/g()\n\treturn 2\n",
            "/g()",
            "/g",
            false,
        ),
    ];
    for (name, before, head, path, verb) in cases {
        let second = format!("{head}\n\treturn 2\n");
        let after = before.replace(&second, &format!("{head}\n\treturn 9\n{second}"));
        let changes = changes_of(name, before, &after);
        assert!(
            changes.refused.is_empty(),
            "{}: {:?}",
            name,
            changes.refused
        );
        let added: Vec<(ProcKey, bool, bool)> = changes
            .new_procs
            .iter()
            .map(|p| (p.code.proc.clone(), p.declared, p.verb))
            .collect();
        assert_eq!(added, [(key(path, 1), false, verb)], "{}", name);
        assert!(reloaded(&changes).is_empty(), "{}", name);

        let changes = changes_of(
            name,
            &after,
            &after.replacen("\treturn 9\n", "\treturn 9\n\treturn 8\n", 1),
        );
        assert_eq!(reloaded(&changes), [key(path, 1)], "{}", name);

        let changes = changes_of(name, &after, before);
        let removed: Vec<(ProcKey, bool)> = changes
            .removed_procs
            .iter()
            .map(|r| (r.proc.clone(), r.verb))
            .collect();
        assert_eq!(removed, [(key(path, 1), verb)], "{}", name);
        assert!(reloaded(&changes).is_empty(), "{}", name);
    }
}

#[test]
fn var_defaults_go_to_the_subtypes_that_inherit_them() {
    let before =
        "/datum/a\n\tvar/x = 1\n\tvar/y = \"one\"\n/datum/a/b\n/datum/a/c\n\tx = 5\n/datum/a/c/d\n";
    let after = before
        .replace("var/x = 1", "var/x = 2")
        .replace("var/y = \"one\"", "var/y = /datum/a/b");
    let changes = changes("defaults", before, &after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    let mut defaults: Vec<(String, ConstValue, Vec<String>)> = changes
        .var_defaults
        .iter()
        .map(|d| (d.var.clone(), d.value.clone(), d.types.clone()))
        .collect();
    defaults.sort_by(|a, b| a.0.cmp(&b.0));
    for d in &mut defaults {
        d.2.sort();
    }
    assert_eq!(
        defaults,
        [
            (
                "x".to_owned(),
                ConstValue::Number(2.0),
                vec!["/datum/a".to_owned(), "/datum/a/b".to_owned()]
            ),
            (
                "y".to_owned(),
                ConstValue::Path("/datum/a/b".to_owned()),
                vec![
                    "/datum/a".to_owned(),
                    "/datum/a/b".to_owned(),
                    "/datum/a/c".to_owned(),
                    "/datum/a/c/d".to_owned()
                ]
            ),
        ]
    );
}

#[test]
fn global_var_values_are_left_with_a_warning() {
    let changes = changes("globals", "var/g = 1\n", "var/g = 2\n");
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    assert!(changes.var_defaults.is_empty());
    assert!(
        changes.warnings.join("").contains("after a restart"),
        "{:?}",
        changes.warnings
    );
}

#[test]
fn parameters_and_waitfor_can_change() {
    let before = "/proc/f(a)\n\treturn a\n";
    let after = "/proc/f(a, mob/b)\n\tset waitfor = 0\n\tset background = 1\n\treturn a\n";
    let changes = changes("signature", before, after);
    assert_eq!(reloaded(&changes), [key("/f", 0)]);
    let reload = &changes.reloads[0];
    let params: Vec<(String, u32)> = reload
        .params
        .iter()
        .map(|p| (p.name.clone(), p.input))
        .collect();
    assert_eq!(params, [("a".to_owned(), 0), ("b".to_owned(), 1)]);
    assert_eq!(
        reload.settings,
        Some(ProcSettings {
            waitfor: false,
            background: true,
            ..ProcSettings::default()
        })
    );
}

#[test]
fn new_vars_and_their_overrides() {
    let before = "/datum/a\n/datum/a/b\n/datum/a/c\n/datum/a/b/d\n";
    let after = "var/g = 3\n/datum/a\n\tvar/x = 1\n\tvar/static/s = \"t\"\n\tvar/tmp/t\n/datum/a/b\n\tx = 2\n/datum/a/c\n/datum/a/b/d\n";
    let changes = changes("new-vars", before, after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    let mut vars = changes.new_vars.clone();
    vars.sort_by(|a, b| a.var.cmp(&b.var));
    let var = |type_path: &str, var: &str, flags: u32, value: ConstValue| NewVar {
        type_path: type_path.to_owned(),
        var: var.to_owned(),
        flags,
        value,
        init: None,
    };
    assert_eq!(
        vars,
        [
            var("", "g", 1, ConstValue::Number(3.0)),
            var("/datum/a", "s", 1, ConstValue::String("t".to_owned())),
            var("/datum/a", "t", 4, ConstValue::Null),
            var("/datum/a", "x", 0, ConstValue::Number(1.0)),
        ]
    );
    let mut defaults: Vec<(String, ConstValue, Vec<String>)> = changes
        .var_defaults
        .iter()
        .map(|d| (d.var.clone(), d.value.clone(), d.types.clone()))
        .collect();
    defaults.sort_by_key(|d| (d.0.clone(), d.2.clone()));
    for d in &mut defaults {
        d.2.sort();
    }
    assert_eq!(
        defaults,
        [
            (
                "t".to_owned(),
                ConstValue::Null,
                vec![
                    "/datum/a".to_owned(),
                    "/datum/a/b".to_owned(),
                    "/datum/a/b/d".to_owned(),
                    "/datum/a/c".to_owned()
                ]
            ),
            (
                "x".to_owned(),
                ConstValue::Number(1.0),
                vec!["/datum/a".to_owned(), "/datum/a/c".to_owned()]
            ),
            (
                "x".to_owned(),
                ConstValue::Number(2.0),
                vec!["/datum/a/b".to_owned(), "/datum/a/b/d".to_owned()]
            ),
        ]
    );
}

#[test]
fn removing_an_override_goes_back_to_the_inherited_default() {
    let before = "/datum/a\n\tvar/x = 1\n/datum/a/b\n\tx = 2\n";
    let after = "/datum/a\n\tvar/x = 1\n/datum/a/b\n";
    let changes = changes("unoverride", before, after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    assert_eq!(changes.var_defaults.len(), 1);
    assert_eq!(changes.var_defaults[0].value, ConstValue::Number(1.0));
    assert_eq!(changes.var_defaults[0].types, ["/datum/a/b"]);
}

#[test]
fn static_locals_come_with_their_initial_values() {
    let before = "/datum/a/proc/f()\n\treturn 1\n";
    let after = "/datum/a/proc/f()\n\tvar/static/n = 3\n\tvar/static/list/l = list()\n\tif (n)\n\t\tvar/static/n = \"x\"\n\t\treturn n\n\treturn l\n";
    let changes = changes("statics", before, after);
    let statics: Vec<(String, String, Option<ConstValue>)> = changes.reloads[0]
        .statics
        .iter()
        .map(|s| (s.name.clone(), s.scope.clone(), s.value.clone()))
        .collect();
    assert_eq!(
        statics,
        [
            (
                "n".to_owned(),
                "proc:/datum/a/proc/f#0".to_owned(),
                Some(ConstValue::Number(3.0))
            ),
            ("l".to_owned(), "proc:/datum/a/proc/f#0".to_owned(), None),
            (
                "n".to_owned(),
                "proc:/datum/a/proc/f#1".to_owned(),
                Some(ConstValue::String("x".to_owned()))
            ),
        ]
    );
}

#[test]
fn another_definition_after_the_others_is_a_new_proc() {
    let before = "/datum/a/proc/f()\n\treturn 1\n";
    let after = "/datum/a/proc/f()\n\treturn 1\n/datum/a/f()\n\treturn ..() + 1\n/datum/a/f()\n\treturn ..() * 10\n";
    let changes = changes("second-definition", before, after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    let added: Vec<(ProcKey, bool)> = changes
        .new_procs
        .iter()
        .map(|p| (p.code.proc.clone(), p.declared))
        .collect();
    assert_eq!(
        added,
        [(key("/datum/a/f", 1), false), (key("/datum/a/f", 2), false)]
    );
}

#[test]
fn another_definition_of_a_global_proc_is_a_proc_of_its_own() {
    let before = "/proc/g()\n\treturn 1\n";
    let changes = changes(
        "global-redefinition",
        before,
        &format!("{before}/g()\n\treturn ..() + 1\n"),
    );
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    let added: Vec<(ProcKey, &str, bool)> = changes
        .new_procs
        .iter()
        .map(|p| (p.code.proc.clone(), p.type_path.as_str(), p.declared))
        .collect();
    assert_eq!(added, [(key("/g", 1), "", false)]);
    // DreamMaker's calls go to the first definition
    assert!(
        changes.warnings.iter().any(|w| w.contains("unreachable")),
        "{:?}",
        changes.warnings
    );
}

#[test]
fn in_lists_are_the_words_dreammaker_writes() {
    // What DreamMaker 516.1688 wrote for each
    let cases = [
        ("mob/a in view()", Some(0x7D01)),
        ("mob/a in view(3)", Some(0x0301)),
        ("mob/a in oview()", Some(0x7D02)),
        ("mob/a in oview(1, usr)", Some(0x0102)),
        ("mob/a in range()", Some(0x7D05)),
        ("mob/a in range(2)", Some(0x0205)),
        ("mob/a in orange()", Some(0x7D06)),
        ("mob/a in orange(2)", Some(0x0206)),
        ("mob/a in world", Some(0x7F10)),
        ("obj/a in usr", Some(0x7F08)),
        ("obj/a in usr.loc", Some(0x7F03)),
        ("obj/a in usr.contents", Some(0x7F08)),
        ("mob/a in view(usr)", Some(0x7D01)),
        ("a", Some(0x7D01)),
        ("obj/a in src", None),
        ("mob/a in view(src)", None),
        ("a in list(1, 2)", None),
    ];
    let code: String = cases
        .iter()
        .enumerate()
        .map(|(i, (param, _))| format!("/mob/proc/p{i}({param})\n\treturn\n"))
        .collect();
    let world = World::new("in-lists", &code);
    let snapshot = world.parse();
    for (i, (param, word)) in cases.iter().enumerate() {
        let reload = snapshot
            .compile_reload(&key(&format!("/mob/p{i}"), 0))
            .unwrap();
        let expected = match word {
            Some(word) => InList::Builtin(*word),
            None => InList::Kept,
        };
        assert_eq!(reload.params[0].in_list, expected, "{}", param);
    }
}

#[test]
fn in_list_expressions_are_kept_or_compiled() {
    let before = "/mob/proc/f(obj/a in src.contents, b)\n\treturn 1\n";
    let changes = changes(
        "in-list-kept",
        before,
        "/mob/proc/f(obj/a in src.contents, b, c)\n\treturn 2\n",
    );
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    assert_eq!(changes.reloads[0].params[0].in_list, InList::Kept);

    // A new one is compiled into a proc that returns the list, on the type
    let changes = changes_of(
        "in-list-changed",
        before,
        "/mob/proc/f(obj/a in src.loc, b)\n\treturn 1\n/mob/proc/g(n in list(1, 2))\n\treturn n\n",
    );
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    let InList::New(code) = &changes.reloads[0].params[0].in_list else {
        panic!("{:?}", changes.reloads[0].params[0].in_list);
    };
    assert!(
        dmasm::format(&code.nodes).contains("cache = src; cache[\"loc\"]"),
        "{}",
        dmasm::format(&code.nodes)
    );
    let InList::New(code) = &changes.new_procs[0].code.params[0].in_list else {
        panic!("{:?}", changes.new_procs[0].code.params[0].in_list);
    };
    assert!(
        dmasm::format(&code.nodes).contains("NewList 2"),
        "{}",
        dmasm::format(&code.nodes)
    );
    // With its file and line, as DreamMaker gives a DEBUG build's
    let text = dmasm::format(&code.nodes);
    assert!(
        text.starts_with("DbgFile") && text.contains("DbgLine 3"),
        "{}",
        text
    );
}

#[test]
fn defaults_that_arent_constants_change_the_init_procs() {
    let before = "/datum/a\n\tvar/list/l = list(1)\n\tvar/n = 2\n/datum/a/b\n/datum/a/c\n\tl = list(3)\n/datum/a/d\n\tvar/list/m = list()\n/datum/e\n";
    let after = before.replace("list(1)", "list(1, 2)");
    let first = changes("init-procs", before, &after);
    assert!(first.refused.is_empty(), "{:?}", first.refused);
    let mut types: Vec<&str> = first
        .init_procs
        .iter()
        .map(|i| i.type_path.as_str())
        .collect();
    types.sort();
    assert_eq!(types, ["/datum/a", "/datum/a/b", "/datum/a/d"]);
    assert!(first.init_procs.iter().all(|i| i.code.is_some()));
    assert!(first.var_defaults.is_empty());

    // A new var whose default isn't a constant, and one that becomes a constant
    let after = before
        .replace("\tvar/n = 2\n", "\tvar/n = 2\n\tvar/list/k = list(\"k\")\n")
        .replace("l = list(3)", "l = null");
    let second = changes("init-procs-2", before, &after);
    assert!(second.refused.is_empty(), "{:?}", second.refused);
    assert_eq!(second.new_vars.len(), 1);
    assert_eq!(second.new_vars[0].value, ConstValue::Null);
    let mut types: Vec<&str> = second
        .init_procs
        .iter()
        .map(|i| i.type_path.as_str())
        .collect();
    types.sort();
    assert_eq!(
        types,
        ["/datum/a", "/datum/a/b", "/datum/a/c", "/datum/a/d"]
    );
    assert_eq!(second.var_defaults.len(), 1, "{:?}", second.var_defaults);
    assert_eq!(second.var_defaults[0].types, ["/datum/a/c"]);
    // New computed variables have null initial() values; report the limitation.
    assert!(
        second
            .warnings
            .iter()
            .any(|w| w.contains("new var k has a computed default")
                && w.contains("initial() remain")),
        "{:?}",
        second.warnings
    );
}

#[test]
fn new_types_with_their_vars_and_procs() {
    let before = "/obj/thing\n\tvar/a = 1\n/obj/thing/proc/f()\n\treturn 1\n/obj/named\n\tname = \"Named\"\n";
    let after = format!(
        "{before}/obj/thing/my_new\n\tvar/b = 2\n\tvar/list/c = list(3)\n\ta = 5\n\ticon_state = \"s\"\n\tdensity = 1\n\
         /obj/thing/my_new/f()\n\treturn 2\n/obj/thing/my_new/proc/g()\n\treturn 3\n/obj/thing/my_new/deeper\n/obj/named/kid\n/datum/d\n"
    );
    let changes = changes("new-types", before, &after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    type Fields = Vec<(String, ConstValue)>;
    let types: Vec<(&str, &str, Fields)> = changes
        .new_types
        .iter()
        .map(|t| {
            (t.path.as_str(), t.parent.as_str(), {
                let mut f = t.fields.clone();
                f.sort_by(|a, b| a.0.cmp(&b.0));
                f
            })
        })
        .collect();
    let s = |v: &str| ConstValue::String(v.to_owned());
    assert_eq!(
        types,
        [
            (
                "/datum/d",
                "/datum",
                vec![("name".to_owned(), s("d")), ("text".to_owned(), s("d"))]
            ),
            ("/obj/named/kid", "/obj/named", vec![]),
            (
                "/obj/thing/my_new",
                "/obj/thing",
                vec![
                    ("density".to_owned(), ConstValue::Number(1.0)),
                    ("icon_state".to_owned(), s("s")),
                    ("name".to_owned(), s("my new")),
                    ("text".to_owned(), s("m")),
                ]
            ),
            (
                "/obj/thing/my_new/deeper",
                "/obj/thing/my_new",
                vec![
                    ("name".to_owned(), s("deeper")),
                    ("text".to_owned(), s("d"))
                ]
            ),
        ]
    );
    let mut vars: Vec<&str> = changes.new_vars.iter().map(|v| v.var.as_str()).collect();
    vars.sort();
    assert_eq!(vars, ["b", "c"]);
    let mut defaults: Vec<(&str, Vec<String>)> = changes
        .var_defaults
        .iter()
        .map(|d| (d.var.as_str(), d.types.clone()))
        .collect();
    defaults.sort();
    assert_eq!(
        defaults,
        [
            (
                "a",
                vec![
                    "/obj/thing/my_new".to_owned(),
                    "/obj/thing/my_new/deeper".to_owned()
                ]
            ),
            (
                "b",
                vec![
                    "/obj/thing/my_new".to_owned(),
                    "/obj/thing/my_new/deeper".to_owned()
                ]
            ),
        ]
    );
    let mut procs: Vec<(String, bool)> = changes
        .new_procs
        .iter()
        .map(|p| (p.code.proc.path.clone(), p.declared))
        .collect();
    procs.sort();
    assert_eq!(
        procs,
        [
            ("/obj/thing/my_new/f".to_owned(), false),
            ("/obj/thing/my_new/g".to_owned(), true)
        ]
    );
    let mut inits: Vec<&str> = changes
        .init_procs
        .iter()
        .map(|i| i.type_path.as_str())
        .collect();
    inits.sort();
    assert_eq!(inits, ["/obj/thing/my_new", "/obj/thing/my_new/deeper"]);
}

#[test]
fn globals_and_statics_that_arent_constants_come_with_code_to_run_once() {
    let before = "/datum/a\n/datum/a/proc/f()\n\treturn 1\n";
    let after = "var/list/g = list(1, 2)\n/datum/a\n\tvar/static/list/s = list(3)\n/datum/a/proc/f()\n\tvar/static/datum/a/l = new\n\treturn l\n";
    let changes = changes("run-once", before, after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    for var in &changes.new_vars {
        assert_eq!(var.value, ConstValue::Null);
        let init = var
            .init
            .as_ref()
            .unwrap_or_else(|| panic!("no init for {}", var.var));
        assert!(init
            .nodes
            .iter()
            .any(|n| matches!(n, Node::Instruction(Instruction::Ret, _))));
    }
    assert_eq!(changes.new_vars.len(), 2);
    let statics = &changes.reloads[0].statics;
    assert_eq!(statics.len(), 1);
    assert!(statics[0].value.is_none() && statics[0].init.is_some());
}

#[test]
fn verb_settings_are_the_words_dreammaker_writes() {
    // (code, flags, packed word) as DreamMaker 516.1688 wrote them
    let cases: &[(&str, u32, u32)] = &[
        ("", 0x4, 0xFF),
        ("set src = usr", 0x6, 0x20FF),
        ("set src = view()", 0x6, 0x017D),
        ("set src in oview(1)", 0x4, 0x0201),
        ("set src in world", 0x4, 0x107F),
        ("set src = usr.loc", 0x6, 0x03FF),
        ("set src in usr.contents", 0x4, 0x087F),
        ("set invisibility = 0", 0x8C, 0xFF00FF),
        ("set invisibility = 1", 0x9C, 0x0100FF),
        ("set invisibility = 101", 0x9C, 0x6500FF),
        ("set popup_menu = 0", 0x44, 0xFF),
        ("set instant = 1", 0x284, 0xFF),
        ("set hidden = 1\n\tset popup_menu = 0", 0x45, 0xFF),
        ("set background = 1", 0x184, 0xFF),
        ("set waitfor = 0", 0x0, 0xFF),
        ("set src = usr.contents", 0x6, 0x087F),
        ("set src in usr.loc", 0x4, 0x037F),
        ("set src in view(usr, 1)", 0x4, 0x0101),
        ("set src in view(0)", 0x4, 0x0100),
        ("set category = null", 0x24, 0xFF),
    ];
    let code: String = cases
        .iter()
        .enumerate()
        .map(|(i, (set, _, _))| format!("/mob/verb/v{i}()\n\t{set}\n\treturn\n"))
        .collect();
    let world = World::new("verb-settings", &format!("{code}/mob/verb/named()\n\tset name = \"Named Verb\"\n\tset category = \"Cat\"\n\tset desc = \"a desc\"\n\treturn\n"));
    let snapshot = world.parse();
    for (i, (set, flags, packed)) in cases.iter().enumerate() {
        let settings = snapshot.settings(&key(&format!("/mob/v{i}"), 0)).unwrap();
        assert_eq!(
            (settings.flags(), settings.packed()),
            (*flags, *packed),
            "{}",
            set
        );
    }
    let named = snapshot.settings(&key("/mob/named", 0)).unwrap();
    assert_eq!(
        (
            named.name.as_deref(),
            named.desc.as_deref(),
            named.category.as_deref()
        ),
        (Some("Named Verb"), Some("a desc"), Some("Cat"))
    );
}

#[test]
fn new_verbs_and_changed_settings() {
    let before = "/mob/sub\n/mob/verb/a()\n\treturn\n/mob/proc/b()\n\tset name = \"B\"\n\treturn\n";
    let after = "/mob/sub\n/mob/verb/a()\n\treturn\n/mob/proc/b()\n\tset name = \"Bee\"\n\tset category = \"Admin\"\n\treturn\n/mob/verb/c()\n\treturn\n/mob/sub/a()\n\treturn ..()\n";
    let changes = changes("new-verbs", before, after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    let mut added: Vec<(String, bool, bool)> = changes
        .new_procs
        .iter()
        .map(|p| (p.code.proc.path.clone(), p.declared, p.verb))
        .collect();
    added.sort();
    assert_eq!(
        added,
        [
            ("/mob/c".to_owned(), true, true),
            ("/mob/sub/a".to_owned(), false, true)
        ]
    );
    assert_eq!(changes.reloads.len(), 1);
    assert_eq!(
        changes.reloads[0]
            .settings
            .as_ref()
            .unwrap()
            .name
            .as_deref(),
        Some("Bee")
    );
}

#[test]
fn settings_come_from_the_declaration() {
    let code = |set: &str| {
        format!("/mob/verb/a()\n\tset name = \"A \\\"verb\\\"\"\n\tset category = \"Cat\"\n\t{set}\n\treturn\n/mob/sub/a()\n\tset background = 1\n\treturn\n/mob/sub/sub/a()\n\treturn\n")
    };
    let world = World::new("settings-inherited", &code("set waitfor = 0"));
    let snapshot = world.parse();
    let settings = |path: &str| snapshot.settings(&key(path, 0)).unwrap();
    assert_eq!(settings("/mob/a").name.as_deref(), Some("A \"verb\""));
    // Overrides inherit declaration settings plus their own settings.
    // They do not pass their settings to subsequent overrides.
    let sub = settings("/mob/sub/a");
    assert_eq!(
        (
            sub.name.as_deref(),
            sub.category.as_deref(),
            sub.waitfor,
            sub.background
        ),
        (Some("A \"verb\""), Some("Cat"), false, true)
    );
    let subsub = settings("/mob/sub/sub/a");
    assert_eq!((subsub.waitfor, subsub.background), (false, false));

    // Changing the declaration updates override settings without replacing code.
    let changes = changes(
        "settings-inherited",
        &code("set waitfor = 0"),
        &code("set waitfor = 1"),
    );
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    assert_eq!(reloaded(&changes), [key("/mob/a", 0)]);
    assert!(changes.reloads[0].settings.as_ref().unwrap().waitfor);
    let mut changed: Vec<(String, bool)> = changes
        .settings
        .iter()
        .map(|c| (c.proc.path.clone(), c.settings.waitfor))
        .collect();
    changed.sort();
    assert_eq!(
        changed,
        [
            ("/mob/sub/a".to_owned(), true),
            ("/mob/sub/sub/a".to_owned(), true)
        ]
    );

    // Body-only changes preserve settings.
    let changes = changes_of(
        "settings-inherited",
        &code(""),
        &code("").replace("/mob/sub/sub/a()\n\treturn", "/mob/sub/sub/a()\n\treturn 1"),
    );
    assert_eq!(reloaded(&changes), [key("/mob/sub/sub/a", 0)]);
    assert_eq!(changes.reloads[0].settings, None);
}

#[test]
fn one_file_parsed_again_on_its_own() {
    let before = "#define TWO 2\n/datum/a\n\tvar/x = 1\n/datum/a/proc/f()\n\treturn TWO\n\n/datum/a/proc/g()\n\treturn 3\n";
    let world = World::new("one-file", before);
    let file = std::path::Path::new("code/code.dm");
    let edited: HashSet<PathBuf> = vec![PathBuf::from("code/code.dm")].into_iter().collect();
    let mut old = world.parse();

    // Incremental and full parses must produce the same changes.
    let after = before.replace("\treturn TWO\n", "\tvar/y = TWO\n\treturn y\n");
    world.write(&after);
    let reload = old.reload_file(&world.dir, file).unwrap();
    let full = diff(&old, &world.parse(), Some(&edited));
    assert!(
        reload.changes.refused.is_empty(),
        "{:?}",
        reload.changes.refused
    );
    assert_eq!(reloaded(&reload.changes), [key("/datum/a/f", 0)]);
    assert_eq!(reload.changes.reloads[0].nodes, full.reloads[0].nodes);
    assert_eq!(reload.changes.line_moves, full.line_moves);
    assert_eq!(reload.changes.line_moves[0].proc, key("/datum/a/g", 0));
    old.keep(reload);

    // The next edit is to the code as it was kept
    let again = after.replace("return 3", "return 4");
    world.write(&again);
    let reload = old.reload_file(&world.dir, file).unwrap();
    assert_eq!(reloaded(&reload.changes), [key("/datum/a/g", 0)]);
    assert!(reload.changes.line_moves.is_empty());
    old.keep(reload);

    // Non-body changes require a full project parse.
    for (name, code, why) in [
        ("var", again.replace("var/x = 1", "var/x = 2"), "vars"),
        (
            "macro",
            again.replace("#define TWO 2", "#define TWO 3"),
            "macros",
        ),
        (
            "proc",
            format!("{}/datum/a/proc/h()\n\treturn 5\n", again),
            "declarations",
        ),
        (
            "override",
            format!("{}/datum/a/f()\n\treturn 5\n", again),
            "added",
        ),
        ("type", format!("{}/datum/b\n", again), "new type"),
        ("parameters", again.replace("g()", "g(z)"), "parameters"),
        (
            "setting",
            again.replace("\treturn 4", "\tset waitfor = FALSE\n\treturn 4"),
            "`set`s",
        ),
    ] {
        world.write(&code);
        match old.reload_file(&world.dir, file) {
            Ok(_) => panic!("{}: reloaded on its own", name),
            Err(e) => assert!(e.contains(why), "{}: {}", name, e),
        }
    }
}

#[test]
fn a_changed_default_recompiles_type_scope_users() {
    // `/datum/thing::size` is the default, compiled into the code
    let before = "/datum/thing\n\tvar/size = 2\n/datum/thing/big\n/proc/a()\n\treturn /datum/thing/big::size\n/proc/b()\n\treturn 2\n";
    let after = before.replace("/datum/thing/big\n", "/datum/thing/big\n\tsize = 9\n");
    let changes = changes("type-scope-default", before, &after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    assert_eq!(changes.defaults_changed, ["size"]);
    assert_eq!(reloaded(&changes), [key("/a", 0)]);
}

#[test]
fn a_new_parent_type_is_a_reparent_and_nothing_else() {
    let before = "/datum/a\n/datum/b\n/datum/a/c\n\tvar/x = 1\n";
    let after = "/datum/a\n/datum/b\n/datum/a/c\n\tparent_type = /datum/b\n\tvar/x = 1\n";
    let changes = changes("parent-type", before, after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    assert_eq!(changes.reparented.len(), 1, "{:?}", changes.reparented);
    assert!(
        changes.var_defaults.is_empty() && changes.builtin_defaults.is_empty(),
        "{:?}",
        changes
    );
    // (parent_type is a built-in const, not a global to set)
    assert!(changes.redeclared.is_empty(), "{:?}", changes.redeclared);
}

#[test]
fn a_removed_name_goes_back_to_each_types_own() {
    let before =
        "/obj/thing\n\tname = \"box\"\n/obj/thing/big_one\n/obj/thing/small\n\ttext = \"s\"\n";
    let after = "/obj/thing\n/obj/thing/big_one\n/obj/thing/small\n\ttext = \"s\"\n";
    let changes = changes("removed-name", before, after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    let mut set: Vec<(String, String, ConstValue)> = changes
        .builtin_defaults
        .iter()
        .flat_map(|d| {
            d.types
                .iter()
                .map(move |t| (t.clone(), d.var.clone(), d.value.clone()))
        })
        .collect();
    set.sort_by(|a, b| (&a.0, &a.1).cmp(&(&b.0, &b.1)));
    let s = |t: &str| ConstValue::String(t.to_owned());
    assert_eq!(
        set,
        [
            ("/obj/thing".to_owned(), "name".to_owned(), s("thing")),
            ("/obj/thing".to_owned(), "text".to_owned(), s("t")),
            (
                "/obj/thing/big_one".to_owned(),
                "name".to_owned(),
                s("big one")
            ),
            ("/obj/thing/big_one".to_owned(), "text".to_owned(), s("b")),
            ("/obj/thing/small".to_owned(), "name".to_owned(), s("small")),
        ]
    );
}

/// The changes when types.dm goes from `before` to `after`, code.dm (which
/// includes it) being `code`; only types.dm counts as edited.
fn types_changes(name: &str, code: &str, before: &str, after: &str) -> Changes {
    let world = World::new(name, &format!("{code}#include \"types.dm\"\n"));
    std::fs::write(world.dir.join("code/types.dm"), before).unwrap();
    let old = world.parse();
    std::fs::write(world.dir.join("code/types.dm"), after).unwrap();
    let new = world.parse();
    let edited: HashSet<PathBuf> = vec![PathBuf::from("code/types.dm")].into_iter().collect();
    diff(&old, &new, Some(&edited))
}

#[test]
fn a_type_scope_default_that_moves_moves_its_users_lines() {
    // Moving a default updates :: users in files that were not edited.
    let code = "\n\n\n/proc/f()\n\treturn /datum/u::x\n";
    let changes = types_changes(
        "moved-default",
        code,
        "/datum/u\n\tvar/x = 3\n",
        "\n/datum/u\n\tvar/x = 3\n",
    );
    assert!(changes.reloads.is_empty(), "{:?}", reloaded(&changes));
    assert_eq!(changes.line_moves.len(), 1, "{:?}", changes.line_moves);
    assert_eq!(changes.line_moves[0].proc, key("/f", 0));
    assert_eq!(changes.line_moves[0].lines, [(2, 3), (5, 5)]);
}

#[test]
fn a_type_scope_default_whose_lines_clash_is_compiled_again() {
    // The proc and default share a line number in different files.
    // A single line-number mapping cannot update just one; recompile.
    let changes = types_changes(
        "clashing-default",
        "/proc/f()\n\treturn /datum/u::x\n",
        "/datum/u\n\tvar/x = 3\n",
        "\n/datum/u\n\tvar/x = 3\n",
    );
    assert_eq!(reloaded(&changes), [key("/f", 0)]);
    assert!(changes.line_moves.is_empty(), "{:?}", changes.line_moves);
}

#[test]
fn a_type_scope_default_moved_to_another_file_is_compiled_again() {
    // Its debug info names the file the default is in now
    let world = World::new(
        "default-to-code",
        "#include \"types.dm\"\n/proc/f()\n\treturn /datum/u::x\n",
    );
    std::fs::write(world.dir.join("code/types.dm"), "/datum/u\n\tvar/x = 3\n").unwrap();
    let old = world.parse();
    std::fs::write(world.dir.join("code/types.dm"), "").unwrap();
    world.write("#include \"types.dm\"\n/proc/f()\n\treturn /datum/u::x\n/datum/u\n\tvar/x = 3\n");
    let new = world.parse();
    let changes = diff(&old, &new, None);
    assert_eq!(reloaded(&changes), [key("/f", 0)]);
}

#[test]
fn a_moved_default_read_with_scope_needs_the_full_read() {
    // A moved default referenced through :: requires a full parse
    // to update dependent debug locations.
    let world = World::new(
        "moved-default-one-file",
        "/proc/g()\n\treturn 1\n/datum/u\n\tvar/x = 3\n/datum/w\n\tvar/name = \"w\"\n/proc/f()\n\treturn /datum/u::x\n",
    );
    let file = std::path::Path::new("code/code.dm");
    let mut old = world.parse();
    world.write("/proc/g()\n\tworld.log << 0\n\treturn 1\n/datum/u\n\tvar/x = 3\n/datum/w\n\tvar/name = \"w\"\n/proc/f()\n\treturn /datum/u::x\n");
    match old.reload_file(&world.dir, file) {
        Ok(_) => panic!("reloaded on its own"),
        Err(e) => assert!(
            e.contains("/f reads a var default with :: that moved"),
            "{e}"
        ),
    }
    // An unreferenced moved default can use the single-file path.
    let world = World::new(
        "moved-unread-default",
        "/proc/g()\n\treturn 1\n/datum/w\n\tvar/name = \"w\"\n/proc/f(obj/o)\n\treturn o::name\n",
    );
    let mut old = world.parse();
    world.write("/proc/g()\n\tworld.log << 0\n\treturn 1\n/datum/w\n\tvar/name = \"w\"\n/proc/f(obj/o)\n\treturn o::name\n");
    let reload = old.reload_file(&world.dir, file).unwrap();
    assert_eq!(reloaded(&reload.changes), [key("/g", 0)]);
}

#[test]
fn a_constant_beyond_32_bits_at_a_bit_operator_is_warned_about() {
    // Warn for the intentional difference in out-of-range bit conversion.
    let changes = changes(
        "big-bits",
        "/proc/f(x)\n\treturn x\n",
        "/proc/f(x)\n\treturn (-1e10) ^ x\n",
    );
    assert_eq!(reloaded(&changes), [key("/f", 0)]);
    assert!(
        changes
            .warnings
            .iter()
            .any(|w| w.starts_with("/f: line 2: bit operand -10000000000")),
        "{:?}",
        changes.warnings
    );
    let changes = changes_of(
        "small-bits",
        "/proc/f(x)\n\treturn x\n",
        "/proc/f(x)\n\treturn (-1e6) ^ x\n",
    );
    assert!(changes.warnings.is_empty(), "{:?}", changes.warnings);
}

#[test]
fn a_static_local_s_initializer_names_its_own_proc() {
    // Static initializers such as tgstation verb registration use the containing proc's `__PROC__`.
    let before = "/datum/a\n/datum/a/proc/f()\n\treturn 1\n";
    let after = "/proc/reg(p)\n\treturn p\n/datum/a\n/datum/a/proc/f()\n\tvar/static/r = reg(__PROC__)\n\treturn 1\n";
    let changes = changes("static-proc", before, after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    let reload = changes
        .reloads
        .iter()
        .find(|r| r.proc.path == "/datum/a/proc/f" || r.proc.path == "/datum/a/f")
        .expect("f is reloaded");
    let init = reload.statics[0].init.as_ref().expect("code to run once");
    let pushed: Vec<String> = init
        .nodes
        .iter()
        .filter_map(|n| match n {
            Node::Instruction(Instruction::PushVal(dmasm::Value::Path(p)), _) => Some(p.clone()),
            _ => None,
        })
        .collect();
    assert!(
        pushed.iter().any(|p| p == "/datum/a/proc/f"),
        "__PROC__ gave {:?}",
        pushed
    );
}

#[test]
fn what_dreammaker_refuses_to_build_is_refused() {
    let before = "var/x = 1\n/datum/a\n/datum/a/b\n/datum/c\n\tvar/list/l = list(1)\n";
    // Reject conflicting declarations on a type and its subtype.
    for (name, after) in [(
        "dup-down",
        before
            .replace("/datum/a\n", "/datum/a\n\tvar/v = 1\n")
            .replace("/datum/a/b\n", "/datum/a/b\n\tvar/v = 2\n"),
    )] {
        let changes = changes(name, before, &after);
        assert!(
            changes
                .refused
                .iter()
                .any(|r| r.contains("v: duplicate definition")),
            "{:?}",
            changes.refused
        );
    }
    // Follow parent types rather than path prefixes: /obj inherits from /atom/movable.
    let obj = "/obj/item\n\tvar/foo = 1\n";
    let refused = changes(
        "dup-parents",
        obj,
        &format!("{obj}/atom/movable\n\tvar/foo = 2\n"),
    )
    .refused;
    assert!(
        refused
            .iter()
            .any(|r| r.contains("foo: duplicate definition (/obj/item declares it too)")),
        "{:?}",
        refused
    );
    // Globals may share a name with type vars.
    let changes_ok = changes(
        "dup-global",
        before,
        &before.replace("/datum/a\n", "/datum/a\n\tvar/x = 2\n"),
    );
    assert!(changes_ok.refused.is_empty(), "{:?}", changes_ok.refused);
    // Reject non-constant values in new consts and vars changed to const.
    let new_const = before.replace("/datum/a\n", "/datum/a\n\tvar/const/list/k = list(1, 2)\n");
    let refused = changes("const-new", before, &new_const).refused;
    assert!(
        refused
            .iter()
            .any(|r| r.contains("constant initializer required")),
        "{:?}",
        refused
    );
    let to_const = before.replace("var/list/l", "var/const/list/l");
    let refused = changes("const-to", before, &to_const).refused;
    assert!(
        refused
            .iter()
            .any(|r| r.contains("constant initializer required")),
        "{:?}",
        refused
    );
    // Reject non-constant replacements for existing const values.
    let had_const = "var/const/g = 1\n/datum/a\n\tvar/const/k = 1\n";
    for (name, after) in [
        ("const-list", had_const.replace("k = 1", "k = list(1, 2)")),
        ("const-new", had_const.replace("k = 1", "k = new /datum")),
        ("const-global", had_const.replace("g = 1", "g = list(1, 2)")),
    ] {
        let refused = changes(name, had_const, &after).refused;
        assert!(
            refused
                .iter()
                .any(|r| r.starts_with("code/code.dm:")
                    && r.contains("constant initializer required")),
            "{name}: {:?}",
            refused
        );
    }
    // Allow constant replacements and expressions using other consts.
    let had_const = format!("{had_const}\tvar/const/k2 = k + 1\n");
    for after in [
        had_const.replace("k = 1", "k = 2"),
        had_const.replace("k + 1", "k + 2"),
        had_const.replace("g = 1", "g = 2"),
    ] {
        let refused = changes("const-other", &had_const, &after).refused;
        assert!(refused.is_empty(), "{:?}", refused);
    }
    // Constructors are invalid const values, but valid for non-const vars.
    for value in [
        "matrix()",
        "icon('x.dmi')",
        "sound('x.ogg')",
        "file(\"x\")",
        "newlist(/datum)",
        "vector(1, 2)",
        "generator(\"num\", 0, 1)",
        "filter(type = \"blur\")",
    ] {
        let new_const = before.replace(
            "/datum/a\n",
            &format!("/datum/a\n\tvar/const/k = {value}\n"),
        );
        let refused = changes("const-call", before, &new_const).refused;
        assert!(
            refused
                .iter()
                .any(|r| r.contains("constant initializer required")),
            "{value}: {:?}",
            refused
        );
        let plain = before.replace("/datum/a\n", &format!("/datum/a\n\tvar/k = {value}\n"));
        let changes_ok = changes("plain-call", before, &plain);
        assert!(
            !changes_ok
                .refused
                .iter()
                .any(|r| r.contains("constant initializer")),
            "{value}: {:?}",
            changes_ok.refused
        );
    }
    // Numbers, text and paths are valid constants.
    let fine = before.replace(
        "/datum/a\n",
        "/datum/a\n\tvar/const/n = 1 << 3\n\tvar/const/t = \"text\"\n\tvar/const/p = /datum/c\n",
    );
    let changes_ok = changes("const-ok", before, &fine);
    assert!(changes_ok.refused.is_empty(), "{:?}", changes_ok.refused);
}

#[test]
fn consts_dreammaker_folds_are_taken_and_those_it_refuses_warned_about() {
    let before = "var/const/gconst = 3\n/datum/a\n/datum/c\n\tvar/const/c1 = 1\n";
    // Accept constants DreamMaker folds beyond SpacemanDMM's evaluator.
    for value in [
        "sqrt(4)",
        "round(1.5)",
        "abs(-1)",
        "md5(\"x\")",
        "global.gconst",
        "1 in list(1)",
        "2 in list(1, \"a\" = 3)",
        "floor(sqrt(4)) * 2",
    ] {
        let after = before.replace(
            "/datum/a\n",
            &format!("/datum/a\n\tvar/const/k = {value}\n"),
        );
        let got = changes("const-folded", before, &after);
        assert!(got.refused.is_empty(), "{value}: {:?}", got.refused);
    }
    // Non-const vars may use recovered consts.
    let after = before.replace(
        "/datum/a\n",
        "/datum/a\n\tvar/const/k = sqrt(16)\n\tvar/y = k + 1\n",
    );
    let got = changes("const-used", before, &after);
    assert!(got.refused.is_empty(), "{:?}", got.refused);
    // Check the folded values against DreamMaker.
    let world = World::new(
        "const-values",
        &before.replace(
            "/datum/a\n",
            "/datum/a\n\tvar/const/k = sqrt(16)\n\tvar/const/k2 = k + 1\n\tvar/const/h = md5(\"x\")\n\tvar/const/i = 3 in list(1, 3)\n",
        ),
    );
    let snapshot = world.parse();
    let a = snapshot.tree().find("/datum/a").unwrap();
    let value = |name: &str| {
        a.get()
            .vars
            .get(name)
            .and_then(|v| v.value.constant.clone())
            .map(|c| c.to_string())
    };
    assert_eq!(value("k").as_deref(), Some("4"));
    assert_eq!(value("k2").as_deref(), Some("5"));
    assert_eq!(
        value("h").as_deref(),
        Some("\"9dd4e461268c8034f5c8564e155c67a6\"")
    );
    assert_eq!(value("i").as_deref(), Some("1"));
    // Resolve chains of const dependencies.
    let after = before.replace(
        "/datum/a\n",
        "/datum/a\n\tvar/const/k = sqrt(16)\n\tvar/const/k2 = k + 1\n",
    );
    let got = changes("const-chain", before, &after);
    assert!(got.refused.is_empty(), "{:?}", got.refused);
    // Warn when SpacemanDMM accepts an initializer DreamMaker rejects.
    for (value, why) in [
        ("type", "it reads type"),
        ("parent_type", "it reads parent_type"),
        ("\"a\" == \"a\"", "it compares"),
        ("1 < 2", "it compares"),
        ("(1 + (2 != 3))", "it compares"),
    ] {
        let after = before.replace(
            "/datum/a\n",
            &format!("/datum/a\n\tvar/const/k = {value}\n"),
        );
        let got = changes("const-refused", before, &after);
        assert!(got.refused.is_empty(), "{value}: {:?}", got.refused);
        assert!(
            got.warnings
                .iter()
                .any(|w| w.contains("const k: DreamMaker refuses") && w.contains(why)),
            "{value}: {:?}",
            got.warnings
        );
    }
    // Do not warn for valid const initializers.
    let after = before.replace("/datum/a\n", "/datum/a\n\tvar/const/k = 1 + 2\n");
    let got = changes("const-plain", before, &after);
    assert!(
        !got.warnings
            .iter()
            .any(|w| w.contains("DreamMaker refuses")),
        "{:?}",
        got.warnings
    );
    // Reject initializers neither evaluator can fold.
    let after = before.replace("/datum/a\n", "/datum/a\n\tvar/const/k = min(1, 2)\n");
    let got = changes("const-unfolded", before, &after);
    assert!(
        !got.refused.is_empty(),
        "non-constant min() initializer was accepted"
    );
}

#[test]
fn a_var_declared_twice_on_one_type_is_refused_where_it_is() {
    let before = "var/x = 1\n/datum/a\n/datum/t\n";
    let after = before.replace("/datum/t\n", "/datum/t\n\tvar/w = 1\n\tvar/w = 2\n");
    let refused = changes("dup-same", before, &after).refused;
    assert_eq!(
        refused,
        ["parse error: code/code.dm:5: duplicate definition of var/w"]
    );
    // Locate a valueless declaration at the end of its source file, rather than in the .dme.
    let after = before.replace("/datum/t\n", "/datum/t\n\tvar/vars\n");
    let refused = changes("dup-builtin", before, &after).refused;
    assert_eq!(
        refused,
        ["code/code.dm:4: vars: duplicate definition (/datum declares it too)"]
    );
}

#[test]
fn an_error_mended_by_reading_its_file_again_is_gone() {
    // A proc-body error hides no definitions; recovered consts are not parse errors.
    let broken =
        "/datum/a\n\tvar/const/k = sqrt(16)\n/datum/a/proc/f()\n\tvar/x = 1 +\n\treturn x\n";
    let world = World::new("mended", broken);
    let mut old = world.parse();
    let errors = old.parse_errors();
    assert_eq!(errors.len(), 1, "{:?}", errors);
    assert!(errors[0].1.starts_with("code/code.dm:4: "), "{:?}", errors);

    world.write(&broken.replace("1 +", "1 + 2"));
    let reload = old
        .reload_file(&world.dir, std::path::Path::new("code/code.dm"))
        .unwrap();
    old.keep(reload);
    assert!(old.parse_errors().is_empty(), "{:?}", old.parse_errors());
    // A repaired error must not block later additions.
    world.write(&format!(
        "{}/proc/g()\n\treturn 2\n",
        broken.replace("1 +", "1 + 2")
    ));
    let changes = diff(&old, &world.parse(), None);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
}

#[test]
fn a_new_type_doesnt_keep_the_init_proc_its_parent_loses() {
    // The runtime ancestor still has an init proc for `l`. A new subtype
    // must not inherit it after `l` becomes static, or each instance would
    // reset the static to list(1).
    let before = "/datum/a\n\tvar/list/l = list(1)\n/datum/a/b\n";
    let after = "/datum/a\n\tvar/static/list/l = list(1)\n/datum/a/b\n/datum/a/b/c\n";
    let reloaded = changes("new-type-init", before, after);
    assert!(reloaded.refused.is_empty(), "{:?}", reloaded.refused);
    let mut inits: Vec<(String, bool)> = reloaded
        .init_procs
        .iter()
        .map(|i| (i.type_path.clone(), i.code.is_some()))
        .collect();
    inits.sort();
    assert_eq!(
        inits,
        [
            ("/datum/a".to_owned(), false),
            ("/datum/a/b".to_owned(), false),
            ("/datum/a/b/c".to_owned(), false),
        ]
    );

    // No init proc is needed when none of the ancestors had one.
    let fresh = changes(
        "new-type-no-init",
        "/datum/a\n\tvar/x = 1\n",
        "/datum/a\n\tvar/x = 1\n/datum/a/b\n",
    );
    assert!(fresh.init_procs.is_empty(), "{:?}", fresh.init_procs);
}

#[test]
fn a_built_in_a_type_stops_setting_comes_from_its_parent() {
    let before = "/mob/m\n\tlayer = 7\n\tsee_in_dark = 3\n/mob/m/sub\n/mob/m/own\n\tlayer = 2\n";
    let after = "/mob/m\n/mob/m/sub\n/mob/m/own\n\tlayer = 2\n";
    let changes = changes("builtin-inherited", before, after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    assert!(
        changes.builtin_defaults.is_empty(),
        "{:?}",
        changes.builtin_defaults
    );
    let mut inherited: Vec<(String, Vec<String>)> = changes
        .builtin_inherited
        .iter()
        .map(|i| (i.var.clone(), i.types.clone()))
        .collect();
    inherited.sort();
    let types = |t: &[&str]| t.iter().map(|t| t.to_string()).collect::<Vec<_>>();
    assert_eq!(
        inherited,
        [
            ("layer".to_owned(), types(&["/mob/m", "/mob/m/sub"])),
            (
                "see_in_dark".to_owned(),
                types(&["/mob/m", "/mob/m/own", "/mob/m/sub"])
            ),
        ]
    );
}

#[test]
fn a_setting_back_as_built_waits_for_nothing() {
    let world = World::new("settings-back", "/world\n\tfps = 10\n");
    let built = world.parse();
    world.write("/world\n\tfps = 20\n");
    let changed = world.parse();
    let edited: HashSet<PathBuf> = vec![PathBuf::from("code/code.dm")].into_iter().collect();
    let mut changes = diff(&built, &changed, Some(&edited));
    changes.settings_as_built(&built, &changed);
    assert_eq!(
        changes.warnings,
        ["/world: fps: changed setting applies only after a restart"]
    );
    // Saved back as built: the world has that value already
    world.write("/world\n\tfps = 10\n");
    let back = world.parse();
    let mut changes = diff(&changed, &back, Some(&edited));
    assert_eq!(
        changes.restart_settings,
        [("/world".to_owned(), "fps".to_owned())]
    );
    changes.settings_as_built(&built, &back);
    assert!(changes.warnings.is_empty(), "{:?}", changes.warnings);
    assert!(changes.restart_settings.is_empty());
}

#[test]
fn world_and_client_settings_apply_after_a_restart() {
    let before = "/world\n\tfps = 10\n\tname = \"a\"\n/client\n\tcontrol_freak = 0\n\tdir = 1\n";
    let after = "/world\n\tfps = 20\n\tname = \"b\"\n\ticon_size = 64\n/client\n\tcontrol_freak = 1\n\tdir = 4\n";
    let changes = changes("world-settings", before, after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    assert!(
        changes.builtin_defaults.is_empty(),
        "{:?}",
        changes.builtin_defaults
    );
    let mut warned: Vec<&str> = changes
        .warnings
        .iter()
        .filter(|w| w.ends_with("changed setting applies only after a restart"))
        .map(|w| w.as_str())
        .collect();
    warned.sort();
    assert_eq!(
        warned,
        [
            "/client: control_freak: changed setting applies only after a restart",
            "/client: dir: changed setting applies only after a restart",
            "/world: fps: changed setting applies only after a restart",
            "/world: icon_size: changed setting applies only after a restart",
            "/world: name: changed setting applies only after a restart",
        ]
    );
}

#[test]
fn built_ins_a_sound_keeps_in_its_var_table_are_var_defaults() {
    let before = "/sound/s\n\tvolume = 50\n/sound/s/sub\n/obj/o\n\tbounds = \"1,1 to 16,16\"\n/client\n\tfps = 20\n";
    let after = "/sound/s\n\tvolume = 60\n\trepeat = 1\n/sound/s/sub\n/sound/n\n\tchannel = 3\n/obj/o\n\tbounds = \"1,1 to 8,8\"\n/client\n\tfps = 40\n";
    let changes = changes("sound-defaults", before, after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    let mut defaults: Vec<(String, Vec<String>)> = changes
        .var_defaults
        .iter()
        .map(|d| {
            let mut types = d.types.clone();
            types.sort();
            (d.var.clone(), types)
        })
        .collect();
    defaults.sort();
    let types = |t: &[&str]| t.iter().map(|t| t.to_string()).collect::<Vec<_>>();
    assert_eq!(
        defaults,
        [
            ("channel".to_owned(), types(&["/sound/n"])),
            ("repeat".to_owned(), types(&["/sound/s", "/sound/s/sub"])),
            ("volume".to_owned(), types(&["/sound/s", "/sound/s/sub"])),
        ]
    );
    let mut builtins: Vec<&str> = changes
        .builtin_defaults
        .iter()
        .map(|d| d.var.as_str())
        .collect();
    builtins.sort();
    assert_eq!(builtins, ["bounds", "fps"]);
    assert_eq!(changes.new_types.len(), 1);
    assert!(
        changes.new_types[0]
            .fields
            .iter()
            .all(|(f, _)| f != "channel"),
        "{:?}",
        changes.new_types
    );
}

#[test]
fn a_parent_change_comes_with_its_subtree() {
    let before = "/datum/a\n\tvar/x = 1\n/datum/a/proc/f()\n\treturn 1\n/datum/b\n\tvar/x = 2\n/datum/b/proc/f()\n\treturn 2\n/datum/t\n\tparent_type = /datum/a\n\tvar/own = 1\n\tx = 3\n/datum/t/f()\n\treturn ..()\n/datum/t/s\n\town = 2\n/obj/a\n\tname = \"ay\"\n/obj/b\n/obj/t\n\tparent_type = /obj/a\n\ticon_state = \"t\"\n";
    let after = before
        .replace("parent_type = /datum/a", "parent_type = /datum/b")
        .replace("parent_type = /obj/a", "parent_type = /obj/b");
    let changes = changes("reparent", before, &after);
    assert!(changes.refused.is_empty(), "{:?}", changes.refused);
    let shown: Vec<(
        String,
        String,
        Vec<(String, Vec<String>, Vec<String>, Vec<String>, ConstValue)>,
    )> = changes
        .reparented
        .iter()
        .map(|r| {
            (
                r.type_path.clone(),
                r.parent.clone(),
                r.types
                    .iter()
                    .map(|t| {
                        (
                            t.path.clone(),
                            t.declares.clone(),
                            t.sets.clone(),
                            t.builtins.clone(),
                            t.name.clone(),
                        )
                    })
                    .collect(),
            )
        })
        .collect();
    let s = |v: &[&str]| v.iter().map(|x| x.to_string()).collect::<Vec<_>>();
    assert_eq!(
        shown,
        vec![
            (
                "/datum/t".to_owned(),
                "/datum/b".to_owned(),
                vec![
                    (
                        "/datum/t".to_owned(),
                        s(&["own"]),
                        s(&["x"]),
                        s(&[]),
                        ConstValue::String("t".to_owned())
                    ),
                    (
                        "/datum/t/s".to_owned(),
                        s(&[]),
                        s(&["own"]),
                        s(&[]),
                        ConstValue::String("s".to_owned())
                    ),
                ]
            ),
            (
                "/obj/t".to_owned(),
                "/obj/b".to_owned(),
                // No ancestor supplies the name; derive it from the path.
                vec![(
                    "/obj/t".to_owned(),
                    s(&[]),
                    s(&[]),
                    s(&["icon_state"]),
                    ConstValue::String("t".to_owned())
                )]
            ),
        ]
    );
}

#[test]
fn parent_changes_a_reload_refuses() {
    let base = "/datum/a\n\tvar/x = 1\n/datum/a/proc/f()\n\treturn 1\n/datum/b\n/datum/t\n\tparent_type = /datum/a\n\tx = 2\n/datum/u\n\tparent_type = /datum/a\n\tvar/y = 1\n/datum/u/proc/g()\n\treturn 1\n/datum/b/var/y = 2\n/datum/b/proc/g()\n\treturn 2\n/obj/o\n\tparent_type = /obj\n";
    let cases = [
        // overrides a var the new ancestors don't declare
        (
            "undeclared",
            base.replace(
                "/datum/t\n\tparent_type = /datum/a",
                "/datum/t\n\tparent_type = /datum/b",
            ),
            "undefined var 'x'",
        ),
        // declares what the new ancestors declare
        (
            "duplicate",
            base.replace(
                "/datum/u\n\tparent_type = /datum/a",
                "/datum/u\n\tparent_type = /datum/b",
            ),
            "declares var y",
        ),
        // an obj can't become a datum
        (
            "kind",
            base.replace(
                "/obj/o\n\tparent_type = /obj",
                "/obj/o\n\tparent_type = /datum/b",
            ),
            "would be a /datum instead of a /obj",
        ),
    ];
    for (name, after, why) in cases.iter() {
        let refused = refusal(name, base, after);
        assert!(
            refused.contains(*why),
            "{}: expected {:?} in {:?}",
            name,
            why,
            refused
        );
    }
}

/// The skin is the last `.dmf` included, `#if` respected, from the `.dme`'s directory.
#[test]
fn the_skin_is_the_last_interface_file_included() {
    let world = World::new("skin", "/proc/a()\n\treturn 1\n");
    assert_eq!(world.parse().skin(), None);
    std::fs::create_dir_all(world.dir.join("ui")).unwrap();
    for name in ["a.dmf", "b.dmf", "c.dmf"] {
        std::fs::write(world.dir.join("ui").join(name), "window \"w\"\n").unwrap();
    }
    std::fs::write(
        world.dir.join("world.dme"),
        "#include \"ui\\a.dmf\"\n#include \"code/code.dm\"\n#include \"ui/b.dmf\"\n#if 0\n#include \"ui/c.dmf\"\n#endif\n",
    )
    .unwrap();
    let snapshot = world.parse();
    assert_eq!(snapshot.skin(), Some(std::path::Path::new("ui/b.dmf")));
    assert!(!snapshot
        .files()
        .iter()
        .any(|file| file.extension() == Some("dmf".as_ref())));
}
