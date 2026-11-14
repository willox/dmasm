//! Index resource references so edited files trigger recompilation.
//! Recompiled code resolves each name to the new resource-cache entry.

use std::collections::HashMap;

use dreammaker::objtree::TypeRef;
use dreammaker::FileId;

use super::{
    default_value, inits, is_builtin, Changes, ConstValue, InitProc, ProcKey, ProcReload, Refusal,
    Snapshot, VarDefault, BUILTIN_VARS,
};

/// Code and defaults referencing a resource file.
#[derive(Debug, Default)]
pub(super) struct Users {
    procs: Vec<ProcKey>,
    /// Types and vars whose own default is this resource.
    defaults: Vec<(String, String)>,
    /// Types whose computed initializers reference this resource.
    inits: Vec<String>,
}

/// Users by normalized resource name (see `normalized`)
#[derive(Debug, Default)]
pub(super) struct Index(HashMap<String, Users>);

/// Names compare as DreamMaker resolves them: either slash, any case
fn normalized(name: &str) -> String {
    name.replace('\\', "/").to_ascii_lowercase()
}

/// Extract resource names from AST debug text. Both `Term::Resource("x")`
/// and `Constant::Resource(x)` print as `Resource("x")`.
fn names_in(text: &str, out: &mut Vec<String>) {
    let mut rest = text;
    while let Some(at) = rest.find("Resource(\"") {
        rest = &rest[at + "Resource(\"".len()..];
        let mut name = String::new();
        let mut chars = rest.char_indices();
        let mut end = rest.len();
        while let Some((i, c)) = chars.next() {
            match c {
                '\\' => {
                    if let Some((_, escaped)) = chars.next() {
                        name.push(escaped);
                    }
                }
                '"' => {
                    end = i + 1;
                    break;
                }
                c => name.push(c),
            }
        }
        out.push(normalized(&name));
        rest = &rest[end..];
    }
}

impl Index {
    pub(super) fn build(snapshot: &Snapshot) -> Index {
        let mut index = Index::default();
        let mut names = vec![];
        for key in snapshot.procs.keys() {
            let Some((_, value)) = snapshot.find(key) else {
                continue;
            };
            names.clear();
            names_in(
                &format!("{:?}{:?}", value.code, value.parameters),
                &mut names,
            );
            names.sort();
            names.dedup();
            for name in names.drain(..) {
                index.0.entry(name).or_default().procs.push(key.clone());
            }
        }
        for ty in snapshot.tree.iter_types() {
            for (var, value) in ty.get().vars.iter() {
                if value.value.location.file == FileId::BUILTINS {
                    continue;
                }
                let text = format!("{:?}{:?}", value.value.expression, value.value.constant);
                names.clear();
                names_in(&text, &mut names);
                names.sort();
                names.dedup();
                for name in names.drain(..) {
                    let users = index.0.entry(name).or_default();
                    match default_value(ty, var) {
                        Some(Some(_)) => users
                            .defaults
                            .push((ty.get().path.clone(), var.to_string())),
                        _ => users.inits.push(ty.get().path.clone()),
                    }
                }
            }
        }
        index
    }
}

impl Snapshot {
    /// Recompile procs, var defaults and type initializers referencing `names`,
    /// including `FILE_DIR` spellings. Globals, statics and consts retain their
    /// runtime values; warn about const references.
    pub fn resource_users(&self, names: &[String]) -> Changes {
        let index = self.resource_index.get_or_init(|| Index::build(self));
        let mut changes = Changes::default();
        let mut procs: Vec<&ProcKey> = vec![];
        let mut defaults: Vec<(&str, &str)> = vec![];
        let mut init_types: Vec<&str> = vec![];
        for name in names {
            let Some(users) = index.0.get(&normalized(name)) else {
                continue;
            };
            procs.extend(&users.procs);
            defaults.extend(users.defaults.iter().map(|(t, v)| (t.as_str(), v.as_str())));
            init_types.extend(users.inits.iter().map(String::as_str));
        }
        procs.sort();
        procs.dedup();
        defaults.sort();
        defaults.dedup();

        for key in procs {
            let reload = self.compile_code(key).and_then(|mut reload| {
                self.compile_in_lists(key, &mut reload, |_, _| false)?;
                Ok(reload)
            });
            match reload {
                Ok(reload) => changes.reloads.push(ProcReload {
                    proc: key.clone(),
                    ..reload
                }),
                Err(e) => changes.refused.push(self.uncompiled(key, &e)),
            }
        }

        for (path, var) in defaults {
            let Some(ty) = self.find_type(path) else {
                continue;
            };
            self.resource_default(ty, var, &mut changes);
        }

        // Computed defaults run in each inheriting subtype's init proc too.
        let mut memo = HashMap::new();
        for ty in self.tree.iter_types() {
            if ty.is_root() {
                continue;
            }
            let list = inits(ty, &mut memo).to_vec();
            if !list
                .iter()
                .any(|init| init_types.contains(&init.on.as_str()))
            {
                continue;
            }
            match self.compile_init(ty, &list) {
                Ok(code) => changes.init_procs.push(InitProc {
                    type_path: ty.get().path.clone(),
                    code: Some(code),
                }),
                Err(e) => changes.refused.push(
                    Refusal::new(
                        ty.get().path.clone(),
                        format!("initializer compilation failed: {e}"),
                    )
                    .at(self, self.type_location(ty)),
                ),
            }
        }
        changes.collect_warnings();
        changes
    }

    /// Reapply this resource default to `ty` and inheriting subtypes, resolving
    /// the name to the edited file.
    fn resource_default(&self, ty: TypeRef, var: &str, changes: &mut Changes) {
        let shown = if ty.is_root() {
            "(global)"
        } else {
            ty.get().path.as_str()
        };
        let Some(declaration) = ty.get_var_declaration(var) else {
            return;
        };
        let flags = declaration.get().var_type.flags;
        if ty.is_root() || flags.is_static() {
            return;
        }
        if flags.is_const() {
            changes.warnings.push(format!(
                "{shown}: const {var} references the edited resource; compiled uses retain the old file until a rebuild"
            ));
            return;
        }
        let Some(Some(value @ ConstValue::Resource(_))) = default_value(ty, var) else {
            return;
        };
        let mut types = vec![];
        for sub in self.tree.iter_types() {
            let mut at = sub;
            loop {
                if at.index() == ty.index() {
                    types.push(sub.get().path.clone());
                    break;
                }
                if at.get().vars.contains_key(var) {
                    break;
                }
                match at.parent_type() {
                    Some(parent) => at = parent,
                    None => break,
                }
            }
        }
        let default = VarDefault {
            var: var.to_owned(),
            value,
            types,
        };
        if is_builtin(ty, var) {
            if BUILTIN_VARS.contains(&var) {
                changes.builtin_defaults.push(default);
            }
        } else {
            changes.var_defaults.push(default);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::names_in;

    #[test]
    fn names_come_out_of_debug_text() {
        let mut names = vec![];
        names_in(
            r#"Call(Resource("icons/A.dmi"), Prefab(x)) Resource("say \"hi\".ogg")"#,
            &mut names,
        );
        assert_eq!(names, vec!["icons/a.dmi", "say \"hi\".ogg"]);
    }
}
