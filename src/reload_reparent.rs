//! Parent changes and the declarations retained by each reparented subtree.
//! The running world takes inherited state from the new parent.

use std::collections::HashSet;

use dreammaker::objtree::TypeRef;
use dreammaker::FileId;

use super::*;

/// Parent change with each type's own declarations and defaults.
/// The runtime preserves these while replacing inherited state.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Reparented {
    pub type_path: String,
    pub parent: String,
    /// The type and its subtypes, each after its parent
    pub types: Vec<ReparentedType>,
}

/// One type of a reparented subtree.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ReparentedType {
    pub path: String,
    /// Vars it declares
    pub declares: Vec<String>,
    /// Vars declared above it that it sets
    pub sets: Vec<String>,
    /// Built-in vars it sets itself
    pub builtins: Vec<String>,
    /// `name` and `text`, including defaults derived from ancestry.
    pub name: ConstValue,
    pub text: ConstValue,
}

/// Nearest built-in ancestor, including the type itself.
/// Determines the runtime kind of its instances.
fn builtin_base(t: TypeRef) -> Option<String> {
    static BUILTIN_TYPES: std::sync::LazyLock<HashSet<String>> = std::sync::LazyLock::new(|| {
        dreammaker::objtree::ObjectTree::with_builtins()
            .iter_types()
            .filter(|t| !t.is_root())
            .map(|t| t.get().path.clone())
            .collect()
    });
    let mut at = Some(t);
    while let Some(t) = at {
        if BUILTIN_TYPES.contains(&t.get().path) {
            return Some(t.get().path.clone());
        }
        at = t.parent_type();
    }
    None
}

/// Whether `t` is `ancestor` or descends from it, following actual parents.
fn descends_from(t: TypeRef, ancestor: TypeRef) -> bool {
    let mut at = Some(t);
    while let Some(t) = at {
        if t == ancestor {
            return true;
        }
        at = t.parent_type();
    }
    false
}

/// A type's name and text in its record, as DreamMaker works them out.
fn record_name(t: TypeRef) -> (ConstValue, ConstValue) {
    let set = |name: &str| {
        let mut at = Some(t);
        while let Some(ty) = at {
            if let Some(var) = ty
                .get()
                .vars
                .get(name)
                .filter(|v| v.value.location.file != FileId::BUILTINS)
            {
                return Some(var_constant(var).unwrap_or(ConstValue::Null));
            }
            at = ty.parent_type();
        }
        None
    };
    let name = set("name").unwrap_or_else(|| {
        ConstValue::String(
            t.get()
                .path
                .rsplit('/')
                .next()
                .unwrap_or_default()
                .replace('_', " "),
        )
    });
    let text = set("text").unwrap_or_else(|| match &name {
        ConstValue::String(name) => ConstValue::String(name.chars().take(1).collect()),
        _ => ConstValue::Null,
    });
    (name, text)
}

/// Validate a parent change and collect the subtree's own declarations.
/// Refuse invalid inheritance or changes unsupported by the running world.
pub(super) fn reparent(old: &Snapshot, o: TypeRef, n: TypeRef, changes: &mut Changes) {
    let path = n.get().path.clone();
    let Some(parent) = n.parent_type().filter(|p| !p.is_root()) else {
        changes.refused.push(format!(
            "{path}: parent type changed to the root; rebuild required"
        ));
        return;
    };
    let parent_path = parent.get().path.clone();
    let refusal = |why: String| format!("{path}: parent type changed: {why}; rebuild required");
    let new_parent = changes.new_types.iter().any(|t| t.path == parent_path);
    let mut refuse = |why: String| changes.refused.push(refusal(why));
    // Built-in parents cannot change (`/client` can declare `/datum`).
    if builtin_base(n).as_deref() == Some(path.as_str()) {
        return refuse("built-in types cannot change parents".to_owned());
    }
    // Existing instances must retain their built-in kind.
    if builtin_base(o) != builtin_base(n) {
        return refuse(format!(
            "its instances would be a {} instead of a {}",
            builtin_base(n).unwrap_or_default(),
            builtin_base(o).unwrap_or_default()
        ));
    }
    if descends_from(parent, n) {
        return refuse(format!("parent {parent_path} is a subtype of this type"));
    }
    if old.find_type(&parent_path).is_none() && !new_parent {
        return refuse(format!("parent {parent_path} does not exist"));
    }

    // The subtree, each type after its parent
    let mut subtree: Vec<TypeRef> = n
        .tree()
        .iter_types()
        .filter(|t| descends_from(*t, n))
        .collect();
    let depth = |t: &TypeRef| {
        let mut depth = 0;
        let mut at = t.parent_type();
        while let Some(p) = at {
            depth += 1;
            at = p.parent_type();
        }
        depth
    };
    subtree.sort_by_key(depth);

    let mut errors = vec![];
    let mut types = vec![];
    for t in &subtree {
        let mut declares = vec![];
        let mut sets = vec![];
        let mut builtins = vec![];
        for (name, var) in t.get().vars.iter() {
            if var.value.location.file == FileId::BUILTINS || name.as_str() == "parent_type" {
                continue;
            }
            if var.declaration.is_some() {
                // DreamMaker: "duplicate definition"
                if let Some(above) = parent.get_var_declaration(name.as_str()) {
                    errors.push(format!(
                        "{} declares var {name}, which {} declares too",
                        t.get().path,
                        above.ty().get().path
                    ));
                }
                declares.push(name.to_string());
            } else if is_builtin(*t, name.as_str()) {
                builtins.push(name.to_string());
            } else if t.get_var_declaration(name.as_str()).is_none() {
                errors.push(format!(
                    "{}: var {name} is no longer declared",
                    t.get().path
                ));
            } else {
                sets.push(name.to_string());
            }
        }
        for (name, proc) in t.get().procs.iter() {
            if real(proc).is_empty() {
                continue;
            }
            if proc.declaration.is_some() {
                if parent.get_proc_declaration(name.as_str()).is_some() {
                    errors.push(format!(
                        "{} declares proc {name}, which its new ancestors declare too",
                        t.get().path
                    ));
                }
            } else if t.get_proc_declaration(name.as_str()).is_none() {
                errors.push(format!(
                    "{}: proc {name} is no longer declared",
                    t.get().path
                ));
            }
        }
        let (name, text) = record_name(*t);
        types.push(ReparentedType {
            path: t.get().path.clone(),
            declares,
            sets,
            builtins,
            name,
            text,
        });
    }
    if !errors.is_empty() {
        changes.refused.push(refusal(errors.join("; ")));
        return;
    }
    changes.reparented.push(Reparented {
        type_path: path,
        parent: parent_path,
        types,
    });
}

/// Recompile procs whose code changes with inherited consts or declared
/// var types after a parent change.
pub(super) fn reparented_users(
    old: &Snapshot,
    new: &Snapshot,
    realigned: &Realigned,
    changes: &mut Changes,
) {
    let types: HashSet<&str> = changes
        .reparented
        .iter()
        .flat_map(|r| r.types.iter().map(|t| t.path.as_str()))
        .collect();
    if types.is_empty() {
        return;
    }
    let reloaded: HashSet<ProcKey> = changes.reloads.iter().map(|r| r.proc.clone()).collect();
    let mut reloads = vec![];
    for key in new.procs.keys() {
        let Some((type_path, _)) = key.path.rsplit_once('/') else {
            continue;
        };
        if !types.contains(type_path) {
            continue;
        }
        let Some(old_key) = old_key(realigned, key) else {
            continue;
        };
        if reloaded.contains(&old_key) {
            continue;
        }
        let (Ok(before), Ok(after)) = (old.compile(&old_key), new.compile(key)) else {
            continue;
        };
        if before.nodes == after.nodes {
            continue;
        }
        match new.compile_code(key).and_then(|mut reload| {
            new.compile_in_lists(key, &mut reload, |_, _| false)?;
            Ok(reload)
        }) {
            Ok(reload) => reloads.push(ProcReload {
                proc: old_key,
                ..reload
            }),
            Err(e) => changes.refused.push(format!("{key}: doesn't compile: {e}")),
        }
    }
    changes
        .line_moves
        .retain(|m| !reloads.iter().any(|r| r.proc == m.proc));
    changes.reloads.extend(reloads);
}
