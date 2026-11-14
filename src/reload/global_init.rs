//! Replacement global initialization for rebooted worlds (see [`GlobalInit`]).
//!
//! DreamMaker stores constant global and static defaults in the `.dmb` var
//! table. A single proc evaluates the rest before map creation, using
//! `<value> SetVar global` for each initializer.
//!
//! The ordering rules below approximate DreamMaker's. Preserve the build's
//! order for existing initializers with `GlobalInit::in_built_order`; use
//! dmasm's order to place additions. Exact ordering remains necessary before
//! that step can be removed.

use std::collections::{BTreeMap, HashMap, HashSet};

use dreammaker::ast::{Expression, Spanned, Statement, Term, VarStatement};
use dreammaker::objtree::TypeRef;
use std::path::{Path, PathBuf};

use dreammaker::{FileId, Location};

use super::{const_value, value_location, var_constant, ConstValue, ProcReload, Refusal, Snapshot};
use crate::operands::{DMString, GlobalVar, Label, Variable};
use crate::{Instruction, Node};

/// Replacement global init proc for a rebooted world (see `Changes::global_init`).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct GlobalInit {
    /// Set changed var-table defaults first (null for newly computed values),
    /// then run computed initializers in `Walk` order. This order is approximate;
    /// `in_built_order` restores the world's existing initializer order.
    ///
    /// Each assignment uses `SetVar global(name)` with a scope: `""` for a
    /// global, the declaring type path for a type static, or `proc:PATH#N`
    /// for a proc static.
    pub code: ProcReload,
    /// Number of initial assignments restoring var-table defaults.
    #[cfg_attr(feature = "serde", serde(default))]
    pub table: usize,
    /// Number of statics with each name per proc scope, including consts.
    /// The `N` in `proc:PATH#N` counts all declarations of that name.
    #[cfg_attr(feature = "serde", serde(default))]
    pub proc_statics: BTreeMap<String, BTreeMap<String, usize>>,
}

/// Count same-named static declarations in procs referenced by `set`
/// (see `GlobalInit::proc_statics`).
fn proc_statics<'a>(
    set: impl Iterator<Item = &'a Initializer<'a>>,
) -> BTreeMap<String, BTreeMap<String, usize>> {
    let mut procs: BTreeMap<String, BTreeMap<String, usize>> = BTreeMap::new();
    for init in set {
        let Some(path) = &init.proc_path else {
            continue;
        };
        let count = init.siblings.iter().filter(|s| **s == init.name).count();
        procs
            .entry(path.clone())
            .or_default()
            .insert(init.name.clone(), count);
    }
    procs
}

/// A global or static var and its initializer
struct Initializer<'a> {
    scope: String,
    name: String,
    /// Declaration location, used for ordering.
    location: Location,
    /// Scope in which to compile the initializer.
    ty: TypeRef<'a>,
    /// Its declared type, for a `new` without one
    declared: String,
    expression: Option<&'a Expression>,
    /// Var-table default for a constant initializer.
    constant: Option<ConstValue>,
    /// Declaring proc, used for `__PROC__` in a proc static.
    proc_path: Option<String>,
    /// Other static names in the same proc.
    siblings: Vec<String>,
}

impl Initializer<'_> {
    fn key(&self) -> (&str, &str) {
        (&self.scope, &self.name)
    }

    /// Runtime initializer expression, or none for a constant.
    fn computed(&self) -> Option<(&str, &str, &str, Option<&Expression>)> {
        if self.constant.is_some() {
            return None;
        }
        Some((&self.scope, &self.name, &self.declared, self.expression))
    }
}

impl Snapshot {
    /// Globals and statics with their initializers in `Walk` order.
    /// Include proc statics only when `procs` is set.
    fn initializers(&self, procs: bool) -> Vec<Initializer<'_>> {
        let mut statics: Statics<'_> = HashMap::new();
        for (key, (_, name, index)) in self.procs.iter().filter(|_| procs) {
            let Some((ty, value)) = self.find(key) else {
                continue;
            };
            let Some(code) = &value.code else {
                continue;
            };
            let mut found = vec![];
            static_statements(code, None, &mut found);
            if found.is_empty() {
                continue;
            }
            let proc_path = crate::compiler::proc_path(ty, name, *index == 0);
            // Number same-named statics as the compiler does, including consts.
            let siblings: Vec<String> = found
                .iter()
                .map(|(_, var)| var.name.as_str().to_owned())
                .collect();
            let mut seen: Vec<&str> = vec![];
            let proc_name = name;
            for (location, var) in found {
                let name = var.name.as_str();
                let number = seen.iter().filter(|n| **n == name).count();
                seen.push(name);
                if var.var_type.flags.is_const() {
                    continue;
                }
                let nowhere = Location {
                    file: FileId::UNKNOWN,
                    line: 0,
                    column: 0,
                };
                let constant = match &var.value {
                    None => Some(ConstValue::Null),
                    Some(value) => value
                        .simple_evaluate(nowhere)
                        .ok()
                        .as_ref()
                        .and_then(const_value),
                };
                statics
                    .entry((ty.index().index(), proc_name.to_owned(), *index))
                    .or_default()
                    .push(Initializer {
                        scope: format!("proc:{proc_path}#{number}"),
                        name: name.to_owned(),
                        location,
                        ty,
                        declared: var.var_type.type_path.to_string(),
                        expression: var.value.as_ref(),
                        constant,
                        proc_path: Some(proc_path.clone()),
                        siblings: siblings.clone(),
                    });
            }
        }
        // Without proc statics, only change detection matters; ignore file order.
        let mut walk = Walk {
            chains: self.include_chains(procs),
            ..Walk::default()
        };
        walk.ty(self, self.tree.root(), &mut statics);
        walk.now.extend(walk.later);
        walk.now
    }

    /// Map files to their first location in the include stream: the include
    /// lines from outermost to innermost. Resolve includes relative to the
    /// including file, then the `.dme`.
    ///
    /// The `.dme` has an empty chain. Files with no located include follow
    /// all others in first-read order. Without `read`, use first-read order
    /// for every file.
    fn include_chains(&self, read: bool) -> HashMap<FileId, Vec<u32>> {
        let mut paths = vec![];
        self.ctx
            .files()
            .for_each(|path| paths.push(path.to_owned()));
        let unfound = |paths: &[PathBuf]| -> HashMap<FileId, Vec<u32>> {
            paths
                .iter()
                .enumerate()
                .filter_map(|(n, path)| {
                    Some((self.ctx.files().get_id(path)?, vec![u32::MAX, n as u32]))
                })
                .collect()
        };
        let Some(root) = self.root.as_ref().filter(|_| read) else {
            return unfound(&paths);
        };
        let by_path: HashMap<String, FileId> = paths
            .iter()
            .filter_map(|path| Some((normal(path), self.ctx.files().get_id(path)?)))
            .collect();
        let mut parents: HashMap<FileId, (FileId, u32)> = HashMap::new();
        let mut dme = None;
        for path in &paths {
            let name = normal(path);
            if !(name.ends_with(".dm") || name.ends_with(".dme")) {
                continue;
            }
            let Some(&id) = by_path.get(&name) else {
                continue;
            };
            if name.ends_with(".dme") && dme.is_none() {
                dme = Some(id);
            }
            let Ok(text) = std::fs::read(root.join(path)) else {
                continue;
            };
            let dir = path.parent().unwrap_or(Path::new(""));
            for (n, line) in String::from_utf8_lossy(&text).lines().enumerate() {
                let Some(included) = line
                    .trim_start()
                    .strip_prefix("#include")
                    .and_then(|rest| rest.trim_start().strip_prefix('"'))
                    .and_then(|rest| rest.split('"').next())
                else {
                    continue;
                };
                let included = included.replace('\\', "/");
                for candidate in [dir.join(&included), PathBuf::from(&included)] {
                    if let Some(&child) = by_path.get(&normal(&candidate)) {
                        if child != id && Some(child) != dme {
                            parents.entry(child).or_insert((id, n as u32 + 1));
                        }
                        break;
                    }
                }
            }
        }
        let mut chains = unfound(&paths);
        for &id in by_path.values() {
            let mut chain = vec![];
            let mut at = id;
            let mut seen = HashSet::new();
            while let Some(&(parent, line)) = parents.get(&at) {
                if !seen.insert(at) {
                    break;
                }
                chain.push(line);
                at = parent;
            }
            if Some(at) == dme {
                chain.reverse();
                chains.insert(id, chain);
            }
        }
        chains
    }

    /// Compile all computed global and static initializers in `Walk` order.
    pub fn global_init_proc(&self) -> Result<GlobalInit, String> {
        let all = self.initializers(true);
        let (nodes, locals) = build(
            self,
            all.iter()
                .filter(|i| i.constant.is_none())
                .map(|i| (i, i.expression)),
        )?;
        Ok(GlobalInit {
            code: ProcReload {
                proc: super::ProcKey {
                    path: "(global init)".to_owned(),
                    override_id: 0,
                },
                nodes,
                locals,
                params: vec![],
                settings: None,
                statics: vec![],
                warnings: vec![],
            },
            table: 0,
            proc_statics: proc_statics(all.iter().filter(|i| i.constant.is_none())),
        })
    }

    /// Whether a proc has static vars
    fn has_statics(&self, key: &super::ProcKey) -> bool {
        let Some(code) = self.find(key).and_then(|(_, value)| value.code.as_ref()) else {
            return false;
        };
        let mut statics = vec![];
        static_statements(code, None, &mut statics);
        !statics.is_empty()
    }
}

/// Changed var-table defaults: new constant values, or null when a constant
/// becomes computed. Return `None` if these and all computed initializers
/// are unchanged.
#[allow(clippy::type_complexity)]
fn differences<'a, 'b>(
    before: &'b [Initializer<'a>],
    after: &'b [Initializer<'a>],
) -> Option<Vec<(&'b Initializer<'a>, Option<&'a Expression>)>> {
    let was: std::collections::HashMap<_, _> = before.iter().map(|i| (i.key(), i)).collect();
    let mut table = vec![];
    for init in after {
        let Some(was) = was.get(&init.key()) else {
            continue;
        };
        match (&was.constant, &init.constant) {
            (Some(was), Some(now)) if was == now => {}
            (_, Some(_)) => table.push((init, init.expression)),
            (Some(ConstValue::Null), None) => {}
            (Some(_), None) => table.push((init, None)),
            (None, None) => {}
        }
    }
    let computed = |list: &'b [Initializer<'a>]| {
        list.iter()
            .filter_map(Initializer::computed)
            .collect::<Vec<_>>()
    };
    if table.is_empty() && computed(before) == computed(after) {
        return None;
    }
    Some(table)
}

/// Defer changed initializers of existing globals and statics. New and
/// newly static vars receive their values immediately.
fn changed(new: &Snapshot, before: &[Initializer<'_>], after: &[Initializer<'_>]) -> Vec<Refusal> {
    let was: HashMap<_, _> = before.iter().map(|i| (i.key(), i)).collect();
    after
        .iter()
        .filter(|init| {
            was.get(&init.key()).is_some_and(|was| {
                was.constant != init.constant || was.expression != init.expression
            })
        })
        .map(|init| {
            let shown = match init.scope.strip_prefix("proc:") {
                // `proc:/proc/f#1`: the number tells same-named statics apart
                Some(proc) => proc.rsplit_once('#').map_or(proc, |(path, _)| path),
                None if init.scope.is_empty() => "(global)",
                None => &init.scope,
            };
            Refusal::new(shown, "changed initializer")
                .named(init.name.clone())
                .until(super::Until::Reboot)
                .at(new, init.location)
        })
        .collect()
}

/// Proc statics indexed by type, proc name and definition index.
type Statics<'a> = HashMap<(usize, String, usize), Vec<Initializer<'a>>>;

/// Approximate DreamMaker's initializer order by walking declarations in
/// source order, entering subtypes where they first appear. Group statics
/// of declared procs at the first proc declaration; visit overrides at
/// their own locations. Root `/var/name` declarations come first.
///
/// Defer initializers referencing globals, procs or types not yet visited,
/// including their own type, a later type, a list's type or element type,
/// and other statics in the same proc. Run deferred initializers last in
/// the same relative order.
#[derive(Default)]
struct Walk<'a> {
    now: Vec<Initializer<'a>>,
    later: Vec<Initializer<'a>>,
    /// Visited type indices and (type index, proc name) pairs.
    types: HashSet<usize>,
    procs: HashSet<(usize, String)>,
    globals: HashSet<String>,
    /// First source location of each type.
    first: HashMap<usize, Option<Vec<u32>>>,
    /// File locations in the include stream (see `include_chains`).
    chains: HashMap<FileId, Vec<u32>>,
    /// Source lines by file.
    texts: HashMap<FileId, Vec<String>>,
}

impl<'a> Walk<'a> {
    /// Location in the include stream: include lines, then source line and
    /// column. Files with no located include follow all others.
    fn at(&self, location: Location) -> Vec<u32> {
        let mut at = match self.chains.get(&location.file) {
            Some(chain) => chain.clone(),
            None => vec![u32::MAX],
        };
        at.extend([location.line, location.column as u32]);
        at
    }

    fn ty(&mut self, snapshot: &'a Snapshot, t: TypeRef<'a>, statics: &mut Statics<'a>) {
        enum Item<'a> {
            Var(&'a str, &'a dreammaker::objtree::TypeVar),
            Proc(&'a str),
            /// Statics of the type's declared procs, or of one override.
            Statics(Option<(&'a str, usize)>),
            Type(TypeRef<'a>),
        }
        let mut items: Vec<(Vec<u32>, Item<'a>)> = vec![];
        for (name, var) in t.get().vars.iter() {
            if let Some(declaration) = &var.declaration {
                if declaration.location.file != FileId::BUILTINS {
                    let mut at = self.at(declaration.location);
                    // Root `/var/name` declarations precede other declarations.
                    if t.is_root() && self.absolute(snapshot, declaration.location) {
                        at.insert(0, 0);
                    }
                    items.push((at, Item::Var(name.as_str(), var)));
                }
            }
        }
        let mut declared: Option<Vec<u32>> = None;
        for (name, p) in t.get().procs.iter() {
            for (index, value) in p
                .value
                .iter()
                .enumerate()
                .filter(|(_, v)| v.location.file != FileId::BUILTINS)
            {
                let at = self.at(value.location);
                items.push((at.clone(), Item::Proc(name.as_str())));
                if index == 0 && p.declaration.is_some() {
                    declared = Some(declared.map_or(at.clone(), |d| d.min(at)));
                } else {
                    items.push((at, Item::Statics(Some((name.as_str(), index)))));
                }
            }
        }
        // Insert after the first proc declaration.
        if let Some(at) = declared {
            items.push((at, Item::Statics(None)));
        }
        for child in t.children() {
            if let Some(at) = self.first(child) {
                items.push((at, Item::Type(child)));
            }
        }
        items.sort_by(|(a, _), (b, _)| a.cmp(b));
        for (_, item) in items {
            match item {
                Item::Var(name, var) => {
                    let declaration = var.declaration.as_ref().unwrap();
                    let flags = declaration.var_type.flags;
                    if !flags.is_const() && (t.is_root() || flags.is_static()) {
                        let init = Initializer {
                            scope: if t.is_root() {
                                String::new()
                            } else {
                                t.get().path.clone()
                            },
                            name: name.to_owned(),
                            location: var.value.location,
                            ty: t,
                            declared: declaration.var_type.type_path.to_string(),
                            expression: var.value.expression.as_ref(),
                            constant: var_constant(var),
                            proc_path: None,
                            siblings: vec![],
                        };
                        self.add(snapshot, init);
                    }
                    if t.is_root() {
                        self.globals.insert(name.to_owned());
                    }
                }
                Item::Proc(name) => {
                    self.procs.insert((t.index().index(), name.to_owned()));
                }
                Item::Statics(definition) => {
                    let mut own = vec![];
                    match definition {
                        Some((name, index)) => own.extend(
                            statics
                                .remove(&(t.index().index(), name.to_owned(), index))
                                .unwrap_or_default(),
                        ),
                        None => {
                            for (name, p) in t.get().procs.iter() {
                                if p.declaration.is_some() {
                                    own.extend(
                                        statics
                                            .remove(&(
                                                t.index().index(),
                                                name.as_str().to_owned(),
                                                0,
                                            ))
                                            .unwrap_or_default(),
                                    );
                                }
                            }
                        }
                    }
                    own.sort_by_cached_key(|init| self.at(init.location));
                    for init in own {
                        self.add(snapshot, init);
                    }
                }
                Item::Type(child) => self.ty(snapshot, child, statics),
            }
        }
        self.types.insert(t.index().index());
    }

    fn add(&mut self, snapshot: &'a Snapshot, init: Initializer<'a>) {
        let ready = init.constant.is_some()
            || (self.declared_ready(snapshot, &init.declared)
                && init
                    .expression
                    .is_none_or(|e| self.ready(snapshot, &init, e)));
        if ready {
            self.now.push(init);
        } else {
            self.later.push(init);
        }
    }

    /// Whether the declaration at `location` starts with `/var`
    fn absolute(&mut self, snapshot: &Snapshot, location: Location) -> bool {
        let lines = self.texts.entry(location.file).or_insert_with(|| {
            let Some(root) = &snapshot.root else {
                return vec![];
            };
            std::fs::read(root.join(snapshot.file_name(location.file)))
                .map(|text| {
                    String::from_utf8_lossy(&text)
                        .lines()
                        .map(str::to_owned)
                        .collect()
                })
                .unwrap_or_default()
        });
        lines
            .get(location.line.saturating_sub(1) as usize)
            .is_some_and(|line| line.trim_start().starts_with("/var/"))
    }

    /// First type definition, member declaration or subtype declaration.
    fn first(&mut self, t: TypeRef<'a>) -> Option<Vec<u32>> {
        if let Some(at) = self.first.get(&t.index().index()) {
            return at.clone();
        }
        let own = std::iter::once(t.get().location)
            .chain(t.get().vars.values().map(|v| v.value.location))
            .chain(
                t.get()
                    .procs
                    .values()
                    .flat_map(|p| p.value.iter())
                    .map(|v| v.location),
            )
            .filter(|at| at.file != FileId::BUILTINS)
            .map(|at| self.at(at))
            .min();
        let children: Vec<_> = t.children().collect();
        let at = children
            .into_iter()
            .filter_map(|c| self.first(c))
            .chain(own)
            .min();
        self.first.insert(t.index().index(), at.clone());
        at
    }

    /// Whether the declared type has been visited; lists are checked by element type.
    fn declared_ready(&self, snapshot: &Snapshot, declared: &str) -> bool {
        let mut path = format!("/{}", declared.trim_start_matches('/'));
        // For lists, check the element type.
        while let Some(of) = path.strip_prefix("/list/") {
            path = format!("/{of}");
        }
        if path == "/" || path == "/list" {
            return true;
        }
        snapshot
            .tree
            .find(&path)
            .is_some_and(|t| self.types.contains(&t.index().index()))
    }

    /// Whether references in an initializer have been visited.
    fn ready(&self, snapshot: &Snapshot, init: &Initializer, expression: &Expression) -> bool {
        let (t, declared, siblings) = (init.ty, init.declared.as_str(), &init.siblings);
        // A proc is visited once its definition has been passed.
        let before =
            |ty: TypeRef, name: &str| self.procs.contains(&(ty.index().index(), name.to_owned()));
        let mut ready = true;
        let root = snapshot.tree.root();
        let path = |path: &str| {
            let proc_at = ["/proc/", ".proc/", "/verb/", ".verb/"]
                .iter()
                .filter_map(|p| path.find(p))
                .min();
            let (ty, proc) = match proc_at {
                Some(at) => (&path[..at], true),
                None => (path, false),
            };
            // `.proc/name` refers to a proc on this type.
            if ty.is_empty() && path.starts_with('.') {
                let name = path.rsplit('/').next().unwrap_or_default();
                let mut at = Some(t);
                while let Some(ty) = at {
                    if ty.get().procs.get(name).is_some_and(|p| {
                        p.value.iter().any(|v| v.location.file != FileId::BUILTINS)
                    }) {
                        return before(ty, name);
                    }
                    at = ty.parent_type();
                }
                return true;
            }
            match snapshot.tree.find(ty.trim_end_matches('/')) {
                Some(found) if proc => {
                    let name = path.rsplit('/').next().unwrap_or_default();
                    before(found, name)
                }
                Some(found) => self.types.contains(&found.index().index()),
                None => false,
            }
        };
        let call = |name: &str, global: bool| {
            let mut at = if global { Some(root) } else { Some(t) };
            while let Some(ty) = at {
                if let Some(p) = ty.get().procs.get(name) {
                    if p.value.iter().any(|v| v.location.file != FileId::BUILTINS) {
                        return before(ty, name);
                    }
                }
                at = if ty.is_root() {
                    None
                } else {
                    ty.parent_type().or(Some(root))
                };
            }
            true
        };
        let global = |name: &str| {
            if !t.is_root()
                && t.get_var_declaration(name)
                    .is_some_and(|d| !d.ty().is_root())
            {
                return true;
            }
            match root.get().vars.get(name) {
                Some(var)
                    if var.declaration.is_some() && var.value.location.file != FileId::BUILTINS =>
                {
                    self.globals.contains(name)
                }
                _ => true,
            }
        };
        terms(expression, &mut |seen| {
            let term = match seen {
                Seen::Term(term) => term,
                // `type::var` refers to the type currently being visited.
                Seen::StaticFieldOf(Term::Ident(name))
                    if ["type", "src"].contains(&name.as_str()) =>
                {
                    ready &= self.types.contains(&t.index().index());
                    return;
                }
                Seen::StaticFieldOf(_) => return,
            };
            ready &= match term {
                Term::Ident(name) if siblings.iter().any(|s| s == name.as_str()) => false,
                Term::Ident(name) | Term::GlobalIdent(name) => global(name),
                Term::Call(name, _) => call(name, false),
                Term::GlobalCall(name, _) => call(name, true),
                Term::Prefab(prefab) | Term::NewPrefab { prefab, .. } => {
                    let text = prefab.path.to_string();
                    !(text.starts_with('/')
                        || text.starts_with(".proc/")
                        || text.starts_with(".verb/"))
                        || path(&text)
                }
                Term::NewImplicit { .. } => self.declared_ready(snapshot, declared),
                _ => true,
            };
        });
        ready
    }
}

/// Visit every term in an expression.
/// `terms` supplies an expression's immediate terms.
enum Seen<'e> {
    Term(&'e Term),
    /// Receiver of a `::` expression.
    StaticFieldOf(&'e Term),
}

fn terms(expression: &Expression, each: &mut dyn FnMut(Seen)) {
    match expression {
        Expression::Base { term, follow } => {
            // (`global.f()` calls a global proc)
            if let (Term::Ident(name), Some(dreammaker::ast::Follow::Call(_, proc, args))) =
                (&term.elem, follow.first().map(|f| &f.elem))
            {
                if name.as_str() == "global" {
                    each(Seen::Term(&Term::GlobalCall(proc.clone(), args.clone())));
                }
            }
            if follow.iter().any(|f| {
                matches!(
                    f.elem,
                    dreammaker::ast::Follow::StaticField(_)
                        | dreammaker::ast::Follow::ProcReference(_)
                )
            }) {
                each(Seen::StaticFieldOf(&term.elem));
            }
            term_and_within(&term.elem, each);
            for f in follow.iter() {
                match &f.elem {
                    dreammaker::ast::Follow::Index(_, e) => terms(e, each),
                    dreammaker::ast::Follow::Call(_, _, args) => {
                        args.iter().for_each(|e| terms(e, each))
                    }
                    _ => {}
                }
            }
        }
        Expression::BinaryOp { lhs, rhs, .. } | Expression::AssignOp { lhs, rhs, .. } => {
            terms(lhs, each);
            terms(rhs, each);
        }
        Expression::TernaryOp { cond, if_, else_ } => {
            terms(cond, each);
            terms(if_, each);
            terms(else_, each);
        }
    }
}

fn term_and_within(term: &Term, each: &mut dyn FnMut(Seen)) {
    each(Seen::Term(term));
    let all =
        |list: &[Expression], each: &mut dyn FnMut(Seen)| list.iter().for_each(|e| terms(e, each));
    match term {
        Term::Expr(e) => terms(e, each),
        Term::Prefab(prefab) => prefab.vars.iter().for_each(|(_, e)| terms(e, each)),
        Term::InterpString(_, parts) => parts
            .iter()
            .filter_map(|(e, _)| e.as_ref())
            .for_each(|e| terms(e, each)),
        Term::Call(_, args)
        | Term::SelfCall(args)
        | Term::ParentCall(args)
        | Term::List(args)
        | Term::Alist(args)
        | Term::GlobalCall(_, args) => all(args, each),
        Term::NewImplicit { args } | Term::NewMiniExpr { args, .. } => {
            if let Some(args) = args {
                all(args, each);
            }
        }
        Term::NewPrefab { prefab, args } => {
            prefab.vars.iter().for_each(|(_, e)| terms(e, each));
            if let Some(args) = args {
                all(args, each);
            }
        }
        Term::Input { args, in_list, .. } | Term::Locate { args, in_list } => {
            all(args, each);
            if let Some(e) = in_list {
                terms(e, each);
            }
        }
        Term::Pick(args) => args.iter().for_each(|(w, e)| {
            if let Some(w) = w {
                terms(w, each);
            }
            terms(e, each);
        }),
        Term::DynamicCall(a, b) => {
            all(a, each);
            all(b, each);
        }
        Term::ExternalCall {
            library,
            function,
            args,
        } => {
            if let Some(l) = library {
                terms(l, each);
            }
            terms(function, each);
            all(args, each);
        }
        _ => {}
    }
}

/// Normalize file paths: forward slashes, no `.` or `..`, lowercase.
fn normal(path: &Path) -> String {
    let mut parts: Vec<&str> = vec![];
    let text = path.to_string_lossy().replace('\\', "/");
    for part in text.split('/') {
        match part {
            "" | "." => {}
            ".." => {
                parts.pop();
            }
            part => parts.push(part),
        }
    }
    parts.join("/").to_ascii_lowercase()
}

/// Proc static names and initializers in declaration order.
pub(super) fn static_initializers(code: &[Spanned<Statement>]) -> Vec<(&str, Option<&Expression>)> {
    let mut found = vec![];
    static_statements(code, None, &mut found);
    found
        .into_iter()
        .map(|(_, var)| (var.name.as_str(), var.value.as_ref()))
        .collect()
}

/// Static declarations in compiler traversal order, with source locations.
fn static_statements<'a>(
    block: &'a [Spanned<Statement>],
    at: Option<Location>,
    out: &mut Vec<(Location, &'a VarStatement)>,
) {
    for statement in block {
        one_statement(&statement.elem, at.unwrap_or(statement.location), out);
    }
}

fn one_statement<'a>(
    statement: &'a Statement,
    location: Location,
    out: &mut Vec<(Location, &'a VarStatement)>,
) {
    let is_static =
        |var: &VarStatement| var.var_type.flags.is_static() || var.var_type.flags.is_const();
    match statement {
        Statement::Var(var) if is_static(var) => out.push((location, var)),
        Statement::Vars(vars) => {
            out.extend(vars.iter().filter(|v| is_static(v)).map(|v| (location, v)))
        }
        Statement::While { block, .. }
        | Statement::DoWhile { block, .. }
        | Statement::ForInfinite { block }
        | Statement::Spawn { block, .. }
        | Statement::Label { block, .. } => static_statements(block, None, out),
        Statement::If { arms, else_arm } => {
            for (_, block) in arms {
                static_statements(block, None, out);
            }
            if let Some(block) = else_arm {
                static_statements(block, None, out);
            }
        }
        Statement::ForLoop {
            init, inc, block, ..
        } => {
            if let Some(init) = init {
                one_statement(init, location, out);
            }
            static_statements(block, None, out);
            if let Some(inc) = inc {
                one_statement(inc, location, out);
            }
        }
        Statement::ForList(each) => static_statements(&each.block, None, out),
        Statement::ForKeyValue(each) => static_statements(&each.block, None, out),
        Statement::ForRange(each) => static_statements(&each.block, None, out),
        Statement::Switch { cases, default, .. } => {
            for (_, block) in cases.iter() {
                static_statements(block, None, out);
            }
            if let Some(block) = default {
                static_statements(block, None, out);
            }
        }
        Statement::TryCatch {
            try_block,
            catch_block,
            ..
        } => {
            static_statements(try_block, None, out);
            static_statements(catch_block, None, out);
        }
        _ => {}
    }
}

/// Compile assignments for `runs`, using null for absent expressions.
/// Return the code and required locals.
#[allow(clippy::type_complexity)]
fn build<'a, 'b>(
    new: &'a Snapshot,
    runs: impl Iterator<Item = (&'b Initializer<'a>, Option<&'a Expression>)>,
) -> Result<(Vec<Node>, Vec<String>), String>
where
    'a: 'b,
{
    let mut nodes = vec![];
    let mut locals: Vec<String> = vec![];
    let mut file = None;
    for (n, (init, expression)) in runs.enumerate() {
        let shown = match init.scope.as_str() {
            "" => format!("(global): {}", init.name),
            scope => format!(
                "{}: {}",
                scope.strip_prefix("proc:").unwrap_or(scope),
                init.name
            ),
        };
        let null;
        let expression = match expression {
            Some(expression) => expression,
            None => {
                null = Expression::Base {
                    term: Box::new(Spanned::new(init.location, Term::Null)),
                    follow: Box::new([]),
                };
                &null
            }
        };
        let location = value_location(expression, init.location);
        let code = match new.compile_value(
            init.ty,
            expression,
            &init.declared,
            location,
            init.proc_path.as_deref(),
        ) {
            Ok(code) => code,
            Err(e) => {
                return Err(format!("{shown}: initializer compilation failed: {e}"));
            }
        };
        let mut value: Vec<Node> = code
            .nodes
            .into_iter()
            .filter(|node| {
                !matches!(
                    node,
                    Node::Instruction(Instruction::DbgFile(_) | Instruction::DbgLine(_), _)
                )
            })
            .collect();
        // Return the initializer's value.
        if !matches!(
            value[..],
            [
                ..,
                Node::Instruction(Instruction::Ret, _),
                Node::Instruction(Instruction::End, _)
            ]
        ) {
            return Err(format!(
                "{shown}: initializer compilation failed: its code doesn't end in a return"
            ));
        }
        value.truncate(value.len() - 2);
        // Keep each initializer's labels distinct.
        let own = |label: &mut Label| {
            if let Label::Named(name) = label {
                *name = format!("{name}_{n}");
            }
        };
        for node in &mut value {
            match node {
                Node::Label(label) => own(label),
                Node::Instruction(ins, _) => {
                    crate::compiler::labels_mut(ins).into_iter().for_each(own)
                }
                Node::Comment(_) => {}
            }
        }
        // Fall back to the declaration location for macros with no source location.
        let places = [location, init.location];
        let place = places
            .iter()
            .copied()
            .find(|at| at.file != FileId::UNKNOWN && at.file != FileId::BUILTINS);
        if let Some(place) = place {
            if file != Some(place.file) {
                file = Some(place.file);
                nodes.push(Node::Instruction(
                    Instruction::DbgFile(DMString(new.debug_file_name(place.file).into_bytes())),
                    (),
                ));
            }
            nodes.push(Node::Instruction(Instruction::DbgLine(place.line), ()));
        }
        nodes.extend(value);
        nodes.push(Node::Instruction(
            Instruction::SetVar(Variable::Global(GlobalVar::named(
                init.name.as_bytes(),
                Some(init.scope.clone()),
            ))),
            (),
        ));
        if code.locals.len() > locals.len() {
            locals = code.locals;
        }
    }
    nodes.push(Node::Instruction(Instruction::End, ()));
    Ok((nodes, locals))
}

/// Build the replacement global init proc and report deferred initializers.
/// Call after comparing procs. Compilation or ordering failures produce a
/// warning and require a rebuild instead of a reboot.
pub(super) fn compare(old: &Snapshot, new: &Snapshot, changes: &mut super::Changes) {
    // Proc statics change with their code. Inspect unchanged procs only
    // if another initializer changed.
    let statics = changes
        .reloads
        .iter()
        .map(|r| &r.proc)
        .chain(changes.new_procs.iter().map(|p| &p.code.proc))
        .chain(changes.removed_procs.iter().map(|r| &r.proc))
        .any(|key| old.has_statics(key) || new.has_statics(key));
    if !statics && differences(&old.initializers(false), &new.initializers(false)).is_none() {
        return;
    }
    let (before, after) = (old.initializers(true), new.initializers(true));
    let Some(table) = differences(&before, &after) else {
        return;
    };
    let deferred = changed(new, &before, &after);
    let table_vars = table.len();
    // Keep the build's initializers after a reboot if replacement fails.
    let without = |why: String| {
        format!("(global init): {why}: reboot will use the build's global and static initializers")
    };

    let proc_statics = proc_statics(
        table
            .iter()
            .map(|(init, _)| *init)
            .chain(after.iter().filter(|i| i.constant.is_none())),
    );
    let runs = table.into_iter().chain(
        after
            .iter()
            .filter(|i| i.constant.is_none())
            .map(|i| (i, i.expression)),
    );
    let (nodes, locals) = match build(new, runs) {
        Ok(code) => code,
        Err(why) => {
            changes.warnings.push(without(why));
            changes
                .deferred
                .extend(deferred.into_iter().map(|d| d.until(super::Until::Rebuild)));
            return;
        }
    };
    changes.deferred.extend(deferred);
    changes.global_init = Some(GlobalInit {
        code: ProcReload {
            proc: super::ProcKey {
                path: "(global init)".to_owned(),
                override_id: 0,
            },
            nodes,
            locals,
            params: vec![],
            settings: None,
            statics: vec![],
            warnings: vec![],
        },
        table: table_vars,
        proc_statics,
    });
}

impl GlobalInit {
    /// Remove each initializer and assignment selected by `leave`.
    /// Retain file records for the following code.
    pub fn without(&self, mut leave: impl FnMut(&GlobalVar) -> bool) -> GlobalInit {
        let mut out = self.clone();
        out.code.nodes = vec![];
        out.table = 0;
        let (mut span, mut sets) = (vec![], 0);
        for node in &self.code.nodes {
            span.push(node.clone());
            if let Node::Instruction(Instruction::SetVar(Variable::Global(var)), _) = node {
                if leave(var) {
                    // Preserve the file record for following code.
                    span.retain(|n| matches!(n, Node::Instruction(Instruction::DbgFile(_), _)));
                } else {
                    out.table += usize::from(sets < self.table);
                }
                out.code.nodes.append(&mut span);
                sets += 1;
            }
        }
        out.code.nodes.append(&mut span);
        out
    }

    /// Restore the build's initializer order. `built` returns each var's
    /// `SetVar` index in the original proc, using its scope and code location,
    /// or `None` for a new or previously constant var. Place those vars after
    /// the preceding var in dmasm's order. Keep var-table defaults first.
    pub fn in_built_order(
        &self,
        mut built: impl FnMut(&GlobalVar, &[Node]) -> Option<usize>,
    ) -> Vec<Node> {
        // Split each assignment with its value code and current file.
        let mut spans: Vec<(Option<usize>, Vec<Node>)> = vec![];
        let (mut span, mut file) = (vec![], None);
        for node in &self.code.nodes {
            match node {
                Node::Instruction(Instruction::End, _) => continue,
                Node::Instruction(Instruction::DbgFile(_), _) => file = Some(node.clone()),
                _ if span.is_empty() => span.extend(file.clone()),
                _ => {}
            }
            span.push(node.clone());
            if let Node::Instruction(Instruction::SetVar(Variable::Global(var)), _) = node {
                let place = if spans.len() < self.table {
                    None
                } else {
                    built(var, &span)
                };
                spans.push((place, std::mem::take(&mut span)));
            }
        }
        let rest = spans.split_off(self.table.min(spans.len()));
        // Use the preceding var's built position, or none at the start.
        let mut ordered: Vec<((Option<usize>, bool, usize), Vec<Node>)> = vec![];
        let mut after = None;
        for (n, (place, span)) in rest.into_iter().enumerate() {
            if place.is_some() {
                after = place;
            }
            ordered.push(((after, place.is_none(), n), span));
        }
        ordered.sort_by_key(|(key, _)| *key);
        let mut nodes: Vec<Node> = spans.into_iter().flat_map(|(_, span)| span).collect();
        nodes.extend(ordered.into_iter().flat_map(|(_, span)| span));
        nodes.push(Node::Instruction(Instruction::End, ()));
        // Emit a file record when the file changes.
        let mut file = None;
        nodes.retain(|node| match node {
            Node::Instruction(Instruction::DbgFile(name), _) => {
                file.replace(name.clone()).as_ref() != Some(name)
            }
            _ => true,
        });
        nodes
    }
}
