//! Compare source snapshots and compile hot reload changes.
//!
//! [`Snapshot::parse`] reads a project; [`diff`] produces [`Changes`] for code,
//! metadata, types and variables. Unchanged procs with shifted source lines
//! use [`LineMove`] so they retain DreamMaker's bytecode.
//!
//! Warnings describe retained runtime state and compatibility differences.
//! Any `refused` diagnostic requires rejecting the change set.
//!
//! [`Snapshot::reload_file`] reparses one file when only proc bodies changed.
//! Procs use auxtools paths without `/proc` or `/verb`, plus a zero-based
//! `override_id` in proc-table order.

use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::{Path, PathBuf};

use dreammaker::ast::{Block, Statement};
use dreammaker::objtree::{ObjectTree, ProcValue, TypeRef};
use dreammaker::preprocessor::DefineHistory;
use dreammaker::{Context, FileId, Severity};

use crate::compiler::{compile_proc, CompiledProc, ProcSource};

#[path = "reload_reparent.rs"]
mod reparent;
use crate::{ByondVersion, Instruction, Node};
pub use reparent::{Reparented, ReparentedType};

/// A proc as auxtools' debug server names it.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ProcKey {
    /// The type's path and the proc's name, without `/proc` or `/verb`.
    pub path: String,
    /// Zero-based definition index for `path`.
    pub override_id: u32,
}

impl std::fmt::Display for ProcKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.override_id == 0 {
            f.write_str(&self.path)
        } else {
            write!(f, "{} (override {})", self.path, self.override_id)
        }
    }
}

/// Compiled replacement for a changed proc.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ProcReload {
    pub proc: ProcKey,
    /// New bytecode including debug instructions. Drop DbgFile and DbgLine
    /// when applying to a world built without DEBUG.
    pub nodes: Vec<Node>,
    /// Local variable names, by slot.
    pub locals: Vec<String>,
    pub params: Vec<Param>,
    /// None if the proc settings are unchanged.
    pub settings: Option<ProcSettings>,
    /// Static locals; allocate and initialize those absent from the running proc.
    pub statics: Vec<StaticVar>,
    /// Compatibility warnings, also included in `Changes::warnings`.
    #[cfg_attr(feature = "serde", serde(default))]
    pub warnings: Vec<String>,
}

/// A static local (or local const) of a proc.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct StaticVar {
    pub name: String,
    /// The scope the proc's code names it with (`proc:PATH#N`).
    pub scope: String,
    /// Constant initializer, or `None` for a computed value.
    pub value: Option<ConstValue>,
    /// Computed initializer to evaluate once in the running world.
    pub init: Option<Box<ProcReload>>,
}

/// Runtime parameter metadata.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Param {
    pub name: String,
    /// Allowed input types encoded as BYOND bits: mob 1, obj 2, text 4, num 8, etc.
    pub input: u32,
    /// `as command_text` is 1, `as color` 2.
    pub input_extra: u32,
    /// Source of choices for the parameter's `in` clause.
    pub in_list: InList,
}

/// A parameter's `in` list.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum InList {
    /// Encoded built-in source: kind in the low byte (`view` 1, `oview` 2,
    /// `usr.loc` 3, `range` 5, `orange` 6, `usr` 8, `world` 0x10), with the
    /// range or 0x7D/0x7F sentinel above it. No clause defaults to 0x7D01.
    Builtin(u32),
    /// Reuse the running parameter's list expression proc.
    Kept,
    /// New list expression compiled as a proc returning the choices.
    New(Box<ProcReload>),
}

/// The word BYOND keeps for an `in` list it has built in.
fn builtin_in_list(in_list: Option<&dreammaker::ast::Expression>) -> Option<u32> {
    use dreammaker::ast::{Expression, Follow, Term};
    const NO_RANGE: u32 = 0x7D;
    const NOT_RANGED: u32 = 0x7F;
    let Some(expression) = in_list else {
        return Some(NO_RANGE << 8 | 1);
    };
    let Expression::Base { term, follow } = expression else {
        return None;
    };
    let fields: Vec<&str> = follow
        .iter()
        .map(|f| match &f.elem {
            Follow::Field(_, name) => Some(name.as_str()),
            _ => None,
        })
        .collect::<Option<_>>()?;
    let is_usr = |e: &Expression| matches!(e, Expression::Base { term, follow } if follow.is_empty() && matches!(&term.elem, Term::Ident(i) if i.as_str() == "usr"));
    match (&term.elem, fields.as_slice()) {
        (Term::Ident(name), []) if name.as_str() == "world" => Some(NOT_RANGED << 8 | 0x10),
        (Term::Ident(name), [] | ["contents"]) if name.as_str() == "usr" => {
            Some(NOT_RANGED << 8 | 8)
        }
        (Term::Ident(name), ["loc"]) if name.as_str() == "usr" => Some(NOT_RANGED << 8 | 3),
        (Term::Call(name, args), []) => {
            let kind = match name.as_str() {
                "view" => 1,
                "oview" => 2,
                "range" => 5,
                "orange" => 6,
                _ => return None,
            };
            let mut range = NO_RANGE;
            for arg in args.iter() {
                match arg {
                    Expression::Base { term, follow } if follow.is_empty() => match term.elem {
                        Term::Int(n) if (0..NO_RANGE as i32).contains(&n) && range == NO_RANGE => {
                            range = n as u32
                        }
                        _ if is_usr(arg) => {}
                        _ => return None,
                    },
                    _ => return None,
                }
            }
            Some(range << 8 | kind)
        }
        _ => None,
    }
}

/// A proc's `set` statements, which BYOND keeps in its table entry.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ProcSettings {
    pub waitfor: bool,
    pub background: bool,
    /// `set name`, `desc` and `category`, where set
    pub name: Option<String>,
    pub desc: Option<String>,
    pub category: Option<String>,
    /// True for `set category = null`, hiding the verb from all categories.
    pub no_category: bool,
    pub hidden: bool,
    pub popup_menu: bool,
    pub instant: bool,
    pub invisibility: Option<u8>,
    /// `set src`: assignment-vs-membership flag and encoded source
    /// (see `InList::Builtin`).
    pub src: Option<(bool, u32)>,
}

impl Default for ProcSettings {
    fn default() -> Self {
        ProcSettings {
            waitfor: true,
            background: false,
            name: None,
            desc: None,
            category: None,
            no_category: false,
            hidden: false,
            popup_menu: true,
            instant: false,
            invisibility: None,
            src: None,
        }
    }
}

impl ProcSettings {
    /// The flags of the proc's table entry, as DreamMaker sets them.
    pub fn flags(&self) -> u32 {
        let mut flags = 0;
        if self.hidden {
            flags |= 0x01;
        }
        if self.src.is_some_and(|(equals, _)| equals) {
            flags |= 0x02;
        }
        if self.waitfor {
            flags |= 0x04;
        }
        if self.no_category {
            flags |= 0x20;
        }
        if let Some(invisibility) = self.invisibility {
            flags |= 0x08;
            if invisibility != 0 {
                flags |= 0x10;
            }
        }
        if !self.popup_menu {
            flags |= 0x40;
        }
        if self.background {
            flags |= 0x100;
        }
        if self.instant {
            flags |= 0x200;
        }
        // DreamMaker writes the longer form for these
        if flags & 0x318 != 0 {
            flags |= 0x80;
        }
        flags
    }

    /// Three packed bytes: source range, source kind and invisibility.
    pub fn packed(&self) -> u32 {
        let (range, kind) = match self.src {
            None => (0xFF, 0),
            Some((_, word)) => (word >> 8, word & 0xFF),
        };
        let invisibility = match self.invisibility {
            None => 0,
            Some(0) => 0xFF,
            Some(n) => n as u32,
        };
        range | kind << 8 | invisibility << 16
    }

    /// Mask of proc flag bits controlled by these settings.
    pub const FLAGS: u32 = 0x3FF;
}

/// New settings for a proc whose code is as it was.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct SettingsChange {
    pub proc: ProcKey,
    pub settings: ProcSettings,
}

/// A proc the running world doesn't have yet.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct NewProc {
    /// The type it's on, `""` for a global proc.
    pub type_path: String,
    pub name: String,
    /// Declared on this type (`proc/name`), rather than overriding a proc of
    /// a parent type.
    pub declared: bool,
    /// A verb: on its type's list of verbs rather than of procs
    pub verb: bool,
    pub code: ProcReload,
}

/// Removed source definition. Unlink it from type lookup and override chains
/// so calls reach the definition it overrode. Global definitions are excluded
/// only from numbering because old bytecode may still call them.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct RemovedProc {
    /// The definition as the running world knows it
    pub proc: ProcKey,
    pub type_path: String,
    pub name: String,
    pub verb: bool,
}

/// A constant value, as a var default can have.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ConstValue {
    Null,
    Number(f32),
    String(String),
    /// A type path
    Path(String),
    /// A file in the resource cache (`'icon.dmi'`)
    Resource(String),
}

/// A var whose default changed.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct VarDefault {
    pub var: String,
    pub value: ConstValue,
    /// The type whose default changed, and every subtype that gets its
    /// default from there.
    pub types: Vec<String>,
}

/// Built-in override removed from a type. Copy the running parent's value
/// to the type and inheriting subtypes to preserve BYOND's built-in defaults.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct BuiltinInherited {
    pub var: String,
    /// Parents first
    pub types: Vec<String>,
}

/// A proc whose code is unchanged but has moved to other lines.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LineMove {
    pub proc: ProcKey,
    /// `(old line, new line)` for every line the proc's `DbgLine`s name,
    /// sorted by old line.
    pub lines: Vec<(u32, u32)>,
}

/// A type the running world doesn't have yet. It starts as a copy of its
/// parent, with these fields of its record set; its vars and procs come as
/// new vars, var defaults, new procs and an init proc.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct NewType {
    pub path: String,
    pub parent: String,
    /// Built-in vars kept in the type's record, where they aren't the
    /// parent's: `name`, `desc`, `icon`, `icon_state`, `text`, `suffix`,
    /// `dir`, `layer`, `density` and `opacity`.
    pub fields: Vec<(String, ConstValue)>,
}

/// A var the running world doesn't have yet.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct NewVar {
    /// The type that declares it, `""` for a global var.
    pub type_path: String,
    pub var: String,
    /// As BYOND keeps them for a declared var: 1 static (or global), 2
    /// const, 4 tmp.
    pub flags: u32,
    /// Initial value for the newly declared variable.
    pub value: ConstValue,
    /// Computed initializer for a global or static variable; evaluate once
    /// and use null as the placeholder `value`.
    pub init: Option<Box<ProcReload>>,
}

/// Variable declaration changes to static/global, const or tmp flags.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Redeclared {
    /// The type that declares it ("" for a global const whose value changed)
    pub type_path: String,
    pub var: String,
    /// As for a new var: 1 static (or global), 2 const, 4 tmp.
    pub flags: u32,
    /// Initial value for a newly static variable or changed const. `None`
    /// preserves the current value. Instance defaults are applied separately.
    pub value: Option<ConstValue>,
    /// Otherwise, code that returns it, for the running world to run once.
    pub init: Option<Box<ProcReload>>,
}

/// Computed-default initializer for new instances, including inherited values.
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct InitProc {
    pub type_path: String,
    /// `None` when the type has no such vars any more.
    pub code: Option<ProcReload>,
}

/// What changed between two snapshots.
#[derive(Debug, Default)]
pub struct Changes {
    /// New types in parent-first order, applied before dependent changes.
    pub new_types: Vec<NewType>,
    pub reloads: Vec<ProcReload>,
    /// Settings changes without bytecode changes.
    pub settings: Vec<SettingsChange>,
    pub new_procs: Vec<NewProc>,
    /// Removed type proc definitions; unlink before adding replacements.
    pub removed_procs: Vec<RemovedProc>,
    /// New variables, applied before defaults on their inheriting subtypes.
    pub new_vars: Vec<NewVar>,
    pub var_defaults: Vec<VarDefault>,
    /// Built-in defaults stored in type records rather than variable tables.
    pub builtin_defaults: Vec<VarDefault>,
    /// Built-in defaults restored from runtime parents after `builtin_defaults`.
    pub builtin_inherited: Vec<BuiltinInherited>,
    /// Changed constants; dependent procs are included in `reloads`.
    pub consts: Vec<String>,
    /// Changed defaults embedded by `/type::var`; recompile dependent procs.
    pub defaults_changed: Vec<String>,
    /// Defaults moved to new source locations; update dependent debug info.
    pub defaults_moved: Vec<String>,
    /// Changed variable type or declaration metadata. Include affected
    /// recompiled procs in `reloads`.
    pub retyped: Vec<String>,
    /// Declaration flag changes, applied after new variables and before defaults.
    pub redeclared: Vec<Redeclared>,
    /// Changed computed-default init procs for new instances.
    pub init_procs: Vec<InitProc>,
    /// Parent changes, parents first, applied after new types and before other changes.
    pub reparented: Vec<Reparented>,
    pub line_moves: Vec<LineMove>,
    /// Runtime state retained by reload and compiler compatibility warnings.
    pub warnings: Vec<String>,
    /// Rejection reasons. Any entry requires rejecting the entire change set.
    pub refused: Vec<String>,
    /// Changed world and client settings, as (type, var). Warn that they take
    /// effect after restart (see `settings_as_built`).
    pub restart_settings: Vec<(String, String)>,
}

impl Changes {
    /// Suppress restart warnings for settings restored to their built values.
    /// The diff compares against the last reload, so these changes otherwise
    /// appear to need a restart.
    pub fn settings_as_built(&mut self, built: &Snapshot, new: &Snapshot) {
        let value = |s: &Snapshot, path: &str, name: &str| {
            s.find_type(path)
                .and_then(|t| t.get_value(name))
                .map(|v| v.expression.clone())
        };
        let mut kept = vec![];
        for (path, name) in std::mem::take(&mut self.restart_settings) {
            if value(built, &path, &name) == value(new, &path, &name) {
                let warning = restart_warning(&path, &name);
                self.warnings.retain(|w| *w != warning);
            } else {
                kept.push((path, name));
            }
        }
        self.restart_settings = kept;
    }

    /// Merge compiled-proc warnings into the change set.
    fn collect_warnings(&mut self) {
        for reload in &self.reloads {
            for warning in &reload.warnings {
                self.warnings.push(format!("{}: {warning}", reload.proc));
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        self.reloads.is_empty()
            && self.settings.is_empty()
            && self.new_types.is_empty()
            && self.new_procs.is_empty()
            && self.removed_procs.is_empty()
            && self.new_vars.is_empty()
            && self.redeclared.is_empty()
            && self.var_defaults.is_empty()
            && self.builtin_defaults.is_empty()
            && self.builtin_inherited.is_empty()
            && self.init_procs.is_empty()
            && self.reparented.is_empty()
            && self.line_moves.is_empty()
            && self.refused.is_empty()
    }
}

/// How to parse a `.dme`.
#[derive(Debug, Clone, Default)]
pub struct ParseOptions {
    /// Defines given to DreamMaker on its command line (`-DNAME` or
    /// `-DNAME=VALUE`), as `(name, value)`.
    pub defines: Vec<(String, Option<String>)>,
    /// The BYOND version the world runs on, `(version, build)`, for
    /// `DM_VERSION` and `DM_BUILD`. SpacemanDMM's own (516.1666) when `None`.
    pub byond: Option<(u32, u32)>,
}

/// What the defines in front of the `.dme` are called, as a file.
const PRELUDE: &str = "<hot reload prelude>";

/// One parse of a codebase.
pub struct Snapshot {
    ctx: Context,
    tree: ObjectTree,
    /// Compiled-source definitions indexed by type, name and override index.
    procs: BTreeMap<ProcKey, (String, String, usize)>,
    /// Macro history used to reparse individual files.
    defines: Option<DefineHistory>,
    /// Reparsed definitions overriding the original tree.
    patched: HashMap<ProcKey, ProcValue>,
    /// The build the code is compiled for
    byond: ByondVersion,
    /// The `.dme`'s directory, when parsed from one
    root: Option<PathBuf>,
    /// Nonstandard debug source names indexed by project-relative path
    /// (see `debug_files`).
    debug_names: HashMap<PathBuf, String>,
    /// Names referenced through `::` in each file, with source lines.
    scope_names: HashMap<FileId, Vec<(String, u32)>>,
    /// The procs defined in each file, once asked for
    procs_by_file: std::sync::OnceLock<HashMap<FileId, Vec<ProcKey>>>,
    /// Cached source locations of defaults embedded in each proc's debug info.
    default_lines: std::sync::Mutex<HashMap<ProcKey, Vec<(Vec<u8>, u32)>>>,
    /// Initializer locations folded by dmasm after SpacemanDMM evaluation failed.
    folded: std::collections::BTreeSet<dreammaker::Location>,
    /// Files reparsed successfully by `keep`; their original parse errors no longer apply.
    mended: HashSet<FileId>,
    /// Last `.dmf` included by the preprocessor: the world's skin.
    skin: Option<PathBuf>,
}

/// Incremental changes from reparsing one file (see `Snapshot::reload_file`).
pub struct FileReload {
    pub changes: Changes,
    /// The file's definitions as they are now
    patched: Vec<(ProcKey, ProcValue)>,
    /// The file, and the names it reads with `::` now
    file: FileId,
    scope_names: Vec<(String, u32)>,
}

impl Snapshot {
    /// Parses `dme` as DreamMaker would with `options`.
    pub fn parse(dme: &Path, options: &ParseOptions) -> Result<Snapshot, String> {
        let ctx = Context::default();
        if let Some((major, build)) = options.byond {
            ByondVersion::new(major, build).read_as(&ctx);
        }
        let mut pp = dreammaker::Preprocessor::new(&ctx, dme.to_owned())
            .map_err(|e| format!("couldn't read {}: {}", dme.display(), e))?;

        // Remove linter-only defines and use the target BYOND version.
        // A synthetic prelude preserves the `.dme`'s original line numbers.
        let mut prelude = String::from("#undef SPACEMAN_DMM\n");
        if let Some((version, build)) = options.byond {
            prelude.push_str(&format!(
                "#undef DM_VERSION\n#define DM_VERSION {version}\n#undef DM_BUILD\n#define DM_BUILD {build}\n"
            ));
        }
        for (name, value) in &options.defines {
            prelude.push_str(&format!(
                "#define {} {}\n",
                name,
                value.as_deref().unwrap_or("")
            ));
        }
        pp.push_file(
            PathBuf::from(PRELUDE),
            std::io::Cursor::new(prelude.into_bytes()),
        )
        .map_err(|e| e.to_string())?;

        let tree = {
            let mut parser = dreammaker::Parser::new(&ctx, &mut pp);
            parser.enable_procs();
            parser.parse_object_tree()
        };
        let skin = pp.skins().last().cloned();
        let defines = pp.finalize();
        let mut snapshot = Self::from_tree(ctx, tree);
        snapshot.defines = Some(defines);
        snapshot.root = dme.parent().map(Path::to_owned);
        snapshot.skin = skin.map(|skin| match &snapshot.root {
            Some(root) => skin.strip_prefix(root).map(Path::to_owned).unwrap_or(skin),
            None => skin,
        });
        if let Some(root) = &snapshot.root {
            let mut scope_names = HashMap::new();
            snapshot.ctx.files().for_each(|path| {
                if let (Some(id), Ok(text)) = (
                    snapshot.ctx.files().get_id(path),
                    std::fs::read(root.join(path)),
                ) {
                    scope_names.insert(id, names_after_scope(&String::from_utf8_lossy(&text)));
                }
            });
            snapshot.scope_names = scope_names;

            let read: HashSet<PathBuf> = snapshot
                .files()
                .iter()
                .map(|path| crate::debug_files::normalize(path))
                .collect();
            snapshot.debug_names = crate::debug_files::names(dme, &|path| read.contains(path))
                .into_iter()
                .filter(|(name, path)| Path::new(name) != path)
                .map(|(name, path)| (path, name))
                .collect();
        }
        if let Some((major, build)) = options.byond {
            snapshot.byond = ByondVersion::new(major, build);
        }
        snapshot.fold_const_initializers();
        Ok(snapshot)
    }

    /// Reparse `file` using its original macro context for proc-body-only edits.
    /// `file` is relative to the project root. Other edits return `Err` with
    /// the reason a full parse is needed.
    ///
    /// Does not mutate the snapshot. Call `keep` after the world applies the changes.
    pub fn reload_file(&mut self, root: &Path, file: &Path) -> Result<FileReload, String> {
        let history = self
            .defines
            .as_ref()
            .ok_or("snapshot has no macro history")?;
        let id = self
            .ctx
            .files()
            .get_id(file)
            .ok_or("file is not part of the snapshot")?;
        let text = std::fs::read(root.join(file)).map_err(|e| e.to_string())?;
        let scope_names = names_after_scope(&String::from_utf8_lossy(&text));
        if String::from_utf8_lossy(&text)
            .lines()
            .any(|line| line.trim_start().starts_with("#include"))
        {
            return Err("file contains includes".to_owned());
        }

        // Preserve file IDs used by macro source locations.
        let ctx = Context::default();
        self.byond.read_as(&ctx);
        self.ctx.files().for_each(|path| {
            ctx.register_file(path);
        });
        let mut pp = history.branch_at_file(id, &ctx);
        pp.push_file(file.to_owned(), std::io::Cursor::new(text))
            .map_err(|e| e.to_string())?;
        let tree = {
            let mut parser = dreammaker::Parser::new(&ctx, &mut pp);
            parser.enable_procs();
            parser.parse_object_tree_without_builtins()
        };
        let defines = pp.finalize();
        if let Some(e) = ctx
            .errors()
            .iter()
            .find(|e| e.severity() == Severity::Error)
        {
            return Err(format!("{}: {}", e.location().line, e.description()));
        }
        if macros_in(history, id) != macros_in(&defines, id) {
            return Err("file macros changed".to_owned());
        }

        let old = self.contents_of(id)?;
        let mut new = FileContents::default();
        for ty in tree.iter_types() {
            let path = &ty.get().path;
            if !ty.is_root() && self.tree.find(path).is_none() {
                return Err(format!("{path} is a new type"));
            }
            new.add(ty, id);
        }
        if old.vars != new.vars {
            return Err("file vars changed".to_owned());
        }
        if old.declarations != new.declarations {
            return Err("proc declarations changed".to_owned());
        }
        // Moving a default referenced through `::` requires recompiling
        // its users during a full parse.
        let moved: Vec<(&String, u32)> = old
            .var_lines
            .iter()
            .filter_map(|(k, line)| {
                new.var_lines
                    .get(k)
                    .filter(|n| *n != line)
                    .map(|_| (&k.1, *line))
            })
            .collect();
        if !moved.is_empty() {
            if let Some(key) = self.reads_moved_default(id, &moved) {
                return Err(format!("{key} reads a var default with :: that moved"));
            }
        }
        let mut pairs = vec![];
        for (proc, old_values) in &old.procs {
            let new_values = new.procs.get(proc).ok_or("proc definitions were removed")?;
            if new_values.len() != old_values.len() {
                return Err("proc definitions were added or removed".to_owned());
            }
            for ((key, old_value), (_, new_value)) in old_values.iter().zip(new_values) {
                let key = key
                    .clone()
                    .ok_or("proc definition is missing from the baseline")?;
                let defaults = |v: &ProcValue| {
                    v.parameters
                        .iter()
                        .map(|p| p.default.clone())
                        .collect::<Vec<_>>()
                };
                if signature(old_value) != signature(new_value)
                    || defaults(old_value) != defaults(new_value)
                {
                    return Err(format!("{key}: parameters changed"));
                }
                if settings(old_value.code.as_ref()) != settings(new_value.code.as_ref()) {
                    return Err(format!("{key}: `set`s changed"));
                }
                pairs.push((key, (*old_value).clone(), (*new_value).clone()));
            }
        }
        if new.procs.len() != old.procs.len() {
            return Err("proc definitions were added".to_owned());
        }

        // Lines that moved need the code as it was, compiled before the new
        // definitions go in
        let mut changes = Changes::default();
        let mut before = HashMap::new();
        for (key, old_value, new_value) in &pairs {
            if old_value.code == new_value.code && old_value.location != new_value.location {
                before.insert(key.clone(), self.compile(key));
            }
        }
        let previous: Vec<(ProcKey, Option<ProcValue>)> = pairs
            .iter()
            .map(|(key, _, new_value)| {
                (
                    key.clone(),
                    self.patched.insert(key.clone(), new_value.clone()),
                )
            })
            .collect();
        for (key, old_value, new_value) in &pairs {
            if old_value.code != new_value.code {
                let reload = self.compile_code(key).and_then(|mut reload| {
                    self.compile_in_lists(key, &mut reload, |_, _| false)?;
                    Ok(reload)
                });
                match reload {
                    Ok(reload) => changes.reloads.push(reload),
                    Err(e) => changes.refused.push(format!("{key}: doesn't compile: {e}")),
                }
            } else if let Some(Ok(old_code)) = before.remove(key) {
                // Every line, as `diff` gives them, when any moved
                let lines: BTreeMap<u32, u32> = match self.compile(key) {
                    Ok(new_code) => dbg_lines(&old_code.nodes)
                        .into_iter()
                        .zip(dbg_lines(&new_code.nodes))
                        .collect(),
                    Err(_) => BTreeMap::new(),
                };
                let lines: Vec<(u32, u32)> = lines.into_iter().collect();
                if lines.iter().any(|(o, n)| o != n) {
                    changes.line_moves.push(LineMove {
                        proc: key.clone(),
                        lines,
                    });
                }
            }
        }
        for (key, value) in previous {
            match value {
                Some(value) => self.patched.insert(key, value),
                None => self.patched.remove(&key),
            };
        }
        changes.collect_warnings();
        Ok(FileReload {
            changes,
            patched: pairs
                .into_iter()
                .map(|(key, _, new_value)| (key, new_value))
                .collect(),
            file: id,
            scope_names,
        })
    }

    /// Accept incremental source changes after successful application.
    pub fn keep(&mut self, reload: FileReload) {
        let default_lines = self.default_lines.get_mut().unwrap();
        for (key, _) in &reload.patched {
            default_lines.remove(key);
        }
        self.patched.extend(reload.patched);
        self.scope_names.insert(reload.file, reload.scope_names);
        // This successful reparse preserved the definitions, so the old error hid no code.
        self.mended.insert(reload.file);
    }

    /// Run `f` against the original parse, excluding incremental replacements.
    pub fn without_patches<R>(&mut self, f: impl FnOnce(&Snapshot) -> R) -> R {
        let patched = std::mem::take(&mut self.patched);
        let result = f(self);
        self.patched = patched;
        result
    }

    /// Discard incremental file replacements.
    pub fn clear_patches(&mut self) {
        self.patched.clear();
        self.mended.clear();
    }

    /// What `file` defines: vars, proc declarations and definitions
    fn contents_of(&self, file: FileId) -> Result<FileContents<'_>, String> {
        let mut keys: HashMap<(&str, &str, usize), &ProcKey> = HashMap::new();
        for (key, (ty, name, index)) in &self.procs {
            keys.insert((ty.as_str(), name.as_str(), *index), key);
        }
        let mut contents = FileContents::default();
        for ty in self.tree.iter_types() {
            contents.add(ty, file);
            let path = &ty.get().path;
            for (name, proc) in ty.get().procs.iter() {
                let Some(values) = contents.procs.get_mut(&(path.clone(), name.to_string())) else {
                    continue;
                };
                // By key, and as the snapshot has them now
                for (key, value) in values.iter_mut() {
                    let index = proc.value.iter().position(|v| std::ptr::eq(v, *value));
                    let found =
                        index.and_then(|i| keys.get(&(path.as_str(), name.as_str(), i)).copied());
                    *key = found.cloned();
                    if let Some(patched) = found.and_then(|k| self.patched.get(k)) {
                        *value = patched;
                    }
                }
            }
        }
        Ok(contents)
    }

    /// A snapshot of code already parsed into `tree`, with `ctx` holding its
    /// file names relative to the `.dme`'s directory.
    pub fn from_tree(ctx: Context, tree: ObjectTree) -> Snapshot {
        crate::compiler::new_tree();
        let mut procs = BTreeMap::new();
        for ty in tree.iter_types() {
            for (name, type_proc) in ty.get().procs.iter() {
                let mut override_id = 0;
                for (index, value) in type_proc.value.iter().enumerate() {
                    // SpacemanDMM's declarations of BYOND's own procs
                    if value.location.file == FileId::BUILTINS {
                        continue;
                    }
                    let key = ProcKey {
                        path: proc_path(ty, name.as_str()),
                        override_id,
                    };
                    override_id += 1;
                    procs.insert(
                        key,
                        (ty.get().path.clone(), name.as_str().to_owned(), index),
                    );
                }
            }
        }
        Snapshot {
            ctx,
            tree,
            procs,
            defines: None,
            patched: HashMap::new(),
            byond: ByondVersion::LATEST,
            root: None,
            debug_names: HashMap::new(),
            scope_names: HashMap::new(),
            procs_by_file: std::sync::OnceLock::new(),
            default_lines: std::sync::Mutex::new(HashMap::new()),
            folded: Default::default(),
            mended: Default::default(),
            skin: None,
        }
    }

    /// Fold const initializers beyond SpacemanDMM's evaluator and suppress their errors.
    /// Repeat to resolve consts that depend on other consts.
    fn fold_const_initializers(&mut self) {
        let described: Vec<(dreammaker::Location, String)> = self
            .ctx
            .errors()
            .iter()
            .filter(|e| e.severity() == Severity::Error)
            .map(|e| (e.location(), e.description().to_owned()))
            .collect();
        let errors: Vec<dreammaker::Location> = described.iter().map(|(at, _)| *at).collect();
        if errors.is_empty() {
            return;
        }
        let mut names: HashSet<String> = HashSet::new();
        loop {
            let mut found = vec![];
            for ty in self.tree.iter_types() {
                for (name, var) in ty.get().vars.iter() {
                    let location = var.value.location;
                    if var.value.constant.is_some()
                        || !errors.contains(&location)
                        || self.folded.contains(&location)
                    {
                        continue;
                    }
                    let Some(expression) = var.value.expression.as_ref() else {
                        continue;
                    };
                    let is_const = ty
                        .get_var_declaration(name.as_str())
                        .is_some_and(|d| d.var_type.flags.is_const());
                    if !is_const {
                        continue;
                    }
                    if let Some(constant) =
                        crate::compiler::fold_initializer(&self.tree, ty, expression, self.byond)
                    {
                        found.push((ty.index(), name.clone(), constant, location));
                    }
                }
            }
            if found.is_empty() {
                break;
            }
            for (ty, name, constant, location) in found {
                if let Some(var) = self.tree[ty].vars.get_mut(&name) {
                    var.value.constant = Some(constant);
                }
                self.folded.insert(location);
                names.insert(name.to_string());
            }
        }
        // SpacemanDMM also reports recursive-reference errors for vars using a const
        // it could not evaluate. Suppress those after resolving the const.
        for (location, description) in described {
            let of = description.strip_prefix("recursive constant reference: ");
            if of.is_some_and(|name| names.contains(name)) {
                self.folded.insert(location);
            }
        }
    }

    /// Target build from `ParseOptions::byond`, or the newest supported build.
    pub fn byond(&self) -> ByondVersion {
        self.byond
    }

    /// Compiles for `byond` from now on.
    pub fn set_byond(&mut self, byond: ByondVersion) {
        self.byond = byond;
    }

    pub fn tree(&self) -> &ObjectTree {
        &self.tree
    }

    pub fn context(&self) -> &Context {
        &self.ctx
    }

    /// Every file the parse read, relative to the `.dme`'s directory (the
    /// `.dme` itself has its own path, as given).
    pub fn files(&self) -> Vec<PathBuf> {
        let mut files = vec![];
        self.ctx.files().for_each(|path| {
            if path != Path::new(PRELUDE) {
                files.push(path.to_owned());
            }
        });
        files
    }

    /// Last `.dmf` included by the `.dme`, relative to the `.dme` directory.
    pub fn skin(&self) -> Option<&Path> {
        self.skin.as_deref()
    }

    /// Every proc with code.
    pub fn procs(&self) -> impl Iterator<Item = &ProcKey> {
        self.procs.keys()
    }

    /// The file a proc is in, from the `.dme`'s directory.
    pub fn proc_file(&self, key: &ProcKey) -> Option<String> {
        let (_, value) = self.find(key)?;
        Some(self.file_name(value.location.file))
    }

    /// Compiles one proc for a reload: its code, parameters and settings.
    pub fn compile_reload(&self, key: &ProcKey) -> Result<ProcReload, String> {
        let mut reload = self.compile_code(key)?;
        reload.settings = Some(self.settings(key)?);
        Ok(reload)
    }

    /// Compiles one proc for a reload, without its settings.
    fn compile_code(&self, key: &ProcKey) -> Result<ProcReload, String> {
        let (_, value) = self.find(key).ok_or_else(|| format!("no proc {key}"))?;
        let compiled = self.compile(key)?;
        let (ty, _) = self.find(key).ok_or_else(|| format!("no proc {key}"))?;
        let mut statics = vec![];
        for s in compiled.statics {
            let constant = s.value.as_ref().and_then(const_value);
            let init = match (&constant, &s.expression) {
                // `__PROC__` must name the containing proc.
                (None, Some(expression)) => Some(Box::new(self.compile_value(
                    ty,
                    expression,
                    &s.type_path,
                    value_location(expression, value.location),
                    static_proc_path(&s.scope),
                )?)),
                _ => None,
            };
            statics.push(StaticVar {
                name: s.name,
                value: constant,
                scope: s.scope,
                init,
            });
        }
        Ok(ProcReload {
            proc: key.clone(),
            nodes: compiled.nodes,
            locals: compiled.locals,
            params: params(value),
            settings: None,
            statics,
            warnings: compiled.warnings,
        })
    }

    /// Compile newly added parameter `in` expressions into generated procs.
    fn compile_in_lists(
        &self,
        key: &ProcKey,
        reload: &mut ProcReload,
        new: impl Fn(&str, &dreammaker::ast::Expression) -> bool,
    ) -> Result<(), String> {
        let (ty, value) = self.find(key).ok_or_else(|| format!("no proc {key}"))?;
        for param in reload.params.iter_mut() {
            if param.in_list != InList::Kept {
                continue;
            }
            let Some(expression) = value
                .parameters
                .iter()
                .find(|p| p.name.as_str() == param.name)
                .and_then(|p| p.in_list.as_ref())
            else {
                continue;
            };
            if new(&param.name, expression) {
                let location = value_location(expression, value.location);
                let code = self
                    .compile_in_list(ty, expression, location)
                    .map_err(|e| format!("parameter {}'s `in` list: {e}", param.name))?;
                param.in_list = InList::New(Box::new(code));
            }
        }
        Ok(())
    }

    /// Compile a parameter `in` expression as a return proc with DEBUG locations.
    fn compile_in_list(
        &self,
        ty: TypeRef,
        expression: &dreammaker::ast::Expression,
        location: dreammaker::Location,
    ) -> Result<ProcReload, String> {
        use dreammaker::ast::{Spanned, Statement};
        let block: Block = Box::new([Spanned::new(
            location,
            Statement::Return(Some(expression.clone())),
        )]);
        self.compile_synthetic(ty, "(in list)", block, location, true, None)
    }

    /// Compile a computed global or static initializer in `ty`'s scope
    /// (root for globals).
    pub(crate) fn compile_value(
        &self,
        ty: TypeRef,
        expression: &dreammaker::ast::Expression,
        declared: &str,
        location: dreammaker::Location,
        proc_path: Option<&str>,
    ) -> Result<ProcReload, String> {
        use dreammaker::ast::{Expression, Ident, PathOp, Prefab, RelativePath, Spanned, Term};
        // A `new` with no type makes the var's declared type
        let mut expression = expression.clone();
        if let Expression::Base { term, .. } = &mut expression {
            if let Term::NewImplicit { args } = &term.elem {
                let mut parts = declared.split('/').filter(|part| !part.is_empty());
                let Some(first) = parts.next() else {
                    return Err("implicit new requires a declared variable type".to_owned());
                };
                let mut path = RelativePath::single(PathOp::Slash, Ident::from_nonstatic(first));
                for part in parts {
                    path.push((PathOp::Slash, Ident::from_nonstatic(part)));
                }
                term.elem = Term::NewPrefab {
                    prefab: Box::new(Prefab::from(path)),
                    args: args.clone(),
                };
            }
        }
        let block: Block = Box::new([Spanned::new(location, Statement::Return(Some(expression)))]);
        self.compile_synthetic(ty, "(value)", block, ty.get().location, false, proc_path)
    }

    /// Compiles a type's init proc: an assignment to `src.var` for each of
    /// `inits`.
    fn compile_init(&self, ty: TypeRef, inits: &[Init]) -> Result<ProcReload, String> {
        use dreammaker::ast::{
            AssignOp, Expression, Follow, Ident, PropertyAccessKind, Spanned, Term,
        };
        let block: Block = inits
            .iter()
            .map(|init| {
                let location = init.location;
                let lhs = Expression::Base {
                    term: Box::new(Spanned::new(
                        location,
                        Term::Ident(Ident::from_nonstatic("src")),
                    )),
                    follow: Box::new([Spanned::new(
                        location,
                        Follow::Field(PropertyAccessKind::Dot, Ident::from_nonstatic(&init.var)),
                    )]),
                };
                Spanned::new(
                    location,
                    Statement::Expr(Expression::AssignOp {
                        op: AssignOp::Assign,
                        lhs: Box::new(lhs),
                        rhs: Box::new(init.value.clone()),
                    }),
                )
            })
            .collect();
        self.compile_synthetic(ty, "(init)", block, ty.get().location, false, None)
    }

    /// Compile a synthetic proc on `ty`, optionally emitting debug locations
    /// and overriding `__PROC__` with `proc_path`.
    fn compile_synthetic(
        &self,
        ty: TypeRef,
        name: &str,
        block: Block,
        location: dreammaker::Location,
        debug_info: bool,
        proc_path: Option<&str>,
    ) -> Result<ProcReload, String> {
        let value = ProcValue {
            location,
            parameters: Box::new([]),
            docs: Default::default(),
            code: Some(block),
            body_range: None,
            first_line: None,
            next_lines: Box::new([]),
            end_lines: Box::new([]),
            operator_lines: Box::new([]),
        };
        let file = self.debug_file_name(location.file);
        let source = ProcSource {
            tree: &self.tree,
            ty,
            name,
            value: &value,
            file: &file,
            debug_info,
            index: None,
            byond: self.byond,
            file_names: Some(&|id| self.debug_file_name(id)),
        };
        let path = format!("{}/{}", ty.get().path, name);
        let compiled = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            crate::compiler::compile_proc_as(&source, proc_path)
        })) {
            Ok(Ok(compiled)) => compiled,
            Ok(Err(e)) => return Err(e.to_string()),
            Err(_) => return Err("the compiler panicked".to_owned()),
        };
        Ok(ProcReload {
            proc: ProcKey {
                path,
                override_id: 0,
            },
            nodes: compiled.nodes,
            locals: compiled.locals,
            params: vec![],
            settings: Some(ProcSettings::default()),
            statics: vec![],
            warnings: compiled.warnings,
        })
    }

    /// Resolve proc settings from its declaration plus this definition's
    /// `set` statements.
    pub fn settings(&self, key: &ProcKey) -> Result<ProcSettings, String> {
        self.settings_with(key, &mut DeclaredSettings::new())
    }

    fn settings_with(
        &self,
        key: &ProcKey,
        declared: &mut DeclaredSettings,
    ) -> Result<ProcSettings, String> {
        let (ty, name, index) = self
            .procs
            .get(key)
            .ok_or_else(|| format!("no proc {key}"))?;
        let ty = self.find_type(ty).ok_or_else(|| format!("no proc {key}"))?;
        settings_of(ty, name, *index, declared)
    }

    /// Compiles one proc, with `DbgFile` and `DbgLine`.
    pub fn compile(&self, key: &ProcKey) -> Result<CompiledProc, String> {
        let (ty, value) = self.find(key).ok_or_else(|| format!("no proc {key}"))?;
        let (_, name, index) = &self.procs[key];
        let file = self.debug_file_name(value.location.file);
        let source = ProcSource {
            tree: &self.tree,
            ty,
            name,
            value,
            file: &file,
            debug_info: true,
            index: Some(*index),
            byond: self.byond,
            file_names: Some(&|id| self.debug_file_name(id)),
        };
        // A compiler panic for one proc must not terminate the source watcher.
        match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| compile_proc(&source))) {
            Ok(Ok(compiled)) => Ok(compiled),
            Ok(Err(e)) => Err(e.to_string()),
            Err(_) => Err("the compiler panicked".to_owned()),
        }
    }

    /// Return unresolved parse errors, excluding recovered const initializers and
    /// successfully reparsed files. Each pair contains a line-independent comparison
    /// key and a diagnostic with the source line, so moving an error does not make it new.
    pub fn parse_errors(&self) -> Vec<(String, String)> {
        self.ctx
            .errors()
            .iter()
            .filter(|e| {
                e.severity() == Severity::Error
                    && !self.folded.contains(&e.location())
                    && !self.mended.contains(&e.location().file)
            })
            .map(|e| {
                let file = self.file_name(e.location().file);
                (
                    format!("{file}: {}", e.description()),
                    format!("{file}:{}: {}", e.location().line, e.description()),
                )
            })
            .collect()
    }

    fn find(&self, key: &ProcKey) -> Option<(TypeRef<'_>, &ProcValue)> {
        let (ty, name, index) = self.procs.get(key)?;
        let ty = self.find_type(ty)?;
        if let Some(value) = self.patched.get(key) {
            return Some((ty, value));
        }
        let value = ty.get().procs.get(name.as_str())?.value.get(*index)?;
        Some((ty, value))
    }

    fn find_type(&self, path: &str) -> Option<TypeRef<'_>> {
        if path.is_empty() {
            Some(self.tree.root())
        } else {
            self.tree.find(path)
        }
    }

    /// Whether a proc embeds a moved default's previous source location
    /// through `::`.
    fn reads_moved_default(&self, file: FileId, moved: &[(&String, u32)]) -> Option<ProcKey> {
        let file_name = self.debug_file_name(file).into_bytes();
        let procs_by_file = self.procs_by_file.get_or_init(|| {
            let mut by_file: HashMap<FileId, Vec<ProcKey>> = HashMap::new();
            for key in self.procs.keys() {
                if let Some((_, value)) = self.find(key) {
                    by_file
                        .entry(value.location.file)
                        .or_default()
                        .push(key.clone());
                }
            }
            by_file
        });
        // The procs with one of the names after `::` in their lines
        let mut candidates = vec![];
        for (id, uses) in &self.scope_names {
            let lines: Vec<u32> = uses
                .iter()
                .filter(|(name, _)| moved.iter().any(|(m, _)| *m == name))
                .map(|(_, line)| *line)
                .collect();
            if lines.is_empty() {
                continue;
            }
            for key in procs_by_file.get(id).into_iter().flatten() {
                let Some((_, value)) = self.find(key) else {
                    continue;
                };
                if lines.iter().any(|l| proc_lines(value).contains(l)) {
                    candidates.push(key.clone());
                }
            }
        }
        let mut default_lines = self.default_lines.lock().unwrap();
        for key in candidates {
            if !default_lines.contains_key(&key) {
                let lines = self.recorded_defaults(&key);
                default_lines.insert(key.clone(), lines);
            }
            if default_lines[&key]
                .iter()
                .any(|(f, line)| *f == file_name && moved.iter().any(|(_, old)| old == line))
            {
                return Some(key);
            }
        }
        None
    }

    /// Find embedded default locations outside the proc's own source range.
    fn recorded_defaults(&self, key: &ProcKey) -> Vec<(Vec<u8>, u32)> {
        let (Some((_, value)), Ok(code)) = (self.find(key), self.compile(key)) else {
            return vec![];
        };
        let own_file = self.debug_file_name(value.location.file).into_bytes();
        let own = proc_lines(value);
        let mut file = own_file.clone();
        let mut lines = vec![];
        for node in &code.nodes {
            match node {
                Node::Instruction(Instruction::DbgFile(name), _) => file = name.0.clone(),
                Node::Instruction(Instruction::DbgLine(line), _)
                    if !(file == own_file && own.contains(line)) =>
                {
                    lines.push((file.clone(), *line))
                }
                _ => {}
            }
        }
        lines
    }

    /// Whether the current source range contains `text`. Return true if
    /// the file cannot be read, so callers conservatively inspect the proc.
    fn source_has(
        &self,
        value: &ProcValue,
        text: &str,
        files: &mut HashMap<FileId, Option<Vec<String>>>,
    ) -> bool {
        let file = value.location.file;
        let lines = files.entry(file).or_insert_with(|| {
            let root = self.root.as_ref()?;
            let source = std::fs::read(root.join(&*self.ctx.file_path(file))).ok()?;
            Some(
                String::from_utf8_lossy(&source)
                    .lines()
                    .map(str::to_owned)
                    .collect(),
            )
        });
        let Some(lines) = lines else { return true };
        let own = proc_lines(value);
        let start = own.start().saturating_sub(1) as usize;
        let end = (*own.end() as usize).min(lines.len());
        lines
            .get(start..end)
            .is_none_or(|l| l.iter().any(|l| l.contains(text)))
    }

    /// Source location as (file path, line, column), stable across parses.
    fn place(&self, location: dreammaker::Location) -> (String, u32, u16) {
        (
            self.file_name(location.file),
            location.line,
            location.column,
        )
    }

    fn file_name(&self, file: FileId) -> String {
        self.ctx
            .file_path(file)
            .to_string_lossy()
            .replace('\\', "/")
    }

    /// Resolve DreamMaker's debug name for a project-relative source path
    /// (see `debug_files`).
    fn debug_file_name(&self, file: FileId) -> String {
        let path = self.ctx.file_path(file);
        match self.debug_names.get(&crate::debug_files::normalize(&path)) {
            Some(name) => name.clone(),
            None => self.file_name(file),
        }
    }
}

/// `/type/path/name`, or `/name` for a global proc.
fn proc_path(ty: TypeRef, name: &str) -> String {
    format!("{}/{}", ty.get().path, name)
}

/// Compare snapshots. `changed_files` contains project-relative edited paths
/// whose unchanged procs may need line remapping. `None` checks all procs.
pub fn diff(old: &Snapshot, new: &Snapshot, changed_files: Option<&HashSet<PathBuf>>) -> Changes {
    let mut changes = Changes::default();

    // New parse errors can omit declarations and falsely appear as removals.
    // Reject them before comparing definitions.
    let mut old_errors: HashMap<String, usize> = HashMap::new();
    for (e, _) in old.parse_errors() {
        *old_errors.entry(e).or_default() += 1;
    }
    for (e, shown) in new.parse_errors() {
        match old_errors.get_mut(&e) {
            Some(n) if *n > 0 => *n -= 1,
            _ => changes.refused.push(format!("parse error: {shown}")),
        }
    }
    if !changes.refused.is_empty() {
        return changes;
    }

    // The paths whose proc definitions were added or removed among others: which
    // old definition each new one is (see `align`)
    let mut realigned = Realigned::new();
    let new_procs = compare_types(old, new, &mut changes, &mut realigned);
    let old_key = |key: &ProcKey| old_key(&realigned, key);
    // Parents first, whatever their paths
    let depth = |path: &str| {
        let mut depth = 0;
        let mut at = new.find_type(path);
        while let Some(t) = at {
            depth += 1;
            at = t.parent_type();
        }
        depth
    };
    changes.new_types.sort_by_cached_key(|t| depth(&t.path));
    compare_inits(old, new, &mut changes);
    for (key, type_path, name, declared, verb) in new_procs {
        let code = new.compile_reload(&key).and_then(|mut code| {
            new.compile_in_lists(&key, &mut code, |_, _| true)?;
            Ok(code)
        });
        match code {
            Ok(code) => changes.new_procs.push(NewProc {
                type_path,
                name,
                declared,
                verb,
                code,
            }),
            Err(e) => changes.refused.push(format!("{key}: doesn't compile: {e}")),
        }
    }

    let in_changed_file = |key: &ProcKey| match changed_files {
        None => true,
        Some(files) => new
            .proc_file(key)
            .map_or(false, |f| files.contains(Path::new(&f))),
    };

    let (mut old_declared, mut new_declared) = (DeclaredSettings::new(), DeclaredSettings::new());
    for (key, _) in new.procs.iter() {
        // A new definition among others is one of the new procs. The rest are
        // known by their numbers in the running world, the old ones.
        let Some(old_key) = old_key(key) else {
            continue;
        };
        let (Some((_, old_value)), Some((_, new_value))) = (old.find(&old_key), new.find(key))
        else {
            // New and removed procs were refused with the types
            continue;
        };

        let moved_file =
            old.file_name(old_value.location.file) != new.file_name(new_value.location.file);
        // Default arguments are compiled into the start of the body
        let defaults = |v: &ProcValue| {
            v.parameters
                .iter()
                .map(|p| p.default.clone())
                .collect::<Vec<_>>()
        };
        let params_changed = signature(old_value) != signature(new_value);
        // Inherited from the declaration, so they can change with code
        // elsewhere
        let settings = new.settings_with(key, &mut new_declared);
        let settings = match (
            old.settings_with(&old_key, &mut old_declared) == settings,
            settings,
        ) {
            (true, _) => None,
            (false, Ok(settings)) => Some(settings),
            (false, Err(e)) => {
                changes.refused.push(format!("{key}: {e}"));
                continue;
            }
        };
        if old_value.code != new_value.code
            || defaults(old_value) != defaults(new_value)
            || params_changed
            || moved_file
        {
            // An `in` list expression the parameter didn't have
            let new_list = |name: &str, expression: &dreammaker::ast::Expression| {
                old_value
                    .parameters
                    .iter()
                    .find(|p| p.name.as_str() == name)
                    .and_then(|p| p.in_list.as_ref())
                    != Some(expression)
            };
            let reload = new.compile_code(key).and_then(|mut reload| {
                new.compile_in_lists(key, &mut reload, new_list)?;
                Ok(reload)
            });
            match reload {
                Ok(reload) => changes.reloads.push(ProcReload {
                    settings,
                    proc: old_key,
                    ..reload
                }),
                Err(e) => changes.refused.push(format!("{key}: doesn't compile: {e}")),
            }
            continue;
        }
        if let Some(settings) = settings {
            changes.settings.push(SettingsChange {
                proc: old_key.clone(),
                settings,
            });
        }

        if in_changed_file(key) {
            match line_move(old, new, &old_key, key, old_value, new_value) {
                LineChange::Same => {}
                LineChange::Moved(lines) => changes.line_moves.push(LineMove {
                    proc: old_key,
                    lines,
                }),
                // Lines can't be moved one for one: compiled again
                LineChange::Recompile => match new.compile_code(key).and_then(|mut reload| {
                    new.compile_in_lists(key, &mut reload, |_, _| false)?;
                    Ok(reload)
                }) {
                    Ok(reload) => changes.reloads.push(ProcReload {
                        settings: None,
                        proc: old_key,
                        ..reload
                    }),
                    Err(e) => changes.refused.push(format!("{key}: doesn't compile: {e}")),
                },
            }
        }
    }

    const_users(old, new, &realigned, &mut changes);
    reparent::reparented_users(old, new, &realigned, &mut changes);
    changes.collect_warnings();
    changes
}

/// Map new definitions to old definitions after insertions and removals.
/// `None` identifies a new definition.
type Realigned = HashMap<String, Vec<Option<u32>>>;

/// The old key of the definition `key` names in the new parse, `None` for a
/// new one.
fn old_key(realigned: &Realigned, key: &ProcKey) -> Option<ProcKey> {
    match realigned.get(&key.path) {
        None => Some(key.clone()),
        Some(map) => map
            .get(key.override_id as usize)
            .copied()
            .flatten()
            .map(|override_id| ProcKey {
                path: key.path.clone(),
                override_id,
            }),
    }
}

/// Recompile users of changed constants or declarations when their
/// bytecode changes, even if their source is unchanged.
fn const_users(old: &Snapshot, new: &Snapshot, realigned: &Realigned, changes: &mut Changes) {
    if changes.consts.is_empty()
        && changes.retyped.is_empty()
        && changes.defaults_changed.is_empty()
        && changes.defaults_moved.is_empty()
    {
        return;
    }
    // Use identifier occurrences in AST debug text as a cheap prefilter.
    let names: Vec<String> = changes
        .consts
        .iter()
        .chain(&changes.retyped)
        .map(|c| format!("{c:?}"))
        .collect();
    // Only `type::var` embeds a default's value or source location in code.
    // Looking for any use of `var` would recompile nearly everything for
    // common names such as `name` or `icon`.
    let moved: Vec<String> = changes
        .defaults_changed
        .iter()
        .chain(&changes.defaults_moved)
        .map(|c| format!("StaticField({c:?})"))
        .collect();
    let reloaded: HashSet<ProcKey> = changes.reloads.iter().map(|r| r.proc.clone()).collect();
    let mut files = HashMap::new();
    for key in new.procs.keys() {
        let Some(old_key) = old_key(realigned, key) else {
            continue;
        };
        if reloaded.contains(&old_key) {
            continue;
        }
        let (Some(_), Some((_, new_value))) = (old.find(&old_key), new.find(key)) else {
            continue;
        };
        // Avoid formatting every proc: only source containing `::` can
        // reference a moved default.
        if names.is_empty() && !new.source_has(new_value, "::", &mut files) {
            continue;
        }
        let text = format!("{:?}{:?}", new_value.code, new_value.parameters);
        if !names.iter().any(|name| text.contains(name.as_str()))
            && !moved.iter().any(|name| text.contains(name.as_str()))
        {
            continue;
        }
        let (Ok(before), Ok(after)) = (old.compile(&old_key), new.compile(key)) else {
            continue;
        };
        // Moved lines only need moving
        let code = |nodes: &[Node]| -> Vec<Node> {
            nodes
                .iter()
                .filter(|n| !matches!(n, Node::Instruction(Instruction::DbgLine(_), _)))
                .cloned()
                .collect()
        };
        if code(&before.nodes) == code(&after.nodes) {
            // A default's line moved: the lines move with it, where they map
            // one for one (this proc's own may have moved too)
            match line_map(&before.nodes, &after.nodes) {
                Some(lines) => {
                    let lines: Vec<(u32, u32)> = lines.into_iter().collect();
                    match changes.line_moves.iter_mut().find(|m| m.proc == old_key) {
                        Some(found) => found.lines = lines,
                        None if lines.iter().any(|(o, n)| o != n) => {
                            changes.line_moves.push(LineMove {
                                proc: old_key,
                                lines,
                            })
                        }
                        None => {}
                    }
                    continue;
                }
                None => {}
            }
        }
        let reload = new.compile_code(key).and_then(|mut reload| {
            new.compile_in_lists(key, &mut reload, |_, _| false)?;
            Ok(reload)
        });
        // Its new code has its new lines
        changes.line_moves.retain(|m| m.proc != old_key);
        match reload {
            Ok(reload) => changes.reloads.push(ProcReload {
                proc: old_key,
                ..reload
            }),
            Err(e) => changes.refused.push(format!("{key}: doesn't compile: {e}")),
        }
    }
}

/// Required update for an unchanged proc whose source locations moved.
enum LineChange {
    Same,
    /// Each line it records, old and new
    Moved(Vec<(u32, u32)>),
    /// Line numbers cannot be remapped independently across files; recompile.
    Recompile,
}

fn line_move(
    old: &Snapshot,
    new: &Snapshot,
    old_key: &ProcKey,
    key: &ProcKey,
    old_value: &ProcValue,
    new_value: &ProcValue,
) -> LineChange {
    let mut lines = BTreeMap::new();
    match (old.compile(old_key), new.compile(key)) {
        (Ok(old_code), Ok(new_code)) => match line_map(&old_code.nodes, &new_code.nodes) {
            Some(map) => lines.extend(map),
            None => return LineChange::Recompile,
        },
        // Without the compiler, all we know is where the proc starts
        _ => {}
    }
    if lines.is_empty() {
        // Every line from the header to the end of the body, by the header's
        // shift
        let Some(range) = old_value.body_range.as_ref() else {
            return LineChange::Same;
        };
        let (o, n) = (old_value.location.line, new_value.location.line);
        for line in o..=range.end.line.max(o) {
            lines.insert(line, (line as i64 + n as i64 - o as i64) as u32);
        }
    }
    if lines.iter().all(|(o, n)| o == n) {
        return LineChange::Same;
    }
    LineChange::Moved(lines.into_iter().collect())
}

/// Map corresponding DbgLines in old and new code. Return `None` for
/// conflicting mappings, or an empty map when DbgLine counts differ.
fn line_map(old: &[Node], new: &[Node]) -> Option<BTreeMap<u32, u32>> {
    let (old_lines, new_lines) = (dbg_lines(old), dbg_lines(new));
    let mut lines = BTreeMap::new();
    if old_lines.len() == new_lines.len() {
        for (o, n) in old_lines.into_iter().zip(new_lines) {
            if *lines.entry(o).or_insert(n) != n {
                return None;
            }
        }
    }
    Some(lines)
}

fn dbg_lines(nodes: &[Node]) -> Vec<u32> {
    nodes
        .iter()
        .filter_map(|node| match node {
            Node::Instruction(Instruction::DbgLine(line), _) => Some(*line),
            _ => None,
        })
        .collect()
}

/// A proc's parameters as BYOND keeps them: names and input types, not
/// default values (which are code).
fn signature(
    v: &ProcValue,
) -> Vec<(
    dreammaker::ast::Ident,
    dreammaker::ast::VarType,
    Option<dreammaker::ast::Expression>,
    bool,
)> {
    v.parameters
        .iter()
        .map(|p| {
            (
                p.name.clone(),
                p.var_type.clone(),
                p.in_list.clone(),
                p.absolute,
            )
        })
        .collect()
}

/// Read parameter metadata, excluding `...` and absolute-path parameters
/// as DreamMaker does.
fn params(v: &ProcValue) -> Vec<Param> {
    use dreammaker::ast::InputType;
    v.parameters
        .iter()
        .filter(|p| !p.absolute && p.name.as_str() != "...")
        .map(|p| {
            let mut input = p.var_type.input_type;
            if input.is_empty() {
                // `obj/thing` alone allows what its root type is
                let path: Vec<&str> = p.var_type.type_path.iter().map(|s| s.as_str()).collect();
                input = match path.as_slice() {
                    ["atom"] => InputType::ATOM,
                    ["atom", "movable", ..] => InputType::MOVABLE,
                    ["obj", ..] => InputType::OBJ,
                    ["mob", ..] => InputType::MOB,
                    ["turf", ..] => InputType::TURF,
                    ["area", ..] => InputType::AREA,
                    _ => InputType::empty(),
                };
            }
            Param {
                name: p.name.as_str().to_owned(),
                input: input.bits() & 0xFFFF,
                input_extra: (input.bits() >> 16) & 0x3,
                in_list: builtin_in_list(p.in_list.as_ref()).map_or(InList::Kept, InList::Builtin),
            }
        })
        .collect()
}

/// File declarations used to detect proc-body-only edits.
#[derive(Default)]
struct FileContents<'a> {
    /// (type, name) -> declaration, type, value
    vars: BTreeMap<
        (String, String),
        (
            Option<dreammaker::ast::VarType>,
            Option<dreammaker::ast::Expression>,
        ),
    >,
    /// (type, name) -> the line of a default written in the file
    var_lines: BTreeMap<(String, String), u32>,
    /// (type, name) -> kind, flags of the procs it declares
    declarations: BTreeMap<(String, String), String>,
    /// (type, name) -> its definitions, in order (with keys, in the snapshot)
    procs: BTreeMap<(String, String), Vec<(Option<ProcKey>, &'a ProcValue)>>,
}

impl<'a> FileContents<'a> {
    fn add(&mut self, ty: TypeRef<'a>, file: FileId) {
        let path = &ty.get().path;
        for (name, var) in ty.get().vars.iter() {
            let declared = var.declaration.as_ref().filter(|d| d.location.file == file);
            if var.value.location.file == file && var.value.expression.is_some() {
                self.var_lines
                    .insert((path.clone(), name.to_string()), var.value.location.line);
            }
            if var.value.location.file == file || declared.is_some() {
                self.vars.insert(
                    (path.clone(), name.to_string()),
                    (
                        declared.map(|d| d.var_type.clone()),
                        var.value.expression.clone(),
                    ),
                );
            }
        }
        for (name, proc) in ty.get().procs.iter() {
            if let Some(d) = proc
                .declaration
                .as_ref()
                .filter(|d| d.location.file == file)
            {
                self.declarations.insert(
                    (path.clone(), name.to_string()),
                    format!("{:?} {:?}", d.kind, d.flags),
                );
            }
            let values: Vec<(Option<ProcKey>, &ProcValue)> = proc
                .value
                .iter()
                .filter(|v| v.location.file == file)
                .map(|v| (None, v))
                .collect();
            if !values.is_empty() {
                self.procs.insert((path.clone(), name.to_string()), values);
            }
        }
    }
}

/// The macros a file defines and takes away, in order
fn macros_in(
    history: &DefineHistory,
    file: FileId,
) -> (
    Vec<(String, Vec<String>, bool, Vec<dreammaker::lexer::Token>)>,
    Vec<String>,
) {
    let mut defined = vec![];
    let mut undefined = vec![];
    for (range, (name, define)) in history.iter() {
        if range.start.file == file {
            defined.push((
                name.to_string(),
                define.params.iter().map(|p| p.to_string()).collect(),
                define.variadic,
                define.subst.clone(),
            ));
        }
        if range.end.file == file {
            undefined.push(name.to_string());
        }
    }
    undefined.sort();
    (defined, undefined)
}

/// The settings of the definitions that declare procs, by the declaring type
/// (its address) and the proc's name: overrides start from them.
type DeclaredSettings = HashMap<(usize, String), Result<ProcSettings, String>>;

/// Resolve settings from the declaring definition and this override.
/// Overrides do not inherit `set` statements from preceding overrides.
fn settings_of(
    ty: TypeRef,
    name: &str,
    index: usize,
    declared: &mut DeclaredSettings,
) -> Result<ProcSettings, String> {
    let mut base = ProcSettings::default();
    let mut at = Some(ty);
    while let Some(t) = at {
        if let Some(proc) = t.get().procs.get(name) {
            if proc.declaration.is_some() {
                let first = proc
                    .value
                    .iter()
                    .position(|v| v.location.file != FileId::BUILTINS);
                if let Some(first) = first {
                    if !(t.index() == ty.index() && first == index) {
                        base = declared
                            .entry((t.get() as *const _ as usize, name.to_owned()))
                            .or_insert_with(|| proc_settings(&proc.value[first], base))
                            .clone()?;
                    }
                }
                break;
            }
        }
        at = t.parent_type();
    }
    let value = &ty.get().procs[name].value[index];
    proc_settings(value, base)
}

/// A definition's `set` statements, on top of `base`.
fn proc_settings(v: &ProcValue, base: ProcSettings) -> Result<ProcSettings, String> {
    use dreammaker::ast::{Expression, SettingMode, Term};
    let mut out = base;
    for setting in settings(v.code.as_ref()) {
        let Statement::Setting { name, mode, value } = setting else {
            continue;
        };
        let name = name.as_str();
        let unclear = || format!("cannot resolve `set {name}` value");
        let number = || -> Result<f32, String> {
            match value {
                Expression::Base { term, follow } if follow.is_empty() => match &term.elem {
                    Term::Int(n) => Ok(*n as f32),
                    Term::Float(f) => Ok(*f),
                    _ => Err(unclear()),
                },
                _ => Err(unclear()),
            }
        };
        let text = || -> Result<Option<String>, String> {
            let nowhere = dreammaker::Location {
                file: FileId::UNKNOWN,
                line: 0,
                column: 0,
            };
            match value.simple_evaluate(nowhere) {
                Ok(dreammaker::constants::Constant::String(s)) => crate::compiler::text(s.as_str())
                    .map(Some)
                    .ok_or_else(unclear),
                Ok(dreammaker::constants::Constant::Null(_)) => Ok(None),
                _ => Err(unclear()),
            }
        };
        match name {
            "waitfor" => out.waitfor = number()? != 0.0,
            "background" => out.background = number()? != 0.0,
            "hidden" => out.hidden = number()? != 0.0,
            "popup_menu" => out.popup_menu = number()? != 0.0,
            "instant" => out.instant = number()? != 0.0,
            "invisibility" => out.invisibility = Some(number()?.clamp(0.0, 255.0) as u8),
            "name" => out.name = text()?,
            "desc" => out.desc = text()?,
            "category" => {
                out.category = text()?;
                out.no_category = out.category.is_none();
            }
            "src" => {
                let equals = matches!(mode, SettingMode::Assign);
                let word = verb_src(value, equals)
                    .ok_or_else(|| "unsupported `set src` expression for hot reload".to_owned())?;
                out.src = Some((equals, word));
            }
            other if other.starts_with("SpacemanDMM_") => {}
            other => return Err(format!("unsupported hot reload setting: `set {other}`")),
        }
    }
    Ok(out)
}

/// `set src`'s source, as an `in` list's word. `src = usr` and `src = usr.loc`
/// are kinds of their own.
fn verb_src(value: &dreammaker::ast::Expression, equals: bool) -> Option<u32> {
    use dreammaker::ast::{Expression, Follow, Term};
    if equals {
        if let Expression::Base { term, follow } = value {
            if let Term::Ident(name) = &term.elem {
                if name.as_str() == "usr" {
                    let fields: Vec<&str> = follow
                        .iter()
                        .filter_map(|f| match &f.elem {
                            Follow::Field(_, name) => Some(name.as_str()),
                            _ => None,
                        })
                        .collect();
                    match (fields.as_slice(), follow.len()) {
                        ([], 0) => return Some(0xFF << 8 | 0x20),
                        (["loc"], 1) => return Some(0xFF << 8 | 0x03),
                        _ => {}
                    }
                }
            }
        }
    }
    builtin_in_list(Some(value))
}

/// Every `set` statement in a proc, in order.
fn settings(code: Option<&Block>) -> Vec<&Statement> {
    fn visit<'a>(block: &'a Block, out: &mut Vec<&'a Statement>) {
        for stmt in block.iter() {
            match &stmt.elem {
                s @ Statement::Setting { .. } => out.push(s),
                Statement::While { block, .. }
                | Statement::DoWhile { block, .. }
                | Statement::ForInfinite { block }
                | Statement::ForLoop { block, .. }
                | Statement::Spawn { block, .. }
                | Statement::Label { block, .. } => visit(block, out),
                Statement::ForList(f) => visit(&f.block, out),
                Statement::ForKeyValue(f) => visit(&f.block, out),
                Statement::ForRange(f) => visit(&f.block, out),
                Statement::If { arms, else_arm } => {
                    for (_, block) in arms {
                        visit(block, out);
                    }
                    if let Some(block) = else_arm {
                        visit(block, out);
                    }
                }
                Statement::Switch { cases, default, .. } => {
                    for (_, block) in cases.iter() {
                        visit(block, out);
                    }
                    if let Some(block) = default {
                        visit(block, out);
                    }
                }
                Statement::TryCatch {
                    try_block,
                    catch_block,
                    ..
                } => {
                    visit(try_block, out);
                    visit(catch_block, out);
                }
                _ => {}
            }
        }
    }
    let mut out = vec![];
    if let Some(code) = code {
        visit(code, &mut out);
    }
    out
}

/// Where an expression starts, as near as the AST says.
fn value_location(
    expression: &dreammaker::ast::Expression,
    otherwise: dreammaker::Location,
) -> dreammaker::Location {
    match expression {
        dreammaker::ast::Expression::Base { term, .. } => term.location,
        _ => otherwise,
    }
}

/// A var a type's init proc sets.
#[derive(Clone)]
struct Init {
    var: String,
    /// The type whose definition it is
    on: String,
    value: dreammaker::ast::Expression,
    location: dreammaker::Location,
}

impl PartialEq for Init {
    fn eq(&self, other: &Self) -> bool {
        // Spanned ignores locations, so a moved line is the same
        self.var == other.var && self.on == other.on && self.value == other.value
    }
}

/// Merge inherited computed initializers with those declared on this type.
/// Remove overridden ancestor initializers, then append local computed values.
/// Global and static variables initialize once at world startup.
fn inits<'a>(t: TypeRef, memo: &'a mut HashMap<usize, Vec<Init>>) -> &'a [Init] {
    if !memo.contains_key(&t.index().index()) {
        let mut list = match t.parent_type() {
            Some(parent) if !t.is_root() => inits(parent, memo).to_vec(),
            _ => vec![],
        };
        let vars = &t.get().vars;
        list.retain(|init| !vars.contains_key(init.var.as_str()));
        if !t.is_root() {
            for (name, var) in vars.iter() {
                if var.value.location.file == FileId::BUILTINS {
                    continue;
                }
                let flags = t
                    .get_var_declaration(name.as_str())
                    .map(|d| d.get().var_type.flags)
                    .unwrap_or_default();
                if flags.is_static() || flags.is_const() {
                    continue;
                }
                if let Some(value) = &var.value.expression {
                    if var_constant(var).is_none() {
                        // Subtypes run it as it is
                        let mut value = value.clone();
                        type_macro_is(&mut value, &t.get().path);
                        list.push(Init {
                            var: name.as_str().to_owned(),
                            on: t.get().path.clone(),
                            value,
                            location: var.value.location,
                        });
                    }
                }
            }
        }
        memo.insert(t.index().index(), list);
    }
    &memo[&t.index().index()]
}

/// Bind `__TYPE__` to the type declaring this initializer.
fn type_macro_is(expression: &mut dreammaker::ast::Expression, path: &str) {
    use dreammaker::ast::{Expression, Follow, Ident, PathOp, Prefab, RelativePath, Term};
    fn term(term: &mut Term, path: &str) {
        let all = |list: &mut [Expression]| list.iter_mut().for_each(|e| type_macro_is(e, path));
        match term {
            Term::__TYPE__ => {
                let mut parts = path.split('/').filter(|part| !part.is_empty());
                let Some(first) = parts.next() else {
                    *term = Term::Null;
                    return;
                };
                let mut relative =
                    RelativePath::single(PathOp::Slash, Ident::from_nonstatic(first));
                for part in parts {
                    relative.push((PathOp::Slash, Ident::from_nonstatic(part)));
                }
                *term = Term::Prefab(Box::new(Prefab::from(relative)));
            }
            Term::Expr(e) => type_macro_is(e, path),
            Term::Prefab(prefab) => prefab
                .vars
                .iter_mut()
                .for_each(|(_, e)| type_macro_is(e, path)),
            Term::InterpString(_, parts) => parts
                .iter_mut()
                .filter_map(|(e, _)| e.as_mut())
                .for_each(|e| type_macro_is(e, path)),
            Term::Call(_, args)
            | Term::SelfCall(args)
            | Term::ParentCall(args)
            | Term::List(args)
            | Term::Alist(args)
            | Term::GlobalCall(_, args) => all(args),
            Term::NewImplicit { args } | Term::NewMiniExpr { args, .. } => {
                args.iter_mut().for_each(|args| all(args))
            }
            Term::NewPrefab { prefab, args } => {
                prefab
                    .vars
                    .iter_mut()
                    .for_each(|(_, e)| type_macro_is(e, path));
                args.iter_mut().for_each(|args| all(args));
            }
            Term::Input { args, in_list, .. } | Term::Locate { args, in_list } => {
                all(args);
                in_list.iter_mut().for_each(|e| type_macro_is(e, path));
            }
            Term::Pick(choices) => choices.iter_mut().for_each(|(weight, e)| {
                weight.iter_mut().for_each(|w| type_macro_is(w, path));
                type_macro_is(e, path);
            }),
            Term::DynamicCall(a, b) => {
                all(a);
                all(b);
            }
            Term::ExternalCall {
                library,
                function,
                args,
            } => {
                library.iter_mut().for_each(|e| type_macro_is(e, path));
                type_macro_is(function, path);
                all(args);
            }
            _ => {}
        }
    }
    match expression {
        Expression::Base { term: t, follow } => {
            term(&mut t.elem, path);
            for f in follow.iter_mut() {
                match &mut f.elem {
                    Follow::Index(_, e) => type_macro_is(e, path),
                    Follow::Call(_, _, args) => {
                        args.iter_mut().for_each(|e| type_macro_is(e, path))
                    }
                    _ => {}
                }
            }
        }
        Expression::BinaryOp { lhs, rhs, .. } | Expression::AssignOp { lhs, rhs, .. } => {
            type_macro_is(lhs, path);
            type_macro_is(rhs, path);
        }
        Expression::TernaryOp { cond, if_, else_ } => {
            type_macro_is(cond, path);
            type_macro_is(if_, path);
            type_macro_is(else_, path);
        }
    }
}

/// The types whose init procs changed, and their new code.
fn compare_inits(old: &Snapshot, new: &Snapshot, changes: &mut Changes) {
    let (mut old_memo, mut new_memo) = (HashMap::new(), HashMap::new());
    for n in new.tree.iter_types() {
        if n.is_root() {
            continue;
        }
        let new_inits = inits(n, &mut new_memo).to_vec();
        match old.find_type(&n.get().path) {
            Some(o) if inits(o, &mut old_memo) == new_inits.as_slice() => continue,
            Some(_) => {}
            // A new type initially copies its nearest existing ancestor's runtime
            // init proc. That proc may still initialize defaults removed in this
            // reload, so the new type must be checked too.
            None => {
                let mut at = n.parent_type();
                let mut copied = None;
                while let Some(t) = at {
                    if let Some(o) = old.find_type(&t.get().path) {
                        copied = Some(inits(o, &mut old_memo).to_vec());
                        break;
                    }
                    at = t.parent_type();
                }
                if new_inits.is_empty() && copied.is_none_or(|c| c.is_empty()) {
                    continue;
                }
            }
        }
        let code = if new_inits.is_empty() {
            None
        } else {
            match new.compile_init(n, &new_inits) {
                Ok(code) => Some(code),
                Err(e) => {
                    changes.refused.push(format!(
                        "{}: initializer compilation failed: {e}",
                        n.get().path
                    ));
                    continue;
                }
            }
        };
        changes.init_procs.push(InitProc {
            type_path: n.get().path.clone(),
            code,
        });
    }
}

/// A type's definitions of a proc, without SpacemanDMM's declarations of
/// BYOND's own procs.
fn real(p: &dreammaker::objtree::TypeProc) -> Vec<&ProcValue> {
    p.value
        .iter()
        .filter(|v| v.location.file != FileId::BUILTINS)
        .collect()
}

/// Match definitions in order by source file, then by code for multiple
/// definitions in one file. `None` identifies additions.
fn align(
    old: &Snapshot,
    new: &Snapshot,
    old_values: &[&ProcValue],
    new_values: &[&ProcValue],
) -> Vec<Option<usize>> {
    let (n, m) = (old_values.len(), new_values.len());
    let weight = |i: usize, j: usize| -> u32 {
        if old.file_name(old_values[i].location.file) != new.file_name(new_values[j].location.file)
        {
            0
        } else if old_values[i].code == new_values[j].code {
            3
        } else {
            2
        }
    };
    // The heaviest common subsequence
    let mut best = vec![vec![0u32; m + 1]; n + 1];
    for i in (0..n).rev() {
        for j in (0..m).rev() {
            let mut b = best[i + 1][j].max(best[i][j + 1]);
            let w = weight(i, j);
            if w > 0 {
                b = b.max(w + best[i + 1][j + 1]);
            }
            best[i][j] = b;
        }
    }
    let mut map = vec![None; m];
    let (mut i, mut j) = (0, 0);
    while i < n && j < m {
        let w = weight(i, j);
        if w > 0 && best[i][j] == w + best[i + 1][j + 1] {
            map[j] = Some(i);
            i += 1;
            j += 1;
        } else if best[i][j] == best[i + 1][j] {
            i += 1;
        } else {
            j += 1;
        }
    }
    map
}

/// Report removed definitions. Keep global entries for existing call sites
/// and remove them only from override numbering.
fn removed(
    old: &Snapshot,
    n: TypeRef,
    name: &str,
    old_values: &[&ProcValue],
    changes: &mut Changes,
) {
    let path = proc_path(n, name);
    if n.is_root() {
        changes.warnings.push(format!(
            "{path}: a definition was removed from source but retained in the running world"
        ));
    }
    let verb = old
        .find_type(&n.get().path)
        .and_then(|o| o.get_proc_declaration(name))
        .is_some_and(|d| d.kind == dreammaker::ast::ProcDeclKind::Verb);
    for override_id in 0..old_values.len() {
        changes.removed_procs.push(RemovedProc {
            proc: ProcKey {
                path: path.clone(),
                override_id: override_id as u32,
            },
            type_path: n.get().path.clone(),
            name: name.to_owned(),
            verb,
        });
    }
}

/// Whether a type's var is one of BYOND's own.
fn is_builtin(t: TypeRef, name: &str) -> bool {
    t.get_var_declaration(name)
        .is_some_and(|d| d.get().location.file == FileId::BUILTINS)
}

/// Whether the type or one of its ancestors sets a built-in var in code.
fn sets_builtin(t: TypeRef, name: &str) -> bool {
    let mut at = Some(t);
    while let Some(t) = at {
        if t.get()
            .vars
            .get(name)
            .is_some_and(|v| v.value.location.file != FileId::BUILTINS)
        {
            return true;
        }
        at = t.parent_type();
    }
    false
}

/// The built-in vars whose constant values a reload can set on a type: the
/// fields of its record, and those it lists with their values.
const BUILTIN_VARS: &[&str] = &[
    // Fields
    "name",
    "desc",
    "icon",
    "icon_state",
    "text",
    "suffix",
    "dir",
    "layer",
    "maptext",
    "maptext_x",
    "maptext_y",
    "maptext_width",
    "maptext_height",
    // Flags
    "density",
    "opacity",
    "gender",
    "mouse_drop_zone",
    "mouse_opacity",
    "animate_movement",
    "appearance_flags",
    // Listed
    "icon_w",
    "icon_z",
    "bounds",
    "alpha",
    "blend_mode",
    "color",
    "invisibility",
    "infra_luminosity",
    "luminosity",
    "pixel_x",
    "pixel_y",
    "pixel_w",
    "pixel_z",
    "plane",
    "render_source",
    "render_target",
    "bound_x",
    "bound_y",
    "bound_width",
    "bound_height",
    "step_size",
    "step_x",
    "step_y",
    "glide_size",
    "screen_loc",
    "vis_flags",
    "mouse_drag_pointer",
    "mouse_drop_pointer",
    "mouse_over_pointer",
    "override",
    // Client values stored in the type's built-in list.
    "fps",
    "view",
    "perspective",
    "tick_lag",
    "default_verb_category",
    "edge_limit",
    "mouse_pointer_icon",
    // A mob type's, in the mob table
    "key",
    "sight",
    "see_in_dark",
    "see_invisible",
];

/// Reports a type the running world doesn't have, with the fields of its
/// record that aren't its parent's, as DreamMaker works them out.
fn new_type(n: TypeRef, changes: &mut Changes) {
    let path = n.get().path.clone();
    let Some(parent) = n.parent_type() else {
        changes
            .refused
            .push(format!("{path}: new type with no parent"));
        return;
    };
    let mut fields = vec![];
    let mut refused = false;
    for (name, var) in n.get().vars.iter() {
        let name = name.as_str();
        if !is_builtin(n, name)
            || var.value.location.file == FileId::BUILTINS
            || builtin_in_var_table(n, name)
        {
            continue;
        }
        if name == "parent_type" {
            continue;
        }
        // One that isn't a constant is set by the type's init proc
        let Some(value) = var_constant(var) else {
            continue;
        };
        if !BUILTIN_VARS.contains(&name) {
            changes.refused.push(format!(
                "{path}: hot reload cannot set built-in variable {name}"
            ));
            refused = true;
            continue;
        }
        fields.push((name.to_owned(), value));
    }
    if refused {
        return;
    }

    // Without a name of its own or an ancestor's, a type is named for its
    // path, and its text is the first letter of its name
    let mut name = fields
        .iter()
        .find(|(field, _)| field == "name")
        .map(|(_, value)| value.clone());
    if name.is_none() && !sets_builtin(n, "name") {
        let last = path
            .rsplit('/')
            .next()
            .unwrap_or_default()
            .replace('_', " ");
        name = Some(ConstValue::String(last.clone()));
        fields.push(("name".to_owned(), ConstValue::String(last)));
    }
    if !sets_builtin(n, "text") {
        if let Some(ConstValue::String(name)) = name {
            let first: String = name.chars().take(1).collect();
            fields.push(("text".to_owned(), ConstValue::String(first)));
        }
    }

    changes.new_types.push(NewType {
        path,
        parent: parent.get().path.clone(),
        fields,
    });
}

/// Compare types, collect unsupported edits and default changes, and return
/// new proc definitions to compile with their type, name and declaration flags.
fn compare_types(
    old: &Snapshot,
    new: &Snapshot,
    changes: &mut Changes,
    realigned: &mut Realigned,
) -> Vec<(ProcKey, String, String, bool, bool)> {
    let mut new_procs = vec![];
    let by_path = |s: &'_ Snapshot| -> BTreeMap<String, ()> {
        s.tree
            .iter_types()
            .map(|t| (t.get().path.clone(), ()))
            .collect()
    };
    let old_types = by_path(old);
    let new_types = by_path(new);
    for path in new_types.keys().filter(|p| !old_types.contains_key(*p)) {
        if let Some(n) = new.find_type(path) {
            new_type(n, changes);
        }
    }
    for path in old_types.keys().filter(|p| !new_types.contains_key(*p)) {
        changes.warnings.push(format!(
            "{path}: type removed from source but retained in the running world"
        ));
    }

    // A new type is compared with an empty one
    let no_vars = Default::default();
    let no_procs = Default::default();
    for path in new_types.keys() {
        let Some(n) = new.find_type(path) else {
            continue;
        };
        let o = old.find_type(path);
        let shown = if path.is_empty() {
            "(global)"
        } else {
            path.as_str()
        };

        let parent = |t: TypeRef| t.parent_type().map(|p| p.get().path.clone());
        if let Some(o) = o.filter(|o| parent(*o) != parent(n)) {
            reparent::reparent(old, o, n, changes);
        }

        let ov = o.map_or(&no_vars, |o| &o.get().vars);
        let nv = &n.get().vars;
        let declaration =
            |v: &dreammaker::objtree::TypeVar| v.declaration.as_ref().map(|d| d.var_type.clone());
        for (name, var) in nv.iter() {
            // Most built-ins are type-record fields. Sound and similar types
            // store theirs as defaults in the var table.
            if o.is_none()
                && is_builtin(n, name.as_str())
                && !builtin_in_var_table(n, name.as_str())
            {
                continue;
            }
            match ov.get(name) {
                None if var.declaration.is_some() => {
                    if new_var(new, n, name.as_str(), var, o.is_none(), changes) {
                        var_default(old, new, path, name.as_str(), true, changes);
                    }
                }
                // Overriding an inherited var, which it didn't before
                None => var_default(old, new, path, name.as_str(), false, changes),
                Some(old_var) if declaration(old_var) != declaration(var) => {
                    // Type metadata affects compilation only; recompile dependent code.
                    let storage = |v: &dreammaker::objtree::TypeVar| {
                        v.declaration.as_ref().map(|d| {
                            (
                                d.var_type.flags.is_static(),
                                d.var_type.flags.is_const(),
                                d.var_type.flags.is_tmp(),
                                d.var_type.input_type,
                            )
                        })
                    };
                    if !changes.retyped.iter().any(|r| r == name.as_str()) {
                        changes.retyped.push(name.to_string());
                    }
                    let kind =
                        |v: &dreammaker::objtree::TypeVar| storage(v).map(|s| (s.0, s.1, s.2));
                    let static_like =
                        |v: &dreammaker::objtree::TypeVar| kind(v).is_some_and(|k| k.0 || k.1);
                    let redeclared = kind(old_var) != kind(var) && !n.is_root();
                    if needs_constant(var) {
                        changes.refused.push(format!(
                            "{shown}: var {name} is const but has a non-constant value (DreamMaker: constant initializer required)"
                        ));
                        continue;
                    }
                    if let Some(why) = dm_refused_const(var) {
                        changes
                            .warnings
                            .push(refused_const_warning(&shown, name, why));
                    }
                    if redeclared {
                        redeclare(new, n, name.as_str(), old_var, var, changes);
                    }
                    // Unless that gave it its values
                    let valued = redeclared
                        && (static_like(old_var) != static_like(var)
                            || kind(var).is_some_and(|k| k.1));
                    if !valued && old_var.value.expression != var.value.expression {
                        var_default(old, new, path, name.as_str(), false, changes);
                    }
                }
                Some(old_var) if old_var.value.expression != var.value.expression => {
                    var_default(old, new, path, name.as_str(), false, changes);
                }
                // Compare moved defaults by path: inserting an include renumbers
                // subsequent files without moving their source locations.
                Some(old_var)
                    if old.place(old_var.value.location) != new.place(var.value.location) =>
                {
                    if !changes.defaults_moved.iter().any(|c| c == name.as_str()) {
                        changes.defaults_moved.push(name.to_string());
                    }
                }
                Some(_) => {}
            }
        }
        for (name, var) in ov.iter().filter(|(name, _)| !nv.contains_key(*name)) {
            if var.declaration.is_some() {
                changes.warnings.push(format!(
                    "{shown}: var {name} was removed from source but retained in the running world"
                ));
            } else if n.get_var_declaration(name.as_str()).is_some() {
                // No longer overridden here: the type goes back to what it inherits
                var_default(old, new, path, name.as_str(), false, changes);
            }
            // (else the var itself was removed, warned about where it was declared)
        }

        let op = o.map_or(&no_procs, |o| &o.get().procs);
        let np = &n.get().procs;
        for (name, proc) in np.iter() {
            let old_values = op.get(name).map(real).unwrap_or_default();
            let new_values = real(proc);
            let (old_count, new_count) = (old_values.len(), new_values.len());
            let path = proc_path(n, name.as_str());
            let verb = n
                .get_proc_declaration(name.as_str())
                .is_some_and(|d| d.kind == dreammaker::ast::ProcDeclKind::Verb);
            // Preserve source order when inserting or removing proc definitions;
            // the runtime renumbers them after application.
            if new_count != old_count {
                let map = align(old, new, &old_values, &new_values);
                for override_id in (0..old_count).filter(|i| !map.contains(&Some(*i))) {
                    if n.is_root() {
                        changes.warnings.push(format!(
                            "{path}: a definition was removed from source but retained in the running world"
                        ));
                    }
                    changes.removed_procs.push(RemovedProc {
                        proc: ProcKey {
                            path: path.clone(),
                            override_id: override_id as u32,
                        },
                        type_path: n.get().path.clone(),
                        name: name.as_str().to_owned(),
                        verb,
                    });
                }
                for (override_id, _) in map.iter().enumerate().filter(|(_, old)| old.is_none()) {
                    // One before the first the type had leaves that one the
                    // declaration (`proc/`) it was made with
                    let declared = override_id == 0 && proc.declaration.is_some() && old_count == 0;
                    if n.is_root() && !declared {
                        // DreamMaker makes it a proc of its own (`/name`), and
                        // calls still go to the declared one
                        changes.warnings.push(format!(
                            "{path}: additional global definition is unreachable; calls retain the original definition, matching DreamMaker"
                        ));
                    }
                    new_procs.push((
                        ProcKey {
                            path: path.clone(),
                            override_id: override_id as u32,
                        },
                        n.get().path.clone(),
                        name.as_str().to_owned(),
                        declared,
                        verb,
                    ));
                }
                realigned.insert(
                    path,
                    map.into_iter().map(|old| old.map(|i| i as u32)).collect(),
                );
                continue;
            }
            let declaration = |p: Option<&dreammaker::objtree::TypeProc>| {
                p.and_then(|p| p.declaration.as_ref())
                    .map(|d| (d.kind, d.flags))
            };
            if declaration(op.get(name)) != declaration(Some(proc)) {
                changes.refused.push(format!(
                    "{path}: proc declaration kind or flags changed; rebuild required"
                ));
            }
        }
        for name in op.keys().filter(|name| !np.contains_key(*name)) {
            let old_values = real(&op[name]);
            if !old_values.is_empty() {
                removed(old, n, name.as_str(), &old_values, changes);
            }
        }
    }

    new_procs
}

/// A var declared on type `n` that the old code didn't have. False when it
/// can't be added.
fn new_var(
    snapshot: &Snapshot,
    n: TypeRef,
    name: &str,
    var: &dreammaker::objtree::TypeVar,
    new_type: bool,
    changes: &mut Changes,
) -> bool {
    let shown = if n.is_root() {
        "(global)".to_owned()
    } else {
        n.get().path.clone()
    };
    let flags = var
        .declaration
        .as_ref()
        .map(|d| d.var_type.flags)
        .unwrap_or_default();
    // Validate declarations DreamMaker would reject.
    let place = var
        .declaration
        .as_ref()
        .map_or(var.value.location, |d| d.location);
    let place = format!("{}:{}", snapshot.file_name(place.file), place.line);
    if let Some(other) = declared_elsewhere(n, name) {
        changes.refused.push(format!(
            "{place}: {name}: duplicate definition ({other} declares it too)"
        ));
        return false;
    }
    if needs_constant(var) {
        changes
            .refused
            .push(format!("{place}: {name}: constant initializer required"));
        return false;
    }
    if let Some(why) = dm_refused_const(var) {
        changes
            .warnings
            .push(refused_const_warning(&place, name, why));
    }
    let static_like = flags.is_static() || flags.is_const() || n.is_root();
    let (value, init) = if static_like {
        // The world sets it when it starts, so the reload sets it now
        match static_initial(snapshot, n, var) {
            Ok(value) => value,
            Err(e) => {
                changes.refused.push(format!(
                    "{shown}: new var {name}: initializer compilation failed: {e}"
                ));
                return false;
            }
        }
    } else {
        match var_constant(var) {
            Some(value) => (value, None),
            // The init proc sets it on new instances
            None => {
                // BYOND's normal initial() placeholder would also become the value
                // on existing instances. Use null for new computed defaults instead.
                if new_type {
                    changes.warnings.push(format!(
                        "{shown}: new var {name} has a computed default; initial() remains null until restart"
                    ));
                } else {
                    changes.warnings.push(format!(
                        "{shown}: new var {name} has a computed default; existing instances and initial() remain null until restart"
                    ));
                }
                (ConstValue::Null, None)
            }
        }
    };
    changes.new_vars.push(NewVar {
        type_path: n.get().path.clone(),
        var: name.to_owned(),
        flags: byond_flags(flags, n.is_root()),
        value,
        init,
    });
    true
}

/// Find an ancestor or descendant whose declaration of `name` would conflict with `n`.
fn declared_elsewhere(n: TypeRef, name: &str) -> Option<String> {
    let declares = |t: TypeRef| {
        t.get()
            .vars
            .get(name)
            .is_some_and(|v| v.declaration.is_some())
    };
    // Same-named globals do not conflict with type vars.
    let mut at = n.parent_type().filter(|t| !t.is_root());
    while let Some(t) = at {
        if declares(t) {
            return Some(t.get().path.clone());
        }
        at = t.parent_type().filter(|t| !t.is_root());
    }
    if n.is_root() {
        return None;
    }
    // Follow actual parents, not path prefixes: `/obj` inherits from `/atom/movable`,
    // and `parent_type` can change a type's ancestry.
    let descends = |t: TypeRef| {
        let mut at = t.parent_type();
        while let Some(parent) = at {
            if parent == n {
                return true;
            }
            at = parent.parent_type();
        }
        false
    };
    n.tree()
        .iter_types()
        .find(|t| *t != n && declares(*t) && descends(*t))
        .map(|t| t.get().path.clone())
}

/// Whether a const initializer lacks a valid constant value. Lists, `new`,
/// constructors such as `matrix()` and `icon()`, and runtime expressions are invalid.
fn needs_constant(var: &dreammaker::objtree::TypeVar) -> bool {
    use dreammaker::constants::Constant;
    let is_const = var
        .declaration
        .as_ref()
        .is_some_and(|d| d.var_type.flags.is_const());
    let Some(expression) = var.value.expression.as_ref().filter(|_| is_const) else {
        return false;
    };
    let constant = var
        .value
        .constant
        .clone()
        .or_else(|| expression.simple_evaluate(var.value.location).ok());
    matches!(
        constant,
        None | Some(Constant::List(_)) | Some(Constant::New { .. }) | Some(Constant::Call(..))
    )
}

/// Reject a changed const value if it is no longer constant. Return whether rejected.
fn refused_const_value(new: &Snapshot, n: TypeRef, name: &str, changes: &mut Changes) -> bool {
    let Some(var) = n.get().vars.get(name).filter(|var| needs_constant(var)) else {
        return false;
    };
    let at = var.value.location;
    changes.refused.push(format!(
        "{}:{}: {name}: constant initializer required",
        new.file_name(at.file),
        at.line
    ));
    true
}

/// Find initializers SpacemanDMM folds but DreamMaker rejects: `type`,
/// `parent_type` and comparisons. Reload accepts these with a warning.
fn dm_refused_const(var: &dreammaker::objtree::TypeVar) -> Option<&'static str> {
    use dreammaker::ast::{BinaryOp, Expression, Term};
    fn why(expr: &Expression) -> Option<&'static str> {
        match expr {
            Expression::Base { term, .. } => match &term.elem {
                Term::Ident(name) if name.as_str() == "type" => Some("it reads type"),
                Term::Ident(name) if name.as_str() == "parent_type" => Some("it reads parent_type"),
                Term::Expr(inner) => why(inner),
                _ => None,
            },
            Expression::BinaryOp { op, lhs, rhs } => match op {
                BinaryOp::Eq
                | BinaryOp::NotEq
                | BinaryOp::Less
                | BinaryOp::LessEq
                | BinaryOp::Greater
                | BinaryOp::GreaterEq
                | BinaryOp::Equiv
                | BinaryOp::NotEquiv => Some("it compares"),
                _ => why(lhs).or_else(|| why(rhs)),
            },
            Expression::TernaryOp { cond, if_, else_ } => {
                why(cond).or_else(|| why(if_)).or_else(|| why(else_))
            }
            _ => None,
        }
    }
    let is_const = var
        .declaration
        .as_ref()
        .is_some_and(|d| d.var_type.flags.is_const());
    why(var.value.expression.as_ref().filter(|_| is_const)?)
}

fn refused_const_warning(place: &str, name: &str, why: &str) -> String {
    format!(
        "{place}: const {name}: DreamMaker refuses this initializer ({why}; constant initializer required). Accepted for hot reload, but a rebuild will fail"
    )
}

/// A declared var's flags, as BYOND keeps them
fn byond_flags(flags: dreammaker::ast::VarTypeFlags, global: bool) -> u32 {
    let mut byond_flags = 0;
    if flags.is_static() || global {
        byond_flags |= 1;
    }
    if flags.is_const() {
        byond_flags |= 3;
    }
    if flags.is_tmp() {
        byond_flags |= 4;
    }
    byond_flags
}

/// Extract a static local's containing proc from its `proc:PATH#N` scope.
fn static_proc_path(scope: &str) -> Option<&str> {
    let path = scope.strip_prefix("proc:")?;
    Some(path.rsplit_once('#').map_or(path, |(path, _)| path))
}

/// Return an initial constant, or null with code for the world to run once.
fn static_initial(
    snapshot: &Snapshot,
    n: TypeRef,
    var: &dreammaker::objtree::TypeVar,
) -> Result<(ConstValue, Option<Box<ProcReload>>), String> {
    if let Some(value) = var_constant(var) {
        return Ok((value, None));
    }
    let Some(expression) = &var.value.expression else {
        return Ok((ConstValue::Null, None));
    };
    let location = value_location(expression, var.value.location);
    let declared = var
        .declaration
        .as_ref()
        .map(|d| d.var_type.type_path.to_string())
        .unwrap_or_default();
    let code = snapshot
        .compile_value(n, expression, &declared, location, None)
        .map_err(|e| e.to_string())?;
    Ok((ConstValue::Null, Some(Box::new(code))))
}

/// Update variable declaration flags and recompile users. Instance variables
/// receive per-type defaults; static variables use global slots.
fn redeclare(
    new: &Snapshot,
    n: TypeRef,
    name: &str,
    old_var: &dreammaker::objtree::TypeVar,
    var: &dreammaker::objtree::TypeVar,
    changes: &mut Changes,
) {
    let path = n.get().path.clone();
    let flags_of = |v: &dreammaker::objtree::TypeVar| {
        v.declaration
            .as_ref()
            .map(|d| d.var_type.flags)
            .unwrap_or_default()
    };
    let (old_flags, flags) = (flags_of(old_var), flags_of(var));
    let is_static = |f: dreammaker::ast::VarTypeFlags| f.is_static() || f.is_const();
    let mut redeclared = Redeclared {
        type_path: path.clone(),
        var: name.to_owned(),
        flags: byond_flags(flags, false),
        value: None,
        init: None,
    };

    if is_static(flags) {
        // A static var keeps the value it has, unless it wasn't one
        if !is_static(old_flags) || flags.is_const() {
            match static_initial(new, n, var) {
                Ok((value, init)) => {
                    redeclared.value = Some(value);
                    redeclared.init = init;
                }
                Err(e) => {
                    changes.refused.push(format!(
                        "{path}: var {name}: initializer compilation failed: {e}"
                    ));
                    return;
                }
            }
        }
        changes.redeclared.push(redeclared);
        return;
    }

    changes.redeclared.push(redeclared);
    if !is_static(old_flags) {
        return;
    }
    // Each type's default, the constants grouped; the init procs set the
    // others on new instances
    let mut defaults: Vec<VarDefault> = vec![];
    let mut computed = false;
    for ty in new.tree.iter_types() {
        let mut at = Some(ty);
        while let Some(t) = at {
            if t.index() == n.index() {
                break;
            }
            at = t.parent_type();
        }
        if at.is_none() {
            continue;
        }
        match default_value(ty, name) {
            Some(Some(value)) => match defaults.iter_mut().find(|d| d.value == value) {
                Some(d) => d.types.push(ty.get().path.clone()),
                None => defaults.push(VarDefault {
                    var: name.to_owned(),
                    value,
                    types: vec![ty.get().path.clone()],
                }),
            },
            _ => computed = true,
        }
    }
    if computed {
        changes.warnings.push(format!(
            "{path}: var {name} has a computed default; existing instances retain null"
        ));
    }
    changes.var_defaults.extend(defaults);
}

/// Find the nearest type defining this variable's default.
fn defined_on<'a>(t: TypeRef<'a>, name: &str) -> Option<TypeRef<'a>> {
    let mut at = Some(t);
    while let Some(ty) = at {
        if ty.get().vars.contains_key(name) {
            return Some(ty);
        }
        at = ty.parent_type();
    }
    None
}

/// Resolve a variable default. Inner `None` means computed by an init proc.
fn default_value(t: TypeRef, name: &str) -> Option<Option<ConstValue>> {
    let var = defined_on(t, name)?.get().vars.get(name)?;
    Some(var_constant(var))
}

/// Resolve a constant default. SpacemanDMM does not fold static or tmp
/// initializers, so those require a literal value.
fn var_constant(var: &dreammaker::objtree::TypeVar) -> Option<ConstValue> {
    match &var.value.expression {
        None => Some(ConstValue::Null),
        Some(expression) => match &var.value.constant {
            Some(constant) => const_value(constant),
            None => const_value(&expression.simple_evaluate(var.value.location).ok()?),
        },
    }
}

/// Collect a changed default for `path` and inheriting subtypes.
/// `is_new` identifies a newly declared variable.
fn var_default(
    old: &Snapshot,
    new: &Snapshot,
    path: &str,
    name: &str,
    is_new: bool,
    changes: &mut Changes,
) {
    let shown = if path.is_empty() { "(global)" } else { path };
    let Some(n) = new.find_type(path) else { return };
    let Some(declaration) = n.get_var_declaration(name) else {
        changes.refused.push(format!(
            "{shown}: var {name} has a value but no declaration"
        ));
        return;
    };
    let flags = declaration.get().var_type.flags;
    // Built-ins stored in a var table, such as sound vars, use ordinary defaults.
    let in_var_table = builtin_in_var_table(n, name);
    let builtin = declaration.get().location.file == FileId::BUILTINS
        && (!in_var_table || !sets_own_builtin(n, name));
    // A global const is set now (no code writes it), and the code it was
    // folded into compiled again
    if n.is_root() && flags.is_const() && !is_new {
        if refused_const_value(new, n, name, changes) {
            return;
        }
        if !changes.consts.iter().any(|c| c == name) {
            changes.consts.push(name.to_owned());
        }
        if let Some(why) = n.get().vars.get(name).and_then(dm_refused_const) {
            changes
                .warnings
                .push(refused_const_warning(shown, name, why));
        }
        if let Some(var) = n.get().vars.get(name) {
            match static_initial(new, n, var) {
                Ok((value, init)) => changes.redeclared.push(Redeclared {
                    type_path: String::new(),
                    var: name.to_owned(),
                    flags: byond_flags(flags, true),
                    value: Some(value),
                    init,
                }),
                Err(e) => changes.refused.push(format!(
                    "{shown}: const {name}: value compilation failed: {e}"
                )),
            }
        }
        return;
    }
    // A new var starts with its value; an existing global keeps the one it
    // has now
    if n.is_root() || flags.is_static() {
        if !is_new {
            changes.warnings.push(format!(
                "{shown}: {name}: changed global initializer applies only after a restart"
            ));
        }
        return;
    }
    // Type constants use global slots. Update their values and recompile
    // users that folded the old value (see `const_users`).
    if flags.is_const() {
        if is_new {
            return;
        }
        if refused_const_value(new, n, name, changes) {
            return;
        }
        if !changes.consts.iter().any(|c| c == name) {
            changes.consts.push(name.to_owned());
        }
        if let Some(why) = n.get().vars.get(name).and_then(dm_refused_const) {
            changes
                .warnings
                .push(refused_const_warning(shown, name, why));
        }
        // (a built-in one, like `parent_type`, isn't a global)
        if builtin {
            return;
        }
        let declaring = declaration.ty();
        let path = declaring.get().path.clone();
        if changes
            .redeclared
            .iter()
            .any(|r| r.type_path == path && r.var == name)
        {
            return;
        }
        let Some(var) = declaring.get().vars.get(name) else {
            return;
        };
        match static_initial(new, declaring, var) {
            Ok((value, init)) => changes.redeclared.push(Redeclared {
                type_path: path,
                var: name.to_owned(),
                flags: byond_flags(flags, false),
                value: Some(value),
                init,
            }),
            Err(e) => changes.refused.push(format!(
                "{shown}: const {name}: value compilation failed: {e}"
            )),
        }
        return;
    }

    // World settings and some client settings are read at startup from the
    // world record, so changes need a restart.
    if builtin && in_world_record(n, name) {
        if !is_new {
            changes.warnings.push(restart_warning(path, name));
            changes
                .restart_settings
                .push((path.to_owned(), name.to_owned()));
        }
        return;
    }

    // `/type::var` is compiled into the code that says it, which is
    // compiled again (see `const_users`)
    if !is_new && !changes.defaults_changed.iter().any(|c| c == name) {
        changes.defaults_changed.push(name.to_owned());
    }

    // Inherit from the running parent: SpacemanDMM lacks some built-in
    // defaults, including mob density, layer and see_in_dark.
    if builtin
        && !is_new
        && !sets_own_builtin(n, name)
        && (in_var_table || BUILTIN_VARS.contains(&name))
        && !matches!(name, "name" | "text")
    {
        // Existing instances read the var table, as with ordinary vars.
        if !in_var_table {
            changes.warnings.push(format!(
                "{shown}: {name}: built-in default applies only to new instances"
            ));
        }
        changes.builtin_inherited.push(BuiltinInherited {
            var: name.to_owned(),
            types: inheriting(new, n, name),
        });
        return;
    }

    // Existing instances have already run their computed initializers.
    let Some(Some(value)) = default_value(n, name) else {
        if !is_new && !builtin {
            changes.warnings.push(format!(
                "{shown}: var {name} has a computed default; only new instances receive it"
            ));
        }
        return;
    };
    if !is_new {
        let old_value = old.find_type(path).and_then(|o| default_value(o, name));
        if old_value == Some(Some(value.clone())) {
            return;
        }
    }

    // The type and its subtypes that don't set the var themselves
    let mut types = vec![];
    for ty in new.tree.iter_types() {
        let mut at = ty;
        loop {
            if at.index() == n.index() {
                types.push(ty.get().path.clone());
                break;
            }
            if at.get().vars.contains_key(name) {
                break;
            }
            match at.parent_type() {
                Some(parent) => at = parent,
                None => break,
            }
        }
    }
    if builtin {
        if !BUILTIN_VARS.contains(&name) {
            changes.refused.push(format!(
                "{shown}: {name}: changing this built-in default requires a rebuild"
            ));
            return;
        }
        changes.warnings.push(format!(
            "{shown}: {name}: built-in default applies only to new instances"
        ));
        if name == "name" {
            builtin_names(new, n, value, types, changes);
            return;
        }
        changes.builtin_defaults.push(VarDefault {
            var: name.to_owned(),
            value,
            types,
        });
        return;
    }
    changes.var_defaults.push(VarDefault {
        var: name.to_owned(),
        value,
        types,
    });
}

fn restart_warning(path: &str, name: &str) -> String {
    let shown = if path.is_empty() { "(global)" } else { path };
    format!("{shown}: {name}: changed setting applies only after a restart")
}

/// Client vars stored in the world record; the others use the type's
/// built-in value list.
const CLIENT_SETTINGS: &[&str] = &[
    "authenticate",
    "command_text",
    "control_freak",
    "dir",
    "lazy_eye",
    "preload_rsc",
    "show_map",
    "show_popup_menus",
    "show_verb_panel",
];

/// Whether a built-in var of `t` is kept in the world record: every `/world`
/// setting, and some of `/client`'s.
fn in_world_record(t: TypeRef, name: &str) -> bool {
    match t.get().path.as_str() {
        "/world" => true,
        "/client" => CLIENT_SETTINGS.contains(&name),
        _ => false,
    }
}

/// Built-in types whose built-in vars use the var table rather than
/// type-record fields.
const VAR_TABLE_BUILTINS: &[&str] = &["/sound", "/particles", "/exception", "/regex", "/icon"];

/// Whether a built-in var of `t` is kept in its var table.
fn builtin_in_var_table(t: TypeRef, name: &str) -> bool {
    t.get_var_declaration(name).is_some_and(|d| {
        d.get().location.file == FileId::BUILTINS
            && VAR_TABLE_BUILTINS.contains(&d.ty().get().path.as_str())
    })
}

/// Whether the source sets this built-in directly on the type.
fn sets_own_builtin(t: TypeRef, name: &str) -> bool {
    t.get()
        .vars
        .get(name)
        .is_some_and(|v| v.value.location.file != FileId::BUILTINS)
}

/// `n` and subtypes inheriting `name` from it, in parent-first order.
fn inheriting(new: &Snapshot, n: TypeRef, name: &str) -> Vec<String> {
    let mut types = vec![];
    for ty in new.tree.iter_types() {
        let mut at = ty;
        let mut depth = 0;
        loop {
            if at.index() == n.index() {
                types.push((depth, ty.get().path.clone()));
                break;
            }
            if at.get().vars.contains_key(name) {
                break;
            }
            depth += 1;
            match at.parent_type() {
                Some(parent) => at = parent,
                None => break,
            }
        }
    }
    types.sort();
    types.into_iter().map(|(_, path)| path).collect()
}

/// Update name/text defaults on `n` and inheriting types. Without an
/// explicit inherited name, derive it from the path; default text is
/// the name's first letter.
fn builtin_names(
    new: &Snapshot,
    n: TypeRef,
    value: ConstValue,
    types: Vec<String>,
    changes: &mut Changes,
) {
    let own_name = |path: &str| {
        ConstValue::String(
            path.rsplit('/')
                .next()
                .unwrap_or_default()
                .replace('_', " "),
        )
    };
    let named = sets_builtin(n, "name");
    let mut names = vec![];
    for path in types {
        let name = if named {
            value.clone()
        } else {
            own_name(&path)
        };
        names.push((path, name));
    }
    for (path, name) in &names {
        let push = |changes: &mut Changes, var: &str, value: ConstValue| match changes
            .builtin_defaults
            .iter_mut()
            .find(|d| d.var == var && d.value == value)
        {
            Some(d) => d.types.push(path.clone()),
            None => changes.builtin_defaults.push(VarDefault {
                var: var.to_owned(),
                value,
                types: vec![path.clone()],
            }),
        };
        push(changes, "name", name.clone());
        let sets_text = new.find_type(path).is_some_and(|t| sets_builtin(t, "text"));
        if let (ConstValue::String(name), false) = (name, sets_text) {
            push(
                changes,
                "text",
                ConstValue::String(name.chars().take(1).collect()),
            );
        }
    }
}

fn const_value(constant: &dreammaker::constants::Constant) -> Option<ConstValue> {
    use dreammaker::constants::Constant;
    Some(match constant {
        Constant::Null(_) => ConstValue::Null,
        Constant::Float(f) => ConstValue::Number(*f),
        Constant::String(s) => ConstValue::String(crate::compiler::text(s.as_str())?),
        Constant::Resource(s) => ConstValue::Resource(s.to_string()),
        // Proc paths too (`/proc/x`, `/datum/a/verb/y`)
        Constant::Prefab(pop) if pop.vars.is_empty() => {
            let path: Vec<&str> = pop.path.iter().map(|s| s.as_str()).collect();
            ConstValue::Path(format!("/{}", path.join("/")))
        }
        _ => return None,
    })
}

/// Return a proc definition's source range, from header through body.
/// For the last proc in a file, use that file's end if the range ends in the next file.
fn proc_lines(value: &ProcValue) -> std::ops::RangeInclusive<u32> {
    let start = value.location.line;
    let end = match &value.body_range {
        Some(r) if r.end.file == value.location.file && r.end.line >= start => r.end.line,
        Some(_) => u32::MAX,
        None => start,
    };
    start..=end
}

/// The names that follow `::` in DM source (`/type::name`, `x::name`), with
/// their lines
fn names_after_scope(text: &str) -> Vec<(String, u32)> {
    let mut found = vec![];
    for (i, line) in text.lines().enumerate() {
        for after in line.split("::").skip(1) {
            let end = after
                .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
                .unwrap_or(after.len());
            if end > 0 {
                found.push((after[..end].to_owned(), i as u32 + 1));
            }
        }
    }
    found
}
