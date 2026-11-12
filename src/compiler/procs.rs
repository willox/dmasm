//! Compile complete procs, resolve their names and track cache-register state.

use std::collections::HashMap;

use dreammaker::ast::Block;
use dreammaker::objtree::{ObjectTree, ProcValue, TypeRef};

use super::{CompileError, Compiler, EvalKind};
use crate::operands::{DMString, GlobalVar, Variable};
use crate::{Instruction, Node};

/// A proc to compile, as SpacemanDMM parsed it.
pub struct ProcSource<'a> {
    pub tree: &'a ObjectTree,
    /// The type the proc is defined on (the root type for global procs).
    pub ty: TypeRef<'a>,
    pub name: &'a str,
    pub value: &'a ProcValue,
    /// The file name for `DbgFile`, as DreamMaker writes it: relative to the
    /// .dme's directory, with forward slashes.
    pub file: &'a str,
    /// Whether to emit DbgFile and DbgLine, as DreamMaker does for worlds
    /// built with `#define DEBUG`.
    pub debug_info: bool,
    /// Override index for a reparsed definition outside the snapshot's tree.
    /// Otherwise infer the index from the tree.
    pub index: Option<usize>,
    /// The build whose DreamMaker the code has to match: the running world's.
    pub byond: crate::ByondVersion,
    /// Debug source names for defaults referenced from other files by `::`.
    /// Without this map, omit those defaults' debug locations.
    pub file_names: Option<&'a dyn Fn(dreammaker::FileId) -> String>,
}

#[derive(Debug)]
pub struct CompiledProc {
    pub nodes: Vec<Node>,
    /// Local variable names, by slot.
    pub locals: Vec<String>,
    pub params: Vec<String>,
    /// Static locals (and local consts), in the order they're declared.
    pub statics: Vec<StaticLocal>,
    /// Compatibility warnings; see COMPILER.md.
    pub warnings: Vec<String>,
}

/// A static local: a global the world sets up when it starts.
#[derive(Debug, Clone)]
pub struct StaticLocal {
    pub name: String,
    /// The scope the proc's code names it with (`proc:PATH#N`).
    pub scope: String,
    /// Its initial value, when that's a constant.
    pub value: Option<dreammaker::constants::Constant>,
    /// Optional initializer expression.
    pub expression: Option<dreammaker::ast::Expression>,
    /// Declared type used to resolve implicit `new`.
    pub type_path: String,
}

/// Marks a static local's entry in `ProcCtx::scopes`.
const STATIC_LOCAL: u32 = 1 << 31;

#[derive(Clone)]
#[allow(dead_code)]
pub(super) struct ProcCtx<'a> {
    pub tree: &'a ObjectTree,
    pub ty: TypeRef<'a>,
    /// Every local slot's name, in allocation order.
    pub locals: Vec<String>,
    /// Each local's declared type path, or "".
    pub local_types: Vec<String>,
    pub param_types: Vec<String>,
    /// Visible locals, innermost scope last.
    pub scopes: Vec<Vec<(String, u32)>>,
    pub loops: Vec<LoopLabels>,
    /// Nested for-in depth; inner loops save the outer iterator with IterPush.
    pub iterators: u32,
    /// Pending loop label, block exit label and active cleanups.
    pub pending_loop_name: Option<(String, String, usize)>,
    /// `var/static` and `var/global` locals: globals under the local's name.
    pub static_locals: Vec<String>,
    pub statics: Vec<StaticLocal>,
    /// The declared type paths of static locals.
    pub static_local_types: std::collections::HashMap<(String, u32), String>,
    /// After the first for loop, label, switch or pick(), initialize
    /// uninitialized declarations to null, matching DreamMaker.
    pub clear_new_vars: bool,
    /// How many try blocks the code being compiled is in.
    pub try_depth: u32,
    /// The for-in iterators active at each `label:` of the proc.
    pub label_iterators: std::collections::HashMap<String, u32>,
    /// The labels written as a bare name, not yet emitted.
    pub bare_labels: std::collections::HashSet<String>,
    /// Cleanup instructions for enclosing loops, innermost last.
    /// For-in uses IterPop; range loops use PopN for bounds.
    pub cleanups: Vec<Instruction>,
    /// Local `var/const`s with number values, which are folded in.
    pub local_consts: std::collections::HashMap<String, dreammaker::constants::Constant>,
    /// The proc's path, for __PROC__.
    pub proc_path: String,
    pub file: String,
    /// The file the proc is in, and how to name the others
    pub file_id: dreammaker::FileId,
    pub file_names: Option<&'a dyn Fn(dreammaker::FileId) -> String>,
    /// The line of the proc's header, for the default-argument prologue.
    pub header_line: u32,
}

#[derive(Clone)]
pub(super) struct LoopLabels {
    pub name: Option<String>,
    /// How many loop cleanups (see `ProcCtx::cleanups`) are live inside it.
    pub cleanups: usize,
    /// How many are live where `break` goes.
    pub break_cleanups: usize,
    pub continue_label: String,
    pub break_label: String,
    /// A labeled block that isn't a loop: only `break label` leaves it.
    pub block: bool,
}

impl ProcCtx<'_> {
    /// Resolve the innermost declaration: local slot or static index
    /// with `STATIC_LOCAL` set.
    fn find_declared(&self, name: &str) -> Option<u32> {
        self.scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.iter().rev())
            .find(|(n, _)| n == name)
            .map(|(_, slot)| *slot)
    }

    pub fn find_local(&self, name: &str) -> Option<u32> {
        self.find_declared(name)
            .filter(|slot| slot & STATIC_LOCAL == 0)
    }

    /// Resolve a static local's declaration index among same-named statics.
    pub fn find_static(&self, name: &str) -> Option<u32> {
        self.find_declared(name)
            .filter(|slot| slot & STATIC_LOCAL != 0)
            .map(|slot| slot & !STATIC_LOCAL)
    }

    /// Declares a static local in the current block, with its type and
    /// constant initial value.
    pub fn declare_static(
        &mut self,
        name: &str,
        ty: String,
        value: Option<dreammaker::constants::Constant>,
        expression: Option<dreammaker::ast::Expression>,
    ) {
        let number = self.static_locals.iter().filter(|s| *s == name).count() as u32;
        self.static_locals.push(name.to_owned());
        self.statics.push(StaticLocal {
            name: name.to_owned(),
            scope: format!("proc:{}#{}", self.proc_path, number),
            value,
            expression,
            type_path: ty.clone(),
        });
        self.static_local_types
            .insert((name.to_owned(), number), ty);
        if let Some(scope) = self.scopes.last_mut() {
            scope.push((name.to_owned(), number | STATIC_LOCAL));
        }
    }

    pub fn declare_typed_local(&mut self, name: &str, ty: String) -> u32 {
        let slot = self.locals.len() as u32;
        self.locals.push(name.to_owned());
        self.local_types.push(ty);
        self.scopes
            .last_mut()
            .expect("a scope is open")
            .push((name.to_owned(), slot));
        slot
    }
}

pub fn compile_proc(source: &ProcSource) -> Result<CompiledProc, CompileError> {
    compile_proc_as(source, None)
}

/// Compile with an optional `__PROC__` override. Static initializers must name
/// their containing proc rather than the synthetic initializer proc.
pub(crate) fn compile_proc_as(
    source: &ProcSource,
    proc_path: Option<&str>,
) -> Result<CompiledProc, CompileError> {
    // Exclude `...` and absolute-path parameters, matching BYOND.
    let declared: Vec<&dreammaker::ast::Parameter> = source
        .value
        .parameters
        .iter()
        .filter(|p| !p.absolute && p.name.as_str() != "...")
        .collect();
    let params: Vec<String> = declared
        .iter()
        .map(|p| p.name.as_str().to_owned())
        .collect();
    let param_types = declared
        .iter()
        .map(|p| p.var_type.type_path.to_string())
        .collect();

    let mut ctx = proc_ctx(source, param_types);
    if let Some(path) = proc_path {
        ctx.proc_path = path.to_owned();
    }
    let mut compiler = Compiler::new(params.clone(), Some(ctx));
    compiler.debug_info = source.debug_info;
    compiler.byond = source.byond;
    compiler.emit_ins(Instruction::DbgFile(DMString(
        source.file.as_bytes().to_vec(),
    )));

    let empty: Block = Box::new([]);
    let code = source.value.code.as_ref().unwrap_or(&empty);
    super::statements::emit_proc_body(&mut compiler, source, code)?;

    compiler.emit_ins(Instruction::End);
    let ctx = compiler.proc.take().unwrap();
    Ok(CompiledProc {
        warnings: compiler.warnings.into_inner(),
        nodes: compiler.nodes,
        locals: ctx.locals,
        params,
        statics: ctx.statics,
    })
}

/// Create the initial name-resolution context for `source`.
pub(super) fn proc_ctx<'a>(source: &ProcSource<'a>, param_types: Vec<String>) -> ProcCtx<'a> {
    ProcCtx {
        tree: source.tree,
        ty: source.ty,
        locals: vec![],
        local_types: vec![],
        param_types,
        scopes: vec![vec![]],
        loops: vec![],
        iterators: 0,
        pending_loop_name: None,
        static_locals: vec![],
        statics: vec![],
        static_local_types: Default::default(),
        clear_new_vars: false,
        try_depth: 0,
        label_iterators: Default::default(),
        bare_labels: Default::default(),
        cleanups: vec![],
        local_consts: Default::default(),
        proc_path: {
            let ty_path = if source.ty.is_root() {
                ""
            } else {
                source.ty.get().path.as_str()
            };
            let declared_here = source
                .ty
                .get()
                .procs
                .get(source.name)
                .and_then(|p| p.declaration.as_ref());
            let first = source
                .ty
                .get()
                .procs
                .get(source.name)
                .map(|p| match source.index {
                    Some(index) => index == 0,
                    None => std::ptr::eq(&p.value[0], source.value),
                });
            match declared_here {
                Some(decl) if first.unwrap_or(false) => {
                    format!("{}/{}/{}", ty_path, decl.kind.name(), source.name)
                }
                _ => format!("{}/{}", ty_path, source.name),
            }
        },
        file: source.file.to_owned(),
        file_id: source.value.location.file,
        file_names: source.file_names,
        header_line: source.value.location.line,
    }
}

/// Resolves a bare identifier inside a proc: locals, then parameters, then
/// `src`'s vars, then globals. `None` outside proc compilation.
pub(super) fn find_var(compiler: &Compiler, name: &str) -> Option<EvalKind> {
    let ctx = compiler.proc.as_ref()?;
    if let Some(slot) = ctx.find_local(name) {
        return Some(EvalKind::Var(Variable::Local(slot)));
    }
    if let Some(index) = compiler.params.iter().rposition(|x| x == name) {
        return Some(EvalKind::Var(Variable::Arg(index as u32)));
    }
    if let Some(number) = ctx.find_static(name) {
        // Static locals are scoped by proc and block. `proc:PATH#N` identifies
        // the Nth declaration of a given name.
        let scope = format!("proc:{}#{}", ctx.proc_path, number);
        return Some(EvalKind::Var(Variable::Global(GlobalVar::named(
            name.as_bytes(),
            Some(scope),
        ))));
    }
    match name {
        "." | "usr" | "src" | "args" | "world" | "caller" | "callee" | "global" => return None,
        _ => {}
    }
    // A debugger's code: the rest is what its scope says (see `ExprScope`)
    if compiler.expr_scope.is_some() {
        return None;
    }
    let mut scope = String::new();
    if !ctx.ty.is_root() {
        if let Some(decl) = member_var(ctx.ty, name) {
            if !is_global_member(&decl) {
                return Some(EvalKind::Field(
                    super::chain_builder::ChainBuilder::begin(Variable::Src),
                    name.to_owned(),
                ));
            }
            scope = decl.ty().get().path.clone();
        }
    }
    Some(EvalKind::Var(Variable::Global(GlobalVar::named(
        name.as_bytes(),
        Some(scope),
    ))))
}

/// Resolve a proc call operand, using dynamic_verb for verb display names.
/// Runtime names use `set name` when present; otherwise replace
/// underscores in the identifier with spaces.
pub(super) fn call_var(ty: Option<TypeRef>, name: &str) -> Variable {
    let is_verb = ty
        .and_then(|t| t.get_proc_declaration(name))
        .is_some_and(|d| d.kind == dreammaker::ast::ProcDeclKind::Verb);
    let name = call_name(ty, name);
    if is_verb {
        Variable::DynamicVerb(name)
    } else {
        Variable::DynamicProc(name)
    }
}

pub(super) fn call_name(ty: Option<TypeRef>, name: &str) -> DMString {
    let mut current = ty;
    while let Some(t) = current {
        if let Some(proc) = t.get().procs.get(name) {
            for value in &proc.value {
                for stmt in value.code.iter().flat_map(|c| c.iter()) {
                    if let dreammaker::ast::Statement::Setting {
                        name: setting,
                        value,
                        ..
                    } = &stmt.elem
                    {
                        if setting.as_str() == "name" {
                            if let Some(s) = value.as_term().and_then(|t| match t {
                                dreammaker::ast::Term::String(s) => Some(s.clone()),
                                _ => None,
                            }) {
                                // (with its escapes, `\"` and the like, worked out)
                                return super::strings::parse(s.as_str())
                                    .unwrap_or_else(|_| DMString(s.as_str().as_bytes().to_vec()));
                            }
                        }
                    }
                }
            }
            if proc.declaration.is_some() {
                break;
            }
        }
        current = t.parent_type();
    }
    super::dynamic_name(name)
}

/// `foo(...)` inside a proc: a proc of `src` by name, or else a global proc.
pub(super) fn emit_bare_call(
    compiler: &mut Compiler,
    ident: &str,
    args: Vec<dreammaker::ast::Expression>,
) -> Result<Option<EvalKind>, CompileError> {
    let Some(ctx) = compiler.proc.as_ref() else {
        return Ok(None);
    };
    // (a debugger's code: its scope says whose it is)
    if compiler.expr_scope.is_some() {
        return Ok(None);
    }
    let ty = ctx.ty;
    let tree = ctx.tree;
    if !ty.is_root() && ty.get_proc(ident).is_some_and(|p| !p.ty().is_root()) {
        let count = compiler.emit_call_args(args)?;
        let var = call_var(Some(ty), ident);
        compiler.emit_ins(Instruction::Call(
            Variable::SetCache(Box::new(Variable::Src), Box::new(var)),
            count,
        ));
        return Ok(Some(EvalKind::Stack));
    }
    if tree.root().get_proc(ident).is_some() {
        let path = crate::operands::Proc(format!("/proc/{}", ident));
        let count = args.len() as u32;
        match super::args::emit(compiler, super::args::ArgsContext::Proc, args)? {
            super::args::ArgsResult::Normal => {
                compiler.emit_ins(Instruction::CallGlob(count, path))
            }
            super::args::ArgsResult::Assoc => {
                compiler.emit_ins(Instruction::NewAssocList(count));
                compiler.emit_ins(Instruction::CallGlobalArgList(path));
            }
            super::args::ArgsResult::ArgList => {
                compiler.emit_ins(Instruction::CallGlobalArgList(path))
            }
        }
        return Ok(Some(EvalKind::Stack));
    }
    Ok(None)
}

/// A var objects of `ty` have, declared on it or a parent type (but not a
/// global var, which the root type holds).
pub(super) fn member_var<'a>(
    ty: TypeRef<'a>,
    name: &str,
) -> Option<dreammaker::objtree::VarDeclarationRef<'a>> {
    ty.get_var_declaration(name).filter(|d| !d.ty().is_root())
}

/// Resolve `ty:name`, searching subtypes depth first if needed.
pub(super) fn colon_var<'a>(
    ty: TypeRef<'a>,
    name: &str,
) -> Option<dreammaker::objtree::VarDeclarationRef<'a>> {
    if let Some(decl) = member_var(ty, name) {
        return Some(decl);
    }
    ty.children().find_map(|child| colon_var(child, name))
}

/// Resolve an unknown receiver's call using the first declaration's
/// display name, matching DreamMaker.
pub(super) fn first_declaring_type<'a>(tree: &'a ObjectTree, name: &str) -> Option<TypeRef<'a>> {
    thread_local! {
        static FIRST: std::cell::RefCell<(usize, HashMap<String, String>)> =
            std::cell::RefCell::new((0, HashMap::new()));
    }
    let key = tree as *const ObjectTree as usize;
    let path = FIRST.with(|cell| {
        let mut cell = cell.borrow_mut();
        if cell.0 != key {
            let mut first: HashMap<String, (dreammaker::objtree::SymbolId, String)> =
                HashMap::new();
            for ty in tree.iter_types() {
                for (proc_name, proc) in ty.get().procs.iter() {
                    let Some(decl) = &proc.declaration else {
                        continue;
                    };
                    let entry = first
                        .entry(proc_name.to_string())
                        .or_insert((decl.id, ty.get().path.clone()));
                    if decl.id < entry.0 {
                        *entry = (decl.id, ty.get().path.clone());
                    }
                }
            }
            *cell = (key, first.into_iter().map(|(k, (_, p))| (k, p)).collect());
        }
        cell.1.get(name).cloned()
    })?;
    tree.find(&path)
}

/// T, for a var declared `/list/T` (with T a type).
pub(super) fn list_elem_type<'a>(compiler: &Compiler<'a>, path: &str) -> Option<TypeRef<'a>> {
    let rest = path.strip_prefix("/list/")?;
    compiler.proc.as_ref()?.tree.find(&format!("/{rest}"))
}

/// The type whose proc `ty:name()` calls: `ty` if it has one, or else the
/// first subtype that does (depth first, like `colon_var`).
pub(super) fn colon_proc_type<'a>(ty: TypeRef<'a>, name: &str) -> Option<TypeRef<'a>> {
    if ty.get_proc(name).is_some() {
        return Some(ty);
    }
    ty.children().find_map(|child| colon_proc_type(child, name))
}

/// Whether a variable is stored globally: static and user-defined const
/// variables, excluding per-object built-in constants such as `type`.
pub(super) fn is_global_member(decl: &dreammaker::objtree::VarDeclarationRef) -> bool {
    let flags = decl.get().var_type.flags;
    flags.is_static()
        || (flags.is_const() && decl.get().location.file != dreammaker::FileId::BUILTINS)
}

/// Follows `::name` steps from `ty` to the declaration they end at, as long
/// as each step's var is declared with a type.
pub(super) fn static_chain<'a>(
    compiler: &Compiler<'a>,
    mut ty: Option<TypeRef<'a>>,
    names: &[&str],
) -> Option<dreammaker::objtree::VarDeclarationRef<'a>> {
    let (last, path) = names.split_last()?;
    for name in path {
        let decl = member_var(ty?, name)?;
        ty = declared_type(compiler, &decl.get().var_type);
    }
    member_var(ty?, last)
}

/// The type a declaration names, if it names one in the tree.
pub(super) fn declared_type<'a>(
    compiler: &Compiler<'a>,
    var_type: &dreammaker::ast::VarType,
) -> Option<TypeRef<'a>> {
    let path = var_type.type_path.to_string();
    find_type(compiler, &path)
}

fn find_type<'a>(compiler: &Compiler<'a>, path: &str) -> Option<TypeRef<'a>> {
    if path.is_empty() || path == "/" {
        return None;
    }
    // `var/list/datum/x` is a list (of datums, which nothing checks)
    let path = if path.starts_with("/list/") {
        "/list"
    } else {
        path
    };
    compiler.proc.as_ref()?.tree.find(path)
}

/// The static type of a term, where it's a variable with a declared type.
pub(super) fn term_type<'a>(
    compiler: &Compiler<'a>,
    term: &dreammaker::ast::Term,
) -> Option<TypeRef<'a>> {
    use dreammaker::ast::Term;
    let ctx = compiler.proc.as_ref()?;
    let name = match term {
        Term::Ident(name) => name.as_str(),
        Term::GlobalIdent(name) => {
            let decl = ctx.tree.root().get_var_declaration(name.as_str())?;
            return declared_type(compiler, &decl.get().var_type);
        }
        _ => return None,
    };
    if let Some(slot) = ctx.find_local(name) {
        return find_type(compiler, &ctx.local_types[slot as usize]);
    }
    if let Some(index) = compiler.params.iter().rposition(|x| x == name) {
        return find_type(compiler, &ctx.param_types[index]);
    }
    if let Some(ty) = ctx
        .find_static(name)
        .and_then(|n| ctx.static_local_types.get(&(name.to_owned(), n)))
    {
        return find_type(compiler, ty);
    }
    match name {
        "src" => return if ctx.ty.is_root() { None } else { Some(ctx.ty) },
        "usr" => return ctx.tree.find("/mob"),
        "world" => return ctx.tree.find("/world"),
        "global" => return None,
        _ => {}
    }
    if !ctx.ty.is_root() {
        if let Some(decl) = member_var(ctx.ty, name) {
            return declared_type(compiler, &decl.get().var_type);
        }
    }
    let decl = ctx.tree.root().get_var_declaration(name)?;
    declared_type(compiler, &decl.get().var_type)
}

/// The static type of an expression made of a variable and field accesses.
pub(super) fn expr_type<'a>(
    compiler: &Compiler<'a>,
    expr: &dreammaker::ast::Expression,
) -> Option<TypeRef<'a>> {
    use dreammaker::ast::{Expression, Follow, Term};
    let Expression::Base { term, follow } = expr else {
        return None;
    };
    if let Term::Expr(inner) = &term.elem {
        if follow.is_empty() {
            return expr_type(compiler, inner);
        }
    }
    // global.x: x's declared type
    let global = matches!(&term.elem, Term::Ident(i) if i.as_str() == "global")
        && matches!(follow.first().map(|f| &f.elem), Some(Follow::Field(..)));
    let mut ty = if global {
        let decl =
            compiler
                .proc
                .as_ref()?
                .tree
                .root()
                .get_var_declaration(match &follow[0].elem {
                    Follow::Field(_, name) => name.as_str(),
                    _ => unreachable!(),
                })?;
        declared_type(compiler, &decl.get().var_type)?
    } else {
        term_type(compiler, &term.elem)?
    };
    for f in &follow[global as usize..] {
        match &f.elem {
            Follow::Field(_, name) => {
                let decl = member_var(ty, name.as_str())?;
                ty = declared_type(compiler, &decl.get().var_type)?;
            }
            _ => return None,
        }
    }
    Some(ty)
}

/// A proc reference like `.proc/foo` or `/obj/x.proc/foo`, as the path of the
/// proc declaration it names (`/obj/proc/foo`).
pub(super) fn resolve_proc_path(compiler: &Compiler, path: &str) -> Option<String> {
    let ctx = compiler.proc.as_ref()?;
    let (type_part, kind, name) = ["proc", "verb"].iter().find_map(|kind| {
        for sep in ['.', '/', ':'] {
            // (`/type/proc.name` too)
            for end in ['/', '.'] {
                let marker = format!("{}{}{}", sep, kind, end);
                if let Some(i) = path.find(&marker) {
                    return Some((&path[..i], *kind, &path[i + marker.len()..]));
                }
            }
        }
        None
    })?;
    let ty = if type_part.is_empty() {
        if path.starts_with('/') {
            ctx.tree.root()
        } else {
            ctx.ty
        }
    } else {
        ctx.tree.find(type_part)?
    };
    let mut current = Some(ty);
    while let Some(t) = current {
        if let Some(proc) = t.get().procs.get(name) {
            if proc.declaration.is_some() {
                let prefix = if t.is_root() {
                    ""
                } else {
                    t.get().path.as_str()
                };
                return Some(format!("{}/{}/{}", prefix, kind, name));
            }
        }
        current = t.parent_type();
    }
    // Global procs are reachable from any type
    if ctx.tree.root().get().procs.contains_key(name) {
        return Some(format!("/{}/{}", kind, name));
    }
    None
}

/// The declared type path of a variable expression, even one that isn't a
/// type in the tree (like `/list`).
pub(super) fn declared_type_path(
    compiler: &Compiler,
    expr: &dreammaker::ast::Expression,
) -> Option<String> {
    use dreammaker::ast::{Expression, Follow, Term};
    let Expression::Base { term, follow } = expr else {
        return None;
    };
    let ctx = compiler.proc.as_ref()?;
    match follow.last().map(|f| &f.elem) {
        None => {
            let Term::Ident(name) = &term.elem else {
                return None;
            };
            let name = name.as_str();
            if let Some(slot) = ctx.find_local(name) {
                return Some(ctx.local_types[slot as usize].clone());
            }
            if let Some(i) = compiler.params.iter().rposition(|x| x == name) {
                return Some(ctx.param_types[i].clone());
            }
            if let Some(ty) = ctx
                .find_static(name)
                .and_then(|n| ctx.static_local_types.get(&(name.to_owned(), n)))
            {
                return Some(ty.clone());
            }
            let decl = if ctx.ty.is_root() {
                None
            } else {
                member_var(ctx.ty, name)
            }
            .or_else(|| ctx.tree.root().get_var_declaration(name))?;
            Some(decl.get().var_type.type_path.to_string())
        }
        Some(Follow::Field(_, name)) => {
            let base = Expression::Base {
                term: term.clone(),
                follow: follow[..follow.len() - 1].to_vec().into_boxed_slice(),
            };
            let ty = expr_type(compiler, &base)?;
            let decl = member_var(ty, name.as_str())?;
            Some(decl.get().var_type.type_path.to_string())
        }
        _ => None,
    }
}

/// The named labels an instruction can jump to.
pub(super) fn label_targets(ins: &Instruction) -> Vec<String> {
    use crate::operands::Label;
    use Instruction::*;
    let named = |l: &Label| match l {
        Label::Named(n) => Some(n.clone()),
        Label::Offset(_) => None,
    };
    match ins {
        Jmp(l)
        | Jz(l)
        | Jnz(l)
        | JmpLoop(l)
        | JzLoop(l)
        | JnzLoop(l)
        | JmpOr(l)
        | JmpAnd(l)
        | SetCacheJmpIfNull(l)
        | SetCachePopJmpIfNull(l)
        | Spawn(l)
        | Try(l)
        | Catch(l)
        | TryJmp(l)
        | ForRange(l, _)
        | ForRangeStep(l, _) => named(l).into_iter().collect(),
        Switch(p) => p
            .cases
            .iter()
            .map(|c| &c.1)
            .chain([&p.default])
            .filter_map(named)
            .collect(),
        SwitchRange(p) => p
            .cases
            .iter()
            .map(|c| &c.1)
            .chain(p.range_cases.iter().map(|c| &c.2))
            .chain([&p.default])
            .filter_map(named)
            .collect(),
        PickSwitch(p) => p
            .cases
            .iter()
            .map(|c| &c.1)
            .chain([&p.default])
            .filter_map(named)
            .collect(),
        PickProb(p) => p.cases.iter().filter_map(named).collect(),
        _ => vec![],
    }
}

/// The value of a `const` var a bare identifier names, if it does (and isn't
/// shadowed by a local or parameter).
pub(super) fn const_value(
    compiler: &Compiler,
    name: &str,
) -> Option<dreammaker::constants::Constant> {
    const_lookup(compiler, name).map(|(value, _)| value)
}

/// The value of `global.name`, where that's a const of the code's own.
pub(super) fn global_const_value(
    compiler: &Compiler,
    name: &str,
) -> Option<dreammaker::constants::Constant> {
    let root = compiler.proc.as_ref()?.tree.root();
    let decl = root.get_var_declaration(name)?;
    if !decl.get().var_type.flags.is_const()
        || decl.get().location.file == dreammaker::FileId::BUILTINS
    {
        return None;
    }
    root.get_value(name)?.constant.clone()
}

/// Whether a bare name is a built-in constant. Emit built-ins as values;
/// user constants remain global reads outside larger constant expressions.
pub(super) fn is_builtin_const(compiler: &Compiler, name: &str) -> bool {
    const_lookup(compiler, name).is_some_and(|(_, builtin)| builtin)
}

fn const_lookup(
    compiler: &Compiler,
    name: &str,
) -> Option<(dreammaker::constants::Constant, bool)> {
    let ctx = compiler.proc.as_ref()?;
    if ctx.find_local(name).is_some() || compiler.params.iter().any(|p| p == name) {
        return None;
    }
    if let Some(c) = ctx.local_consts.get(name) {
        return Some((c.clone(), false));
    }
    if ctx.find_static(name).is_some() {
        return None;
    }
    let ty = if !ctx.ty.is_root() && member_var(ctx.ty, name).is_some() {
        ctx.ty
    } else {
        ctx.tree.root()
    };
    let decl = ty.get_var_declaration(name)?;
    if !decl.get().var_type.flags.is_const() {
        return None;
    }
    // Builtin vars like `type` are declared const but differ per object
    let builtin = decl.get().location.file == dreammaker::FileId::BUILTINS;
    if !ty.is_root() && builtin {
        return None;
    }
    Some((ty.get_value(name)?.constant.clone()?, builtin))
}

/// `/type::name` (a var's initial value, or a static var) or `/type::name()`
/// (a proc reference).
pub(super) fn emit_type_scope(
    compiler: &mut Compiler,
    path: &str,
    follow: &dreammaker::ast::Follow,
) -> Result<EvalKind, CompileError> {
    use dreammaker::ast::Follow;
    let tree = compiler.proc.as_ref().unwrap().tree;
    let ty = tree
        .find(path)
        .ok_or(CompileError::UnsupportedStatement("::"))?;
    match follow {
        // A type's own path and its parent's
        Follow::StaticField(name) if name.as_str() == "type" || name.as_str() == "parent_type" => {
            let path = if name.as_str() == "type" {
                ty.get().path.clone()
            } else {
                ty.parent_type()
                    .map(|p| p.get().path.clone())
                    .unwrap_or_default()
            };
            compiler.emit_ins(Instruction::PushVal(crate::operands::Value::Path(path)));
            Ok(EvalKind::Stack)
        }
        Follow::StaticField(name) => {
            let decl =
                member_var(ty, name.as_str()).ok_or(CompileError::UnsupportedStatement("::"))?;
            if is_global_member(&decl) {
                let scope = decl.ty().get().path.clone();
                return Ok(EvalKind::Var(Variable::Global(GlobalVar::named(
                    name.as_str().as_bytes(),
                    Some(scope),
                ))));
            }
            // The initial value: a constant, or else its expression evaluated
            // again (a list, say)
            let value = ty.get_value(name.as_str());
            // recorded where it's written, which may be another file (and the
            // next compound term records its own line again)
            if let Some(at) = value.filter(|v| v.expression.is_some()).map(|v| v.location) {
                let ctx = compiler.proc.as_ref().unwrap();
                if at.file == ctx.file_id {
                    compiler.emit_ins(Instruction::DbgLine(at.line));
                    compiler.line_pending = true;
                } else if let Some(names) = ctx.file_names {
                    let file = names(at.file);
                    compiler.emit_ins(Instruction::DbgFile(DMString(file.into_bytes())));
                    compiler.last_line = None;
                    compiler.emit_ins(Instruction::DbgLine(at.line));
                    compiler.other_file = true;
                    compiler.line_pending = true;
                }
            }
            use dreammaker::constants::Constant as C;
            match value.and_then(|v| v.constant.clone()) {
                Some(
                    constant @ (C::Null(_)
                    | C::Float(_)
                    | C::String(_)
                    | C::Resource(_)
                    | C::Prefab(_)),
                ) => compiler.emit_constant(constant)?,
                _ => match value.and_then(|v| v.expression.clone()) {
                    Some(expr) => {
                        let kind = compiler.emit_expr(expr)?;
                        compiler.emit_move_to_stack(kind)?;
                    }
                    None => compiler.emit_constant(dreammaker::constants::Constant::Null(None))?,
                },
            }
            Ok(EvalKind::Stack)
        }
        Follow::ProcReference(name) => {
            // The type's own version of the proc, or the nearest parent's: an
            // override is named without `proc/`
            let mut current = Some(ty);
            let mut found = None;
            while let Some(t) = current {
                if let Some(proc) = t.get().procs.get(name.as_str()) {
                    if proc.declaration.is_none() && !t.is_root() {
                        found = Some(format!("{}/{}", t.get().path, name));
                    }
                    break;
                }
                current = t.parent_type();
            }
            let proc_path = found
                .or_else(|| resolve_proc_path(compiler, &format!("{}.proc/{}", path, name)))
                .ok_or(CompileError::UnsupportedStatement("::"))?;
            compiler.emit_ins(Instruction::PushVal(crate::operands::Value::Path(
                proc_path,
            )));
            Ok(EvalKind::Stack)
        }
        _ => unreachable!(),
    }
}

/// Whether an instruction reads or changes the cache register.
pub(super) fn touches_cache(ins: &Instruction) -> bool {
    use Instruction::*;
    if matches!(
        ins,
        PushCache
            | PopCache
            | PushCacheKey
            | PopCacheKey
            | SetCacheJmpIfNull(_)
            | SetCachePopJmpIfNull(_)
    ) {
        return true;
    }
    fn uses(var: &Variable) -> bool {
        match var {
            Variable::Null
            | Variable::World
            | Variable::Usr
            | Variable::Src
            | Variable::Args
            | Variable::Dot
            | Variable::Caller
            | Variable::Callee
            | Variable::Arg(_)
            | Variable::Local(_)
            | Variable::Global(_)
            | Variable::StaticProc(_)
            | Variable::StaticVerb(_) => false,
            Variable::Initial(v)
            | Variable::IsSaved(v)
            | Variable::PtrRef(v)
            | Variable::PtrDeref(v) => uses(v),
            _ => true,
        }
    }
    let mut ins = ins.clone();
    var_operand(&mut ins).is_some_and(|v| uses(v))
}

/// The variable operand of an instruction, if it has one.
pub(super) fn var_operand(ins: &mut Instruction) -> Option<&mut Variable> {
    use Instruction::*;
    match ins {
        GetVar(v)
        | SetVar(v)
        | SetVarExpr(v)
        | AugAdd(v)
        | AugSub(v)
        | AugMul(v)
        | AugDiv(v)
        | AugMod(v)
        | AugBand(v)
        | AugBor(v)
        | AugXor(v)
        | AugLShift(v)
        | AugRShift(v)
        | AugFloatMod(v)
        | AssignInto(v)
        | PreInc(v)
        | PostInc(v)
        | PreDec(v)
        | PostDec(v)
        | Inc(v)
        | Dec(v)
        | Call(v, _)
        | CallStatement(v, _)
        | KeyValueIter(v) => Some(v),
        ForRange(_, v) | ForRangeStep(_, v) => Some(v),
        _ => None,
    }
}

/// Remove cache-chain prefixes already loaded and update cache state.
pub(super) fn track_cache(
    cache: &mut Option<Vec<Variable>>,
    ins: &mut Instruction,
    explicit_src: &mut bool,
) {
    track_cache_inner(cache, ins, explicit_src);
    // (`src.` applies to the chain right after it: `src?.x` has none)
    *explicit_src = false;
    // Invalidate field chains after calls outside the cache. Calls through
    // `a.b.f()` retain the old receiver even if f changes the field chain;
    // existing code depends on this DreamMaker behavior.
    use Instruction::*;
    let call = matches!(
        ins,
        CallGlob(..)
            | CallGlobalArgList(_)
            | CallPath(_)
            | CallName(_)
            | CallNameArgList
            | CallPathArgList
            | CallSelf
            | CallSelfArgs(_)
            | CallSelfArgList
            | Del
            | Sleep
    ) || matches!(ins, Call(v, _) | CallStatement(v, _) if !uses_cache(v))
        // (and after `a.b.c++` or `a.b.c += v` as a value)
        || matches!(ins, PreInc(_) | PostInc(_) | PreDec(_) | PostDec(_) | PushEval);
    if call && cache.as_ref().is_some_and(|held| held.len() > 1) {
        *cache = None;
    }
}

/// Whether a variable operand reads the cache (a chain from it, or one that
/// loads it).
pub(super) fn uses_cache(var: &Variable) -> bool {
    match var {
        Variable::Cache | Variable::CacheKey | Variable::CacheIndex => true,
        Variable::SetCache(..) | Variable::Field(_) => true,
        Variable::DynamicProc(_)
        | Variable::DynamicVerb(_)
        | Variable::StaticProc(_)
        | Variable::StaticVerb(_) => true,
        _ => false,
    }
}

fn track_cache_inner(
    cache: &mut Option<Vec<Variable>>,
    ins: &mut Instruction,
    explicit_src: &mut bool,
) {
    match ins {
        Instruction::SetVar(Variable::Cache)
        | Instruction::PopCache
        | Instruction::SetCacheJmpIfNull(_)
        | Instruction::SetCachePopJmpIfNull(_) => {
            *cache = None;
            return;
        }
        _ => {}
    }

    let writes = !matches!(
        ins,
        Instruction::GetVar(_) | Instruction::Call(..) | Instruction::CallStatement(..)
    );
    let Some(var) = var_operand(ins) else {
        return;
    };

    // Writing to what the cache was loaded from leaves it stale
    if writes
        && cache
            .as_ref()
            .is_some_and(|held| held.first() == Some(&*var))
    {
        *cache = None;
    }

    let mut bases: Vec<Variable> = vec![];
    let mut cursor = &*var;
    while let Variable::SetCache(base, rest) = cursor {
        bases.push((**base).clone());
        cursor = rest;
    }

    if bases.is_empty() {
        return;
    }

    // Explicit `src.x` and bare `x` access the same value, but DreamMaker
    // does not reuse their cache states interchangeably.
    if bases[0] == Variable::Src && *explicit_src {
        bases[0] = Variable::PtrRef(Box::new(Variable::Src));
        *explicit_src = false;
    }
    if let Some(held) = cache.as_ref() {
        if *held == bases {
            for _ in 0..held.len() {
                let Variable::SetCache(_, rest) = std::mem::replace(var, Variable::Null) else {
                    unreachable!()
                };
                *var = *rest;
            }
        }
    }

    *cache = Some(bases);
}
