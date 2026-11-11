use dreammaker::ast::{Follow, ListAccessKind, PropertyAccessKind, UnaryOp};

use crate::{
    compiler::{
        args, chain_builder::ChainBuilder, operands, unary, CompileError, Compiler, EvalKind,
    },
    operands::{DMString, GlobalVar, Label, Variable},
    Instruction,
};

pub(super) fn emit(
    compiler: &mut Compiler,
    follow: Vec<Follow>,
    kind: EvalKind,
) -> Result<EvalKind, CompileError> {
    emit_typed(compiler, follow, kind, None)
}

/// Resolve receiver calls. Colon access also searches subtypes; unknown
/// receiver types use the first matching declaration's display name.
fn callee_var<'a>(
    compiler: &Compiler<'a>,
    ty: Option<dreammaker::objtree::TypeRef<'a>>,
    access: PropertyAccessKind,
    name: &str,
) -> Variable {
    let ty = match ty {
        Some(t) if access == PropertyAccessKind::Colon => {
            Some(super::procs::colon_proc_type(t, name).unwrap_or(t))
        }
        Some(t) => Some(t),
        None => compiler
            .proc
            .as_ref()
            .and_then(|ctx| super::procs::first_declaring_type(ctx.tree, name)),
    };
    super::procs::call_var(ty, name)
}

/// `ty` is the static type of the value being followed, when known.
pub(super) fn emit_typed<'a>(
    compiler: &mut Compiler<'a>,
    follow: Vec<Follow>,
    kind: EvalKind,
    ty: Option<dreammaker::objtree::TypeRef<'a>>,
) -> Result<EvalKind, CompileError> {
    let mut kind = kind;
    let mut ty = ty;
    // what indexing the value gives, for a `/list/T` var
    let mut elem_ty = compiler.list_elem_type.take();

    // sequential field accessors (example: a.b.c) get buffered into a single operation
    // TODO: Move this state and the commit function into a struct!
    let mut field_buffer = vec![];

    let mut follow = follow.into_iter();
    while let Some(sub_expr) = follow.next() {
        let more_fields = matches!(
            follow.as_slice().first(),
            Some(
                Follow::Field(PropertyAccessKind::Dot | PropertyAccessKind::Colon, _)
                    | Follow::Call(PropertyAccessKind::Dot | PropertyAccessKind::Colon, _, _)
            )
        );
        if compiler.proc.is_some() && is_safe(&sub_expr) {
            kind = commit_field_buffer(compiler, kind, &mut field_buffer)?;
            compiler.emit_move_to_stack(kind)?;
            return emit_safe(compiler, sub_expr, follow.collect(), ty);
        }
        let global_field = compiler.global_field;
        if matches!(
            sub_expr,
            Follow::Field(..) | Follow::Call(..) | Follow::StaticField(_)
        ) && !(matches!(kind, EvalKind::Global) && matches!(sub_expr, Follow::Field(..)))
        {
            compiler.global_field = true;
        }
        if !matches!(sub_expr, Follow::Field(..) | Follow::Index(..)) {
            elem_ty = None;
        }
        match sub_expr {
            Follow::Field(access_kind, ident) => {
                // A static var is a global: DreamMaker reads it directly and
                // doesn't evaluate what it was reached through
                let field_ty = ty.and_then(|t| match access_kind {
                    PropertyAccessKind::Colon => super::procs::colon_var(t, ident.as_str()),
                    _ => super::procs::member_var(t, ident.as_str()),
                });
                ty =
                    field_ty.and_then(|d| super::procs::declared_type(compiler, &d.get().var_type));
                elem_ty = field_ty.and_then(|d| {
                    super::procs::list_elem_type(compiler, &d.get().var_type.type_path.to_string())
                });
                // global.x: the global's declared type
                if matches!(kind, EvalKind::Global) && field_buffer.is_empty() && field_ty.is_none()
                {
                    ty = compiler
                        .proc
                        .as_ref()
                        .and_then(|p| p.tree.root().get_var_declaration(ident.as_str()))
                        .and_then(|d| super::procs::declared_type(compiler, &d.get().var_type));
                }
                if let Some(decl) = field_ty {
                    let is_static = super::procs::is_global_member(&decl)
                        && matches!(
                            access_kind,
                            PropertyAccessKind::Dot | PropertyAccessKind::Colon
                        );
                    // Further along a chain it's read through the chain
                    let scope = decl.ty().get().path.clone();
                    if is_static && more_fields {
                        let mut name = super::chain_builder::STATIC_MARKER.to_vec();
                        name.extend_from_slice(ident.as_str().as_bytes());
                        name.push(0);
                        name.extend_from_slice(scope.as_bytes());
                        field_buffer.push(String::from_utf8(name).unwrap());
                        continue;
                    }
                    if is_static
                        && matches!(
                            kind,
                            EvalKind::Var(_) | EvalKind::Field(..) | EvalKind::Global
                        )
                    {
                        // (which isn't a field read to `||=`)
                        compiler.global_field = global_field;
                        field_buffer.clear();
                        kind = EvalKind::Var(Variable::Global(GlobalVar::named(
                            ident.as_str().as_bytes(),
                            Some(scope),
                        )));
                        continue;
                    }
                    // (a value, like a list element, is still evaluated into the cache)
                    if is_static && matches!(kind, EvalKind::Stack | EvalKind::ListRef) {
                        compiler.emit_move_to_stack(kind)?;
                        compiler.emit_ins(Instruction::SetVar(Variable::Cache));
                        kind = EvalKind::Var(Variable::Global(GlobalVar::named(
                            ident.as_str().as_bytes(),
                            Some(scope),
                        )));
                        continue;
                    }
                }
                // We currently treat `.` and `:` as the same
                // TODO: Should we type check?
                match access_kind {
                    PropertyAccessKind::Dot
                    | PropertyAccessKind::Colon
                    | PropertyAccessKind::Scope => {
                        field_buffer.push(ident.into_owned());
                    }

                    PropertyAccessKind::SafeDot | PropertyAccessKind::SafeColon => {
                        kind = commit_field_buffer(compiler, kind, &mut field_buffer)?;
                        compiler.emit_move_to_stack(kind)?;

                        let short_circuit = compiler.short_circuit();
                        compiler
                            .emit_ins(Instruction::SetCacheJmpIfNull(Label::Named(short_circuit)));

                        kind = EvalKind::Field(
                            ChainBuilder::begin(Variable::Cache),
                            ident.into_owned(),
                        );
                    }
                }
            }

            Follow::Index(access_kind, expr) => {
                ty = elem_ty.take();
                kind = commit_field_buffer(compiler, kind, &mut field_buffer)?;

                match access_kind {
                    ListAccessKind::Normal => {
                        // Move base to the stack
                        compiler.emit_move_to_stack(kind)?;

                        // Move inner expression to stack
                        let expr = compiler.emit_expr(*expr)?;
                        compiler.emit_move_to_stack(expr)?;

                        kind = EvalKind::ListRef;
                    }

                    ListAccessKind::Safe => {
                        // Move base to the stack
                        compiler.emit_move_to_stack(kind)?;

                        // Short-circuit if base is null
                        // TODO: Can we do this without using cache?
                        let short_circuit = compiler.short_circuit();
                        compiler
                            .emit_ins(Instruction::SetCacheJmpIfNull(Label::Named(short_circuit)));
                        compiler.emit_ins(Instruction::GetVar(Variable::Cache));

                        // Move inner expression to stack
                        let expr = compiler.emit_expr(*expr)?;
                        compiler.emit_move_to_stack(expr)?;

                        kind = EvalKind::ListRef;
                    }
                }
            }

            Follow::Call(index_kind, ident, args) => {
                let receiver_ty = ty.take();
                let arg_count = args.len() as u32;

                match index_kind {
                    // Global call syntax `global.f()`
                    PropertyAccessKind::Dot
                    | PropertyAccessKind::Colon
                    | PropertyAccessKind::Scope
                        if matches!(kind, EvalKind::Global) && field_buffer.is_empty() =>
                    {
                        match args::emit(compiler, args::ArgsContext::Proc, args.into_vec())? {
                            args::ArgsResult::Normal => {
                                // We're treating all Term::Call expressions as global calls
                                compiler.emit_ins(Instruction::CallGlob(
                                    arg_count,
                                    operands::Proc(format!("/proc/{}", ident)),
                                ));
                            }

                            args::ArgsResult::Assoc => {
                                compiler.emit_ins(Instruction::NewAssocList(arg_count));
                                compiler.emit_ins(Instruction::CallGlobalArgList(operands::Proc(
                                    format!("/proc/{}", ident),
                                )));
                            }

                            args::ArgsResult::ArgList => {
                                compiler.emit_ins(Instruction::CallGlobalArgList(operands::Proc(
                                    format!("/proc/{}", ident),
                                )));
                            }
                        }
                    }

                    // Debugger expressions lack static types. Dynamic call-by-name
                    // lookup resolves either a proc or a verb.
                    PropertyAccessKind::Dot | PropertyAccessKind::Colon
                        if compiler.proc.is_none() =>
                    {
                        kind = commit_field_buffer(compiler, kind, &mut field_buffer)?;
                        compiler.emit_move_to_stack(kind)?;
                        compiler.emit_ins(Instruction::PushVal(crate::Value::DMString(DMString(
                            ident.as_bytes().to_vec(),
                        ))));
                        match args::emit(compiler, args::ArgsContext::List, args.into_vec())? {
                            args::ArgsResult::Normal => {
                                compiler.emit_ins(Instruction::CallName(arg_count))
                            }
                            args::ArgsResult::Assoc => {
                                compiler.emit_ins(Instruction::NewAssocList(arg_count));
                                compiler.emit_ins(Instruction::CallNameArgList);
                            }
                            args::ArgsResult::ArgList => {
                                compiler.emit_ins(Instruction::CallNameArgList)
                            }
                        }
                    }

                    // We just treat these as the same
                    // TODO: Should we type check?
                    access @ (PropertyAccessKind::Dot
                    | PropertyAccessKind::Colon
                    | PropertyAccessKind::Scope)
                        if compiler.proc.is_some()
                            && (!field_buffer.is_empty()
                                || matches!(kind, EvalKind::Var(_) | EvalKind::Field(..))) =>
                    {
                        let callee = callee_var(compiler, receiver_ty, access, &ident);
                        // Emit a variable-chain receiver in the call operand after its
                        // arguments. Save a computed cache receiver across argument evaluation.
                        let parked = matches!(kind, EvalKind::Stack | EvalKind::ListRef);
                        kind = commit_field_buffer(compiler, kind, &mut field_buffer)?;
                        let builder = match kind {
                            EvalKind::Var(var) => ChainBuilder::begin(var),
                            EvalKind::Field(mut builder, field) => {
                                builder.append(DMString(field.into()));
                                builder
                            }
                            _ => unreachable!(),
                        };
                        if parked {
                            compiler.emit_ins(Instruction::PushCache);
                        }
                        // (a `src.` the receiver was written with is its own,
                        // not the arguments')
                        let explicit_src = std::mem::take(&mut compiler.explicit_src);
                        let count = compiler.emit_call_args(args.into_vec())?;
                        compiler.explicit_src = explicit_src;
                        if parked {
                            compiler.emit_ins(Instruction::PopCache);
                        }
                        compiler.emit_ins(Instruction::Call(builder.get_leaf(callee), count));
                    }

                    access @ (PropertyAccessKind::Dot
                    | PropertyAccessKind::Colon
                    | PropertyAccessKind::Scope) => {
                        let callee = callee_var(compiler, receiver_ty, access, &ident);
                        // TODO: Can emit much cleaner code when no params
                        kind = commit_field_buffer(compiler, kind, &mut field_buffer)?;
                        compiler.emit_move_to_stack(kind)?;

                        // We'll need our src after pushing the parameters
                        compiler.emit_ins(Instruction::SetVar(Variable::Cache));
                        compiler.emit_ins(Instruction::PushCache);

                        match args::emit(compiler, args::ArgsContext::Proc, args.into_vec())? {
                            args::ArgsResult::Normal => {
                                compiler.emit_ins(Instruction::PopCache);

                                compiler.emit_ins(Instruction::Call(callee.clone(), arg_count));
                            }

                            args::ArgsResult::Assoc => {
                                compiler.emit_ins(Instruction::NewAssocList(arg_count));

                                compiler.emit_ins(Instruction::PopCache);

                                compiler.emit_ins(Instruction::Call(
                                    callee.clone(),
                                    65535, // TODO: remove hardcoded value
                                ));
                            }

                            args::ArgsResult::ArgList => {
                                compiler.emit_ins(Instruction::PopCache);

                                compiler.emit_ins(Instruction::Call(
                                    callee.clone(),
                                    65535, // TODO: remove hardcoded value
                                ));
                            }
                        }
                    }

                    PropertyAccessKind::SafeDot | PropertyAccessKind::SafeColon => {
                        // TODO: Can emit much cleaner code when no params
                        kind = commit_field_buffer(compiler, kind, &mut field_buffer)?;
                        compiler.emit_move_to_stack(kind)?;

                        let short_circuit = compiler.short_circuit();
                        compiler
                            .emit_ins(Instruction::SetCacheJmpIfNull(Label::Named(short_circuit)));

                        // We'll need our src after pushing the parameters
                        compiler.emit_ins(Instruction::PushCache);

                        match args::emit(compiler, args::ArgsContext::Proc, args.into_vec())? {
                            args::ArgsResult::Normal => {
                                compiler.emit_ins(Instruction::PopCache);

                                compiler.emit_ins(Instruction::Call(
                                    Variable::DynamicProc(super::dynamic_name(&ident)),
                                    arg_count,
                                ));
                            }

                            args::ArgsResult::Assoc => {
                                compiler.emit_ins(Instruction::NewAssocList(arg_count));

                                compiler.emit_ins(Instruction::PopCache);

                                compiler.emit_ins(Instruction::Call(
                                    Variable::DynamicProc(super::dynamic_name(&ident)),
                                    65535, // TODO: remove hardcoded value
                                ));
                            }

                            args::ArgsResult::ArgList => {
                                compiler.emit_ins(Instruction::PopCache);

                                compiler.emit_ins(Instruction::Call(
                                    Variable::DynamicProc(super::dynamic_name(&ident)),
                                    65535, // TODO: remove hardcoded value
                                ));
                            }
                        }
                    }
                }

                kind = EvalKind::Stack;
            }

            Follow::Unary(op) => {
                ty = None;
                kind = commit_field_buffer(compiler, kind, &mut field_buffer)?;
                kind = unary::emit(compiler, vec![op], kind)?;
            }

            // x::name: a static var directly, or else the var's initial value
            Follow::StaticField(ident) if compiler.proc.is_some() => {
                let decl = ty.and_then(|t| super::procs::member_var(t, ident.as_str()));
                let static_scope = decl
                    .filter(super::procs::is_global_member)
                    .map(|d| d.ty().get().path.clone());
                let is_static = static_scope.is_some();
                // In x::a::b, where b is static, nothing before it is evaluated
                let mut names = vec![ident.as_str()];
                for f in follow.as_slice() {
                    match f {
                        Follow::StaticField(name) => names.push(name.as_str()),
                        _ => break,
                    }
                }
                let leads_to_static = !is_static
                    && (2..=names.len()).any(|n| {
                        super::procs::static_chain(compiler, ty, &names[..n])
                            .is_some_and(|d| super::procs::is_global_member(&d))
                    });
                if leads_to_static {
                    ty =
                        decl.and_then(|d| super::procs::declared_type(compiler, &d.get().var_type));
                    continue;
                }
                kind = commit_field_buffer(compiler, kind, &mut field_buffer)?;
                if is_static {
                    if kind == EvalKind::Stack {
                        compiler.emit_ins(Instruction::Pop);
                    }
                    kind = EvalKind::Var(Variable::Global(GlobalVar::named(
                        ident.as_str().as_bytes(),
                        static_scope,
                    )));
                } else {
                    compiler.emit_move_to_stack(kind)?;
                    compiler.emit_ins(Instruction::SetVar(Variable::Cache));
                    compiler.emit_ins(Instruction::GetVar(Variable::Initial(Box::new(
                        Variable::Field(DMString(ident.as_str().into())),
                    ))));
                    kind = EvalKind::Stack;
                }
                ty = decl.and_then(|d| super::procs::declared_type(compiler, &d.get().var_type));
            }

            // Without static types, resolve `x::name` at runtime through
            // `initial(x.name)`, which also accepts typepaths.
            Follow::StaticField(ident) => {
                kind = commit_field_buffer(compiler, kind, &mut field_buffer)?;
                compiler.emit_move_to_stack(kind)?;
                compiler.emit_ins(Instruction::SetVar(Variable::Cache));
                compiler.emit_ins(Instruction::GetVar(Variable::Initial(Box::new(
                    Variable::Field(DMString(ident.as_str().into())),
                ))));
                kind = EvalKind::Stack;
                ty = None;
            }

            // which proc that is takes the types
            Follow::ProcReference(_) => {
                return Err(CompileError::UnsupportedStatement(
                    "`::` naming a proc, without the code's types",
                ))
            }
        }
    }

    kind = commit_field_buffer(compiler, kind, &mut field_buffer)?;
    Ok(kind)
}

fn commit_field_buffer(
    compiler: &mut Compiler,
    kind: EvalKind,
    field_chain: &mut Vec<String>,
) -> Result<EvalKind, CompileError> {
    if field_chain.is_empty() {
        return Ok(kind);
    }

    // We need a ChainBuilder
    // TODO: Lots of repeated code from emit_move_to_chain_builder. We should call that for the non-specialized ones.
    let mut builder = match kind {
        EvalKind::Stack => {
            compiler.emit_ins(Instruction::SetVar(Variable::Cache));
            ChainBuilder::begin(Variable::Cache)
        }

        EvalKind::ListRef => {
            compiler.emit_ins(Instruction::ListGet);
            compiler.emit_ins(Instruction::SetVar(Variable::Cache));
            ChainBuilder::begin(Variable::Cache)
        }

        EvalKind::Range => return Err(CompileError::UnexpectedRange),
        EvalKind::ArgList => return Err(CompileError::UnexpectedArgList),

        // Bit hacky.
        EvalKind::Global => {
            let name = field_chain.remove(0);
            // global.x. A debugger's expression reads it by name (see
            // `ExprScope::globals`).
            if compiler.expr_scope.is_some() {
                let kind = compiler.emit_global_by_name(&name)?;
                return commit_field_buffer(compiler, kind, field_chain);
            }
            let var = Variable::Global(GlobalVar::named(name.as_bytes(), Some(String::new())));
            return commit_field_buffer(compiler, EvalKind::Var(var), field_chain);
        }

        EvalKind::Field(mut builder, field) => {
            builder.append(DMString(field.into()));
            builder
        }

        EvalKind::Var(var) => ChainBuilder::begin(var),
    };

    let last_field = field_chain.pop().unwrap();

    for field in field_chain.iter() {
        builder.append(DMString(field.clone().into()));
    }

    field_chain.clear();
    Ok(EvalKind::Field(builder, last_field))
}

fn is_safe(follow: &Follow) -> bool {
    matches!(
        follow,
        Follow::Field(
            PropertyAccessKind::SafeDot | PropertyAccessKind::SafeColon,
            _
        ) | Follow::Call(
            PropertyAccessKind::SafeDot | PropertyAccessKind::SafeColon,
            _,
            _
        ) | Follow::Index(ListAccessKind::Safe, _)
    )
}

/// Compile safe access with x on the stack, skipping on null. Save the
/// cache for nontrivial chains and restore it before calls or at the end.
/// `ty` is the receiver's static type, if known.
fn emit_safe<'a>(
    compiler: &mut Compiler<'a>,
    first: Follow,
    mut rest: Vec<Follow>,
    ty: Option<dreammaker::objtree::TypeRef<'a>>,
) -> Result<EvalKind, CompileError> {
    // Unary operators apply to the whole chain's result, after it
    let mut unary = vec![];
    while let Some(Follow::Unary(_)) = rest.last() {
        unary.insert(0, rest.pop().unwrap());
    }
    // `a?.b++` changes a.b when a isn't null
    if let [Follow::Unary(
        op @ (UnaryOp::PreIncr | UnaryOp::PostIncr | UnaryOp::PreDecr | UnaryOp::PostDecr),
    )] = unary.as_slice()
    {
        let op = *op;
        let mut follows = vec![first.clone()];
        follows.extend(rest.iter().cloned());
        let step = move |var: Variable| {
            vec![match op {
                UnaryOp::PreIncr => Instruction::PreInc(var),
                UnaryOp::PostIncr => Instruction::PostInc(var),
                UnaryOp::PreDecr => Instruction::PreDec(var),
                _ => Instruction::PostDec(var),
            }]
        };
        let write = super::statements::SafeWrite::Change(&step);
        if let Some(chain) = super::statements::safe_chain(&follows) {
            if chain.writable(&write, true) {
                let indexed = matches!(follows.last(), Some(Follow::Index(..)));
                let static_var = super::statements::static_member(compiler, ty, &follows, indexed);
                super::statements::emit_safe_chain_write(
                    compiler, chain, None, static_var, None, write, true,
                )?;
                return Ok(EvalKind::Stack);
            }
        }
    }
    let kind = emit_safe_inner(compiler, first, rest, ty)?;
    if unary.is_empty() {
        return Ok(kind);
    }
    emit(compiler, unary, kind)
}

fn emit_safe_inner<'a>(
    compiler: &mut Compiler<'a>,
    first: Follow,
    rest: Vec<Follow>,
    ty: Option<dreammaker::objtree::TypeRef<'a>>,
) -> Result<EvalKind, CompileError> {
    let end = compiler.new_label();
    compiler.emit_ins(Instruction::SetCacheJmpIfNull(Label::Named(end.clone())));

    // The access the `?` applies to, done against the cache
    let mut follows = vec![match first {
        Follow::Field(_, ident) => Follow::Field(PropertyAccessKind::Dot, ident),
        Follow::Call(_, ident, args) => Follow::Call(PropertyAccessKind::Dot, ident, args),
        Follow::Index(_, expr) => Follow::Index(ListAccessKind::Normal, expr),
        other => other,
    }];
    follows.extend(rest);

    let plain = follows.iter().all(|f| {
        matches!(
            f,
            Follow::Field(PropertyAccessKind::Dot | PropertyAccessKind::Colon, _)
        )
    });
    if plain {
        let kind = emit_typed(compiler, follows, EvalKind::Var(Variable::Cache), ty)?;
        compiler.emit_move_to_stack(kind)?;
        compiler.emit_label(end);
        return Ok(EvalKind::Stack);
    }

    compiler.emit_ins(Instruction::PushCache);
    let mut popped = false;
    let mut called = false;
    let mut kind = EvalKind::Var(Variable::Cache);
    let mut field_buffer: Vec<String> = vec![];
    let mut ty = ty;
    let mut it = follows.into_iter();
    while let Some(f) = it.next() {
        if is_safe(&f) {
            kind = commit_field_buffer(compiler, kind, &mut field_buffer)?;
            compiler.emit_move_to_stack(kind)?;
            kind = emit_safe(compiler, f, it.by_ref().collect(), ty)?;
            break;
        }
        let receiver_ty = ty;
        ty = None;
        match f {
            Follow::Field(_, ident) => {
                let decl = receiver_ty.and_then(|t| super::procs::member_var(t, ident.as_str()));
                ty = decl.and_then(|d| super::procs::declared_type(compiler, &d.get().var_type));
                match decl {
                    // A static var is read directly
                    Some(decl) if super::procs::is_global_member(&decl) => {
                        field_buffer.clear();
                        let scope = decl.ty().get().path.clone();
                        kind = EvalKind::Var(Variable::Global(GlobalVar::named(
                            ident.as_str().as_bytes(),
                            Some(scope),
                        )));
                    }
                    _ => field_buffer.push(ident.into_owned()),
                }
            }
            Follow::Call(_, ident, args) if !called => {
                called = true;
                kind = commit_field_buffer(compiler, kind, &mut field_buffer)?;
                let builder = match kind {
                    EvalKind::Var(var) => ChainBuilder::begin(var),
                    EvalKind::Field(mut builder, field) => {
                        builder.append(DMString(field.into()));
                        builder
                    }
                    other => {
                        compiler.emit_move_to_stack(other)?;
                        compiler.emit_ins(Instruction::SetVar(Variable::Cache));
                        ChainBuilder::begin(Variable::Cache)
                    }
                };
                // The cache is restored before the call, unless the arguments
                // don't change it and the chain goes on after it
                let start = compiler.nodes.len();
                let count = compiler.emit_call_args(args.into_vec())?;
                let touched = compiler.nodes[start..].iter().any(|n| {
                    matches!(n, crate::Node::Instruction(i, _) if super::procs::touches_cache(i))
                });
                let pop = touched || it.as_slice().is_empty();
                if pop {
                    compiler.emit_ins(Instruction::PopCache);
                    popped = true;
                }
                let var = super::procs::call_var(receiver_ty, &ident);
                compiler.emit_ins(Instruction::Call(builder.get_leaf(var), count));
                kind = EvalKind::Stack;
            }
            other => {
                kind = commit_field_buffer(compiler, kind, &mut field_buffer)?;
                kind = emit(compiler, vec![other], kind)?;
            }
        }
    }
    kind = commit_field_buffer(compiler, kind, &mut field_buffer)?;
    compiler.emit_move_to_stack(kind)?;
    if !popped {
        compiler.emit_ins(Instruction::PopCache);
    }
    compiler.emit_label(end);
    Ok(EvalKind::Stack)
}
