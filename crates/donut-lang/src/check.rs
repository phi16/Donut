use crate::types::common::*;
use crate::types::item::*;
use crate::types::semtree;
use crate::types::token::Token;
use std::rc::Rc;

// --- semtree → item conversion ---

fn convert_a_val(a: &A<semtree::Val>) -> Option<S<Val>> {
    let (v, span) = a.accepted()?;
    let val = convert_val(v)?;
    Some(S(val, span.clone()))
}

fn convert_val(val: &semtree::Val) -> Option<Val> {
    match val {
        semtree::Val::Path(path_a) => {
            let path = convert_path(path_a)?;
            Some(Val::Path(path))
        }
        semtree::Val::Lit(lit_a) => {
            let lit = convert_lit(lit_a)?;
            Some(Val::Lit(lit))
        }
        semtree::Val::Op(l, op_a, _params, r) => {
            let op = op_a.inner()?;
            match op {
                semtree::Op::Comp(axis) => {
                    let mut children = Vec::new();
                    collect_comp_flat(l, *axis, &mut children);
                    collect_comp_flat(r, *axis, &mut children);
                    Some(Val::Comp(*axis, children))
                }
                semtree::Op::CompStar => {
                    let mut children = Vec::new();
                    collect_comp_star_flat(l, &mut children);
                    collect_comp_star_flat(r, &mut children);
                    Some(Val::CompStar(children))
                }
                semtree::Op::Arrow(arrow_ty) => {
                    let kind = match arrow_ty {
                        semtree::ArrowTy::To => ArrowKind::To,
                        semtree::ArrowTy::Eq => ArrowKind::Eq,
                        semtree::ArrowTy::Functor => ArrowKind::Functor,
                    };
                    let l_s = convert_val_with_span(l)?;
                    let r_s = convert_val_with_span(r)?;
                    Some(Val::Arrow(kind, Box::new(l_s), Box::new(r_s)))
                }
            }
        }
    }
}

fn convert_val_with_span(val: &semtree::Val) -> Option<S<Val>> {
    let span = val_span(val)?;
    let v = convert_val(val)?;
    Some(S(v, span))
}

fn val_span(val: &semtree::Val) -> Option<TokenSpan> {
    match val {
        semtree::Val::Path(path_a) => {
            let (_, span) = path_a.accepted()?;
            Some(span.clone())
        }
        semtree::Val::Lit(lit_a) => {
            let (_, span) = lit_a.accepted()?;
            Some(span.clone())
        }
        semtree::Val::Op(l, _, _, r) => {
            let l_span = val_span(l)?;
            let r_span = val_span(r)?;
            Some(TokenSpan {
                start: l_span.start,
                end: r_span.end,
            })
        }
    }
}

fn convert_path(path_a: &A<semtree::Path>) -> Option<Path> {
    let (path, _) = path_a.accepted()?;
    let segments: Vec<S<Segment>> = path
        .0
        .iter()
        .filter_map(|seg_a| convert_segment(seg_a))
        .collect();
    let applicand = match &path.1 {
        Some(v) => Some(Box::new(convert_a_val(v)?)),
        None => None,
    };
    Some(Path {
        segments,
        applicand,
    })
}

fn convert_segment(seg_a: &A<semtree::Segment>) -> Option<S<Segment>> {
    let (seg, span) = seg_a.accepted()?;
    let (name, _) = seg.0.accepted()?;
    let params: Vec<ParamVal> = seg
        .1
         .0
        .iter()
        .filter_map(|pv_a| convert_param_val(pv_a))
        .collect();
    Some(S(
        Segment {
            name: name.0.clone(),
            params,
        },
        span.clone(),
    ))
}

fn convert_param_val(pv_a: &A<semtree::ParamVal>) -> Option<ParamVal> {
    let (pv, _) = pv_a.accepted()?;
    let name = pv
        .name
        .as_ref()
        .and_then(|n| n.accepted())
        .map(|(n, _)| n.0.clone());
    let val = convert_a_val(&pv.val)?;
    Some(ParamVal { name, val })
}

fn convert_lit(lit_a: &A<semtree::Lit>) -> Option<Lit> {
    let (lit, _) = lit_a.accepted()?;
    match lit {
        semtree::Lit::Number(s) => Some(Lit::Number(s.clone())),
        semtree::Lit::String(s) => Some(Lit::String(s.clone())),
        semtree::Lit::Array(vs) => {
            let items: Vec<S<Val>> = vs.iter().filter_map(|v| convert_a_val(v)).collect();
            Some(Lit::Array(items))
        }
        semtree::Lit::Object(kvs) => {
            let items: Vec<(String, S<Val>)> = kvs
                .iter()
                .filter_map(|(k, v)| {
                    let key = match k {
                        A::Accepted(semtree::Key::Name(n), _) => n.inner()?.0.clone(),
                        A::Accepted(semtree::Key::String(s), _) => s.inner()?.clone(),
                        _ => return None,
                    };
                    let val = convert_a_val(v)?;
                    Some((key, val))
                })
                .collect();
            Some(Lit::Object(items))
        }
    }
}

fn collect_comp_flat(val: &semtree::Val, axis: u32, out: &mut Vec<S<Val>>) {
    if let semtree::Val::Op(l, op_a, _, r) = val {
        if matches!(op_a.inner(), Some(semtree::Op::Comp(n)) if *n == axis) {
            collect_comp_flat(l, axis, out);
            collect_comp_flat(r, axis, out);
            return;
        }
    }
    if let Some(s) = convert_val_with_span(val) {
        out.push(s);
    }
}

fn collect_comp_star_flat(val: &semtree::Val, out: &mut Vec<S<Val>>) {
    if let semtree::Val::Op(l, op_a, _, r) = val {
        if matches!(op_a.inner(), Some(semtree::Op::CompStar)) {
            collect_comp_star_flat(l, out);
            collect_comp_star_flat(r, out);
            return;
        }
    }
    if let Some(s) = convert_val_with_span(val) {
        out.push(s);
    }
}

/// Convert semtree ParamDecl ty (Rc<A<Val>>) to item::S<Val>
fn convert_param_ty(ty: &Rc<A<semtree::Val>>) -> Option<S<Val>> {
    convert_a_val(ty)
}

// --- Checker internals (using item types for scope) ---

fn is_functor_type(val: &A<semtree::Val>) -> bool {
    if let Some(semtree::Val::Op(_, op_a, _, _)) = val.inner() {
        matches!(
            op_a.inner(),
            Some(semtree::Op::Arrow(semtree::ArrowTy::Functor))
        )
    } else {
        false
    }
}

fn seg_decl_name(seg_a: &A<semtree::SegmentDecl>) -> Option<(&str, &TokenSpan)> {
    let (seg, span) = seg_a.accepted()?;
    let (name, _) = seg.0.accepted()?;
    Some((&name.0, span))
}

fn seg_val_name(seg_a: &A<semtree::Segment>) -> Option<(&str, &TokenSpan)> {
    let (seg, span) = seg_a.accepted()?;
    let (name, _) = seg.0.accepted()?;
    Some((&name.0, span))
}

fn decl_item_kind(unit: &semtree::DeclUnit) -> Option<ItemKind> {
    let kind = match &unit.body {
        Some(vm) => match vm {
            semtree::ValMod::Val(_) => match unit.op.inner()? {
                semtree::AssignOp::Alias => ItemKind::Alias,
                semtree::AssignOp::Def => ItemKind::Def,
                semtree::AssignOp::Decl => ItemKind::Decl,
                semtree::AssignOp::Add => return None,
            },
            semtree::ValMod::Mod(_) => return None,
        },
        None => match unit.op.inner()? {
            semtree::AssignOp::Decl => ItemKind::Decl,
            _ => return None,
        },
    };
    Some(kind)
}

trait Check {
    fn check(&self, c: &mut Checker<'_>);
}

impl<T: Check> Check for A<T> {
    fn check(&self, c: &mut Checker<'_>) {
        if let Some(v) = self.inner() {
            v.check(c);
        }
    }
}

struct Checker<'a> {
    tokens: &'a [Token<'a>],
    scopes: Vec<Module>,
    errors: Vec<Error>,
}

impl<'a> Checker<'a> {
    fn new(tokens: &'a [Token<'a>]) -> Self {
        Checker {
            tokens,
            scopes: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn error_at(&mut self, span: &TokenSpan, msg: impl Into<String>) {
        if let Some(token) = self.tokens.get(span.start) {
            self.errors.push((token.pos.clone(), msg.into()));
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(Module::new());
    }
    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define(&mut self, name: String, item: Item) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.define(name, item);
        }
    }

    fn lookup(&self, name: &str) -> Option<&Item> {
        for scope in self.scopes.iter().rev() {
            if let Some(item) = scope.get(name) {
                return Some(item);
            }
        }
        None
    }

    fn lookup_mut(&mut self, name: &str) -> Option<&mut Item> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(item) = scope.get_mut(name) {
                return Some(item);
            }
        }
        None
    }

    /// Resolve a sequence of (name, span) through scope lookup and member chain.
    /// Reports errors for undefined names/members. Returns the final resolved item.
    fn resolve_segments(&mut self, segments: &[(&str, &TokenSpan)]) -> Option<Item> {
        let mut current: Option<Item> = None;
        for (i, &(name, span)) in segments.iter().enumerate() {
            if i == 0 {
                match self.lookup(name) {
                    Some(item) => current = Some(item.clone()),
                    None => {
                        if name != "*" && !crate::convert::is_number_str(name) {
                            self.error_at(span, format!("undefined name `{}`", name));
                        }
                        return None;
                    }
                }
            } else {
                let next = current
                    .take()
                    .and_then(|item| item.members().and_then(|m| m.get(name).cloned()));
                if next.is_none() {
                    self.error_at(span, format!("undefined member `{}`", name));
                    return None;
                }
                current = next;
            }
        }
        current
    }

    // --- Module ---

    fn collect_module_members_owned(&mut self, module: A<semtree::Module>) -> Module {
        match module {
            A::Accepted(semtree::Module::Block(decls), _) => {
                self.push_scope();
                for d in decls {
                    self.process_decl(d);
                }
                self.scopes.pop().unwrap()
            }
            _ => Module::new(), // Import: TODO
        }
    }

    // --- Registration ---

    fn register_unit_results(
        &mut self,
        names: &[A<semtree::PathDecl>],
        op: &A<semtree::AssignOp>,
        item: Item,
    ) {
        // For +=, merge into existing item
        if matches!(op.inner(), Some(semtree::AssignOp::Add)) {
            self.register_add(names, item);
            return;
        }

        // Register each declared name
        for name_a in names {
            if let Some((pd, _)) = name_a.accepted() {
                self.register_path_decl(pd, item.clone());
            }
        }
    }

    fn register_path_decl(&mut self, pd: &semtree::PathDecl, item: Item) {
        let segs = &pd.0;
        match segs.len() {
            0 => {}
            1 => {
                if let Some((name, _)) = segs.first().and_then(seg_decl_name) {
                    self.define(name.to_owned(), item);
                }
            }
            _ => self.insert_into_dotted(segs, item),
        }
    }

    fn insert_into_dotted(&mut self, segs: &[A<semtree::SegmentDecl>], item: Item) {
        if segs.len() < 2 {
            return;
        }
        let Some((first_name, _)) = seg_decl_name(&segs[0]) else {
            return;
        };
        let Some((last_name, last_span)) = seg_decl_name(segs.last().unwrap()) else {
            return;
        };

        let mut conflict = false;
        if let Some(root) = self.lookup_mut(first_name) {
            let mut current = root;
            for seg_a in &segs[1..segs.len() - 1] {
                if let Some((name, _)) = seg_decl_name(seg_a) {
                    let Some(members) = current.members_mut() else {
                        return;
                    };
                    if !members.contains_key(name) {
                        return;
                    }
                    current = members.get_mut(name).unwrap();
                }
            }
            if let Some(members) = current.members_mut() {
                if members.contains_key(last_name) {
                    conflict = true;
                } else {
                    members.define(last_name.to_owned(), item);
                }
            }
        }

        if conflict {
            self.error_at(last_span, format!("duplicate member `{}`", last_name));
        }
    }

    fn register_add(
        &mut self,
        names: &[A<semtree::PathDecl>],
        item: Item,
    ) {
        let members_to_merge = match item.body {
            ItemBody::Value { val, members } => {
                if let Some(ref val_s) = val {
                    self.resolve_val_as_members(val_s)
                        .unwrap_or_else(Module::new)
                } else {
                    members
                }
            }
            ItemBody::Functor { .. } => Module::new(),
        };

        for name_a in names {
            if let A::Accepted(pd, _) = name_a {
                self.merge_into_path(pd, members_to_merge.clone());
            }
        }
    }

    fn resolve_val_as_members(&self, val_s: &S<Val>) -> Option<Module> {
        let path = match &val_s.0 {
            Val::Path(path) => path,
            _ => return None,
        };
        let mut current: Option<&Item> = None;
        for (i, seg_s) in path.segments.iter().enumerate() {
            let name = &seg_s.0.name;
            if i == 0 {
                current = self.lookup(name);
            } else {
                current =
                    current.and_then(|item| item.members().and_then(|m| m.get(name)));
            }
        }
        Some(current?.members()?.clone())
    }

    fn merge_into_path(&mut self, pd: &semtree::PathDecl, new_members: Module) {
        if new_members.entries.is_empty() {
            return;
        }
        let segs = &pd.0;
        if segs.is_empty() {
            return;
        }

        if segs.len() == 1 {
            if let Some((name, span)) = segs.first().and_then(seg_decl_name) {
                let conflicts = if let Some(existing) = self.lookup_mut(name) {
                    if let Some(members) = existing.members_mut() {
                        members.merge(new_members)
                    } else {
                        Vec::new()
                    }
                } else {
                    Vec::new()
                };
                for key in conflicts {
                    self.error_at(span, format!("duplicate member `{}`", key));
                }
            }
        }
        // TODO: multi-segment += / with paths
    }

    // --- Unit locals ---

    fn register_unit_locals(&mut self, unit: &semtree::DeclUnit) {
        let is_add = matches!(unit.op.inner(), Some(semtree::AssignOp::Add));
        let item_kind = decl_item_kind(unit);
        for name_a in &unit.names {
            if let A::Accepted(pd, _) = name_a {
                for seg_a in &pd.0 {
                    if let A::Accepted(seg, _) = seg_a {
                        for param_a in &seg.1.0 {
                            if let A::Accepted(param, _) = param_a {
                                if let A::Accepted(name, _) = &param.name {
                                    let ty = convert_param_ty(&param.ty)
                                        .unwrap_or_else(|| {
                                            S(
                                                Val::Path(Path {
                                                    segments: Vec::new(),
                                                    applicand: None,
                                                }),
                                                TokenSpan { start: 0, end: 0 },
                                            )
                                        });
                                    self.define(
                                        name.0.clone(),
                                        Item::param(ty),
                                    );
                                }
                            }
                        }
                    }
                }
                if !is_add && pd.0.len() == 1 {
                    if let Some((name, _)) = pd.0.first().and_then(seg_decl_name) {
                        self.define(name.to_owned(), Item::new(item_kind));
                    }
                }
            }
        }
    }
}

// --- Process (owned) ---

impl<'a> Checker<'a> {
    fn process_program(&mut self, program: A<semtree::Program>) {
        if let A::Accepted(prog, _) = program {
            for d in prog.0 {
                self.process_decl(d);
            }
        }
    }

    fn check_decl_unit(unit: &semtree::DeclUnit, c: &mut Checker<'_>) {
        for name_a in &unit.names {
            if let Some((pd, _)) = name_a.accepted() {
                pd.check(c);
            }
        }
        if let Some(ty) = &unit.ty {
            ty.check(c);
        }
        if matches!(unit.op.inner(), Some(semtree::AssignOp::Add)) {
            for name_a in &unit.names {
                if let Some((pd, _)) = name_a.accepted() {
                    if pd.0.len() == 1 {
                        if let Some((name, span)) = pd.0.first().and_then(seg_decl_name) {
                            if c.lookup(name).is_none() {
                                c.error_at(
                                    span,
                                    format!("`{}` must be declared before `+=`", name),
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    fn extract_params(names: &[A<semtree::PathDecl>]) -> Vec<Param> {
        let mut params = Vec::new();
        if let Some(first_name) = names.first() {
            if let Some(pd) = first_name.inner() {
                for seg_a in &pd.0 {
                    if let A::Accepted(seg, _) = seg_a {
                        for param_a in &seg.1.0 {
                            if let A::Accepted(param, _) = param_a {
                                if let A::Accepted(name, _) = &param.name {
                                    if let Some(ty) = convert_param_ty(&param.ty) {
                                        params.push(Param {
                                            name: name.0.clone(),
                                            ty,
                                        });
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        params
    }

    fn process_decl(&mut self, decl_a: A<semtree::Decl>) {
        let A::Accepted(decl, _) = decl_a else {
            return;
        };
        let semtree::Decl {
            decos,
            main,
            with_clauses,
            where_clauses,
        } = decl;

        // 1. Check decorators (borrow)
        for deco in &decos {
            deco.check(self);
        }

        // 2. Unit pre-checks in outer scope (borrow)
        if let semtree::DeclMain::Unit(ref u) = main {
            if let Some(unit) = u.inner() {
                Self::check_decl_unit(unit, self);
            }
        }

        // 3. Push local scope
        self.push_scope();

        // Deco params → define
        for deco_a in &decos {
            if let A::Accepted(semtree::Decorator::Param(name_a, ty), _) = deco_a {
                if let A::Accepted(name, _) = name_a {
                    let ty = convert_param_ty(ty)
                        .unwrap_or_else(|| {
                            S(
                                Val::Path(Path {
                                    segments: Vec::new(),
                                    applicand: None,
                                }),
                                TokenSpan { start: 0, end: 0 },
                            )
                        });
                    self.define(name.0.clone(), Item::param(ty));
                }
            }
        }

        // Unit locals → register (borrow main via ref)
        if let semtree::DeclMain::Unit(ref u) = main {
            if let Some(unit) = u.inner() {
                self.register_unit_locals(unit);
            }
        }

        // 4. Where clauses (consume)
        for wm in where_clauses.into_iter().rev() {
            if let A::Accepted(semtree::Module::Block(decls), _) = wm {
                for d in decls {
                    self.process_decl(d);
                }
            }
        }

        // 5. Body val check (borrow main via ref)
        if let semtree::DeclMain::Unit(ref u) = main {
            if let Some(unit) = u.inner() {
                if let Some(semtree::ValMod::Val(v)) = &unit.body {
                    v.check(self);
                }
            }
        }

        // 6. Extract deco values (convert to item::Val)
        let deco_vals: Vec<S<Val>> = decos
            .into_iter()
            .filter_map(|d| match d {
                A::Accepted(semtree::Decorator::Deco(val), _) => convert_a_val(&val),
                _ => None,
            })
            .collect();

        // 7. Consume main
        match main {
            semtree::DeclMain::Unit(unit_a) => {
                if let A::Accepted(unit, _) = unit_a {
                    let kind = decl_item_kind(&unit);
                    let semtree::DeclUnit {
                        names,
                        ty,
                        op,
                        body,
                    } = unit;

                    // Functor application constraints
                    let has_functor_app = names.iter().any(|n| {
                        n.inner().map_or(false, |pd| pd.1.is_some())
                    });
                    if has_functor_app {
                        if !matches!(op.inner(), Some(semtree::AssignOp::Alias)) {
                            if let Some((_, span)) = op.accepted() {
                                self.error_at(
                                    span,
                                    "functor application only allows `=`",
                                );
                            }
                        }
                        if let Some(semtree::ValMod::Mod(m)) = &body {
                            if let Some((_, span)) = m.accepted() {
                                self.error_at(
                                    span,
                                    "functor application cannot have a module body",
                                );
                            }
                        }
                        for wc in &with_clauses {
                            if let Some((_, span)) = wc.accepted() {
                                self.error_at(
                                    span,
                                    "functor application cannot have `with` clauses",
                                );
                            }
                        }
                    }

                    let params = Self::extract_params(&names);

                    let ty_converted = ty.as_ref().and_then(|t| convert_a_val(t));

                    let (val, body_members) = match body {
                        Some(semtree::ValMod::Val(v)) => (convert_a_val(&v), Module::new()),
                        Some(semtree::ValMod::Mod(m)) => {
                            (None, self.collect_module_members_owned(m))
                        }
                        None => (None, Module::new()),
                    };

                    // 7. With clauses (consume)
                    let mut result = body_members;
                    for with_mod in with_clauses {
                        let span = match &with_mod {
                            A::Accepted(_, s) => Some(s.clone()),
                            _ => None,
                        };
                        let with_members = self.collect_module_members_owned(with_mod);
                        let conflicts = result.merge(with_members);
                        if let Some(span) = &span {
                            for key in conflicts {
                                self.error_at(span, format!("duplicate member `{}`", key));
                            }
                        }
                    }

                    // 8. Pop scope
                    self.pop_scope();

                    // 10. Register
                    if has_functor_app {
                        for name_a in names {
                            if let A::Accepted(pd, _) = name_a {
                                let semtree::PathDecl(segs, applicand) = pd;
                                if let Some(applicand) = applicand {
                                    if let Some((fname, fspan)) =
                                        segs.first().and_then(|s| seg_decl_name(s))
                                    {
                                        let fname = fname.to_owned();
                                        let fspan = fspan.clone();
                                        if let Some(existing) = self.lookup_mut(&fname) {
                                            if let ItemBody::Functor { mappings } =
                                                &mut existing.body
                                            {
                                                if let Some(v) = &val {
                                                    if let Some(app_s) =
                                                        convert_a_val(&applicand)
                                                    {
                                                        mappings.push(FunctorMapping {
                                                            applicand: app_s,
                                                            val: v.clone(),
                                                        });
                                                    }
                                                }
                                            } else {
                                                self.error_at(
                                                    &fspan,
                                                    format!("`{}` is not a functor", fname),
                                                );
                                            }
                                        } else {
                                            self.error_at(
                                                &fspan,
                                                format!("undefined functor `{}`", fname),
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        let is_functor = ty
                            .as_ref()
                            .map_or(false, |t| is_functor_type(t));
                        let body = if is_functor {
                            ItemBody::Functor {
                                mappings: Vec::new(),
                            }
                        } else {
                            ItemBody::Value {
                                val,
                                members: result,
                            }
                        };
                        let item = Item {
                            kind,
                            ty: ty_converted,
                            params,
                            body,
                            decos: deco_vals,
                        };
                        self.register_unit_results(&names, &op, item);
                    }
                } else {
                    // unit_a is Error
                    self.pop_scope();
                }
            }
            semtree::DeclMain::Mod(mod_a) => {
                let span = match &mod_a {
                    A::Accepted(_, s) => Some(s.clone()),
                    _ => None,
                };

                // With clauses (consume)
                let mut result = self.collect_module_members_owned(mod_a);
                for with_mod in with_clauses {
                    let with_span = match &with_mod {
                        A::Accepted(_, s) => Some(s.clone()),
                        _ => None,
                    };
                    let with_members = self.collect_module_members_owned(with_mod);
                    let conflicts = result.merge(with_members);
                    if let Some(span) = &with_span {
                        for key in conflicts {
                            self.error_at(span, format!("duplicate member `{}`", key));
                        }
                    }
                }

                self.pop_scope();

                // Mod → promote to current scope
                if let Some(scope) = self.scopes.last_mut() {
                    let conflicts = scope.merge(result);
                    if let Some(span) = &span {
                        for key in conflicts {
                            self.error_at(span, format!("duplicate member `{}`", key));
                        }
                    }
                }
            }
        }
    }
}

// --- Check impls ---

impl Check for semtree::Decorator {
    fn check(&self, c: &mut Checker<'_>) {
        match self {
            semtree::Decorator::Param(_, ty) => ty.check(c),
            semtree::Decorator::Deco(v) => v.check(c),
        }
    }
}

impl Check for semtree::Val {
    fn check(&self, c: &mut Checker<'_>) {
        match self {
            semtree::Val::Path(path_a) => path_a.check(c),
            semtree::Val::Lit(lit_a) => lit_a.check(c),
            semtree::Val::Op(l, _, params, r) => {
                l.check(c);
                if let Some(p) = params {
                    p.check(c);
                }
                r.check(c);
            }
        }
    }
}

impl Check for semtree::Path {
    fn check(&self, c: &mut Checker<'_>) {
        let name_spans: Vec<_> = self.0.iter().filter_map(seg_val_name).collect();
        c.resolve_segments(&name_spans);

        for seg_a in &self.0 {
            if let A::Accepted(seg, _) = seg_a {
                seg.1.check(c);
            }
        }
        if let Some(v) = &self.1 {
            v.check(c);
        }
    }
}

impl Check for semtree::ParamsVal {
    fn check(&self, c: &mut Checker<'_>) {
        for param_a in &self.0 {
            if let A::Accepted(param, _) = param_a {
                param.val.check(c);
            }
        }
    }
}

impl Check for semtree::Lit {
    fn check(&self, c: &mut Checker<'_>) {
        match self {
            semtree::Lit::Array(vs) => {
                for v in vs {
                    v.check(c);
                }
            }
            semtree::Lit::Object(kvs) => {
                for (_, v) in kvs {
                    v.check(c);
                }
            }
            _ => {}
        }
    }
}

impl Check for semtree::PathDecl {
    fn check(&self, c: &mut Checker<'_>) {
        let segs = &self.0;

        if segs.len() > 1 {
            let prefix: Vec<_> = segs[..segs.len() - 1]
                .iter()
                .filter_map(seg_decl_name)
                .collect();
            c.resolve_segments(&prefix);
        }

        for seg_a in segs {
            if let A::Accepted(seg, _) = seg_a {
                for param_a in &seg.1.0 {
                    if let A::Accepted(param, _) = param_a {
                        param.ty.check(c);
                    }
                }
            }
        }

        if let Some(v) = &self.1 {
            v.check(c);
        }
    }
}

pub fn check(program: A<semtree::Program>, tokens: &[Token]) -> (Module, Vec<Error>) {
    let mut checker = Checker::new(tokens);
    checker.push_scope();
    checker.process_program(program);
    let module = checker.scopes.pop().unwrap();
    (module, checker.errors)
}
