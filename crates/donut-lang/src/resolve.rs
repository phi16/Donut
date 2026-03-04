use crate::types::common::*;
use crate::types::item::*;
use crate::types::semtree;
use crate::types::token::Token;

// --- Resolve trait ---

trait Resolve {
    type Output;
    fn resolve(self, ctx: &mut Checker) -> Self::Output;
}

// --- Helper functions ---

fn is_functor_type(val: &Val) -> bool {
    matches!(val, Val::Arrow(ArrowKind::Functor, _, _))
}

fn resolve_arrow_kind(arrow_ty: &semtree::ArrowTy) -> ArrowKind {
    match arrow_ty {
        semtree::ArrowTy::To => ArrowKind::To,
        semtree::ArrowTy::Eq => ArrowKind::Eq,
        semtree::ArrowTy::Functor => ArrowKind::Functor,
    }
}

// --- Resolve trait implementations (Val types only, owned) ---

impl Resolve for S<semtree::Val> {
    type Output = S<Val>;
    fn resolve(self, ctx: &mut Checker) -> S<Val> {
        let S(v, span) = self;
        let val = v.resolve(ctx);
        S(val, span)
    }
}

impl Resolve for semtree::Val {
    type Output = Val;
    fn resolve(self, ctx: &mut Checker) -> Val {
        match self {
            semtree::Val::Path(path_s) => {
                let path = (*path_s).resolve(ctx);
                Val::Path(path)
            }
            semtree::Val::Lit(lit_s) => {
                let lit = lit_s.resolve(ctx);
                Val::Lit(lit)
            }
            semtree::Val::Op(l, op_s, _params, r) => {
                let S(op, _) = op_s;
                match op {
                    semtree::Op::Comp(axis) => {
                        let mut children = Vec::new();
                        ctx.resolve_comp_flat(*l, axis, &mut children);
                        ctx.resolve_comp_flat(*r, axis, &mut children);
                        Val::Comp(axis, children)
                    }
                    semtree::Op::CompStar => {
                        let mut children = Vec::new();
                        ctx.resolve_comp_star_flat(*l, &mut children);
                        ctx.resolve_comp_star_flat(*r, &mut children);
                        Val::CompStar(children)
                    }
                    semtree::Op::Arrow(arrow_ty) => {
                        let kind = resolve_arrow_kind(&arrow_ty);
                        let l_s = (*l).resolve(ctx);
                        let r_s = (*r).resolve(ctx);
                        Val::Arrow(kind, Box::new(l_s), Box::new(r_s))
                    }
                }
            }
            semtree::Val::Any => Val::Hole(Hole::Any),
        }
    }
}

impl Resolve for S<semtree::Path<semtree::ParamVal>> {
    type Output = Path;
    fn resolve(self, ctx: &mut Checker) -> Path {
        let S(path, _) = self;

        // Name resolution
        let name_spans: Vec<(&str, &TokenSpan)> = path.0.iter().map(|seg_s| {
            let S(seg, span) = seg_s;
            (seg.0 .0.as_str(), span)
        }).collect();
        ctx.resolve_segments(&name_spans);

        // Convert segments (resolving param vals recursively)
        let segments: Vec<S<Segment>> = path
            .0
            .into_iter()
            .map(|seg_s| seg_s.resolve(ctx))
            .collect();

        let applicand = path.1.map(|v| Box::new(v.resolve(ctx)));

        Path {
            segments,
            applicand,
        }
    }
}

impl Resolve for S<semtree::Segment<semtree::ParamVal>> {
    type Output = S<Segment>;
    fn resolve(self, ctx: &mut Checker) -> S<Segment> {
        let S(seg, span) = self;
        let params: Vec<ParamVal> = seg
            .1
             .0
            .into_iter()
            .map(|pv| pv.resolve(ctx))
            .collect();
        S(
            Segment {
                name: seg.0 .0,
                params,
            },
            span,
        )
    }
}

impl Resolve for semtree::ParamVal {
    type Output = ParamVal;
    fn resolve(self, ctx: &mut Checker) -> ParamVal {
        let name = self.name.map(|n| n.0);
        let val = self.val.resolve(ctx);
        ParamVal { name, val }
    }
}

impl Resolve for S<semtree::Lit> {
    type Output = Lit;
    fn resolve(self, ctx: &mut Checker) -> Lit {
        let S(lit, _) = self;
        match lit {
            semtree::Lit::Number(s) => Lit::Number(s),
            semtree::Lit::String(s) => Lit::String(s),
            semtree::Lit::Array(vs) => {
                let items: Vec<S<Val>> =
                    vs.into_iter().map(|v| v.resolve(ctx)).collect();
                Lit::Array(items)
            }
            semtree::Lit::Object(kvs) => {
                let items: Vec<(String, S<Val>)> = kvs
                    .into_iter()
                    .map(|(k, v)| {
                        let S(key, _) = k;
                        let key_str = match key {
                            semtree::Key::Name(n) => n.0,
                            semtree::Key::String(s) => s,
                        };
                        let val = v.resolve(ctx);
                        (key_str, val)
                    })
                    .collect();
                Lit::Object(items)
            }
        }
    }
}

// --- Checker ---

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

    // --- Comp flat helpers ---

    fn resolve_comp_flat(&mut self, val_s: S<semtree::Val>, axis: u32, out: &mut Vec<S<Val>>) {
        match val_s {
            S(semtree::Val::Op(l, op_s, _, r), _)
                if matches!(&op_s.0, semtree::Op::Comp(n) if *n == axis) =>
            {
                self.resolve_comp_flat(*l, axis, out);
                self.resolve_comp_flat(*r, axis, out);
            }
            other => {
                out.push(other.resolve(self));
            }
        }
    }

    fn resolve_comp_star_flat(&mut self, val_s: S<semtree::Val>, out: &mut Vec<S<Val>>) {
        match val_s {
            S(semtree::Val::Op(l, op_s, _, r), _)
                if matches!(&op_s.0, semtree::Op::CompStar) =>
            {
                self.resolve_comp_star_flat(*l, out);
                self.resolve_comp_star_flat(*r, out);
            }
            other => {
                out.push(other.resolve(self));
            }
        }
    }

    // --- Scope resolution ---

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

    // --- Module / Decl resolution (owned) ---

    fn resolve_module(&mut self, mod_s: S<semtree::Module>) -> Module {
        let S(module, _) = mod_s;
        match module {
            semtree::Module::Block(decls) => {
                self.push_scope();
                for d in decls {
                    self.resolve_decl(d);
                }
                self.scopes.pop().unwrap()
            }
            semtree::Module::Import(_) => Module::new(), // TODO
        }
    }

    fn resolve_decl(&mut self, decl: semtree::Decl) {
        let semtree::Decl {
            decos,
            main,
            with_clauses,
            where_clauses,
        } = decl;

        let (deco_param_defs, deco_vals) = self.resolve_decorators(decos);

        match main {
            semtree::DeclMain::Unit(unit) => {
                self.process_unit_decl(unit, deco_param_defs, deco_vals, with_clauses, where_clauses);
            }
            semtree::DeclMain::Mod(mod_s) => {
                self.process_mod_decl(mod_s, deco_param_defs, with_clauses, where_clauses);
            }
        }
    }

    // --- Decorators (owned) ---

    fn resolve_decorators(
        &mut self,
        decos: Vec<semtree::Decorator>,
    ) -> (Vec<(String, S<Val>)>, Vec<S<Val>>) {
        let mut param_defs = Vec::new();
        let mut deco_vals = Vec::new();
        for deco in decos {
            match deco {
                semtree::Decorator::Param(names, ty) => {
                    let resolved_ty = ty.resolve(self);
                    for name in names {
                        param_defs.push((name.0, resolved_ty.clone()));
                    }
                }
                semtree::Decorator::Deco(v) => {
                    deco_vals.push(v.resolve(self));
                }
            }
        }
        (param_defs, deco_vals)
    }

    // --- Inner scope helpers ---

    fn enter_inner_scope(&mut self, deco_param_defs: &[(String, S<Val>)]) {
        self.push_scope();
        for (name, ty) in deco_param_defs {
            self.define(name.clone(), Item::param(ty.clone()));
        }
    }

    fn resolve_where_clauses(&mut self, where_clauses: Vec<S<semtree::Module>>) {
        for wm in where_clauses.into_iter().rev() {
            let S(module, _) = wm;
            if let semtree::Module::Block(decls) = module {
                for d in decls {
                    self.resolve_decl(d);
                }
            }
        }
    }

    // --- With clause merging ---

    fn merge_with_clauses(
        &mut self,
        mut result: Module,
        with_clauses: Vec<S<semtree::Module>>,
    ) -> Module {
        for with_mod in with_clauses {
            let span = with_mod.1.clone();
            let with_members = self.resolve_module(with_mod);
            let conflicts = result.merge(with_members);
            for key in conflicts {
                self.error_at(&span, format!("duplicate member `{}`", key));
            }
        }
        result
    }

    // --- Registration ---

    fn register_path(&mut self, segs: &[(String, TokenSpan)], item: Item) {
        match segs.len() {
            0 => {}
            1 => {
                self.define(segs[0].0.clone(), item);
            }
            _ => self.insert_into_dotted(segs, item),
        }
    }

    fn insert_into_dotted(&mut self, segs: &[(String, TokenSpan)], item: Item) {
        if segs.len() < 2 {
            return;
        }
        let first_name = &segs[0].0;
        let (last_name, last_span) = &segs[segs.len() - 1];

        let mut conflict = false;
        if let Some(root) = self.lookup_mut(first_name) {
            let mut current = root;
            for (name, _) in &segs[1..segs.len() - 1] {
                let Some(members) = current.members_mut() else {
                    return;
                };
                if !members.contains_key(name) {
                    return;
                }
                current = members.get_mut(name).unwrap();
            }
            if let Some(members) = current.members_mut() {
                if members.contains_key(last_name) {
                    conflict = true;
                } else {
                    members.define(last_name.clone(), item);
                }
            }
        }

        if conflict {
            self.error_at(last_span, format!("duplicate member `{}`", last_name));
        }
    }

    fn register_add(&mut self, all_seg_names: &[Vec<(String, TokenSpan)>], item: Item) {
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

        for seg_names in all_seg_names {
            self.merge_into_path(seg_names, members_to_merge.clone());
        }
    }

    fn merge_into_path(&mut self, segs: &[(String, TokenSpan)], new_members: Module) {
        if new_members.entries.is_empty() || segs.is_empty() {
            return;
        }

        if segs.len() == 1 {
            let (name, span) = &segs[0];
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
        // TODO: multi-segment += / with paths
    }

    // --- Process declarations ---

    fn process_unit_decl(
        &mut self,
        unit: semtree::DeclUnit,
        deco_param_defs: Vec<(String, S<Val>)>,
        deco_vals: Vec<S<Val>>,
        with_clauses: Vec<S<semtree::Module>>,
        where_clauses: Vec<S<semtree::Module>>,
    ) {
        let semtree::DeclUnit { names, ty, op, body } = unit;

        // Compute item kind from op and body
        let kind = match &body {
            Some(vm) => match vm {
                semtree::ValMod::Val(_) => match &op.0 {
                    semtree::AssignOp::Alias => Some(ItemKind::Alias),
                    semtree::AssignOp::Def => Some(ItemKind::Def),
                    semtree::AssignOp::Decl => Some(ItemKind::Decl),
                    semtree::AssignOp::Add => None,
                },
                semtree::ValMod::Mod(_) => None,
            },
            None => match &op.0 {
                semtree::AssignOp::Decl => Some(ItemKind::Decl),
                _ => None,
            },
        };

        // Split body into val/mod
        let (body_val, body_mod) = match body {
            Some(semtree::ValMod::Val(v)) => (Some(v), None),
            Some(semtree::ValMod::Mod(m)) => (None, Some(m)),
            None => (None, None),
        };

        // --- Outer scope: extract seg_names (borrowing names) ---
        let all_seg_names: Vec<Vec<(String, TokenSpan)>> = names
            .iter()
            .map(|name_s| {
                let S(pd, _) = name_s;
                pd.0.iter()
                    .map(|seg_s| {
                        let S(seg, span) = seg_s;
                        (seg.0 .0.clone(), span.clone())
                    })
                    .collect()
            })
            .collect();

        // --- Resolve prefixes (borrowing seg_names) ---
        for seg_names in &all_seg_names {
            if seg_names.len() > 1 {
                let prefix: Vec<_> = seg_names[..seg_names.len() - 1]
                    .iter()
                    .map(|(name, span)| (name.as_str(), span))
                    .collect();
                self.resolve_segments(&prefix);
            }
        }

        // --- Consume names → params + applicands ---
        let mut params = Vec::new();
        let mut applicands: Vec<Option<S<semtree::Val>>> = Vec::new();
        for (i, name_s) in names.into_iter().enumerate() {
            let S(pd, _) = name_s;
            let semtree::Path(segs, applicand) = pd;
            applicands.push(applicand);
            for seg_s in segs {
                let S(seg, _) = seg_s;
                for param_decl in seg.1 .0 {
                    let resolved_ty = param_decl.ty.resolve(self);
                    if i == 0 {
                        for name in param_decl.names {
                            params.push(Param {
                                name: name.0,
                                ty: resolved_ty.clone(),
                            });
                        }
                    }
                }
            }
        }

        // --- Resolve ty (owned) ---
        let ty_resolved = ty.map(|t| t.resolve(self));

        // --- += pre-check ---
        if matches!(&op.0, semtree::AssignOp::Add) {
            for seg_names in &all_seg_names {
                if seg_names.len() == 1 {
                    let (name, span) = &seg_names[0];
                    if self.lookup(name).is_none() {
                        self.error_at(
                            span,
                            format!("`{}` must be declared before `+=`", name),
                        );
                    }
                }
            }
        }

        // --- Inner scope ---
        self.enter_inner_scope(&deco_param_defs);

        // Define params
        for param in &params {
            self.define(param.name.clone(), Item::param(param.ty.clone()));
        }

        // Forward refs
        let is_add = matches!(&op.0, semtree::AssignOp::Add);
        for seg_names in &all_seg_names {
            if !is_add && seg_names.len() == 1 {
                self.define(seg_names[0].0.clone(), Item::new(kind));
            }
        }

        // Where clauses (after forward refs so declared names are visible)
        self.resolve_where_clauses(where_clauses);

        // --- Functor constraints ---
        let has_functor_app = applicands.iter().any(|a| a.is_some());
        if has_functor_app {
            if !matches!(&op.0, semtree::AssignOp::Alias) {
                self.error_at(&op.1, "functor application only allows `=`");
            }
            if let Some(ref m) = body_mod {
                self.error_at(
                    &m.1,
                    "functor application cannot have a module body",
                );
            }
            for wc in &with_clauses {
                self.error_at(
                    &wc.1,
                    "functor application cannot have `with` clauses",
                );
            }
        }

        // --- Resolve body (owned) ---
        let body_val_resolved = body_val.map(|v| v.resolve(self));
        let body_members = match body_mod {
            Some(m) => self.resolve_module(m),
            None => Module::new(),
        };

        // With clauses + pop scope
        let result = self.merge_with_clauses(body_members, with_clauses);
        self.pop_scope();

        // --- Registration ---
        if has_functor_app {
            for (seg_names, applicand) in all_seg_names.iter().zip(applicands.into_iter()) {
                if let Some(applicand) = applicand {
                    let (fname, fspan) = (&seg_names[0].0, &seg_names[0].1);
                    let app_resolved = applicand.resolve(self);
                    if let Some(existing) = self.lookup_mut(fname) {
                        if let ItemBody::Functor { mappings } = &mut existing.body {
                            if let Some(v) = &body_val_resolved {
                                mappings.push(FunctorMapping {
                                    applicand: app_resolved,
                                    val: v.clone(),
                                });
                            }
                        } else {
                            self.error_at(
                                fspan,
                                format!("`{}` is not a functor", fname),
                            );
                        }
                    } else {
                        self.error_at(
                            fspan,
                            format!("undefined functor `{}`", fname),
                        );
                    }
                }
            }
        } else {
            let is_functor = ty_resolved
                .as_ref()
                .map_or(false, |t| is_functor_type(&t.0));
            let body = if is_functor {
                ItemBody::Functor {
                    mappings: Vec::new(),
                }
            } else {
                ItemBody::Value {
                    val: body_val_resolved,
                    members: result,
                }
            };
            let item = Item {
                kind,
                ty: ty_resolved,
                params,
                body,
                decos: deco_vals,
            };

            if is_add {
                self.register_add(&all_seg_names, item);
            } else {
                for seg_names in &all_seg_names {
                    self.register_path(seg_names, item.clone());
                }
            }
        }
    }

    fn process_mod_decl(
        &mut self,
        mod_s: S<semtree::Module>,
        deco_param_defs: Vec<(String, S<Val>)>,
        with_clauses: Vec<S<semtree::Module>>,
        where_clauses: Vec<S<semtree::Module>>,
    ) {
        let span = mod_s.1.clone();

        self.enter_inner_scope(&deco_param_defs);
        self.resolve_where_clauses(where_clauses);

        let result = self.resolve_module(mod_s);
        let result = self.merge_with_clauses(result, with_clauses);

        self.pop_scope();

        // Mod → promote to current scope
        if let Some(scope) = self.scopes.last_mut() {
            let conflicts = scope.merge(result);
            for key in conflicts {
                self.error_at(&span, format!("duplicate member `{}`", key));
            }
        }
    }
}

pub fn resolve(program: semtree::Program, tokens: &[Token]) -> (Module, Vec<Error>) {
    let mut checker = Checker::new(tokens);
    checker.push_scope();
    for d in program.0 {
        checker.resolve_decl(d);
    }
    let module = checker.scopes.pop().unwrap();
    (module, checker.errors)
}
