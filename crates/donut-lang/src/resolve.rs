use crate::types::common::*;
use crate::types::item::*;
use crate::types::semtree;
use crate::types::token::Token;
use std::collections::HashSet;

// --- Resolve trait ---

trait Resolve {
    type Output;
    fn resolve(self, ctx: &mut Checker) -> Self::Output;
}

// --- Helper types ---

struct NameInfo {
    seg_names: Vec<(String, TokenSpan)>,
    applicand: Option<S<semtree::Val>>,
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
    type Output = ValId;
    fn resolve(self, ctx: &mut Checker) -> ValId {
        let S(v, span) = self;
        let val = v.resolve(ctx);
        ctx.alloc_val(S(val, span))
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
                        let l_id = (*l).resolve(ctx);
                        let r_id = (*r).resolve(ctx);
                        Val::Arrow(kind, l_id, r_id)
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

        let applicand = path.1.map(|v| {
            ctx.in_applicand = true;
            let result = v.resolve(ctx);
            ctx.in_applicand = false;
            result
        });

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
                let items: Vec<ValId> =
                    vs.into_iter().map(|v| v.resolve(ctx)).collect();
                Lit::Array(items)
            }
            semtree::Lit::Object(kvs) => {
                let items: Vec<(String, ValId)> = kvs
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
    items: Vec<Item>,
    vals: Vec<S<Val>>,
    scopes: Vec<Module>,
    errors: Vec<Error>,
    deco_params: HashSet<ItemId>,
    in_applicand: bool,
}

impl<'a> Checker<'a> {
    fn new(tokens: &'a [Token<'a>]) -> Self {
        Checker {
            tokens,
            items: Vec::new(),
            vals: Vec::new(),
            scopes: Vec::new(),
            errors: Vec::new(),
            deco_params: HashSet::new(),
            in_applicand: false,
        }
    }

    fn alloc_val(&mut self, val: S<Val>) -> ValId {
        let id = ValId(self.vals.len());
        self.vals.push(val);
        id
    }

    fn alloc_item(&mut self, item: Item) -> ItemId {
        let id = ItemId(self.items.len());
        self.items.push(item);
        id
    }

    fn item(&self, id: ItemId) -> &Item {
        &self.items[id.0]
    }

    fn item_mut(&mut self, id: ItemId) -> &mut Item {
        &mut self.items[id.0]
    }

    fn val(&self, id: ValId) -> &S<Val> {
        &self.vals[id.0]
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

    fn define(&mut self, name: String, item: ItemId) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.define(name, item);
        }
    }

    fn lookup(&self, name: &str) -> Option<ItemId> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.get(name) {
                return Some(id);
            }
        }
        None
    }

    // --- Comp flat helpers ---

    fn resolve_comp_flat(&mut self, val_s: S<semtree::Val>, axis: u32, out: &mut Vec<ValId>) {
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

    fn resolve_comp_star_flat(&mut self, val_s: S<semtree::Val>, out: &mut Vec<ValId>) {
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

    fn resolve_segments(&mut self, segments: &[(&str, &TokenSpan)]) {
        let Some((&(first_name, first_span), rest)) = segments.split_first() else {
            return;
        };
        let Some(mut current) = self.lookup(first_name) else {
            if first_name != "*" && !crate::convert::is_number_str(first_name) {
                self.error_at(first_span, format!("undefined name `{}`", first_name));
            }
            return;
        };
        if self.deco_params.contains(&current) && !self.in_applicand {
            self.error_at(
                first_span,
                format!(
                    "decorator parameter `{}` can only be used in functor applicand",
                    first_name
                ),
            );
        }
        for &(name, span) in rest {
            match self.item(current).members().and_then(|m| m.get(name)) {
                Some(id) => current = id,
                None => {
                    self.error_at(span, format!("undefined member `{}`", name));
                    return;
                }
            }
        }
    }

    fn lookup_path(&self, names: &[impl AsRef<str>]) -> Option<ItemId> {
        let (first, rest) = names.split_first()?;
        let mut current = self.lookup(first.as_ref())?;
        for name in rest {
            current = self.item(current).members()?.get(name.as_ref())?;
        }
        Some(current)
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
    ) -> (Vec<(String, ValId)>, Vec<ValId>) {
        let mut param_defs = Vec::new();
        let mut deco_vals = Vec::new();
        for deco in decos {
            match deco {
                semtree::Decorator::Param(names, ty) => {
                    let resolved_ty = ty.resolve(self);
                    for name in names {
                        param_defs.push((name.0, resolved_ty));
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

    fn enter_inner_scope(&mut self, deco_param_defs: &[(String, ValId)]) {
        self.push_scope();
        for (name, val_id) in deco_param_defs {
            let span = self.val(*val_id).1.clone();
            let item = Item::param(*val_id, span);
            let id = self.alloc_item(item);
            self.deco_params.insert(id);
            self.define(name.clone(), id);
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

    fn register_path(&mut self, segs: &[(String, TokenSpan)], item_id: ItemId) {
        match segs.len() {
            0 => {}
            1 => {
                self.define(segs[0].0.clone(), item_id);
            }
            _ => self.insert_into_dotted(segs, item_id),
        }
    }

    fn insert_into_dotted(&mut self, segs: &[(String, TokenSpan)], item_id: ItemId) {
        if segs.len() < 2 {
            return;
        }
        let first_name = &segs[0].0;
        let (last_name, last_span) = &segs[segs.len() - 1];

        let mut conflict = false;
        if let Some(mut current_id) = self.lookup(first_name) {
            for (name, _) in &segs[1..segs.len() - 1] {
                let next = self.item(current_id).members().and_then(|m| m.get(name));
                match next {
                    Some(id) => current_id = id,
                    None => return,
                }
            }
            if let Some(members) = self.item_mut(current_id).members_mut() {
                if members.contains_key(last_name) {
                    conflict = true;
                } else {
                    members.define(last_name.clone(), item_id);
                }
            }
        }

        if conflict {
            self.error_at(last_span, format!("duplicate member `{}`", last_name));
        }
    }

    fn register_add(&mut self, all_seg_names: &[&[(String, TokenSpan)]], members_to_merge: Module) {
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
            let conflicts = if let Some(id) = self.lookup(name) {
                if let Some(members) = self.item_mut(id).members_mut() {
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
        deco_param_defs: Vec<(String, ValId)>,
        deco_vals: Vec<ValId>,
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
        let name_infos_partial: Vec<Vec<(String, TokenSpan)>> = names
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
        for seg_names in &name_infos_partial {
            if seg_names.len() > 1 {
                let prefix: Vec<_> = seg_names[..seg_names.len() - 1]
                    .iter()
                    .map(|(name, span)| (name.as_str(), span))
                    .collect();
                self.resolve_segments(&prefix);
            }
        }

        // --- Consume names → params + name_infos ---
        let mut params = Vec::new();
        let mut name_infos: Vec<NameInfo> = Vec::new();
        for (i, (name_s, seg_names)) in names.into_iter().zip(name_infos_partial).enumerate() {
            let S(pd, _) = name_s;
            let semtree::Path(segs, applicand) = pd;
            for seg_s in segs {
                let S(seg, _) = seg_s;
                for param_decl in seg.1 .0 {
                    let resolved_ty = param_decl.ty.resolve(self);
                    if i == 0 {
                        for name in param_decl.names {
                            params.push(Param {
                                name: name.0,
                                ty: resolved_ty,
                            });
                        }
                    }
                }
            }
            name_infos.push(NameInfo {
                seg_names,
                applicand,
            });
        }

        // --- += pre-check ---
        if matches!(&op.0, semtree::AssignOp::Add) {
            for ni in &name_infos {
                if ni.seg_names.len() == 1 {
                    let (name, span) = &ni.seg_names[0];
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

        // Define params (before ty so params are in scope for type expressions)
        for param in &params {
            let span = self.val(param.ty).1.clone();
            let item = Item::param(param.ty, span);
            let id = self.alloc_item(item);
            self.define(param.name.clone(), id);
        }

        // Forward refs
        let is_add = matches!(&op.0, semtree::AssignOp::Add);
        for ni in &name_infos {
            if !is_add && ni.seg_names.len() == 1 {
                let item = Item::new(kind, ni.seg_names[0].1.clone());
                let id = self.alloc_item(item);
                self.define(ni.seg_names[0].0.clone(), id);
            }
        }

        // Where clauses (after forward refs so declared names are visible)
        self.resolve_where_clauses(where_clauses);

        // --- Resolve ty (after params/forward refs/where so all names are in scope) ---
        let ty_resolved = ty.map(|t| t.resolve(self));

        // --- Functor constraints ---
        let has_functor_app = name_infos.iter().any(|ni| ni.applicand.is_some());
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
        let mut body_members = match body_mod {
            Some(m) => self.resolve_module(m),
            None => Module::new(),
        };

        // If body is a path to a module, inherit its members
        if body_members.entries.is_empty() {
            if let Some(val_id) = body_val_resolved {
                let val_s = self.val(val_id);
                if let Val::Path(path) = &val_s.0 {
                    let path_names: Vec<String> =
                        path.segments.iter().map(|s| s.0.name.clone()).collect();
                    if let Some(id) = self.lookup_path(&path_names) {
                        if let Some(m) = self.item(id).members() {
                            if !m.entries.is_empty() {
                                body_members = m.clone();
                            }
                        }
                    }
                }
            }
        }

        // With clauses + pop scope
        let result = self.merge_with_clauses(body_members, with_clauses);
        self.pop_scope();

        // --- Registration ---
        if has_functor_app {
            self.register_functor_app(name_infos, body_val_resolved);
        } else if is_add {
            let all_seg_names: Vec<&[(String, TokenSpan)]> =
                name_infos.iter().map(|ni| ni.seg_names.as_slice()).collect();
            let members_to_merge = match body_val_resolved {
                Some(val_id) => {
                    let val_s = self.val(val_id);
                    let span = val_s.1.clone();
                    match &val_s.0 {
                        Val::Path(path) => {
                            let path_names: Vec<String> = path
                                .segments
                                .iter()
                                .map(|s| s.0.name.clone())
                                .collect();
                            self.lookup_path(&path_names)
                                .and_then(|id| self.item(id).members())
                                .cloned()
                                .unwrap_or_else(Module::new)
                        }
                        _ => {
                            self.error_at(&span, "`+=` requires a path or module body");
                            Module::new()
                        }
                    }
                }
                None => result,
            };
            self.register_add(&all_seg_names, members_to_merge);
        } else {
            let is_functor = ty_resolved
                .map_or(false, |id| is_functor_type(&self.val(id).0));
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
            let span = name_infos
                .first()
                .and_then(|ni| ni.seg_names.last())
                .map(|(_, s)| s.clone())
                .unwrap_or(TokenSpan { start: 0, end: 0 });
            let item = Item {
                span,
                kind,
                ty: ty_resolved,
                params,
                body,
                decos: deco_vals,
            };
            let item_id = self.alloc_item(item);
            for ni in &name_infos {
                self.register_path(&ni.seg_names, item_id);
            }
        }
    }

    fn register_functor_app(
        &mut self,
        name_infos: Vec<NameInfo>,
        body_val_resolved: Option<ValId>,
    ) {
        for ni in name_infos {
            if let Some(applicand) = ni.applicand {
                let (fname, fspan) = (&ni.seg_names[0].0, &ni.seg_names[0].1);
                self.in_applicand = true;
                let app_resolved = applicand.resolve(self);
                self.in_applicand = false;
                let lookup_result = self.lookup(fname);
                match lookup_result {
                    Some(id) => {
                        let is_functor =
                            matches!(&self.item(id).body, ItemBody::Functor { .. });
                        if is_functor {
                            if let Some(v) = body_val_resolved {
                                if let ItemBody::Functor { mappings } =
                                    &mut self.item_mut(id).body
                                {
                                    mappings.push(FunctorMapping {
                                        applicand: app_resolved,
                                        val: v,
                                    });
                                }
                            }
                        } else {
                            self.error_at(
                                fspan,
                                format!("`{}` is not a functor", fname),
                            );
                        }
                    }
                    None => {
                        self.error_at(
                            fspan,
                            format!("undefined functor `{}`", fname),
                        );
                    }
                }
            }
        }
    }

    fn process_mod_decl(
        &mut self,
        mod_s: S<semtree::Module>,
        deco_param_defs: Vec<(String, ValId)>,
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

pub fn resolve(program: semtree::Program, tokens: &[Token]) -> (Program, Vec<Error>) {
    let mut checker = Checker::new(tokens);
    // Prelude scope: built-in decorator names
    checker.push_scope();
    for name in &["gray", "hsv", "rgb"] {
        let dummy_span = TokenSpan { start: 0, end: 0 };
        let item = Item::new(None, dummy_span);
        let id = checker.alloc_item(item);
        checker.define(name.to_string(), id);
    }
    // User scope
    checker.push_scope();
    for d in program.0 {
        checker.resolve_decl(d);
    }
    let module = checker.scopes.pop().unwrap();
    let prog = Program {
        root: module,
        items: checker.items,
        vals: checker.vals,
    };
    (prog, checker.errors)
}
