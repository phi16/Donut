use crate::types::common::*;
use crate::types::item::*;
use crate::types::semtree;
use crate::types::token::Token;
use std::collections::{HashMap, HashSet};

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
            assert!(!ctx.in_applicand);
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
    deco_param_stack: Vec<HashSet<ItemId>>,
    used_deco_params: HashSet<ItemId>,
    in_applicand: bool,
    import_cache: HashMap<String, (Module, Vec<CheckNode>)>,
    /// Origin name for items being resolved inside an import (e.g. "sys").
    current_origin: Option<String>,
    /// Extra import sources (name → source code) for testing/extensibility.
    extra_sources: HashMap<String, String>,
    /// Stack of CheckNode lists being built (mirrors scope nesting).
    check_order_stack: Vec<Vec<CheckNode>>,
    /// Stack of qualified name prefixes for building qnames/cnames.
    prefix_stack: Vec<String>,
}

impl<'a> Checker<'a> {
    fn new(tokens: &'a [Token<'a>]) -> Self {
        Checker {
            tokens,
            items: Vec::new(),
            vals: Vec::new(),
            scopes: Vec::new(),
            errors: Vec::new(),
            deco_param_stack: Vec::new(),
            used_deco_params: HashSet::new(),
            in_applicand: false,
            import_cache: HashMap::new(),
            current_origin: None,
            extra_sources: HashMap::new(),
            check_order_stack: Vec::new(),
            prefix_stack: Vec::new(),
        }
    }

    fn alloc_val(&mut self, val: S<Val>) -> ValId {
        let id = ValId(self.vals.len());
        self.vals.push(val);
        id
    }

    fn alloc_item(&mut self, mut item: Item) -> ItemId {
        if item.origin.is_none() {
            item.origin = self.current_origin.clone();
        }
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

    fn make_item_name(&self, lname: String) -> ItemName {
        let qname = if let Some(prefix) = self.prefix_stack.last() {
            format!("{}.{}", prefix, lname)
        } else {
            lname.clone()
        };
        let cname = match &self.current_origin {
            Some(origin) => format!("{}::{}", origin, qname),
            None => qname.clone(),
        };
        ItemName { lname, qname, cname }
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

    // --- CheckNode helpers ---

    fn push_check_order(&mut self) {
        self.check_order_stack.push(Vec::new());
    }

    fn pop_check_order(&mut self) -> Vec<CheckNode> {
        self.check_order_stack.pop().unwrap_or_default()
    }

    fn emit_check_node(&mut self, node: CheckNode) {
        if let Some(top) = self.check_order_stack.last_mut() {
            top.push(node);
        }
    }

    /// Generate CheckNodes for an already-resolved Module (used for imports and path inheritance).
    fn generate_check_nodes_for_module(&self, module: &Module) -> Vec<CheckNode> {
        let mut nodes = Vec::new();
        for (_, item_id) in module.internal.iter().chain(&module.entries) {
            nodes.push(CheckNode::Item(*item_id));
            if let Some(members) = self.item(*item_id).members() {
                if !members.entries.is_empty() || !members.internal.is_empty() {
                    let children = self.generate_check_nodes_for_module(members);
                    nodes.push(CheckNode::Scope { item_id: *item_id, children });
                }
            }
        }
        nodes
    }

    /// Insert a name into the current scope. Returns true if newly defined, false if duplicate.
    fn define(&mut self, name: String, item: ItemId) -> bool {
        let scope = self.scopes.last_mut().unwrap();
        scope.define(name, item).is_none()
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

    fn resolve_comp_flat(&mut self, val_s: S<semtree::Val>, axis: donut_core::common::Axis, out: &mut Vec<ValId>) {
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
            if first_name != "*" && first_name != "meta" && !crate::convert::is_number_str(first_name) {
                self.error_at(first_span, format!("undefined name `{}`", first_name));
            }
            return;
        };
        if self.is_deco_param(current) && self.in_applicand {
            self.used_deco_params.insert(current);
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

    fn resolve_decls(&mut self, decls: Vec<semtree::Decl>) -> (Module, Vec<CheckNode>) {
        self.push_scope();
        self.push_check_order();
        for d in decls {
            self.resolve_decl(d);
        }
        let check_nodes = self.pop_check_order();
        let mut module = self.scopes.pop().unwrap();
        module.finalize_used();
        (module, check_nodes)
    }

    fn resolve_module(&mut self, mod_s: S<semtree::Module>) -> (Module, Vec<CheckNode>) {
        let S(module, span) = mod_s;
        match module {
            semtree::Module::Block(decls) => self.resolve_decls(decls),
            semtree::Module::Import(lit_s) | semtree::Module::Use(lit_s) => {
                let name = match &lit_s.0 {
                    semtree::Lit::String(s) => s.trim_matches('"').to_string(),
                    _ => {
                        self.error_at(&span, "import requires a string literal");
                        return (Module::new(), Vec::new());
                    }
                };
                if let Some(cached) = self.import_cache.get(&name) {
                    return cached.clone();
                }
                let source = builtin_source(&name)
                    .map(|s| s.to_string())
                    .or_else(|| self.extra_sources.get(&name).cloned());
                match source {
                    Some(source) => {
                        let old_origin = self.current_origin.take();
                        let old_prefix = std::mem::take(&mut self.prefix_stack);
                        self.current_origin = Some(name.clone());
                        let (mut module, check_nodes) = self.resolve_import(&source);
                        self.current_origin = old_origin;
                        self.prefix_stack = old_prefix;
                        module.origin = Some(name.clone());
                        self.import_cache.insert(name, (module.clone(), check_nodes.clone()));
                        (module, check_nodes)
                    }
                    None => {
                        self.error_at(&span, format!("unknown import: \"{}\"", name));
                        (Module::new(), Vec::new())
                    }
                }
            }
        }
    }

    fn resolve_import(&mut self, source: &str) -> (Module, Vec<CheckNode>) {
        let (tokens, _, _) = crate::tokenize::tokenize(source);
        let (program, _) = crate::parse::parse(&tokens);
        let (sem_prog, _) = crate::convert::convert(program, &tokens);
        self.resolve_decls(sem_prog.0)
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
                if matches!(&mod_s.0, semtree::Module::Use(_)) {
                    let span = mod_s.1.clone();
                    let (result, check_nodes) = self.resolve_module(mod_s);
                    let scope = self.scopes.last_mut().unwrap();
                    let conflicts = scope.merge_used(result);
                    for key in conflicts {
                        self.error_at(&span, format!("duplicate member `{}`", key));
                    }
                    for node in check_nodes {
                        self.emit_check_node(node);
                    }
                } else {
                    self.process_mod_decl(mod_s, deco_param_defs, with_clauses, where_clauses);
                }
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
        let mut params = HashSet::new();
        for (name, val_id) in deco_param_defs {
            let span = self.val(*val_id).1.clone();
            let names = self.make_item_name(name.clone());
            let item = Item::param(names, *val_id, span);
            let id = self.alloc_item(item);
            params.insert(id);
            self.define(name.clone(), id);
        }
        self.deco_param_stack.push(params);
    }

    fn exit_inner_scope(&mut self) {
        self.pop_scope();
        self.deco_param_stack.pop();
    }

    fn is_deco_param(&self, id: ItemId) -> bool {
        self.deco_param_stack.iter().any(|s| s.contains(&id))
    }

    fn has_deco_params(&self) -> bool {
        self.deco_param_stack.iter().any(|s| !s.is_empty())
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
    ) -> (Module, Vec<CheckNode>) {
        if with_clauses.is_empty() {
            return (result, Vec::new());
        }
        // Make body members visible in with clauses via merge_used
        let scope = self.scopes.last_mut().unwrap();
        let _ = scope.merge_used(result.clone());
        let mut all_check_nodes = Vec::new();
        for with_mod in with_clauses {
            let span = with_mod.1.clone();
            let (with_members, check_nodes) = self.resolve_module(with_mod);
            let conflicts = result.merge(with_members);
            for key in conflicts {
                self.error_at(&span, format!("duplicate member `{}`", key));
            }
            all_check_nodes.extend(check_nodes);
        }
        // Clean up: finalize_used will move the merged entries to internal
        let scope = self.scopes.last_mut().unwrap();
        scope.finalize_used();
        (result, all_check_nodes)
    }

    // --- Registration ---

    fn register_path(&mut self, segs: &[(String, TokenSpan)], item_id: ItemId) {
        assert!(!segs.is_empty(), "register_path: segs must not be empty");
        match segs.len() {
            1 => {
                // Forward ref already defined in outer scope; replace with real item
                let scope = self.scopes.last_mut().unwrap();
                scope.replace(&segs[0].0, item_id);
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
        let seg_names_list: Vec<Vec<(String, TokenSpan)>> = names
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
        for seg_names in &seg_names_list {
            if seg_names.len() > 1 {
                let prefix: Vec<_> = seg_names[..seg_names.len() - 1]
                    .iter()
                    .map(|(name, span)| (name.as_str(), span))
                    .collect();
                self.resolve_segments(&prefix);
            }
        }

        // --- Extract first_lname early (before seg_names_list is consumed) ---
        let first_lname = seg_names_list.first()
            .and_then(|sns| sns.last())
            .map(|(n, _)| n.clone())
            .unwrap();

        // --- += pre-check (before inner scope) ---
        let is_add = matches!(&op.0, semtree::AssignOp::Add);
        if is_add {
            for seg_names in &seg_names_list {
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

        // --- Forward refs (outer scope, before inner scope) ---
        // Functor app lines (has_applicand) don't define new names.
        if !is_add {
            for (name_s, seg_names) in names.iter().zip(&seg_names_list) {
                let has_applicand = name_s.0 .1.is_some();
                if !has_applicand && seg_names.len() == 1 {
                    let (name, span) = &seg_names[0];
                    let names = self.make_item_name(name.clone());
                    let item = Item::new(names, kind, span.clone());
                    let id = self.alloc_item(item);
                    if !self.define(name.clone(), id) {
                        self.error_at(span, format!("duplicate definition `{}`", name));
                    }
                }
            }
        }

        // --- Inner scope (before params so params are defined incrementally) ---
        self.enter_inner_scope(&deco_param_defs);

        // --- Consume names → params + name_infos ---
        // Params are defined immediately so subsequent params can reference earlier ones.
        let mut params = Vec::new();
        let mut name_infos: Vec<NameInfo> = Vec::new();
        for (i, (name_s, seg_names)) in names.into_iter().zip(seg_names_list).enumerate() {
            let S(pd, _) = name_s;
            let semtree::Path(segs, applicand) = pd;
            for seg_s in segs {
                let S(seg, _) = seg_s;
                for param_decl in seg.1 .0 {
                    let resolved_ty = param_decl.ty.resolve(self);
                    if i == 0 {
                        for name in param_decl.names {
                            let param = Param {
                                name: name.0,
                                ty: resolved_ty,
                            };
                            let span = self.val(param.ty).1.clone();
                            let names = self.make_item_name(param.name.clone());
                            let item = Item::param(names, param.ty, span);
                            let id = self.alloc_item(item);
                            self.define(param.name.clone(), id);
                            params.push(param);
                        }
                    }
                }
            }
            name_infos.push(NameInfo {
                seg_names,
                applicand,
            });
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
        } else if self.has_deco_params() {
            self.error_at(&op.1, "decorator parameters require a functor application");
        }

        // --- Resolve body (owned) ---
        let body_val_resolved = body_val.map(|v| v.resolve(self));

        // Push prefix for body module / with clause resolution
        let has_body_or_with = body_mod.is_some() || !with_clauses.is_empty();
        if has_body_or_with {
            let parent_qname = self.make_item_name(first_lname.clone()).qname;
            self.prefix_stack.push(parent_qname);
        }

        let (mut body_members, body_check_nodes) = match body_mod {
            Some(m) => self.resolve_module(m),
            None => (Module::new(), Vec::new()),
        };

        // If body is a path to a module, inherit its members
        // Note: body_check_nodes is NOT generated here because check's try_module_alias
        // handles instantiation (including substitution for parametric modules).
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
        let (result, with_check_nodes) = self.merge_with_clauses(body_members, with_clauses);

        if has_body_or_with {
            self.prefix_stack.pop();
        }

        // Resolve functor applicands before popping scope (deco params must be visible)
        let resolved_apps = if has_functor_app {
            let mapping_params: Vec<Param> = deco_param_defs
                .iter()
                .map(|(name, val_id)| Param {
                    name: name.clone(),
                    ty: *val_id,
                })
                .collect();
            let apps: Vec<Option<ValId>> = name_infos
                .iter_mut()
                .map(|ni| {
                    ni.applicand.take().map(|applicand| {
                        let fspan = &ni.seg_names[0].1;
                        self.used_deco_params.clear();
                        assert!(!self.in_applicand);
                        self.in_applicand = true;
                        let app_resolved = applicand.resolve(self);
                        self.in_applicand = false;
                        let all_deco_params: HashSet<ItemId> = self.deco_param_stack.iter().flatten().copied().collect();
                        for dp in &all_deco_params {
                            if !self.used_deco_params.contains(dp) {
                                self.error_at(
                                    fspan,
                                    "decorator parameter must appear in functor applicand",
                                );
                                break;
                            }
                        }
                        app_resolved
                    })
                })
                .collect();
            Some((apps, mapping_params))
        } else {
            None
        };

        self.exit_inner_scope();

        // --- Registration ---
        if let Some((resolved_apps, mapping_params)) = resolved_apps {
            self.register_functor_app(name_infos, body_val_resolved, mapping_params, resolved_apps);
        } else if is_add {
            let all_seg_names: Vec<&[(String, TokenSpan)]> =
                name_infos.iter().map(|ni| ni.seg_names.as_slice()).collect();
            let (members_to_merge, merge_check_nodes) = match body_val_resolved {
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
                            let module = self.lookup_path(&path_names)
                                .and_then(|id| self.item(id).members())
                                .cloned()
                                .unwrap_or_else(Module::new);
                            let cn = self.generate_check_nodes_for_module(&module);
                            (module, cn)
                        }
                        _ => {
                            self.error_at(&span, "`+=` requires a path or module body");
                            (Module::new(), Vec::new())
                        }
                    }
                }
                None => {
                    let mut cn = body_check_nodes;
                    cn.extend(with_check_nodes);
                    (result, cn)
                }
            };
            // Emit Scope CheckNodes for += targets
            for seg_names in &all_seg_names {
                if let Some((name, _)) = seg_names.first() {
                    if let Some(target_id) = self.lookup(name) {
                        self.emit_check_node(CheckNode::Scope {
                            item_id: target_id,
                            children: merge_check_nodes.clone(),
                        });
                    }
                }
            }
            for seg_names in &all_seg_names {
                self.merge_into_path(seg_names, members_to_merge.clone());
            }
        } else {
            let is_functor = ty_resolved
                .map_or(false, |id| is_functor_type(&self.val(id).0));
            let has_members = !result.entries.is_empty() || !result.internal.is_empty();
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
                .unwrap();
            let names = self.make_item_name(first_lname.clone());
            let item = Item {
                names,
                span,
                kind,
                ty: ty_resolved,
                params,
                body,
                decos: deco_vals,
                origin: None,
            };
            let item_id = self.alloc_item(item);
            for ni in &name_infos {
                self.register_path(&ni.seg_names, item_id);
            }
            // Emit CheckNodes
            self.emit_check_node(CheckNode::Item(item_id));
            if has_members && !is_functor {
                let mut children = body_check_nodes;
                children.extend(with_check_nodes);
                self.emit_check_node(CheckNode::Scope { item_id, children });
            }
        }
    }

    fn register_functor_app(
        &mut self,
        name_infos: Vec<NameInfo>,
        body_val_resolved: Option<ValId>,
        mapping_params: Vec<Param>,
        resolved_apps: Vec<Option<ValId>>,
    ) {
        for (ni, app_resolved) in name_infos.into_iter().zip(resolved_apps) {
            if let Some(app_resolved) = app_resolved {
                let (fname, fspan) = (&ni.seg_names[0].0, &ni.seg_names[0].1);
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
                                        params: mapping_params.clone(),
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

        let (result, check_nodes) = self.resolve_module(mod_s);
        let (result, with_check_nodes) = self.merge_with_clauses(result, with_clauses);

        self.exit_inner_scope();

        // Mod → promote to current scope
        let scope = self.scopes.last_mut().unwrap();
        let conflicts = scope.merge(result);
        for key in conflicts {
            self.error_at(&span, format!("duplicate member `{}`", key));
        }
        // Emit check nodes for promoted entries
        for node in check_nodes.into_iter().chain(with_check_nodes) {
            self.emit_check_node(node);
        }
    }
}

fn builtin_source(name: &str) -> Option<&'static str> {
    match name {
        "base" => Some(include_str!("builtins/base.donut")),
        "ui" => Some(include_str!("builtins/ui.donut")),
        "sys" => Some(include_str!("builtins/sys.donut")),
        _ => None,
    }
}

pub fn resolve(program: semtree::Program, tokens: &[Token]) -> (Program, Vec<Error>) {
    resolve_with_sources(program, tokens, HashMap::new())
}

pub fn resolve_with_sources(
    program: semtree::Program,
    tokens: &[Token],
    extra_sources: HashMap<String, String>,
) -> (Program, Vec<Error>) {
    let mut checker = Checker::new(tokens);
    checker.extra_sources = extra_sources;
    let (module, check_order) = checker.resolve_decls(program.0);
    let prog = Program {
        root: module,
        items: checker.items,
        vals: checker.vals,
        check_order,
    };
    (prog, checker.errors)
}
