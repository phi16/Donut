use crate::types::common::*;
use crate::types::item::*;
use crate::types::semtree;
use crate::types::token::Token;
use std::collections::{HashMap, HashSet};

// --- Number path detection ---

fn try_path_as_number(path: &semtree::Path<semtree::ParamVal>) -> Option<String> {
    if path.1.is_some() {
        return None; // has applicand
    }
    let mut parts = Vec::new();
    for seg_s in &path.0 {
        let S(seg, _) = seg_s;
        if !seg.1.0.is_empty() {
            return None; // has params
        }
        let name = &seg.0.0;
        if !crate::convert::is_number_str(name) {
            return None;
        }
        parts.push(name.as_str());
    }
    if parts.len() <= 1 {
        return None; // single segment already handled by convert
    }
    Some(parts.join("."))
}

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

// --- Resolve trait implementations (Val types) ---

impl Resolve for S<semtree::Val> {
    type Output = ValId;
    fn resolve(self, ctx: &mut Checker) -> ValId {
        let S(v, span) = self;
        let val = v.resolve(ctx);
        ctx.alloc_val(val, span)
    }
}

impl Resolve for semtree::Val {
    type Output = Val;
    fn resolve(self, ctx: &mut Checker) -> Val {
        match self {
            semtree::Val::Path(path_s) => {
                if let Some(num) = try_path_as_number(&path_s.0) {
                    return Val::Lit(Lit::Number(num));
                }
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
        let name_spans: Vec<(&str, &TokenSpan)> = path
            .0
            .iter()
            .map(|seg_s| {
                let S(seg, span) = seg_s;
                (seg.0.0.as_str(), span)
            })
            .collect();
        let (segments, resolved) = ctx.resolve_segments(&name_spans);

        // Flatten param args from all segments
        let mut args: Vec<ValId> = Vec::new();
        for seg_s in path.0 {
            let S(seg, _) = seg_s;
            for pv in seg.1.0 {
                args.push(pv.val.resolve(ctx));
            }
        }

        let applicand = path.1.map(|v| v.resolve(ctx));

        // Use a dummy Ref if unresolved (error already reported)
        let target = resolved.unwrap_or(Ref::Def(DefId(0)));

        Path {
            segments,
            target,
            args,
            applicand,
        }
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
                let items: Vec<ValId> = vs.into_iter().map(|v| v.resolve(ctx)).collect();
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

// --- Scope (for name resolution during processing) ---

struct Scope {
    /// All names in scope (for lookup).
    names: HashMap<String, Ref>,
    /// Entries defined in this scope, in order.
    entries: Vec<(String, Ref)>,
    /// Names added via `use` — visible for resolution but not exported.
    used: HashSet<String>,
}

impl Scope {
    fn new() -> Self {
        Scope {
            names: HashMap::new(),
            entries: Vec::new(),
            used: HashSet::new(),
        }
    }

    fn define(&mut self, name: String, r: Ref) -> Option<Ref> {
        if let Some(&existing) = self.names.get(&name) {
            Some(existing)
        } else {
            self.names.insert(name.clone(), r);
            self.entries.push((name, r));
            None
        }
    }

    fn define_used(&mut self, name: String, r: Ref) -> Option<Ref> {
        if let Some(&existing) = self.names.get(&name) {
            if existing != r {
                return Some(existing);
            }
            None
        } else {
            self.used.insert(name.clone());
            self.names.insert(name.clone(), r);
            self.entries.push((name, r));
            None
        }
    }

    fn replace(&mut self, name: &str, new_r: Ref) {
        if let Some(r) = self.names.get_mut(name) {
            *r = new_r;
        }
        for e in &mut self.entries {
            if e.0 == name {
                e.1 = new_r;
                break;
            }
        }
    }

    fn get(&self, name: &str) -> Option<Ref> {
        self.names.get(name).copied()
    }

    /// Build an output Module from non-used Def entries.
    fn to_module(&self) -> Module {
        let mut module = Module::new();
        for (name, r) in &self.entries {
            if !self.used.contains(name) {
                if let Ref::Def(def_id) = r {
                    module.define(name.clone(), *def_id);
                }
            }
        }
        module
    }

    /// Merge all entries from a Module as used (for `use` imports).
    fn merge_used(&mut self, module: &Module, defs: &[Def]) -> Vec<String> {
        let mut conflicts = Vec::new();
        for &def_id in &module.entries {
            let lname = defs[def_id.0].lname.clone();
            let r = Ref::Def(def_id);
            if let Some(_) = self.define_used(lname.clone(), r) {
                conflicts.push(lname);
            }
        }
        conflicts
    }

    /// Merge entries from a Module (for `with` clauses, `+=`, etc.).
    fn merge(&mut self, module: &Module, defs: &[Def]) -> Vec<String> {
        let mut conflicts = Vec::new();
        for &def_id in &module.entries {
            let lname = defs[def_id.0].lname.clone();
            let r = Ref::Def(def_id);
            if let Some(_) = self.define(lname.clone(), r) {
                conflicts.push(lname);
            }
        }
        conflicts
    }
}

// --- ItemKind from syntax ---

fn item_kind_from_op(op: &semtree::AssignOp, has_body: bool) -> Option<ItemKind> {
    match op {
        semtree::AssignOp::Decl => Some(ItemKind::Decl),
        semtree::AssignOp::DeclDef if has_body => Some(ItemKind::DeclDef),
        semtree::AssignOp::DeclDef => Some(ItemKind::Decl),
        semtree::AssignOp::Alias => None,
        semtree::AssignOp::Add => None,
    }
}

// --- Checker ---

struct Checker<'a> {
    tokens: &'a [Token<'a>],

    // Arenas
    items: Vec<Item>,
    defs: Vec<Def>,
    vals: Vec<S<Val>>,

    // Item deduplication (cname → ItemId)
    item_by_cname: HashMap<String, ItemId>,

    // Scopes
    scopes: Vec<Scope>,
    errors: Vec<Error>,

    // Decorator param tracking
    deco_param_stack: Vec<HashSet<ItemId>>,
    used_deco_params_in_applicand: Option<HashSet<ItemId>>,

    // Name building
    /// Prefix stack for qnames (use-site qualified names).
    /// NOT cleared during imports — qnames include the outer context.
    prefix_stack: Vec<String>,
    /// Prefix stack for cnames (canonical names within the current module).
    /// Cleared during imports — cnames use origin:: prefix instead.
    cname_prefix_stack: Vec<String>,
    /// Parameter counts at each module nesting level.
    param_count_stack: Vec<usize>,
    /// Origin name for items resolved inside an import (e.g. "sys").
    current_origin: Option<String>,

    // Extra import sources (for testing).
    extra_sources: HashMap<String, String>,

    // DefTree generation
    def_order_stack: Vec<Vec<DefTree>>,
}

impl<'a> Checker<'a> {
    fn new(tokens: &'a [Token<'a>]) -> Self {
        let mut checker = Checker {
            tokens,
            items: Vec::new(),
            defs: Vec::new(),
            vals: Vec::new(),
            item_by_cname: HashMap::new(),
            scopes: Vec::new(),
            errors: Vec::new(),
            deco_param_stack: Vec::new(),
            used_deco_params_in_applicand: None,
            prefix_stack: Vec::new(),
            cname_prefix_stack: Vec::new(),
            param_count_stack: Vec::new(),
            current_origin: None,
            extra_sources: HashMap::new(),
            def_order_stack: Vec::new(),
        };
        checker.register_builtins();
        checker
    }

    fn register_builtins(&mut self) {
        // meta: meta (self-referential type)
        // We know meta will be ItemId(0), so we can create the self-referencing Val
        let meta_ty_val = self.alloc_val(
            Val::Path(Path {
                segments: vec![],
                target: Ref::Item(ItemId(0)),
                args: vec![],
                applicand: None,
            }),
            TokenSpan { start: 0, end: 0 },
        );
        let meta_id = self.alloc_item(Item {
            cname: "meta".into(),
            lname: "meta".into(),
            kind: ItemKind::Decl,
            ty: meta_ty_val,
            params: vec![],
        });
        debug_assert_eq!(meta_id, ItemId(0));

        // *: meta
        let star_ty_val = self.alloc_val(
            Val::Path(Path {
                segments: vec![],
                target: Ref::Item(meta_id),
                args: vec![],
                applicand: None,
            }),
            TokenSpan { start: 0, end: 0 },
        );
        let star_id = self.alloc_item(Item {
            cname: "*".into(),
            lname: "*".into(),
            kind: ItemKind::Decl,
            ty: star_ty_val,
            params: vec![],
        });

        // Register in initial scope
        self.push_scope();
        self.define("meta".into(), Ref::Item(meta_id));
        self.define("*".into(), Ref::Item(star_id));
    }

    // --- Arena allocators ---

    fn alloc_val(&mut self, val: Val, span: TokenSpan) -> ValId {
        let id = ValId(self.vals.len());
        self.vals.push(S(val, span));
        id
    }

    fn alloc_item(&mut self, item: Item) -> ItemId {
        // Dedup by cname: reuse existing Item if same cname
        if let Some(&existing) = self.item_by_cname.get(&item.cname) {
            return existing;
        }
        let id = ItemId(self.items.len());
        self.item_by_cname.insert(item.cname.clone(), id);
        self.items.push(item);
        id
    }

    fn alloc_def(&mut self, def: Def) -> DefId {
        let id = DefId(self.defs.len());
        self.defs.push(def);
        id
    }

    fn def(&self, id: DefId) -> &Def {
        &self.defs[id.0]
    }

    fn def_mut(&mut self, id: DefId) -> &mut Def {
        &mut self.defs[id.0]
    }

    #[allow(dead_code)]
    fn val(&self, id: ValId) -> &Val {
        &self.vals[id.0].0
    }

    fn val_span(&self, id: ValId) -> &TokenSpan {
        &self.vals[id.0].1
    }

    // --- Deep clone for module instantiation ---

    fn deep_clone_module(&mut self, source: &Module, mapping: &HashMap<ItemId, ValId>) -> Module {
        let mut new_module = Module::new();
        for &def_id in &source.entries {
            let new_def_id = self.deep_clone_def(def_id, mapping);
            let lname = self.def(new_def_id).lname.clone();
            new_module.define(lname, new_def_id);
        }
        new_module
    }

    fn wrap_body_with_subst(
        &mut self,
        def_id: DefId,
        mapping: &HashMap<ItemId, ValId>,
        span: &TokenSpan,
    ) -> DefBody {
        match &self.def(def_id).body {
            DefBody::Decl { item, .. } => {
                let item = *item;
                let s = span.clone();
                let path_val = self.alloc_val(
                    Val::Path(Path {
                        segments: vec![],
                        target: Ref::Item(item),
                        args: vec![],
                        applicand: None,
                    }),
                    s.clone(),
                );
                DefBody::Alias {
                    val: self.alloc_val(Val::Subst(path_val, mapping.clone()), s),
                }
            }
            DefBody::Alias { val } => {
                let val = *val;
                let s = self.val_span(val).clone();
                DefBody::Alias {
                    val: self.alloc_val(Val::Subst(val, mapping.clone()), s),
                }
            }
            DefBody::Functor { mappings } => {
                let orig_mappings: Vec<_> = mappings
                    .iter()
                    .map(|m| (m.params.clone(), m.applicand, m.val))
                    .collect();
                DefBody::Functor {
                    mappings: orig_mappings
                        .into_iter()
                        .map(|(params, applicand, val)| {
                            let app_s = self.val_span(applicand).clone();
                            let val_s = self.val_span(val).clone();
                            FunctorMapping {
                                params,
                                applicand: self
                                    .alloc_val(Val::Subst(applicand, mapping.clone()), app_s),
                                val: self.alloc_val(Val::Subst(val, mapping.clone()), val_s),
                            }
                        })
                        .collect(),
                }
            }
            DefBody::None => DefBody::None,
        }
    }

    fn deep_clone_def(&mut self, def_id: DefId, mapping: &HashMap<ItemId, ValId>) -> DefId {
        let original = self.def(def_id);
        let qname = original.qname.clone();
        let lname = original.lname.clone();
        let span = original.span.clone();
        let origin = original.origin.clone();
        let param_counts = original.param_counts.clone();
        let params = original.params.clone();
        let decos = original.decos.clone();
        let original_ty = original.ty;
        let original_members = original.members.clone();

        // Wrap ty in Val::Subst
        let new_ty = original_ty.map(|ty_id| {
            let s = self.val_span(ty_id).clone();
            self.alloc_val(Val::Subst(ty_id, mapping.clone()), s)
        });

        // Build new body with Val::Subst wrapping
        // Decl → Alias(Subst(Path(Item), mapping)): reuses original PrimId with substituted boundaries
        // Alias → Alias(Subst(val, mapping))
        let new_body = self.wrap_body_with_subst(def_id, mapping, &span);

        // Recursively deep clone members
        let new_members = self.deep_clone_module(&original_members, mapping);

        let new_def = Def {
            qname,
            lname,
            span,
            ty: new_ty,
            params,
            body: new_body,
            members: new_members,
            decos,
            origin,
            param_counts,
        };
        self.alloc_def(new_def)
    }

    // --- DefTree generation ---

    fn emit_def_tree(&mut self, tree: DefTree) {
        if let Some(top) = self.def_order_stack.last_mut() {
            top.push(tree);
        }
    }

    fn emit_def_with_scope(&mut self, def_id: DefId, before: Vec<DefTree>, after: Vec<DefTree>) {
        self.emit_def_tree(DefTree { def_id, before, after });
    }

    fn emit_module_as_trees(&mut self, module: &Module) {
        for &def_id in &module.entries {
            let children_module = self.def(def_id).members.clone();
            let mut after = Vec::new();
            for &child_id in &children_module.entries {
                let grandchildren_module = self.def(child_id).members.clone();
                after.push(DefTree {
                    def_id: child_id,
                    before: vec![],
                    after: self.collect_module_trees(&grandchildren_module),
                });
            }
            self.emit_def_tree(DefTree { def_id, before: vec![], after });
        }
    }

    fn collect_module_trees(&self, module: &Module) -> Vec<DefTree> {
        let mut trees = Vec::new();
        for &def_id in &module.entries {
            let after = self.collect_module_trees(&self.def(def_id).members);
            trees.push(DefTree { def_id, before: vec![], after });
        }
        trees
    }

    // --- Name building ---

    fn current_param_counts(&self, own_count: usize) -> Vec<usize> {
        let mut counts = self.param_count_stack.clone();
        if own_count > 0 {
            counts.push(own_count);
        }
        counts
    }

    /// Build a use-site qualified name from a local name.
    fn make_qname(&self, lname: &str) -> String {
        if let Some(prefix) = self.prefix_stack.last() {
            format!("{}.{}", prefix, lname)
        } else {
            lname.to_string()
        }
    }

    /// Build a canonical name (for Items) from a local name.
    fn make_cname(&self, lname: &str) -> String {
        let local_path = if let Some(prefix) = self.cname_prefix_stack.last() {
            format!("{}.{}", prefix, lname)
        } else {
            lname.to_string()
        };
        match &self.current_origin {
            Some(origin) => format!("{}::{}", origin, local_path),
            None => local_path,
        }
    }

    /// Build a canonical name for a parameter Item.
    fn make_param_cname(&self, parent_lname: &str, param_name: &str) -> String {
        let parent_cname = self.make_cname(parent_lname);
        format!("{}#{}", parent_cname, param_name)
    }

    // --- Error reporting ---

    fn error_at(&mut self, span: &TokenSpan, msg: impl Into<String>) {
        if let Some(token) = self.tokens.get(span.start) {
            self.errors.push((token.pos.clone(), msg.into()));
        }
    }

    // --- Scope management ---

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn pop_scope(&mut self) -> Module {
        let scope = self.scopes.pop().unwrap();
        scope.to_module()
    }

    fn define(&mut self, name: String, r: Ref) -> bool {
        let scope = self.scopes.last_mut().unwrap();
        scope.define(name, r).is_none()
    }

    fn lookup(&self, name: &str) -> Option<Ref> {
        for scope in self.scopes.iter().rev() {
            if let Some(r) = scope.get(name) {
                return Some(r);
            }
        }
        None
    }

    /// Look up a Ref and get the DefId, if it's a Def.
    fn lookup_def(&self, name: &str) -> Option<DefId> {
        match self.lookup(name)? {
            Ref::Def(id) => Some(id),
            Ref::Item(_) => None,
        }
    }

    // --- Comp flat helpers ---

    fn resolve_comp_flat(
        &mut self,
        val_s: S<semtree::Val>,
        axis: donut_core::common::Axis,
        out: &mut Vec<ValId>,
    ) {
        match val_s {
            S(semtree::Val::Op(l, op_s, _, r), _) if matches!(&op_s.0, semtree::Op::Comp(n) if *n == axis) =>
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
            S(semtree::Val::Op(l, op_s, _, r), _) if matches!(&op_s.0, semtree::Op::CompStar) => {
                self.resolve_comp_star_flat(*l, out);
                self.resolve_comp_star_flat(*r, out);
            }
            other => {
                out.push(other.resolve(self));
            }
        }
    }

    // --- Name resolution ---

    /// Resolve a dotted path segment-by-segment.
    /// Returns the per-segment Refs (for Path.segments) and the final Ref.
    fn resolve_segments(&mut self, segments: &[(&str, &TokenSpan)]) -> (Vec<Ref>, Option<Ref>) {
        let Some((&(first_name, first_span), rest)) = segments.split_first() else {
            return (vec![], None);
        };
        let Some(first_ref) = self.lookup(first_name) else {
            if !crate::convert::is_number_str(first_name) {
                self.error_at(first_span, format!("undefined name `{}`", first_name));
            }
            return (vec![], None);
        };
        // Track deco param usage in applicand context
        if let Ref::Item(item_id) = first_ref {
            if let Some(ref mut used) = self.used_deco_params_in_applicand {
                if self.deco_param_stack.iter().any(|s| s.contains(&item_id)) {
                    used.insert(item_id);
                }
            }
        }
        // Follow member path, collecting per-segment refs
        let mut seg_refs = vec![first_ref];
        let mut current_ref = first_ref;
        for &(name, span) in rest {
            let current_def = match current_ref {
                Ref::Def(id) => Some(id),
                Ref::Item(_) => None,
            };
            let next = current_def.and_then(|id| self.def(id).members.get(name));
            match next {
                Some(id) => {
                    current_ref = Ref::Def(id);
                    seg_refs.push(current_ref);
                }
                None => {
                    self.error_at(span, format!("undefined member `{}`", name));
                    return (seg_refs, None);
                }
            }
        }
        (seg_refs, Some(current_ref))
    }

    fn lookup_path(&self, names: &[impl AsRef<str>]) -> Option<DefId> {
        let (first, rest) = names.split_first()?;
        let mut current = self.lookup_def(first.as_ref())?;
        for name in rest {
            current = self.def(current).members.get(name.as_ref())?;
        }
        Some(current)
    }

    // --- Decorators ---

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

    // --- Inner scope (decorator params) ---

    fn enter_inner_scope(&mut self, first_lname: &str, deco_param_defs: &[(String, ValId)]) {
        self.push_scope();
        self.def_order_stack.push(Vec::new());
        let mut param_items = HashSet::new();
        for (name, val_id) in deco_param_defs {
            // Create Item for deco param
            let cname = self.make_param_cname(first_lname, &format!("@{}", name));
            let item = Item {
                cname,
                lname: name.clone(),
                kind: ItemKind::Param,
                ty: *val_id,
                params: vec![],
            };
            let item_id = self.alloc_item(item);
            param_items.insert(item_id);
            self.define(name.clone(), Ref::Item(item_id));
        }
        self.deco_param_stack.push(param_items);
    }

    fn exit_inner_scope(&mut self) -> Vec<DefTree> {
        self.pop_scope();
        self.deco_param_stack.pop();
        self.def_order_stack.pop().unwrap_or_default()
    }

    fn has_deco_params(&self) -> bool {
        self.deco_param_stack.iter().any(|s| !s.is_empty())
    }

    // --- Where/With clauses ---

    /// Resolve where clauses. Returns the DefTrees they produced,
    /// which must be placed in the parent def's `before` list.
    fn resolve_where_clauses(&mut self, where_clauses: Vec<S<semtree::Module>>) -> Vec<DefTree> {
        let before_len = self.def_order_stack.last().map_or(0, |v| v.len());
        for wm in where_clauses.into_iter().rev() {
            let S(module, _) = wm;
            if let semtree::Module::Block(decls) = module {
                for d in decls {
                    self.resolve_decl(d);
                }
            }
        }
        if let Some(stack) = self.def_order_stack.last_mut() {
            stack.split_off(before_len)
        } else {
            vec![]
        }
    }

    fn merge_with_clauses(
        &mut self,
        mut result: Module,
        with_clauses: Vec<S<semtree::Module>>,
    ) -> Module {
        if with_clauses.is_empty() {
            return result;
        }
        // Make body members visible in with clauses via merge_used
        {
            let scope = self.scopes.last_mut().unwrap();
            scope.merge_used(&result, &self.defs);
        }
        for with_mod in with_clauses {
            let span = with_mod.1.clone();
            let with_members = self.resolve_module(with_mod);
            let conflicts = result.merge_from(&with_members, &self.defs);
            for key in conflicts {
                self.error_at(&span, format!("duplicate member `{}`", key));
            }
        }
        result
    }

    // --- Module / Import resolution ---

    fn resolve_decls(&mut self, decls: Vec<semtree::Decl>) -> Module {
        self.push_scope();
        for d in decls {
            self.resolve_decl(d);
        }
        self.pop_scope()
    }

    fn resolve_decls_root(&mut self, decls: Vec<semtree::Decl>) -> (Module, Vec<DefTree>) {
        self.push_scope();
        self.def_order_stack.push(Vec::new());
        for d in decls {
            self.resolve_decl(d);
        }
        let def_order = self.def_order_stack.pop().unwrap();
        let module = self.pop_scope();
        (module, def_order)
    }

    fn resolve_module(&mut self, mod_s: S<semtree::Module>) -> Module {
        let S(module, span) = mod_s;
        match module {
            semtree::Module::Block(decls) => self.resolve_decls(decls),
            semtree::Module::Import(lit_s) | semtree::Module::Use(lit_s) => {
                let name = match &lit_s.0 {
                    semtree::Lit::String(s) => s.trim_matches('"').to_string(),
                    _ => {
                        self.error_at(&span, "import requires a string literal");
                        return Module::new();
                    }
                };
                let source = builtin_source(&name)
                    .map(|s| s.to_string())
                    .or_else(|| self.extra_sources.get(&name).cloned());
                match source {
                    Some(source) => {
                        let old_origin = self.current_origin.take();
                        let old_cname_prefix = std::mem::take(&mut self.cname_prefix_stack);
                        let old_param_counts = std::mem::take(&mut self.param_count_stack);
                        // prefix_stack is NOT cleared — qnames include outer context
                        self.current_origin = Some(name.clone());
                        let module = self.resolve_import(&source);
                        self.current_origin = old_origin;
                        self.cname_prefix_stack = old_cname_prefix;
                        self.param_count_stack = old_param_counts;
                        module
                    }
                    None => {
                        self.error_at(&span, format!("unknown import: \"{}\"", name));
                        Module::new()
                    }
                }
            }
        }
    }

    fn resolve_import(&mut self, source: &str) -> Module {
        let (tokens, _, _) = crate::tokenize::tokenize(source);
        let (program, _) = crate::parse::parse(&tokens);
        let (sem_prog, _) = crate::convert::convert(program, &tokens);
        self.resolve_decls(sem_prog.0)
    }

    // --- Declaration processing ---

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
                self.process_unit_decl(
                    unit,
                    deco_param_defs,
                    deco_vals,
                    with_clauses,
                    where_clauses,
                );
            }
            semtree::DeclMain::Mod(mod_s) => {
                if matches!(&mod_s.0, semtree::Module::Use(_)) {
                    let span = mod_s.1.clone();
                    let result = self.resolve_module(mod_s);
                    // DefTrees already emitted by resolve_decls during resolve_module
                    let scope = self.scopes.last_mut().unwrap();
                    let conflicts = scope.merge_used(&result, &self.defs);
                    for key in conflicts {
                        self.error_at(&span, format!("duplicate member `{}`", key));
                    }
                } else {
                    self.process_mod_decl(mod_s, deco_param_defs, with_clauses, where_clauses);
                }
            }
        }
    }

    fn process_unit_decl(
        &mut self,
        unit: semtree::DeclUnit,
        deco_param_defs: Vec<(String, ValId)>,
        deco_vals: Vec<ValId>,
        with_clauses: Vec<S<semtree::Module>>,
        where_clauses: Vec<S<semtree::Module>>,
    ) {
        let semtree::DeclUnit {
            names,
            ty,
            op,
            body,
        } = unit;

        // Determine if this declaration creates an Item
        let has_body_val = matches!(&body, Some(semtree::ValMod::Val(_)));
        let item_kind = item_kind_from_op(&op.0, has_body_val);

        // Split body into val/mod
        let (body_val, body_mod) = match body {
            Some(semtree::ValMod::Val(v)) => (Some(v), None),
            Some(semtree::ValMod::Mod(m)) => (None, Some(m)),
            None => (None, None),
        };

        // --- Outer scope: extract seg_names ---
        let seg_names_list: Vec<Vec<(String, TokenSpan)>> = names
            .iter()
            .map(|name_s| {
                let S(pd, _) = name_s;
                pd.0.iter()
                    .map(|seg_s| {
                        let S(seg, span) = seg_s;
                        (seg.0.0.clone(), span.clone())
                    })
                    .collect()
            })
            .collect();

        // --- Resolve prefixes ---
        for seg_names in &seg_names_list {
            if seg_names.len() > 1 {
                let prefix: Vec<_> = seg_names[..seg_names.len() - 1]
                    .iter()
                    .map(|(name, span)| (name.as_str(), span))
                    .collect();
                self.resolve_segments(&prefix);
            }
        }

        let first_lname = seg_names_list
            .first()
            .and_then(|sns| sns.last())
            .map(|(n, _)| n.clone())
            .unwrap();

        // --- += pre-check ---
        let is_add = matches!(&op.0, semtree::AssignOp::Add);
        if is_add {
            for seg_names in &seg_names_list {
                if seg_names.len() == 1 {
                    let (name, span) = &seg_names[0];
                    if self.lookup(name).is_none() {
                        self.error_at(span, format!("`{}` must be declared before `+=`", name));
                    }
                }
            }
        }

        // --- Forward refs (outer scope, before inner scope) ---
        if !is_add {
            for (name_s, seg_names) in names.iter().zip(&seg_names_list) {
                let has_applicand = name_s.0.1.is_some();
                if !has_applicand && seg_names.len() == 1 {
                    let (name, span) = &seg_names[0];
                    let qname = self.make_qname(name);
                    let def = Def {
                        qname,
                        lname: name.clone(),
                        span: span.clone(),
                        ty: None,
                        params: vec![],
                        body: DefBody::None,
                        members: Module::new(),
                        decos: vec![],
                        origin: self.current_origin.clone(),
                        param_counts: vec![],
                    };
                    let id = self.alloc_def(def);
                    if !self.define(name.clone(), Ref::Def(id)) {
                        self.error_at(span, format!("duplicate definition `{}`", name));
                    }
                }
            }
        }

        // --- Inner scope ---
        self.enter_inner_scope(&first_lname, &deco_param_defs);

        // --- Extract params (defined incrementally) ---
        let mut params = Vec::new();
        let mut name_infos: Vec<NameInfo> = Vec::new();
        for (i, (name_s, seg_names)) in names.into_iter().zip(seg_names_list).enumerate() {
            let S(pd, _) = name_s;
            let semtree::Path(segs, applicand) = pd;
            for seg_s in segs {
                let S(seg, _) = seg_s;
                for param_decl in seg.1.0 {
                    let resolved_ty = param_decl.ty.resolve(self);
                    if i == 0 {
                        for name in param_decl.names {
                            // Create Item for parameter
                            let param_cname = self.make_param_cname(&first_lname, &name.0);
                            let item_id = self.alloc_item(Item {
                                cname: param_cname,
                                lname: name.0.clone(),
                                kind: ItemKind::Param,
                                ty: resolved_ty,
                                params: vec![],
                            });
                            let param = Param {
                                name: name.0.clone(),
                                ty: resolved_ty,
                                item: item_id,
                            };
                            // Register in scope as Ref::Item
                            self.define(param.name.clone(), Ref::Item(item_id));
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

        // Where clauses — trees emitted before parent def
        let where_trees = self.resolve_where_clauses(where_clauses);

        // --- Resolve type ---
        let ty_resolved = ty.map(|t| t.resolve(self));

        // --- Functor constraints ---
        let has_functor_app = name_infos.iter().any(|ni| ni.applicand.is_some());
        if has_functor_app {
            if !matches!(&op.0, semtree::AssignOp::Alias) {
                self.error_at(&op.1, "functor application only allows `=`");
            }
            if let Some(ref m) = body_mod {
                self.error_at(&m.1, "functor application cannot have a module body");
            }
            for wc in &with_clauses {
                self.error_at(&wc.1, "functor application cannot have `with` clauses");
            }
        } else if self.has_deco_params() {
            self.error_at(&op.1, "decorator parameters require a functor application");
        }

        // --- Resolve body val ---
        let body_val_resolved = body_val.map(|v| v.resolve(self));

        // --- Push prefixes for body module / with clause resolution ---
        let has_body_or_with = body_mod.is_some() || !with_clauses.is_empty();
        if has_body_or_with {
            let parent_qname = self.make_qname(&first_lname);
            self.prefix_stack.push(parent_qname);

            let parent_cname_local = if let Some(cp) = self.cname_prefix_stack.last() {
                format!("{}.{}", cp, first_lname)
            } else {
                first_lname.clone()
            };
            self.cname_prefix_stack.push(parent_cname_local);

            self.param_count_stack.push(params.len());
        }

        let mut body_members = match body_mod {
            Some(m) => self.resolve_module(m),
            None => Module::new(),
        };

        // If body is a path to a module, inherit its members
        if body_members.entries.is_empty() {
            if let Some(val_id) = body_val_resolved {
                if let Val::Path(path) = &self.vals[val_id.0].0 {
                    if let Ref::Def(id) = path.target {
                        let m = &self.def(id).members;
                        if !m.entries.is_empty() {
                            if path.args.is_empty() {
                                body_members = m.clone();
                            } else {
                                // Build substitution mapping: param ItemId → arg ValId
                                let target_params = &self.def(id).params;
                                let mapping: HashMap<ItemId, ValId> = target_params
                                    .iter()
                                    .zip(path.args.iter())
                                    .map(|(param, &arg)| (param.item, arg))
                                    .collect();
                                let m_clone = m.clone();
                                body_members = self.deep_clone_module(&m_clone, &mapping);
                            }
                            // Emit inherited members as DefTrees
                            self.emit_module_as_trees(&body_members);
                        }
                    }
                }
            }
        }

        // With clauses
        let result = self.merge_with_clauses(body_members, with_clauses);

        if has_body_or_with {
            self.prefix_stack.pop();
            self.cname_prefix_stack.pop();
            self.param_count_stack.pop();
        }

        // --- Resolve functor applicands ---
        let resolved_apps = if has_functor_app {
            let mapping_params: Vec<Param> = deco_param_defs
                .iter()
                .map(|(name, val_id)| {
                    let Ref::Item(item) = self.lookup(name).unwrap() else {
                        unreachable!("deco param must be Item")
                    };
                    Param {
                        name: name.clone(),
                        ty: *val_id,
                        item,
                    }
                })
                .collect();
            let apps: Vec<Option<ValId>> = name_infos
                .iter_mut()
                .map(|ni| {
                    ni.applicand.take().map(|applicand| {
                        let fspan = &ni.seg_names[0].1;
                        self.used_deco_params_in_applicand = Some(HashSet::new());
                        let app_resolved = applicand.resolve(self);
                        let used = self.used_deco_params_in_applicand.take().unwrap();
                        let all_deco_params: HashSet<ItemId> =
                            self.deco_param_stack.iter().flatten().copied().collect();
                        for dp in &all_deco_params {
                            if !used.contains(dp) {
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

        let inner_children = self.exit_inner_scope();

        // --- Registration ---
        if let Some((resolved_apps, mapping_params)) = resolved_apps {
            self.register_functor_app(name_infos, body_val_resolved, mapping_params, resolved_apps);
        } else if is_add {
            self.register_add(name_infos, body_val_resolved, result, inner_children);
        } else {
            let span = name_infos
                .first()
                .and_then(|ni| ni.seg_names.last())
                .map(|(_, s)| s.clone())
                .unwrap();
            let is_functor = ty_resolved.map_or(false, |id| is_functor_type(&self.vals[id.0].0));
            let is_decldef = matches!(item_kind, Some(ItemKind::DeclDef));
            let item_id = if let Some(ik) = item_kind {
                let ty_id = if let Some(ty_id) = ty_resolved {
                    Some(ty_id)
                } else if is_decldef {
                    // DeclDef without explicit type: use Hole (inferred from body in check)
                    Some(self.alloc_val(Val::Hole(Hole::Any), span.clone()))
                } else {
                    None
                };
                if let Some(ty_id) = ty_id {
                    let cname = self.make_cname(&first_lname);
                    Some(self.alloc_item(Item {
                        cname,
                        lname: first_lname.clone(),
                        kind: ik,
                        ty: ty_id,
                        params: params.clone(),
                    }))
                } else {
                    None
                }
            } else {
                None
            };
            let body = if is_functor {
                DefBody::Functor {
                    mappings: Vec::new(),
                }
            } else if let Some(item) = item_id {
                DefBody::Decl {
                    item,
                    def_val: if is_decldef { body_val_resolved } else { None },
                }
            } else if let Some(val) = body_val_resolved {
                DefBody::Alias { val }
            } else {
                DefBody::None
            };
            let qname = self.make_qname(&first_lname);
            let param_counts = self.current_param_counts(params.len());
            let def = Def {
                qname,
                lname: first_lname.clone(),
                span: span.clone(),
                ty: ty_resolved,
                params,
                body,
                members: result,
                decos: deco_vals,
                origin: self.current_origin.clone(),
                param_counts,
            };
            // Single-segment names always have a forward-ref Def (created
            // before inner scope / with-clause resolution).  Reuse it so
            // that paths already resolved inside with-clauses keep the
            // same DefId.
            let def_id = if let Some(ni) = name_infos.first().filter(|ni| ni.seg_names.len() == 1) {
                let fwd = match self.lookup(&ni.seg_names[0].0) {
                    Some(Ref::Def(id)) => id,
                    _ => unreachable!("forward-ref Def must exist for single-segment declaration"),
                };
                *self.def_mut(fwd) = def;
                fwd
            } else {
                self.alloc_def(def)
            };

            // --- DeclDef: create member def `x.def : x ~ y` ---
            let mut inner_children = inner_children;
            if let DefBody::Decl { item: main_item_id, def_val: Some(body_val) } =
                &self.defs[def_id.0].body
            {
                let main_item_id = *main_item_id;
                let body_val = *body_val;
                let x_val = self.alloc_val(
                    Val::Path(Path {
                        segments: vec![],
                        target: Ref::Item(main_item_id),
                        args: vec![],
                        applicand: None,
                    }),
                    span.clone(),
                );
                let eq_val = self.alloc_val(
                    Val::Arrow(ArrowKind::Eq, x_val, body_val),
                    span.clone(),
                );
                let def_cname = format!(
                    "{}.{}",
                    self.items[main_item_id.0].cname,
                    DEF_MEMBER_NAME
                );
                let def_item_id = self.alloc_item(Item {
                    cname: def_cname,
                    lname: DEF_MEMBER_NAME.to_string(),
                    kind: ItemKind::Decl,
                    ty: eq_val,
                    params: vec![],
                });
                let def_qname = format!(
                    "{}.{}",
                    self.defs[def_id.0].qname,
                    DEF_MEMBER_NAME
                );
                let mut member_param_counts = self.defs[def_id.0].param_counts.clone();
                member_param_counts.push(0);
                let member_def = Def {
                    qname: def_qname,
                    lname: DEF_MEMBER_NAME.to_string(),
                    span: span.clone(),
                    ty: Some(eq_val),
                    params: vec![],
                    body: DefBody::Decl {
                        item: def_item_id,
                        def_val: None,
                    },
                    members: Module::new(),
                    decos: vec![],
                    origin: self.current_origin.clone(),
                    param_counts: member_param_counts,
                };
                let member_def_id = self.alloc_def(member_def);
                self.def_mut(def_id)
                    .members
                    .define(DEF_MEMBER_NAME.to_string(), member_def_id);
                inner_children.push(DefTree {
                    def_id: member_def_id,
                    before: vec![],
                    after: vec![],
                });
            }

            for ni in &name_infos {
                self.register_path(&ni.seg_names, def_id);
            }
            self.emit_def_with_scope(def_id, where_trees, inner_children);
        }
    }

    // --- Registration helpers ---

    fn register_path(&mut self, segs: &[(String, TokenSpan)], def_id: DefId) {
        assert!(!segs.is_empty(), "register_path: segs must not be empty");
        match segs.len() {
            1 => {
                let scope = self.scopes.last_mut().unwrap();
                scope.replace(&segs[0].0, Ref::Def(def_id));
            }
            _ => self.insert_into_dotted(segs, def_id),
        }
    }

    fn insert_into_dotted(&mut self, segs: &[(String, TokenSpan)], def_id: DefId) {
        if segs.len() < 2 {
            return;
        }
        let first_name = &segs[0].0;
        let (last_name, last_span) = &segs[segs.len() - 1];

        let mut conflict = false;
        if let Some(Ref::Def(mut current_id)) = self.lookup(first_name) {
            for (name, _) in &segs[1..segs.len() - 1] {
                let next = self.def(current_id).members.get(name);
                match next {
                    Some(id) => current_id = id,
                    None => return,
                }
            }
            let members = &mut self.def_mut(current_id).members;
            if members.contains_key(last_name) {
                conflict = true;
            } else {
                members.define(last_name.clone(), def_id);
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
            // Collect lnames first to avoid borrow conflict
            let lnames: Vec<(DefId, String)> = new_members
                .entries
                .iter()
                .map(|&id| (id, self.defs[id.0].lname.clone()))
                .collect();
            let conflicts = if let Some(Ref::Def(id)) = self.lookup(name) {
                let members = &mut self.def_mut(id).members;
                let mut cs = Vec::new();
                for (new_id, lname) in &lnames {
                    if members.define(lname.clone(), *new_id).is_some() {
                        cs.push(lname.clone());
                    }
                }
                cs
            } else {
                Vec::new()
            };
            for key in conflicts {
                self.error_at(span, format!("duplicate member `{}`", key));
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
                let lookup_result = self.lookup_def(fname);
                match lookup_result {
                    Some(id) => {
                        let is_functor = matches!(&self.def(id).body, DefBody::Functor { .. });
                        if is_functor {
                            if let Some(v) = body_val_resolved {
                                if let DefBody::Functor { mappings } = &mut self.def_mut(id).body {
                                    mappings.push(FunctorMapping {
                                        params: mapping_params.clone(),
                                        applicand: app_resolved,
                                        val: v,
                                    });
                                }
                            }
                        } else {
                            self.error_at(fspan, format!("`{}` is not a functor", fname));
                        }
                    }
                    None => {
                        self.error_at(fspan, format!("undefined functor `{}`", fname));
                    }
                }
            }
        }
    }

    fn register_add(
        &mut self,
        name_infos: Vec<NameInfo>,
        body_val_resolved: Option<ValId>,
        result: Module,
        inner_children: Vec<DefTree>,
    ) {
        let all_seg_names: Vec<&[(String, TokenSpan)]> = name_infos
            .iter()
            .map(|ni| ni.seg_names.as_slice())
            .collect();

        let members_to_merge = match body_val_resolved {
            Some(val_id) => {
                let span = self.vals[val_id.0].1.clone();
                match &self.vals[val_id.0].0 {
                    Val::Path(path) => {
                        if let Ref::Def(id) = path.target {
                            self.def(id).members.clone()
                        } else {
                            Module::new()
                        }
                    }
                    _ => {
                        self.error_at(&span, "`+=` requires a path or module body");
                        Module::new()
                    }
                }
            }
            None => result,
        };

        for seg_names in &all_seg_names {
            self.merge_into_path(seg_names, members_to_merge.clone());
        }

        // Emit DefTree for each += target (children only, def itself already emitted)
        if !members_to_merge.entries.is_empty() {
            for seg_names in &all_seg_names {
                let names: Vec<&str> = seg_names.iter().map(|(n, _)| n.as_str()).collect();
                if let Some(def_id) = self.lookup_path(&names) {
                    self.emit_def_tree(DefTree {
                        def_id,
                        before: vec![],
                        after: inner_children.clone(),
                    });
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

        self.enter_inner_scope("", &deco_param_defs);
        let where_trees = self.resolve_where_clauses(where_clauses);

        let result = self.resolve_module(mod_s);
        let result = self.merge_with_clauses(result, with_clauses);

        let inner_children = self.exit_inner_scope();

        // Where trees first, then promote inner children
        for tree in where_trees.into_iter().chain(inner_children) {
            self.emit_def_tree(tree);
        }

        // Mod → promote to current scope
        let scope = self.scopes.last_mut().unwrap();
        let conflicts = scope.merge(&result, &self.defs);
        for key in conflicts {
            self.error_at(&span, format!("duplicate member `{}`", key));
        }
    }
}

// --- Builtin sources ---

fn builtin_source(name: &str) -> Option<&'static str> {
    match name {
        "base" => Some(include_str!("builtins/base.donut")),
        "ui" => Some(include_str!("builtins/ui.donut")),
        "sys" => Some(include_str!("builtins/sys.donut")),
        _ => None,
    }
}

// --- Public API ---

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
    let (root, def_order) = checker.resolve_decls_root(program.0);
    let prog = Program {
        root,
        items: checker.items,
        defs: checker.defs,
        vals: checker.vals,
        def_order,
    };
    (prog, checker.errors)
}
