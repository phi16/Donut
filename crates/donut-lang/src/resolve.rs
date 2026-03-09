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

// --- Resolve trait implementations (Val types) ---

impl Resolve for S<semtree::Val> {
    type Output = ValId;
    fn resolve(self, ctx: &mut Checker) -> ValId {
        let S(v, span) = self;
        let val = v.resolve(ctx);
        ctx.alloc_val(val, Some(span))
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

        // Name resolution (validate names exist)
        let name_spans: Vec<(&str, &TokenSpan)> = path
            .0
            .iter()
            .map(|seg_s| {
                let S(seg, span) = seg_s;
                (seg.0 .0.as_str(), span)
            })
            .collect();
        ctx.resolve_segments(&name_spans);

        // Convert segments (drop spans — not needed for check)
        let segments: Vec<Segment> = path
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
    type Output = Segment;
    fn resolve(self, ctx: &mut Checker) -> Segment {
        let S(seg, _) = self;
        let params: Vec<ParamVal> = seg.1 .0.into_iter().map(|pv| pv.resolve(ctx)).collect();
        Segment {
            name: seg.0 .0,
            params,
        }
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
    names: HashMap<String, ItemId>,
    /// Entries defined in this scope, in order.
    entries: Vec<(String, ItemId)>,
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

    fn define(&mut self, name: String, item_id: ItemId) -> Option<ItemId> {
        if let Some(&existing) = self.names.get(&name) {
            Some(existing)
        } else {
            self.names.insert(name.clone(), item_id);
            self.entries.push((name, item_id));
            None
        }
    }

    fn define_used(&mut self, name: String, item_id: ItemId) -> Option<ItemId> {
        if let Some(&existing) = self.names.get(&name) {
            if existing != item_id {
                return Some(existing);
            }
            None
        } else {
            self.used.insert(name.clone());
            self.names.insert(name.clone(), item_id);
            self.entries.push((name, item_id));
            None
        }
    }

    fn replace(&mut self, name: &str, new_id: ItemId) {
        if let Some(id) = self.names.get_mut(name) {
            *id = new_id;
        }
        for e in &mut self.entries {
            if e.0 == name {
                e.1 = new_id;
                break;
            }
        }
    }

    fn get(&self, name: &str) -> Option<ItemId> {
        self.names.get(name).copied()
    }

    fn contains_key(&self, name: &str) -> bool {
        self.names.contains_key(name)
    }

    /// Build an output Module from non-used entries.
    fn to_module(&self) -> Module {
        let mut module = Module::new();
        for (name, item_id) in &self.entries {
            if !self.used.contains(name) {
                module.define(name.clone(), *item_id);
            }
        }
        module
    }

    /// Merge all entries from a Module as used (for `use` imports).
    fn merge_used(&mut self, module: &Module, items: &[Item]) -> Vec<String> {
        let mut conflicts = Vec::new();
        for &item_id in &module.entries {
            let lname = items[item_id.0].lname.clone();
            if let Some(_) = self.define_used(lname.clone(), item_id) {
                conflicts.push(lname);
            }
        }
        conflicts
    }

    /// Merge entries from a Module (for `with` clauses, `+=`, etc.).
    fn merge(&mut self, module: &Module, items: &[Item]) -> Vec<String> {
        let mut conflicts = Vec::new();
        for &item_id in &module.entries {
            let lname = items[item_id.0].lname.clone();
            if let Some(_) = self.define(lname.clone(), item_id) {
                conflicts.push(lname);
            }
        }
        conflicts
    }
}

// --- GenKind from syntax ---

fn gen_kind_from_op(op: &semtree::AssignOp, has_body: bool) -> Option<GenKind> {
    match op {
        semtree::AssignOp::Decl => Some(GenKind::Decl),
        semtree::AssignOp::Def if has_body => Some(GenKind::Def),
        semtree::AssignOp::Def => Some(GenKind::Decl),
        semtree::AssignOp::Alias => None,
        semtree::AssignOp::Add => None,
    }
}

// --- Checker ---

struct Checker<'a> {
    tokens: &'a [Token<'a>],

    // Arenas
    gens: Vec<Gen>,
    items: Vec<Item>,
    vals: Vec<Val>,
    val_spans: Vec<Option<TokenSpan>>,

    // Gen deduplication (cname → GenId)
    gen_by_cname: HashMap<String, GenId>,

    // Scopes
    scopes: Vec<Scope>,
    errors: Vec<Error>,

    // Decorator param tracking
    deco_param_stack: Vec<HashSet<ItemId>>,
    used_deco_params: HashSet<ItemId>,
    in_applicand: bool,

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
}

impl<'a> Checker<'a> {
    fn new(tokens: &'a [Token<'a>]) -> Self {
        Checker {
            tokens,
            gens: Vec::new(),
            items: Vec::new(),
            vals: Vec::new(),
            val_spans: Vec::new(),
            gen_by_cname: HashMap::new(),
            scopes: Vec::new(),
            errors: Vec::new(),
            deco_param_stack: Vec::new(),
            used_deco_params: HashSet::new(),
            in_applicand: false,
            prefix_stack: Vec::new(),
            cname_prefix_stack: Vec::new(),
            param_count_stack: Vec::new(),
            current_origin: None,
            extra_sources: HashMap::new(),
        }
    }

    // --- Arena allocators ---

    fn alloc_val(&mut self, val: Val, span: Option<TokenSpan>) -> ValId {
        let id = ValId(self.vals.len());
        self.vals.push(val);
        self.val_spans.push(span);
        id
    }

    fn alloc_gen(&mut self, generator: Gen) -> GenId {
        // Dedup by cname: reuse existing Gen if same cname
        if let Some(&existing) = self.gen_by_cname.get(&generator.cname) {
            return existing;
        }
        let id = GenId(self.gens.len());
        self.gen_by_cname.insert(generator.cname.clone(), id);
        self.gens.push(generator);
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

    #[allow(dead_code)]
    fn val(&self, id: ValId) -> &Val {
        &self.vals[id.0]
    }

    fn val_span(&self, id: ValId) -> Option<&TokenSpan> {
        self.val_spans[id.0].as_ref()
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

    /// Build a canonical name (for Gens) from a local name.
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

    /// Build a canonical name for a parameter Gen.
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

    fn define(&mut self, name: String, item_id: ItemId) -> bool {
        let scope = self.scopes.last_mut().unwrap();
        scope.define(name, item_id).is_none()
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

    fn resolve_comp_flat(
        &mut self,
        val_s: S<semtree::Val>,
        axis: donut_core::common::Axis,
        out: &mut Vec<ValId>,
    ) {
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

    // --- Name resolution ---

    fn resolve_segments(&mut self, segments: &[(&str, &TokenSpan)]) {
        let Some((&(first_name, first_span), rest)) = segments.split_first() else {
            return;
        };
        let Some(mut current) = self.lookup(first_name) else {
            if first_name != "*"
                && first_name != "meta"
                && !crate::convert::is_number_str(first_name)
            {
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

    fn enter_inner_scope(&mut self, deco_param_defs: &[(String, ValId)]) {
        self.push_scope();
        let mut params = HashSet::new();
        for (name, val_id) in deco_param_defs {
            let span = self
                .val_span(*val_id)
                .cloned()
                .unwrap_or(TokenSpan { start: 0, end: 0 });
            let qname = self.make_qname(name);
            let item = Item {
                qname,
                lname: name.clone(),
                span,
                ty: Some(*val_id),
                params: vec![],
                body: ItemBody::Value {
                    val: None,
                    members: Module::new(),
                },
                decos: vec![],
                origin: None,
                param_counts: vec![],
            };
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

    // --- Where/With clauses ---

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
            scope.merge_used(&result, &self.items);
        }
        for with_mod in with_clauses {
            let span = with_mod.1.clone();
            let with_members = self.resolve_module(with_mod);
            let conflicts = result.merge_from(&with_members, &self.items);
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
                        let old_cname_prefix =
                            std::mem::take(&mut self.cname_prefix_stack);
                        let old_param_counts =
                            std::mem::take(&mut self.param_count_stack);
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
                    let scope = self.scopes.last_mut().unwrap();
                    let conflicts = scope.merge_used(&result, &self.items);
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

        // Determine if this declaration creates a Gen
        let has_body_val = matches!(&body, Some(semtree::ValMod::Val(_)));
        let gen_kind = gen_kind_from_op(&op.0, has_body_val);

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
                        (seg.0 .0.clone(), span.clone())
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
                        self.error_at(
                            span,
                            format!("`{}` must be declared before `+=`", name),
                        );
                    }
                }
            }
        }

        // --- Forward refs (outer scope, before inner scope) ---
        if !is_add {
            for (name_s, seg_names) in names.iter().zip(&seg_names_list) {
                let has_applicand = name_s.0 .1.is_some();
                if !has_applicand && seg_names.len() == 1 {
                    let (name, span) = &seg_names[0];
                    let qname = self.make_qname(name);
                    let item = Item {
                        qname,
                        lname: name.clone(),
                        span: span.clone(),
                        ty: None,
                        params: vec![],
                        body: ItemBody::Value {
                            val: None,
                            members: Module::new(),
                        },
                        decos: vec![],
                        origin: None,
                        param_counts: vec![],
                    };
                    let id = self.alloc_item(item);
                    if !self.define(name.clone(), id) {
                        self.error_at(span, format!("duplicate definition `{}`", name));
                    }
                }
            }
        }

        // --- Inner scope ---
        self.enter_inner_scope(&deco_param_defs);

        // --- Extract params (defined incrementally) ---
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
                                name: name.0.clone(),
                                ty: resolved_ty,
                            };
                            // Create Gen for parameter
                            let param_cname =
                                self.make_param_cname(&first_lname, &name.0);
                            let _gen_id = self.alloc_gen(Gen {
                                cname: param_cname,
                                lname: name.0.clone(),
                                kind: GenKind::Param,
                                ty: param.ty,
                                params: vec![],
                            });
                            // Create Item for parameter (in scope)
                            let span = self
                                .val_span(param.ty)
                                .cloned()
                                .unwrap_or(TokenSpan { start: 0, end: 0 });
                            let qname = self.make_qname(&param.name);
                            let item = Item {
                                qname,
                                lname: param.name.clone(),
                                span,
                                ty: Some(param.ty),
                                params: vec![],
                                body: ItemBody::Value {
                                    val: None,
                                    members: Module::new(),
                                },
                                decos: vec![],
                                origin: None,
                                param_counts: vec![],
                            };
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

        // Where clauses
        self.resolve_where_clauses(where_clauses);

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
                if let Val::Path(path) = &self.vals[val_id.0] {
                    let path_names: Vec<String> =
                        path.segments.iter().map(|s| s.name.clone()).collect();
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
                        let all_deco_params: HashSet<ItemId> =
                            self.deco_param_stack.iter().flatten().copied().collect();
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
            self.register_functor_app(
                name_infos,
                body_val_resolved,
                mapping_params,
                resolved_apps,
            );
        } else if is_add {
            self.register_add(name_infos, body_val_resolved, result);
        } else {
            // Create Gen if this is a declaration/def
            if let Some(gk) = gen_kind {
                if let Some(ty_id) = ty_resolved {
                    let cname = self.make_cname(&first_lname);
                    self.alloc_gen(Gen {
                        cname,
                        lname: first_lname.clone(),
                        kind: gk,
                        ty: ty_id,
                        params: params.clone(),
                    });
                }
            }

            let is_functor =
                ty_resolved.map_or(false, |id| is_functor_type(&self.vals[id.0]));
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
            let qname = self.make_qname(&first_lname);
            let param_counts = self.current_param_counts(params.len());
            let item = Item {
                qname,
                lname: first_lname.clone(),
                span,
                ty: ty_resolved,
                params,
                body,
                decos: deco_vals,
                origin: None,
                param_counts,
            };
            let item_id = self.alloc_item(item);
            for ni in &name_infos {
                self.register_path(&ni.seg_names, item_id);
            }
        }
    }

    // --- Registration helpers ---

    fn register_path(&mut self, segs: &[(String, TokenSpan)], item_id: ItemId) {
        assert!(!segs.is_empty(), "register_path: segs must not be empty");
        match segs.len() {
            1 => {
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
            // Collect lnames first to avoid borrow conflict
            let lnames: Vec<(ItemId, String)> = new_members
                .entries
                .iter()
                .map(|&id| (id, self.items[id.0].lname.clone()))
                .collect();
            let conflicts = if let Some(id) = self.lookup(name) {
                if let Some(members) = self.item_mut(id).members_mut() {
                    let mut cs = Vec::new();
                    for (new_id, lname) in &lnames {
                        if members.define(lname.clone(), *new_id).is_some() {
                            cs.push(lname.clone());
                        }
                    }
                    cs
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

    fn register_add(
        &mut self,
        name_infos: Vec<NameInfo>,
        body_val_resolved: Option<ValId>,
        result: Module,
    ) {
        let all_seg_names: Vec<&[(String, TokenSpan)]> = name_infos
            .iter()
            .map(|ni| ni.seg_names.as_slice())
            .collect();

        let members_to_merge = match body_val_resolved {
            Some(val_id) => {
                let span = self.val_spans[val_id.0]
                    .clone()
                    .unwrap_or(TokenSpan { start: 0, end: 0 });
                match &self.vals[val_id.0] {
                    Val::Path(path) => {
                        let path_names: Vec<String> =
                            path.segments.iter().map(|s| s.name.clone()).collect();
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

        for seg_names in &all_seg_names {
            self.merge_into_path(seg_names, members_to_merge.clone());
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

        self.exit_inner_scope();

        // Mod → promote to current scope
        let scope = self.scopes.last_mut().unwrap();
        let conflicts = scope.merge(&result, &self.items);
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
    let root = checker.resolve_decls(program.0);
    let prog = Program {
        root,
        gens: checker.gens,
        items: checker.items,
        vals: checker.vals,
        val_spans: checker.val_spans,
    };
    (prog, checker.errors)
}
