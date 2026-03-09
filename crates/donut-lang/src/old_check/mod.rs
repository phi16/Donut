mod color;
mod eval;

use crate::types::common::Error;
use crate::types::old_env::*;
use crate::types::old_item::*;
use crate::types::token::Token;
use color::ColorSpec;
use donut_core::cell::{Diagram, Globular, check_prim};
use donut_core::common::{Level, Prim, PureVal, PrimId};
use donut_core::free_cell::FreeCell;
use donut_core::pure_cell::PureCell;
use eval::{apply_functor, make_cell, match_ty};
use std::collections::HashMap;

pub use crate::types::old_env::{
    Color, Entry, EntryKind, Env, MetaType, ParamInfo, ParamKind, PrimDecl, display_cell_type,
    display_prim, display_pure_cell,
};

// --- Check error ---

pub(super) enum CheckError {
    NotConvertible(PureCell, PureCell),
    IncompatibleDimension,
    EmptyComposition,
    NoFunctorMapping(Prim),
    ParamCount {
        name: String,
        expected: usize,
        got: usize,
    },
    General(String),
}

impl CheckError {
    pub(super) fn general(msg: impl Into<String>) -> Self {
        CheckError::General(msg.into())
    }

    pub(super) fn format(&self, prim_decls: &HashMap<PrimId, PrimDecl>) -> String {
        match self {
            CheckError::NotConvertible(a, b) => format!(
                "{}\nis not convertible to\n{}",
                display_pure_cell(a, prim_decls),
                display_pure_cell(b, prim_decls),
            ),
            CheckError::IncompatibleDimension => "incompatible dimension".to_string(),
            CheckError::EmptyComposition => "empty composition".to_string(),
            CheckError::NoFunctorMapping(prim) => {
                format!("functor: no mapping for {}", display_prim(prim, prim_decls),)
            }
            CheckError::ParamCount {
                name,
                expected,
                got,
            } => format!("{}: expected {} parameter(s), got {}", name, expected, got,),
            CheckError::General(msg) => msg.clone(),
        }
    }
}

impl From<donut_core::common::Error> for CheckError {
    fn from(e: donut_core::common::Error) -> Self {
        match e {
            donut_core::common::Error::NotConvertible(a, b) => CheckError::NotConvertible(a, b),
            donut_core::common::Error::IncompatibleDimension => CheckError::IncompatibleDimension,
            donut_core::common::Error::EmptyComposition => CheckError::EmptyComposition,
        }
    }
}

type Result<T> = std::result::Result<T, CheckError>;

// --- Check context for a single item ---

struct ItemCtx<'a> {
    qname: &'a str,
    cname: &'a str,
    span: &'a TokenSpan,
    color: Option<ColorSpec>,
    param_freshes: &'a [ParamInfo],
    param_counts: &'a [usize],
    origin: Option<&'a str>,
}

// --- Module member reference ---

#[derive(Clone)]
pub(super) struct MemberRef {
    pub(super) name: String,
    pub(super) entry: Option<usize>,
}

// --- Functor map entry ---

pub(super) struct FunctorEntry {
    param_prims: Vec<PrimId>,
    cell: PureCell,
}

// --- Checker ---

pub(super) struct Checker<'a> {
    pub(super) program: &'a Program,
    pub(super) tokens: &'a [Token<'a>],
    pub(super) next_prim: PrimId,

    pub(super) entries: Vec<Entry>,
    pub(super) lookup: HashMap<String, usize>,
    pub(super) errors: Vec<Error>,

    pub(super) prefixes: Vec<String>,

    pub(super) module_params: HashMap<String, Vec<ParamInfo>>,
    pub(super) module_members: HashMap<String, Vec<MemberRef>>,
    pub(super) entry_params: HashMap<usize, Vec<ParamInfo>>,
    pub(super) accumulated_args: Vec<PureVal>,

    pub(super) meta_prim_ids: HashMap<String, PrimId>,
    pub(super) meta_ret_types: HashMap<PrimId, MetaType>,
    pub(super) meta_values: HashMap<usize, PureVal>,

    pub(super) qname_cache: HashMap<ItemId, String>,

    pub(super) functor_maps: HashMap<String, HashMap<PrimId, FunctorEntry>>,

    pub(super) prim_decls: HashMap<PrimId, PrimDecl>,

    /// Saved param info for re-entering scopes (item_id → (freshes, args)).
    pub(super) scope_params: HashMap<ItemId, (Vec<ParamInfo>, Vec<PureVal>)>,
}

impl<'a> Checker<'a> {
    fn new(program: &'a Program, tokens: &'a [Token<'a>]) -> Self {
        // Reserve PrimId 0 for the built-in 'meta' type
        let mut meta_prim_ids = HashMap::new();
        meta_prim_ids.insert("meta".to_string(), 0);

        Checker {
            program,
            tokens,
            next_prim: 1, // 0 is reserved for 'meta'
            entries: Vec::new(),
            lookup: HashMap::new(),
            errors: Vec::new(),
            prefixes: Vec::new(),
            module_params: HashMap::new(),
            module_members: HashMap::new(),
            entry_params: HashMap::new(),
            accumulated_args: Vec::new(),
            meta_prim_ids,
            meta_ret_types: HashMap::new(),
            meta_values: HashMap::new(),
            qname_cache: HashMap::new(),
            functor_maps: HashMap::new(),
            prim_decls: HashMap::new(),
            scope_params: HashMap::new(),
        }
    }

    fn into_result(self) -> (Env, Vec<Error>) {
        let meta_prim_names: HashMap<PrimId, String> = self
            .entries
            .iter()
            .filter_map(|e| match &e.body {
                EntryBody::Meta(prim) => Some((prim.id, e.name.clone())),
                _ => None,
            })
            .collect();
        let module_members: HashMap<String, Vec<String>> = self
            .module_members
            .into_iter()
            .map(|(k, refs)| (k, refs.into_iter().map(|r| r.name).collect()))
            .collect();
        let env = Env {
            entries: self.entries,
            lookup: self.lookup,
            prim_decls: self.prim_decls,
            entry_params: self.entry_params,
            module_params: self.module_params,
            meta_ret_types: self.meta_ret_types,
            meta_prim_names,
            module_members,
        };
        (env, self.errors)
    }

    fn error_at(&mut self, span: &TokenSpan, msg: impl Into<String>) {
        if let Some(token) = self.tokens.get(span.start) {
            self.errors.push((token.pos.clone(), msg.into()));
        }
    }

    fn check_error_at(&mut self, span: &TokenSpan, err: CheckError) {
        self.error_at(span, err.format(&self.prim_decls));
    }

    // --- Prim generation ---

    fn fresh_prim_id(&mut self) -> PrimId {
        let id = self.next_prim;
        self.next_prim += 1;
        id
    }

    fn make_prim(&mut self) -> Prim {
        let id = self.fresh_prim_id();
        Prim::with_id_args(id, self.accumulated_args.clone())
    }

    // --- Entry management ---

    fn add_entry(
        &mut self,
        name: String,
        color: Color,
        body: EntryBody,
        param_counts: Vec<usize>,
        origin: Option<String>,
    ) -> usize {
        let idx = self.entries.len();
        self.lookup.insert(name.clone(), idx);
        self.entries.push(Entry {
            name,
            color,
            body,
            param_counts,
            origin,
        });
        idx
    }

    fn qualified_name(&self, name: &str) -> String {
        if let Some(prefix) = self.prefixes.last() {
            format!("{}.{}", prefix, name)
        } else {
            name.to_string()
        }
    }

    /// Resolve a name with prefix search: try each prefix in reverse, then bare name.
    fn resolve_qualified(&self, name: &str, exists: impl Fn(&str) -> bool) -> Option<String> {
        for prefix in self.prefixes.iter().rev() {
            let qualified = format!("{}.{}", prefix, name);
            if exists(&qualified) {
                return Some(qualified);
            }
        }
        if exists(name) {
            Some(name.to_string())
        } else {
            None
        }
    }

    fn resolve_name(&self, name: &str) -> Option<usize> {
        let qname = self.resolve_qualified(name, |n| self.lookup.contains_key(n))?;
        self.lookup.get(&qname).copied()
    }

    // --- CheckNode processing ---

    fn process_check_order(&mut self, nodes: &[CheckNode]) {
        for node in nodes {
            match node {
                CheckNode::Item(item_id) => {
                    self.check_item(*item_id);
                }
                CheckNode::Scope { item_id, children } => {
                    self.process_scope(*item_id, children);
                }
            }
        }
    }

    fn process_scope(&mut self, item_id: ItemId, children: &[CheckNode]) {
        let qname = match self.qname_cache.get(&item_id).cloned() {
            Some(q) => q,
            None => return,
        };

        let item = self.program.item(item_id);

        let canonical = item
            .members()
            .and_then(|m| m.origin.as_deref())
            .unwrap_or(&qname)
            .to_string();
        let need_alias = canonical != qname;

        // Re-enter params
        let (param_freshes, param_args) =
            self.scope_params.get(&item_id).cloned().unwrap_or_default();
        self.accumulated_args.extend_from_slice(&param_args);

        self.prefixes.push(canonical.clone());

        // Check all items and sub-scopes
        self.process_check_order(children);

        // Collect members after all children are processed
        // (sub-module scopes must be registered before we check is_sub_module)
        let mut new_members = Vec::new();
        for child in children {
            if let CheckNode::Item(child_id) = child {
                let name = &self.program.item(*child_id).names.lname;
                let full_name = format!("{}.{}", canonical, name);
                let entry_idx = self.lookup.get(&full_name).copied();
                let is_sub_module = self.module_members.contains_key(&full_name);
                if entry_idx.is_some() || is_sub_module {
                    new_members.push(MemberRef {
                        name: name.clone(),
                        entry: entry_idx,
                    });
                }
            }
        }

        self.prefixes.pop();

        // Exit params
        for _ in &param_args {
            self.accumulated_args.pop();
        }

        // Record module_params
        if !param_freshes.is_empty() {
            self.module_params
                .entry(qname.clone())
                .or_insert(param_freshes);
        }

        // Merge new members with existing
        if !new_members.is_empty() || !param_args.is_empty() {
            let mut all_members = self
                .module_members
                .get(&canonical)
                .cloned()
                .unwrap_or_default();
            all_members.extend(new_members);
            self.module_members.insert(canonical.clone(), all_members);
        }

        if need_alias {
            self.alias_members(&canonical, &qname);
        }
    }

    fn check_item(&mut self, item_id: ItemId) {
        let item = self.program.item(item_id);
        if matches!(item.kind, Some(ItemKind::Param)) {
            return;
        }
        self.check_item_inner(item_id);
    }

    fn check_item_inner(&mut self, item_id: ItemId) {
        let item = self.program.item(item_id);
        let qname = self.qualified_name(&item.names.lname);
        let cname = item.names.cname.clone();

        // Reuse cached entry for same ItemId (from import cache)
        if let Some(old_qname) = self.qname_cache.get(&item_id).cloned() {
            if let Some(&idx) = self.lookup.get(&old_qname) {
                self.lookup.insert(qname.clone(), idx);
            }
            self.alias_members(&old_qname, &qname);
            if let Some(params) = self.module_params.get(&old_qname).cloned() {
                self.module_params.insert(qname, params);
            }
            return;
        }

        let decos = self.eval_decorators(&item.decos);
        let color = self.extract_color(&decos);
        let span = item.span.clone();

        // Handle params: create fresh prims
        let param_freshes = match self.enter_params(&item.params) {
            Ok(f) => f,
            Err(msg) => {
                self.check_error_at(&span, msg);
                return;
            }
        };
        let has_params = !param_freshes.is_empty();

        let origin = item.origin.as_deref();
        let param_counts = &item.param_counts;
        let ctx = ItemCtx {
            qname: &qname,
            cname: &cname,
            span: &span,
            color,
            param_freshes: &param_freshes,
            param_counts,
            origin,
        };

        match &item.body {
            ItemBody::Value { val, members: _ } => {
                let body_val = if matches!(item.kind, Some(ItemKind::Def)) {
                    &None
                } else {
                    val
                };

                // Check if body refers to a parametric module (module alias)
                if let Some(body_id) = body_val {
                    let body_s = self.program.val(*body_id);
                    if let Val::Path(path) = &body_s.0 {
                        match self.try_module_alias(ctx.qname, path) {
                            Ok(Some(())) => {
                                self.exit_params(&param_freshes);
                                if has_params {
                                    self.module_params.insert(qname.clone(), param_freshes);
                                }
                                self.qname_cache.insert(item_id, qname);
                                return;
                            }
                            Ok(None) => {}
                            Err(msg) => {
                                self.check_error_at(ctx.span, msg);
                                self.exit_params(&param_freshes);
                                return;
                            }
                        }
                    }
                }

                match (&item.ty, body_val) {
                    (Some(ty_id), None) => {
                        self.check_decl(&ctx, *ty_id);
                    }
                    (dim_ty, Some(body_id)) => {
                        let declared_ty = dim_ty.and_then(|ty_id| {
                            let ty_s = self.program.val(ty_id);
                            self.eval_ty(&ty_s.0).ok()
                        });
                        self.check_body(&ctx, *body_id, declared_ty);
                    }
                    (None, None) => {}
                }
            }
            ItemBody::Functor { mappings } => {
                self.check_functor(&ctx, item_id, mappings);
            }
        }

        // Save param info for scope re-entry
        if has_params {
            let start = self.accumulated_args.len() - param_freshes.len();
            let args = self.accumulated_args[start..].to_vec();
            self.scope_params
                .insert(item_id, (param_freshes.clone(), args));
        }

        self.exit_params(&param_freshes);

        self.qname_cache.insert(item_id, qname);
    }

    /// Create lookup aliases for all members of an already-checked module.
    fn alias_members(&mut self, old_prefix: &str, new_prefix: &str) {
        let Some(members) = self.module_members.get(old_prefix).cloned() else {
            return;
        };
        for m in &members {
            let old_name = format!("{}.{}", old_prefix, m.name);
            let new_name = format!("{}.{}", new_prefix, m.name);
            if let Some(idx) = m.entry {
                self.lookup.insert(new_name.clone(), idx);
            }
            self.alias_members(&old_name, &new_name);
        }
        self.module_members.insert(new_prefix.to_string(), members);
    }

    /// Register a declaration (type annotation only, no body).
    fn check_decl(&mut self, ctx: &ItemCtx, ty_id: ValId) {
        let ty_s = self.program.val(ty_id);
        match self.eval_ty(&ty_s.0) {
            Ok((_, Ty::Meta(mt))) => {
                let prim = self.make_prim();
                let prim_id = prim.id;
                let idx = self.register_meta_entry(&ctx, prim, Some(mt), None);
                self.register_prim_decl(prim_id, ctx.cname, 0, idx);
            }
            Ok((_, ty)) => {
                let prim = self.make_prim();
                let prim_id = prim.id;
                match make_cell(prim, &ty) {
                    Ok(cell) => {
                        let level = cell.pure.dim().in_space;
                        let idx = self.register_entry(&ctx, EntryBody::Cell(cell));
                        self.register_prim_decl(prim_id, ctx.cname, level, idx);
                    }
                    Err(e) => self.check_error_at(ctx.span, e),
                }
            }
            Err(msg) => self.check_error_at(ctx.span, msg),
        }
    }

    /// Try to register the body as a meta entry. Returns Ok(()) on success.
    fn try_register_meta(
        &mut self,
        ctx: &ItemCtx,
        body_val: &Val,
        declared_ret: Option<MetaType>,
    ) -> Result<()> {
        let meta_val = self.eval_meta_val(body_val)?;
        let prim = self.make_prim();
        let prim_id = prim.id;
        let body_ret = self.check_meta_type(body_val);

        let ret = match (&declared_ret, &body_ret) {
            (Some(decl), Some(body_ty)) => {
                if decl != body_ty {
                    self.error_at(ctx.span, "meta body type does not match declared type");
                }
                Some(decl.clone())
            }
            (Some(decl), None) => Some(decl.clone()),
            (None, body_ty) => body_ty.clone(),
        };

        let idx = self.register_meta_entry(&ctx, prim, ret, Some(meta_val));
        self.register_prim_decl(prim_id, ctx.cname, 0, idx);
        Ok(())
    }

    fn check_body(&mut self, ctx: &ItemCtx, body_id: ValId, declared_ty: Option<(u8, Ty)>) {
        let declared_meta = match &declared_ty {
            Some((_, Ty::Meta(mt))) => Some(mt.clone()),
            _ => None,
        };

        if let Some(declared_ret) = declared_meta {
            // Declared as meta type → must be meta
            let body_val = &self.program.val(body_id).0;
            if let Err(msg) = self.try_register_meta(ctx, body_val, Some(declared_ret)) {
                self.check_error_at(ctx.span, msg);
            }
            return;
        }

        // Try cell interpretation first
        let body_s = self.program.val(body_id);
        let cell_err = match self.eval_val(&body_s.0) {
            Ok(mut cell) => {
                if let Some((declared_dim, ref declared_ty)) = declared_ty {
                    // Lift cell to declared dimension via id (e.g., 1-cell x → id(x) as 2-cell)
                    while cell.pure.dim().in_space < declared_dim {
                        cell = FreeCell::from_pure(&PureCell::id(cell.pure));
                    }
                    let dim = cell.pure.dim().in_space;
                    if declared_dim != dim {
                        self.error_at(ctx.span, "declared type dimension does not match body");
                    } else {
                        let body_ty = if dim == 0 {
                            Ty::Zero
                        } else {
                            Ty::Succ(
                                FreeCell::from_pure(&cell.pure.s()),
                                FreeCell::from_pure(&cell.pure.t()),
                            )
                        };
                        if let Err(msg) = match_ty(declared_ty, &body_ty) {
                            self.check_error_at(ctx.span, msg);
                        }
                    }
                }
                self.register_entry(ctx, EntryBody::Cell(cell));
                return;
            }
            Err(msg) => msg,
        };

        // Fallbacks only when untyped
        if declared_ty.is_some() {
            self.check_error_at(ctx.span, cell_err);
            return;
        }

        let body_val = &self.program.val(body_id).0;

        // Try meta fallback
        if self.try_register_meta(ctx, body_val, None).is_ok() {
            return;
        }

        // Try type alias fallback
        if let Ok((dim, ty)) = self.eval_ty(body_val) {
            self.register_entry(ctx, EntryBody::Type(dim, ty));
            return;
        }

        self.check_error_at(ctx.span, cell_err);
    }

    fn check_functor(&mut self, ctx: &ItemCtx, item_id: ItemId, mappings: &[FunctorMapping]) {
        let item = self.program.item(item_id);

        let ty_id = match item.ty {
            Some(id) => id,
            None => {
                self.error_at(ctx.span, "functor must have a type");
                return;
            }
        };
        let ty_s = self.program.val(ty_id);
        let (src_cell, tgt_cell) = match &ty_s.0 {
            Val::Arrow(ArrowKind::Functor, l_id, r_id) => {
                let l = self.program.val(*l_id);
                let r = self.program.val(*r_id);
                match (self.eval_val(&l.0), self.eval_val(&r.0)) {
                    (Ok(s), Ok(t)) => (s, t),
                    (Err(e), _) | (_, Err(e)) => {
                        self.check_error_at(ctx.span, e);
                        return;
                    }
                }
            }
            _ => {
                self.error_at(ctx.span, "functor type must be `A ~> B`");
                return;
            }
        };

        let src_prim_id = match src_cell.pure.extract_prim_id() {
            Some(id) => id,
            None => {
                self.error_at(ctx.span, "functor source must be a primitive cell");
                return;
            }
        };

        let mut functor_map: HashMap<PrimId, FunctorEntry> = HashMap::new();
        functor_map.insert(
            src_prim_id,
            FunctorEntry {
                param_prims: vec![],
                cell: tgt_cell.pure.clone(),
            },
        );

        for mapping in mappings {
            // Enter mapping-level params (e.g., [n: nat])
            let mapping_freshes = match self.enter_params(&mapping.params) {
                Ok(f) => f,
                Err(msg) => {
                    self.check_error_at(ctx.span, msg);
                    continue;
                }
            };

            let app_span = &self.program.val(mapping.applicand).1;
            let val_span = &self.program.val(mapping.val).1;
            // Clone spans to avoid borrow issues
            let app_span = app_span.clone();
            let val_span = val_span.clone();

            let app_val = &self.program.val(mapping.applicand).0;
            if let Val::Path(path) = app_val {
                if path.applicand.is_some() {
                    self.error_at(
                        &app_span,
                        "functor application cannot be used in mapping left-hand side",
                    );
                    self.exit_params(&mapping_freshes);
                    continue;
                }
            }
            let app_cell = match self.eval_val(app_val) {
                Ok(c) => c,
                Err(e) => {
                    self.check_error_at(&app_span, e);
                    self.exit_params(&mapping_freshes);
                    continue;
                }
            };
            let app_prim_id = match app_cell.pure.extract_prim_id() {
                Some(id) => id,
                None => {
                    self.error_at(
                        &app_span,
                        "functor mapping applicand must be a primitive cell",
                    );
                    self.exit_params(&mapping_freshes);
                    continue;
                }
            };

            let val_val = &self.program.val(mapping.val).0;
            let val_cell = match self.eval_val(val_val) {
                Ok(c) => c,
                Err(e) => {
                    self.check_error_at(&val_span, e);
                    self.exit_params(&mapping_freshes);
                    continue;
                }
            };

            self.exit_params(&mapping_freshes);

            let dim = app_cell.pure.dim().in_space;
            if dim == 0 {
                if app_prim_id == src_prim_id {
                    if !val_cell.pure.is_convertible(&tgt_cell.pure) {
                        self.error_at(&val_span, "functor 0-cell mapping contradicts base case");
                    }
                    continue;
                }
                self.error_at(
                    &app_span,
                    "functor mapping for this 0-cell is already implicitly defined",
                );
                continue;
            }
            let val_dim = val_cell.pure.dim().in_space;
            if val_dim > dim {
                self.error_at(&val_span, "functor mapping dimension mismatch");
                continue;
            }
            // Lift val_cell to the expected dimension via id
            let mut val_pure = val_cell.pure.clone();
            while val_pure.dim().in_space < dim {
                val_pure = PureCell::id(val_pure);
            }

            let expected_s = apply_functor(&app_cell.pure.s(), &functor_map);
            let expected_t = apply_functor(&app_cell.pure.t(), &functor_map);

            match (expected_s, expected_t) {
                (Ok(es), Ok(et)) => {
                    let actual_s = val_pure.s();
                    let actual_t = val_pure.t();
                    if !es.is_convertible(&actual_s) {
                        self.error_at(
                            &val_span,
                            format!(
                                "functor mapping source mismatch:\n  expected {}\n  got {}",
                                display_pure_cell(&es, &self.prim_decls),
                                display_pure_cell(&actual_s, &self.prim_decls),
                            ),
                        );
                        continue;
                    }
                    if !et.is_convertible(&actual_t) {
                        self.error_at(
                            &val_span,
                            format!(
                                "functor mapping target mismatch:\n  expected {}\n  got {}",
                                display_pure_cell(&et, &self.prim_decls),
                                display_pure_cell(&actual_t, &self.prim_decls),
                            ),
                        );
                        continue;
                    }
                }
                (Err(e), _) | (_, Err(e)) => {
                    self.check_error_at(&app_span, e);
                    continue;
                }
            }

            let param_prims: Vec<PrimId> = mapping_freshes.iter().map(|p| p.prim_id).collect();
            functor_map.insert(
                app_prim_id,
                FunctorEntry {
                    param_prims,
                    cell: val_pure,
                },
            );
        }

        self.functor_maps.insert(ctx.qname.to_string(), functor_map);
    }

    // --- Params ---

    fn enter_params(&mut self, params: &[Param]) -> Result<Vec<ParamInfo>> {
        let mut freshes = Vec::new();
        for param in params {
            let ty_s = self.program.val(param.ty);

            // Check for special param types (nat)
            let param_kind = self.detect_param_kind(&ty_s.0);
            let fresh_id = self.fresh_prim_id();

            let param_color = Color::gray();
            match param_kind {
                ParamKind::Cell => {
                    let (_, ty) = self.eval_ty(&ty_s.0)?;
                    let prim = Prim::new(fresh_id);
                    let cell = make_cell(prim, &ty)?;
                    let level = cell.pure.dim().in_space;
                    self.accumulated_args.push(PureVal::Cell(cell.pure.clone()));
                    let param_name = self.qualified_name(&param.name);
                    self.add_entry(param_name, param_color, EntryBody::Cell(cell), vec![], None);
                    self.register_param_prim_decl(fresh_id, &param.name, level);
                }
                ParamKind::Meta(_) => {
                    let prim = Prim::new(fresh_id);
                    let param_name = self.qualified_name(&param.name);
                    let idx = self.add_entry(
                        param_name,
                        param_color,
                        EntryBody::Meta(prim),
                        vec![],
                        None,
                    );
                    self.meta_values.insert(idx, PureVal::App(fresh_id, vec![]));
                    self.accumulated_args.push(PureVal::App(fresh_id, vec![]));
                    self.register_param_prim_decl(fresh_id, &param.name, 0);
                }
            }
            freshes.push(ParamInfo {
                name: param.name.clone(),
                prim_id: fresh_id,
                kind: param_kind,
            });
        }
        Ok(freshes)
    }

    fn exit_params(&mut self, freshes: &[ParamInfo]) {
        for _ in freshes {
            self.accumulated_args.pop();
        }
    }

    fn meta_id(&self, name: &str) -> Option<PrimId> {
        self.meta_prim_ids.get(name).copied()
    }

    /// Register a prim_decl entry with canonical name.
    fn register_prim_decl(&mut self, prim_id: PrimId, cname: &str, level: Level, entry_idx: usize) {
        let entry = &self.entries[entry_idx];
        self.prim_decls.insert(
            prim_id,
            PrimDecl {
                name: cname.to_string(),
                level,
                color: entry.color,
                param_counts: entry.param_counts.clone(),
            },
        );
    }

    /// Register a prim_decl entry for a parameter (gray, no param_counts).
    /// Uses the local (unqualified) name for display purposes.
    fn register_param_prim_decl(&mut self, prim_id: PrimId, local_name: &str, level: Level) {
        self.prim_decls.insert(
            prim_id,
            PrimDecl {
                name: local_name.to_string(),
                level,
                color: Color::gray(),
                param_counts: vec![],
            },
        );
    }

    /// Register an entry with the common pattern: add_entry + entry_params.
    fn register_entry(&mut self, ctx: &ItemCtx, body: EntryBody) -> usize {
        let color = self.resolve_color(ctx.color, &body);
        let idx = self.add_entry(
            ctx.qname.to_string(),
            color,
            body,
            ctx.param_counts.to_vec(),
            ctx.origin.map(|s| s.to_string()),
        );
        if !ctx.param_freshes.is_empty() {
            self.entry_params.insert(idx, ctx.param_freshes.to_vec());
        }
        idx
    }

    /// Register a meta entry: add entry, store meta return type and value.
    fn register_meta_entry(
        &mut self,
        ctx: &ItemCtx,
        prim: Prim,
        ret: Option<MetaType>,
        meta_val: Option<PureVal>,
    ) -> usize {
        if let Some(ret) = ret {
            self.meta_ret_types.insert(prim.id, ret);
        }
        // Register short name for meta_id() lookups
        let short_name = ctx
            .qname
            .rsplit('.')
            .next()
            .unwrap_or(ctx.qname)
            .to_string();
        self.meta_prim_ids.insert(short_name, prim.id);
        let idx = self.register_entry(ctx, EntryBody::Meta(prim));
        if let Some(val) = meta_val {
            self.meta_values.insert(idx, val);
        }
        idx
    }
}

// --- Public API ---

pub fn check(program: &Program, tokens: &[Token]) -> (Env, Vec<Error>) {
    let mut checker = Checker::new(program, tokens);
    checker.process_check_order(&program.check_order);
    checker.into_result()
}
