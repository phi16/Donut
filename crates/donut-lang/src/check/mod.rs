mod color;
mod eval;

use crate::types::common::Error;
use crate::types::env::*;
use crate::types::item::*;
use crate::types::token::Token;
use color::ColorSpec;
use donut_core::cell::{check_prim, Diagram, Globular};
use donut_core::common::{Level, Prim, PrimArg, PrimId};
use donut_core::free_cell::FreeCell;
use donut_core::pure_cell::PureCell;
use eval::{apply_functor, make_cell, match_ty};
use std::collections::HashMap;

pub use crate::types::env::{Entry, Env, MetaSig, MetaType, ParamInfo, ParamKind, PrimDecl};

type Result<T> = std::result::Result<T, String>;

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
    pub(super) module_members: HashMap<String, Vec<(String, Option<usize>)>>,
    pub(super) entry_params: HashMap<usize, Vec<ParamInfo>>,
    pub(super) accumulated_args: Vec<PrimArg>,
    pub(super) param_count_stack: Vec<usize>,

    pub(super) meta_prim_ids: HashMap<String, PrimId>,
    pub(super) meta_sigs: HashMap<PrimId, MetaSig>,
    pub(super) meta_values: HashMap<usize, PrimArg>,

    pub(super) item_cache: HashMap<ItemId, String>,

    pub(super) functor_maps: HashMap<String, HashMap<PrimId, FunctorEntry>>,

    pub(super) prim_decls: HashMap<PrimId, PrimDecl>,

    pub(super) current_origin: Option<(String, usize)>,
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
            param_count_stack: Vec::new(),
            meta_prim_ids,
            meta_sigs: HashMap::new(),
            meta_values: HashMap::new(),
            item_cache: HashMap::new(),
            functor_maps: HashMap::new(),
            prim_decls: HashMap::new(),
            current_origin: None,
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
        let env = Env {
            entries: self.entries,
            lookup: self.lookup,
            prim_decls: self.prim_decls,
            entry_params: self.entry_params,
            module_params: self.module_params,
            meta_sigs: self.meta_sigs,
            meta_prim_names,
        };
        (env, self.errors)
    }

    fn error_at(&mut self, span: &TokenSpan, msg: impl Into<String>) {
        if let Some(token) = self.tokens.get(span.start) {
            self.errors.push((token.pos.clone(), msg.into()));
        }
    }

    // --- Prim generation ---

    fn fresh_prim_id(&mut self) -> PrimId {
        let id = self.next_prim;
        self.next_prim += 1;
        id
    }

    fn make_prim(&mut self) -> Prim {
        let id = self.fresh_prim_id();
        Prim::with_args(id, self.accumulated_args.clone())
    }

    // --- Entry management ---

    fn add_entry(
        &mut self,
        name: String,
        color: (u8, u8, u8),
        body: EntryBody,
        param_counts: Vec<usize>,
    ) -> usize {
        let idx = self.entries.len();
        // Track meta prim ids by short name
        if let EntryBody::Meta(prim) = &body {
            let short_name = name.rsplit('.').next().unwrap_or(&name);
            self.meta_prim_ids.insert(short_name.to_string(), prim.id);
        }
        self.lookup.insert(name.clone(), idx);
        self.entries.push(Entry {
            name,
            color,
            body,
            param_counts,
        });
        idx
    }

    fn current_param_counts(&self, own_count: usize) -> Vec<usize> {
        let mut counts = self.param_count_stack.clone();
        if own_count > 0 {
            counts.push(own_count);
        }
        counts
    }

    fn qualified_name(&self, name: &str) -> String {
        if let Some(prefix) = self.prefixes.last() {
            format!("{}.{}", prefix, name)
        } else {
            name.to_string()
        }
    }

    /// Compute the canonical name for a prim_decl entry.
    /// Uses current_origin (origin_name, prefix_depth) to strip user-scope prefix
    /// and prepend origin with `::` separator.
    fn canonical_name(&self, qname: &str) -> String {
        if let Some((ref origin, depth)) = self.current_origin {
            let local = if depth > 0 {
                let prefix = &self.prefixes[depth - 1];
                qname.strip_prefix(prefix.as_str())
                    .and_then(|s| s.strip_prefix('.'))
                    .unwrap_or(qname)
            } else {
                qname
            };
            format!("{}::{}", origin, local)
        } else {
            qname.to_string()
        }
    }

    fn resolve_name(&self, name: &str) -> Option<usize> {
        for prefix in self.prefixes.iter().rev() {
            let qualified = format!("{}.{}", prefix, name);
            if let Some(&idx) = self.lookup.get(&qualified) {
                return Some(idx);
            }
        }
        self.lookup.get(name).copied()
    }

    // --- Module processing ---

    fn check_module(&mut self, module: &Module) {
        // Process internal items first (meta type registration etc.)
        for (name, item_id) in &module.internal {
            let item = self.program.item(*item_id);
            self.check_item(name, item, *item_id);
        }
        for (name, item_id) in &module.entries {
            let item = self.program.item(*item_id);
            self.check_item(name, item, *item_id);
        }
    }

    fn check_item(&mut self, name: &str, item: &Item, item_id: ItemId) {
        if matches!(item.kind, Some(ItemKind::Param)) {
            return;
        }

        // Track origin context from item for canonical naming
        let prev_origin = self.current_origin.clone();
        if let Some(ref origin) = item.origin {
            if self.current_origin.as_ref().map_or(true, |co| co.0 != *origin) {
                self.current_origin = Some((origin.clone(), self.prefixes.len()));
            }
        }

        self.check_item_inner(name, item, item_id);

        self.current_origin = prev_origin;
    }

    fn check_item_inner(&mut self, name: &str, item: &Item, item_id: ItemId) {
        let qname = self.qualified_name(name);

        // Reuse cached entry for same ItemId (from import cache)
        if let Some(old_qname) = self.item_cache.get(&item_id).cloned() {
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
        let span = &item.span;

        // Handle params: create fresh prims
        let param_freshes = match self.enter_params(&item.params) {
            Ok(f) => f,
            Err(msg) => {
                self.error_at(span, msg);
                return;
            }
        };
        let has_params = !param_freshes.is_empty();

        match &item.body {
            ItemBody::Value { val, members } => {
                let body_val = if matches!(item.kind, Some(ItemKind::Def)) {
                    &None
                } else {
                    val
                };

                // Check if body refers to a parametric module (module alias)
                if let Some(body_id) = body_val {
                    let body_s = self.program.val(*body_id);
                    if let Val::Path(path) = &body_s.0 {
                        match self.try_module_alias(&qname, path) {
                            Ok(Some(())) => {
                                self.exit_params(&param_freshes);
                                if has_params {
                                    self.module_params
                                        .insert(qname.clone(), param_freshes.clone());
                                }
                                self.item_cache.insert(item_id, qname);
                                return;
                            }
                            Ok(None) => {}
                            Err(msg) => {
                                self.error_at(span, msg);
                                self.exit_params(&param_freshes);
                                return;
                            }
                        }
                    }
                }

                let param_types = self.resolve_item_param_types(&item.params);

                match (&item.ty, body_val) {
                    (Some(ty_id), None) => {
                        self.check_decl(
                            &qname, span, color, *ty_id,
                            &param_types, &param_freshes,
                        );
                    }
                    (dim_ty, Some(body_id)) => {
                        let declared_ty = dim_ty.and_then(|ty_id| {
                            let ty_s = self.program.val(ty_id);
                            self.eval_ty(&ty_s.0).ok()
                        });
                        self.check_body(
                            &qname, span, color, *body_id, declared_ty,
                            &param_types, &param_freshes,
                        );
                    }
                    (None, None) => {}
                }

                if !members.entries.is_empty() {
                    self.check_members(&qname, members, param_freshes.len());
                }
            }
            ItemBody::Functor { mappings } => {
                self.check_functor(item, &qname, mappings);
            }
        }

        self.exit_params(&param_freshes);

        // Record module params for parametric modules
        if has_params && item.members().map_or(false, |m| !m.entries.is_empty()) {
            self.module_params
                .insert(qname.clone(), param_freshes);
        }

        self.item_cache.insert(item_id, qname);
    }

    /// Create lookup aliases for all members of an already-checked module.
    fn alias_members(&mut self, old_prefix: &str, new_prefix: &str) {
        let Some(members) = self.module_members.get(old_prefix).cloned() else { return };
        for (member_name, entry_idx) in &members {
            let old_name = format!("{}.{}", old_prefix, member_name);
            let new_name = format!("{}.{}", new_prefix, member_name);
            if let Some(idx) = entry_idx {
                self.lookup.insert(new_name.clone(), *idx);
            }
            self.alias_members(&old_name, &new_name);
        }
        self.module_members.insert(new_prefix.to_string(), members);
    }

    /// Register a declaration (type annotation only, no body).
    fn check_decl(
        &mut self,
        qname: &str,
        span: &TokenSpan,
        color: Option<ColorSpec>,
        ty_id: ValId,
        param_types: &[MetaType],
        param_freshes: &[ParamInfo],
    ) {
        let ty_s = self.program.val(ty_id);
        match self.eval_ty(&ty_s.0) {
            Ok((_, Ty::Meta(mt))) => {
                let prim = self.make_prim();
                let prim_id = prim.id;
                let idx = self.register_meta_entry(
                    qname.to_string(), color, prim, Some(mt),
                    param_types.to_vec(), None, param_freshes,
                );
                self.register_prim_decl(prim_id, qname, 0, idx);
            }
            Ok((_, ty)) => {
                let prim = self.make_prim();
                let prim_id = prim.id;
                match make_cell(prim, &ty) {
                    Ok(cell) => {
                        let level = cell.pure.dim().in_space;
                        let idx = self.register_entry(
                            qname.to_string(), color, EntryBody::Cell(cell), param_freshes,
                        );
                        self.register_prim_decl(prim_id, qname, level, idx);
                    }
                    Err(msg) => self.error_at(span, msg),
                }
            }
            Err(msg) => self.error_at(span, msg),
        }
    }

    /// Try to register the body as a meta entry. Returns Ok(()) on success.
    fn try_register_meta(
        &mut self,
        qname: &str,
        span: &TokenSpan,
        color: Option<ColorSpec>,
        body_val: &Val,
        declared_ret: Option<MetaType>,
        param_types: &[MetaType],
        param_freshes: &[ParamInfo],
    ) -> Result<()> {
        let meta_val = self.eval_meta_val(body_val)?;
        let prim = self.make_prim();
        let prim_id = prim.id;
        let body_ret = self.check_meta_type(body_val);

        let ret = match (&declared_ret, &body_ret) {
            (Some(decl), Some(body_ty)) => {
                if decl != body_ty {
                    self.error_at(span, "meta body type does not match declared type");
                }
                Some(decl.clone())
            }
            (Some(decl), None) => Some(decl.clone()),
            (None, body_ty) => body_ty.clone(),
        };

        let idx = self.register_meta_entry(
            qname.to_string(), color, prim, ret,
            param_types.to_vec(), Some(meta_val), param_freshes,
        );
        self.register_prim_decl(prim_id, qname, 0, idx);
        Ok(())
    }

    fn check_body(
        &mut self,
        qname: &str,
        span: &TokenSpan,
        color: Option<ColorSpec>,
        body_id: ValId,
        declared_ty: Option<(u8, Ty)>,
        param_types: &[MetaType],
        param_freshes: &[ParamInfo],
    ) {
        let declared_meta = match &declared_ty {
            Some((_, Ty::Meta(mt))) => Some(mt.clone()),
            _ => None,
        };

        if let Some(declared_ret) = declared_meta {
            // Declared as meta type → must be meta
            let body_val = &self.program.val(body_id).0;
            if let Err(msg) = self.try_register_meta(
                qname, span, color, body_val, Some(declared_ret),
                param_types, param_freshes,
            ) {
                self.error_at(span, msg);
            }
            return;
        }

        // Try cell interpretation first
        let body_s = self.program.val(body_id);
        let cell_err = match self.eval_val(&body_s.0) {
            Ok(cell) => {
                if let Some((declared_dim, ref declared_ty)) = declared_ty {
                    let dim = cell.pure.dim().in_space;
                    if declared_dim != dim {
                        self.error_at(span, "declared type dimension does not match body");
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
                            self.error_at(span, msg);
                        }
                    }
                }
                self.register_entry(
                    qname.to_string(), color, EntryBody::Cell(cell), param_freshes,
                );
                return;
            }
            Err(msg) => msg,
        };

        // Fallbacks only when untyped
        if declared_ty.is_some() {
            self.error_at(span, cell_err);
            return;
        }

        let body_val = &self.program.val(body_id).0;

        // Try meta fallback
        if self.try_register_meta(
            qname, span, color, body_val, None, param_types, param_freshes,
        ).is_ok() {
            return;
        }

        // Try type alias fallback
        if let Ok((dim, ty)) = self.eval_ty(body_val) {
            self.register_entry(
                qname.to_string(), color, EntryBody::Type(dim, ty), param_freshes,
            );
            return;
        }

        self.error_at(span, cell_err);
    }

    fn check_functor(&mut self, item: &Item, qname: &str, mappings: &[FunctorMapping]) {
        let span = &item.span;

        let ty_id = match item.ty {
            Some(id) => id,
            None => {
                self.error_at(span, "functor must have a type");
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
                    (Err(msg), _) | (_, Err(msg)) => {
                        self.error_at(span, msg);
                        return;
                    }
                }
            }
            _ => {
                self.error_at(span, "functor type must be `A ~> B`");
                return;
            }
        };

        let src_prim_id = match src_cell.pure.extract_prim_id() {
            Some(id) => id,
            None => {
                self.error_at(span, "functor source must be a primitive cell");
                return;
            }
        };

        let mut functor_map: HashMap<PrimId, FunctorEntry> = HashMap::new();
        functor_map.insert(src_prim_id, FunctorEntry {
            param_prims: vec![],
            cell: tgt_cell.pure.clone(),
        });

        for mapping in mappings {
            // Enter mapping-level params (e.g., [n: nat])
            let mapping_freshes = match self.enter_params(&mapping.params) {
                Ok(f) => f,
                Err(msg) => {
                    self.error_at(span, msg);
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
                    self.error_at(&app_span, "functor application cannot be used in mapping left-hand side");
                    self.exit_params(&mapping_freshes);
                    continue;
                }
            }
            let app_cell = match self.eval_val(app_val) {
                Ok(c) => c,
                Err(msg) => {
                    self.error_at(&app_span, msg);
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
                Err(msg) => {
                    self.error_at(&val_span, msg);
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
                                es, actual_s
                            ),
                        );
                        continue;
                    }
                    if !et.is_convertible(&actual_t) {
                        self.error_at(
                            &val_span,
                            format!(
                                "functor mapping target mismatch:\n  expected {}\n  got {}",
                                et, actual_t
                            ),
                        );
                        continue;
                    }
                }
                (Err(msg), _) | (_, Err(msg)) => {
                    self.error_at(&app_span, msg);
                    continue;
                }
            }

            let param_prims: Vec<PrimId> = mapping_freshes.iter()
                .map(|(_, id, _)| *id)
                .collect();
            functor_map.insert(app_prim_id, FunctorEntry {
                param_prims,
                cell: val_pure,
            });
        }

        self.functor_maps.insert(qname.to_string(), functor_map);
    }

    fn check_members(&mut self, prefix: &str, module: &Module, parent_param_count: usize) {
        let canonical = module.origin.as_deref().unwrap_or(prefix);
        let need_alias = canonical != prefix;

        // If canonical entries already registered, just alias
        if need_alias && self.module_members.contains_key(canonical) {
            self.alias_members(canonical, prefix);
            return;
        }

        self.prefixes.push(canonical.to_string());
        self.param_count_stack.push(parent_param_count);
        // Process internal items first (for meta type registration etc.)
        for (member_name, item_id) in &module.internal {
            let item = self.program.item(*item_id);
            self.check_item(member_name, item, *item_id);
        }
        // Process exported entries
        let mut members = Vec::new();
        for (member_name, item_id) in &module.entries {
            let full_name = format!("{}.{}", canonical, member_name);
            let item = self.program.item(*item_id);
            self.check_item(member_name, item, *item_id);
            let entry_idx = self.lookup.get(&full_name).copied();
            let is_sub_module = self.module_members.contains_key(&full_name);
            if entry_idx.is_some() || is_sub_module {
                members.push((member_name.clone(), entry_idx));
            }
        }
        self.param_count_stack.pop();
        self.prefixes.pop();

        if parent_param_count > 0 || !members.is_empty() {
            self.module_members.insert(canonical.to_string(), members);
        }

        if need_alias {
            self.alias_members(canonical, prefix);
        }
    }

    // --- Params ---

    fn enter_params(&mut self, params: &[Param]) -> Result<Vec<ParamInfo>> {
        let mut freshes = Vec::new();
        for param in params {
            let ty_s = self.program.val(param.ty);

            // Check for special param types (nat)
            let param_kind = self.detect_param_kind(&ty_s.0);
            let fresh_id = self.fresh_prim_id();

            const PARAM_COLOR: (u8, u8, u8) = (128, 128, 128);
            match param_kind {
                ParamKind::Cell => {
                    let (_, ty) = self.eval_ty(&ty_s.0)?;
                    let prim = Prim::new(fresh_id);
                    let cell = make_cell(prim, &ty)?;
                    let level = cell.pure.dim().in_space;
                    let param_name = self.qualified_name(&param.name);
                    self.add_entry(param_name.clone(), PARAM_COLOR, EntryBody::Cell(cell.clone()), vec![]);
                    self.accumulated_args.push(PrimArg::Cell(cell.pure.clone()));
                    self.register_param_prim_decl(fresh_id, &param_name, level);
                }
                ParamKind::Nat | ParamKind::Rat | ParamKind::Meta(_) => {
                    let prim = Prim::new(fresh_id);
                    let param_name = self.qualified_name(&param.name);
                    let idx = self.add_entry(param_name.clone(), PARAM_COLOR, EntryBody::Meta(prim), vec![]);
                    self.meta_values.insert(idx, PrimArg::App(fresh_id, vec![]));
                    self.accumulated_args.push(PrimArg::App(fresh_id, vec![]));
                    self.register_param_prim_decl(fresh_id, &param_name, 0);
                }
            }
            freshes.push((param.name.clone(), fresh_id, param_kind));
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

    /// Register a prim_decl entry with canonical name from the given entry.
    fn register_prim_decl(&mut self, prim_id: PrimId, qname: &str, level: Level, entry_idx: usize) {
        let canonical = self.canonical_name(qname);
        let entry = &self.entries[entry_idx];
        self.prim_decls.insert(prim_id, PrimDecl {
            name: canonical,
            level,
            color: entry.color,
            param_counts: entry.param_counts.clone(),
        });
    }

    /// Register a prim_decl entry for a parameter (gray, no param_counts).
    fn register_param_prim_decl(&mut self, prim_id: PrimId, qname: &str, level: Level) {
        let canonical = self.canonical_name(qname);
        self.prim_decls.insert(prim_id, PrimDecl {
            name: canonical,
            level,
            color: (128, 128, 128),
            param_counts: vec![],
        });
    }

    /// Register an entry with the common pattern: add_entry + entry_params.
    fn register_entry(
        &mut self,
        qname: String,
        color: Option<ColorSpec>,
        body: EntryBody,
        param_freshes: &[ParamInfo],
    ) -> usize {
        let color = self.resolve_color(color, &body);
        let pc = self.current_param_counts(param_freshes.len());
        let idx = self.add_entry(qname, color, body, pc);
        if !param_freshes.is_empty() {
            self.entry_params.insert(idx, param_freshes.to_vec());
        }
        idx
    }

    /// Register a meta entry: create MetaSig, add entry, store meta value.
    fn register_meta_entry(
        &mut self,
        qname: String,
        color: Option<ColorSpec>,
        prim: Prim,
        ret: Option<MetaType>,
        param_types: Vec<MetaType>,
        meta_val: Option<PrimArg>,
        param_freshes: &[ParamInfo],
    ) -> usize {
        if let Some(ret) = ret {
            self.meta_sigs.insert(prim.id, MetaSig { params: param_types, ret });
        }
        let idx = self.register_entry(qname, color, EntryBody::Meta(prim), param_freshes);
        if let Some(val) = meta_val {
            self.meta_values.insert(idx, val);
        }
        idx
    }

}

// --- Public API ---

pub fn check(program: &Program, tokens: &[Token]) -> (Env, Vec<Error>) {
    let mut checker = Checker::new(program, tokens);
    checker.check_module(&program.root);
    checker.into_result()
}
