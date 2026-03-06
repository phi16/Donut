use crate::types::common::Error;
use crate::types::item::*;
use crate::types::token::Token;
use donut_core::cell::{check_prim, Diagram, Globular};
use donut_core::common::{Prim, PrimArg, PrimId};
use donut_core::free_cell::FreeCell;
use donut_core::pure_cell::PureCell;
use std::collections::HashMap;

type Result<T> = std::result::Result<T, String>;

// --- Output types ---

#[derive(Debug, Clone)]
enum Ty {
    Zero,
    Succ(FreeCell, FreeCell),
    Meta(MetaType),
}

#[derive(Debug, Clone)]
pub enum EntryBody {
    Cell(FreeCell),
    Meta(Prim),
}

impl EntryBody {
    pub fn as_cell(&self) -> Option<&FreeCell> {
        match self {
            EntryBody::Cell(c) => Some(c),
            EntryBody::Meta(_) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Entry {
    pub name: String,
    pub color: (u8, u8, u8),
    pub body: EntryBody,
    pub param_counts: Vec<usize>,
}

#[derive(Debug, Clone)]
pub struct Env {
    pub entries: Vec<Entry>,
    pub lookup: HashMap<String, usize>,
}

// --- Meta type ---

#[derive(Debug, Clone, PartialEq, Eq)]
struct MetaType(PrimId, Vec<MetaType>);

struct MetaSig {
    params: Vec<MetaType>,
    ret: MetaType,
}

// --- Param kind ---

#[derive(Debug, Clone, Copy, PartialEq)]
enum ParamKind {
    Cell,
    Nat,
    Rat,
    Meta,
}

type ParamInfo = (String, PrimId, ParamKind);

// --- Checker ---

struct Checker<'a> {
    program: &'a Program,
    tokens: &'a [Token<'a>],
    next_prim: PrimId,

    // Output (flat)
    entries: Vec<Entry>,
    lookup: HashMap<String, usize>,
    errors: Vec<Error>,

    // Module nesting
    prefixes: Vec<String>,

    // Parametric support
    module_params: HashMap<String, Vec<ParamInfo>>,
    module_members: HashMap<String, Vec<(String, Option<usize>)>>,
    entry_params: HashMap<usize, Vec<ParamInfo>>,
    accumulated_args: Vec<PrimArg>,
    param_count_stack: Vec<usize>,

    // Meta type tracking (short_name → PrimId)
    meta_prim_ids: HashMap<String, PrimId>,
    // Meta type signatures (constructor PrimId → signature)
    meta_sigs: HashMap<PrimId, MetaSig>,
    // Meta values (entry_idx → evaluated PrimArg)
    meta_values: HashMap<usize, PrimArg>,

    // Functor support
    functor_maps: HashMap<String, HashMap<PrimId, PureCell>>,
}

impl<'a> Checker<'a> {
    fn new(program: &'a Program, tokens: &'a [Token<'a>]) -> Self {
        Checker {
            program,
            tokens,
            next_prim: 0,
            entries: Vec::new(),
            lookup: HashMap::new(),
            errors: Vec::new(),
            prefixes: Vec::new(),
            module_params: HashMap::new(),
            module_members: HashMap::new(),
            entry_params: HashMap::new(),
            accumulated_args: Vec::new(),
            param_count_stack: Vec::new(),
            meta_prim_ids: HashMap::new(),
            meta_sigs: HashMap::new(),
            meta_values: HashMap::new(),
            functor_maps: HashMap::new(),
        }
    }

    fn into_result(self) -> (Env, Vec<Error>) {
        let env = Env {
            entries: self.entries,
            lookup: self.lookup,
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
        color: Option<(u8, u8, u8)>,
        body: EntryBody,
        param_counts: Vec<usize>,
    ) -> usize {
        let color = color.unwrap_or_else(|| auto_color(self.entries.len()));
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
        for (name, item_id) in &module.entries {
            let item = self.program.item(*item_id);
            self.check_item(name, item);
        }
    }

    fn check_item(&mut self, name: &str, item: &Item) {
        if matches!(item.kind, Some(ItemKind::Param)) {
            return;
        }

        let decos = self.eval_decorators(&item.decos);
        let color = self.extract_color(&decos);
        let qname = self.qualified_name(name);
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

                match (&item.ty, body_val) {
                    (Some(ty_id), None) => {
                        // Declaration without body
                        let ty_s = self.program.val(*ty_id);
                        match self.eval_ty(&ty_s.0) {
                            Ok((_, Ty::Meta(mt))) => {
                                let prim = self.make_prim();
                                let param_types = self.resolve_item_param_types(&item.params);
                                self.register_meta_entry(
                                    qname.clone(), color, prim, Some(mt),
                                    param_types, None, &param_freshes,
                                );
                            }
                            Ok((_, ty)) => {
                                let prim = self.make_prim();
                                match &ty {
                                    Ty::Zero => {
                                        self.register_cell_entry(
                                            qname.clone(), color, FreeCell::zero(prim), &param_freshes,
                                        );
                                    }
                                    Ty::Succ(s, t) => {
                                        match FreeCell::prim(prim, s.clone(), t.clone()) {
                                            Ok(cell) => {
                                                self.register_cell_entry(
                                                    qname.clone(), color, cell, &param_freshes,
                                                );
                                            }
                                            Err(msg) => self.error_at(span, msg),
                                        }
                                    }
                                    Ty::Meta(_) => unreachable!(),
                                }
                            }
                            Err(msg) => self.error_at(span, msg),
                        }
                    }
                    (dim_ty, Some(body_id)) => {
                        // Determine declared type
                        let declared_ty = dim_ty.and_then(|ty_id| {
                            let ty_s = self.program.val(ty_id);
                            self.eval_ty(&ty_s.0).ok()
                        });
                        let declared_meta = match &declared_ty {
                            Some((_, Ty::Meta(mt))) => Some(mt.clone()),
                            _ => None,
                        };

                        if declared_meta.is_some() {
                            // Meta body
                            self.check_meta_body(
                                &qname, span, color, body_id, declared_meta,
                                &item.params, &param_freshes,
                            );
                        } else {
                            // Cell body (with meta fallback if untyped)
                            self.check_cell_body(
                                &qname, span, color, body_id, declared_ty,
                                &item.params, &param_freshes,
                            );
                        }
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
    }

    fn check_meta_body(
        &mut self,
        qname: &str,
        span: &TokenSpan,
        color: Option<(u8, u8, u8)>,
        body_id: &ValId,
        declared_ret: Option<MetaType>,
        params: &[Param],
        param_freshes: &[ParamInfo],
    ) {
        let body_s = self.program.val(*body_id);
        match self.eval_meta_val(&body_s.0) {
            Ok(meta_val) => {
                let prim = self.make_prim();
                let body_ret = self.check_meta_type(&body_s.0);

                // Check declared type against body type
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

                let param_types = self.resolve_item_param_types(params);
                self.register_meta_entry(
                    qname.to_string(), color, prim, ret,
                    param_types, Some(meta_val), param_freshes,
                );
            }
            Err(msg) => self.error_at(span, msg),
        }
    }

    fn check_cell_body(
        &mut self,
        qname: &str,
        span: &TokenSpan,
        color: Option<(u8, u8, u8)>,
        body_id: &ValId,
        declared_ty: Option<(u8, Ty)>,
        params: &[Param],
        param_freshes: &[ParamInfo],
    ) {
        let body_s = self.program.val(*body_id);
        match self.eval_val(&body_s.0) {
            Ok(cell) => {
                // Validate against declared type if present
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

                self.register_cell_entry(
                    qname.to_string(), color, cell, param_freshes,
                );
            }
            Err(msg) => {
                // If no explicit type, try meta fallback
                if declared_ty.is_none() {
                    let body_s = self.program.val(*body_id);
                    if let Ok(meta_val) = self.eval_meta_val(&body_s.0) {
                        let prim = self.make_prim();
                        let ret = self.check_meta_type(&body_s.0);
                        let param_types = self.resolve_item_param_types(params);
                        self.register_meta_entry(
                            qname.to_string(), color, prim, ret,
                            param_types, Some(meta_val), param_freshes,
                        );
                        return;
                    }
                }
                self.error_at(span, msg);
            }
        }
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

        let mut functor_map: HashMap<PrimId, PureCell> = HashMap::new();
        functor_map.insert(src_prim_id, tgt_cell.pure.clone());

        for mapping in mappings {
            let app_span = &self.program.val(mapping.applicand).1;
            let val_span = &self.program.val(mapping.val).1;
            // Clone spans to avoid borrow issues
            let app_span = app_span.clone();
            let val_span = val_span.clone();

            let app_val = &self.program.val(mapping.applicand).0;
            if let Val::Path(path) = app_val {
                if path.applicand.is_some() {
                    self.error_at(&app_span, "functor application cannot be used in mapping left-hand side");
                    continue;
                }
            }
            let app_cell = match self.eval_val(app_val) {
                Ok(c) => c,
                Err(msg) => {
                    self.error_at(&app_span, msg);
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
                    continue;
                }
            };

            let val_val = &self.program.val(mapping.val).0;
            let val_cell = match self.eval_val(val_val) {
                Ok(c) => c,
                Err(msg) => {
                    self.error_at(&val_span, msg);
                    continue;
                }
            };

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

            functor_map.insert(app_prim_id, val_pure);
        }

        self.functor_maps.insert(qname.to_string(), functor_map);
    }

    fn check_members(&mut self, prefix: &str, module: &Module, parent_param_count: usize) {
        self.prefixes.push(prefix.to_string());
        self.param_count_stack.push(parent_param_count);
        let mut members = Vec::new();
        for (member_name, item_id) in &module.entries {
            let full_name = format!("{}.{}", prefix, member_name);
            let item = self.program.item(*item_id);
            self.check_item(member_name, item);
            let entry_idx = self.lookup.get(&full_name).copied();
            let is_sub_module = self.module_members.contains_key(&full_name);
            if entry_idx.is_some() || is_sub_module {
                members.push((member_name.clone(), entry_idx));
            }
        }
        self.param_count_stack.pop();
        self.prefixes.pop();

        if parent_param_count > 0 || !members.is_empty() {
            self.module_members.insert(prefix.to_string(), members);
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

            match param_kind {
                ParamKind::Cell => {
                    let (_, ty) = self.eval_ty(&ty_s.0)?;
                    let prim = Prim::new(fresh_id);
                    let cell = match &ty {
                        Ty::Zero => FreeCell::zero(prim),
                        Ty::Succ(s, t) => FreeCell::prim(prim, s.clone(), t.clone())?,
                        Ty::Meta(_) => return Err("meta type cannot be used as cell parameter".to_string()),
                    };
                    let param_name = self.qualified_name(&param.name);
                    self.add_entry(param_name, None, EntryBody::Cell(cell.clone()), vec![]);
                    self.accumulated_args.push(PrimArg::Cell(cell.pure.clone()));
                }
                ParamKind::Nat | ParamKind::Rat | ParamKind::Meta => {
                    // Create a meta entry so it can be referenced in meta expressions
                    let prim = Prim::new(fresh_id);
                    let param_name = self.qualified_name(&param.name);
                    let idx = self.add_entry(param_name, None, EntryBody::Meta(prim), vec![]);
                    self.meta_values.insert(idx, PrimArg::App(fresh_id, vec![]));
                    self.accumulated_args.push(PrimArg::App(fresh_id, vec![]));
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

    /// Resolve param types from item params to MetaTypes.
    fn resolve_item_param_types(&self, params: &[Param]) -> Vec<MetaType> {
        params.iter()
            .filter_map(|p| {
                let ty_s = self.program.val(p.ty);
                self.resolve_meta_type(&ty_s.0)
            })
            .collect()
    }

    /// Register a meta entry: create MetaSig, add entry, store meta value.
    /// Returns the entry index.
    fn register_meta_entry(
        &mut self,
        qname: String,
        color: Option<(u8, u8, u8)>,
        prim: Prim,
        ret: Option<MetaType>,
        param_types: Vec<MetaType>,
        meta_val: Option<PrimArg>,
        param_freshes: &[ParamInfo],
    ) -> usize {
        if let Some(ret) = ret {
            self.meta_sigs.insert(prim.id, MetaSig { params: param_types, ret });
        }
        let pc = self.current_param_counts(param_freshes.len());
        let idx = self.add_entry(qname, color, EntryBody::Meta(prim), pc);
        if let Some(val) = meta_val {
            self.meta_values.insert(idx, val);
        }
        if !param_freshes.is_empty() {
            self.entry_params.insert(idx, param_freshes.to_vec());
        }
        idx
    }

    /// Register a cell entry: add entry + entry params.
    fn register_cell_entry(
        &mut self,
        qname: String,
        color: Option<(u8, u8, u8)>,
        cell: FreeCell,
        param_freshes: &[ParamInfo],
    ) -> usize {
        let pc = self.current_param_counts(param_freshes.len());
        let idx = self.add_entry(qname, color, EntryBody::Cell(cell), pc);
        if !param_freshes.is_empty() {
            self.entry_params.insert(idx, param_freshes.to_vec());
        }
        idx
    }

    /// Check if meta type `from` can be coerced to `to` (e.g., nat → rat).
    fn meta_type_coercible(&self, from: &MetaType, to: &MetaType) -> bool {
        if let (Some(nat_id), Some(rat_id)) = (self.meta_id("nat"), self.meta_id("rat")) {
            *from == MetaType(nat_id, vec![]) && *to == MetaType(rat_id, vec![])
        } else {
            false
        }
    }

    fn detect_param_kind(&self, val: &Val) -> ParamKind {
        match self.eval_ty(val) {
            Ok((_, Ty::Meta(mt))) => {
                if self.meta_id("nat") == Some(mt.0) {
                    ParamKind::Nat
                } else if self.meta_id("rat") == Some(mt.0) {
                    ParamKind::Rat
                } else {
                    ParamKind::Meta
                }
            }
            _ => ParamKind::Cell,
        }
    }

    // --- Meta evaluation ---

    fn eval_meta_val(&self, val: &Val) -> Result<PrimArg> {
        match val {
            Val::Path(path) => {
                // Try implicit number resolution first
                if let Some(num) = try_path_as_number(path) {
                    return match num {
                        ImplicitNum::Nat(n) => Ok(PrimArg::Nat(n)),
                        ImplicitNum::Rat(r) => Ok(PrimArg::rat(r)),
                    };
                }

                let name = path_name(path).ok_or_else(|| "invalid meta path".to_string())?;
                let index = self
                    .resolve_name(&name)
                    .ok_or_else(|| format!("unknown: {}", name))?;

                match &self.entries[index].body {
                    EntryBody::Meta(base_prim) => {
                        let mapping = self.build_meta_mapping(index, path)?;

                        // If this entry has a stored body value, substitute params into it
                        if let Some(stored) = self.meta_values.get(&index) {
                            if mapping.is_empty() {
                                return Ok(stored.clone());
                            } else {
                                return Ok(stored.subst(&mapping));
                            }
                        }

                        let new_args = base_prim
                            .args
                            .iter()
                            .map(|a| a.subst(&mapping))
                            .collect();
                        Ok(PrimArg::App(base_prim.id, new_args))
                    }
                    EntryBody::Cell(_) => {
                        Err(format!("{} is not a meta entry", name))
                    }
                }
            }
            Val::Lit(Lit::Number(s)) => {
                if s.contains('.') {
                    let r: f64 = s.parse().map_err(|e: std::num::ParseFloatError| e.to_string())?;
                    Ok(PrimArg::rat(r))
                } else {
                    let n: f64 = s.parse().map_err(|e: std::num::ParseFloatError| e.to_string())?;
                    Ok(PrimArg::Nat(n as u64))
                }
            }
            _ => Err("unsupported in meta context".to_string()),
        }
    }

    fn build_meta_mapping(
        &self,
        entry_idx: usize,
        path: &Path,
    ) -> Result<HashMap<PrimId, PrimArg>> {
        let mut mapping = HashMap::new();
        if let Some(params) = self.entry_params.get(&entry_idx) {
            let seg_params = path
                .segments
                .last()
                .map(|s| &s.0.params[..])
                .unwrap_or(&[]);
            for (i, (_, fresh_id, kind)) in params.iter().enumerate() {
                if let Some(pv) = seg_params.get(i) {
                    let arg = self.resolve_meta_param(pv, *kind)?;
                    mapping.insert(*fresh_id, arg);
                }
            }
        }
        Ok(mapping)
    }

    fn resolve_meta_param(&self, pv: &ParamVal, kind: ParamKind) -> Result<PrimArg> {
        let val_s = self.program.val(pv.val);
        let mut arg = self.eval_meta_val(&val_s.0)?;
        // Nat→Rat coercion when param expects Rat
        if kind == ParamKind::Rat {
            if let PrimArg::Nat(n) = arg {
                arg = PrimArg::rat(n as f64);
            }
        }
        Ok(arg)
    }

    // --- Meta reduction ---

    fn reduce_meta(&self, arg: &PrimArg) -> PrimArg {
        match arg {
            PrimArg::Nat(_) | PrimArg::Rat(_) | PrimArg::Cell(_) => arg.clone(),
            PrimArg::App(id, args) => {
                let reduced: Vec<PrimArg> = args.iter().map(|a| self.reduce_meta(a)).collect();
                if let Some(result) = self.builtin_reduce(*id, &reduced) {
                    return result;
                }
                PrimArg::App(*id, reduced)
            }
        }
    }

    fn builtin_reduce(&self, id: PrimId, args: &[PrimArg]) -> Option<PrimArg> {
        let rgb_id = self.meta_id("rgb")?;
        if self.meta_id("hsv") == Some(id) {
            let h = args.get(0)?.as_rat()?;
            let s = args.get(1).and_then(|a| a.as_rat()).unwrap_or(1.0);
            let v = args.get(2).and_then(|a| a.as_rat()).unwrap_or(1.0);
            let (r, g, b) = hsv2rgb(h, s, v);
            return Some(PrimArg::App(rgb_id, vec![
                PrimArg::Nat(r as u64), PrimArg::Nat(g as u64), PrimArg::Nat(b as u64),
            ]));
        }
        if self.meta_id("lerp") == Some(id) {
            let a = args.get(0)?;
            let b = args.get(1)?;
            let x = args.get(2)?.as_rat()?;
            if let (PrimArg::App(a_id, a_args), PrimArg::App(b_id, b_args)) = (a, b) {
                if *a_id == rgb_id && *b_id == rgb_id {
                    let interp = |i: usize| -> Option<PrimArg> {
                        let av = match a_args.get(i)? { PrimArg::Nat(n) => *n as f64, _ => return None };
                        let bv = match b_args.get(i)? { PrimArg::Nat(n) => *n as f64, _ => return None };
                        Some(PrimArg::Nat((av + (bv - av) * x).round() as u64))
                    };
                    return Some(PrimArg::App(rgb_id, vec![interp(0)?, interp(1)?, interp(2)?]));
                }
            }
        }
        None
    }

    // --- Color extraction ---

    /// Evaluate and type-check all decorators. Returns reduced PrimArg values
    /// for successfully evaluated decorators of type `base.decorator`.
    fn eval_decorators(&mut self, decos: &[ValId]) -> Vec<PrimArg> {
        let decorator_type = self.meta_id("decorator")
            .map(|id| MetaType(id, vec![]));
        let mut result = Vec::new();
        for &deco_id in decos {
            let deco = self.program.val(deco_id);
            let span = deco.1.clone();
            let is_decorator = self.check_meta_type(&deco.0)
                .filter(|ty| decorator_type.as_ref() == Some(ty))
                .is_some();
            if is_decorator {
                match self.eval_meta_val(&deco.0) {
                    Ok(prim_arg) => result.push(self.reduce_meta(&prim_arg)),
                    Err(msg) => self.error_at(&span, msg),
                }
            } else {
                self.error_at(&span, "decorator must have type `base.decorator`");
            }
        }
        result
    }

    /// Extract color from evaluated decorators.
    /// Looks for style[rgb[r, g, b]] by checking PrimIds.
    fn extract_color(&self, decos: &[PrimArg]) -> Option<(u8, u8, u8)> {
        let style_id = self.meta_id("style")?;
        let rgb_id = self.meta_id("rgb")?;
        for deco in decos {
            let PrimArg::App(id, args) = deco else { continue };
            if *id != style_id { continue; }
            let Some(PrimArg::App(color_id, color_args)) = args.first() else { continue };
            if *color_id != rgb_id { continue; }
            let r = match color_args.get(0)? { PrimArg::Nat(n) => *n as u8, _ => continue };
            let g = match color_args.get(1)? { PrimArg::Nat(n) => *n as u8, _ => continue };
            let b = match color_args.get(2)? { PrimArg::Nat(n) => *n as u8, _ => continue };
            return Some((r, g, b));
        }
        None
    }

    // --- Module alias / instantiation ---

    fn try_module_alias(
        &mut self,
        dest_name: &str,
        path: &Path,
    ) -> Result<Option<()>> {
        let (source_name, mapping) = match self.resolve_path_to_module(path)? {
            Some(v) => v,
            None => return Ok(None),
        };
        self.instantiate_module(&source_name, dest_name, &mapping)?;
        Ok(Some(()))
    }

    fn resolve_path_to_module(
        &self,
        path: &Path,
    ) -> Result<Option<(String, HashMap<PrimId, PrimArg>)>> {
        let mut current = String::new();
        let mut mapping = HashMap::new();

        for seg_s in &path.segments {
            let seg = &seg_s.0;
            if current.is_empty() {
                current = seg.name.clone();
            } else {
                current = format!("{}.{}", current, seg.name);
            }

            let resolved_name = self.resolve_module_name(&current);
            let resolved_name = match resolved_name {
                Some(n) => n,
                None => current.clone(),
            };

            if let Some(params) = self.module_params.get(&resolved_name).cloned() {
                for (i, (_, fresh_id, kind)) in params.iter().enumerate() {
                    if let Some(pv) = seg.params.get(i) {
                        let arg = self.resolve_param_arg(pv, *kind)?;
                        mapping.insert(*fresh_id, arg);
                    }
                }
            }

            current = resolved_name;
        }

        if self.module_members.contains_key(&current) {
            Ok(Some((current, mapping)))
        } else {
            Ok(None)
        }
    }

    fn resolve_module_name(&self, name: &str) -> Option<String> {
        for prefix in self.prefixes.iter().rev() {
            let qualified = format!("{}.{}", prefix, name);
            if self.module_members.contains_key(&qualified)
                || self.module_params.contains_key(&qualified)
            {
                return Some(qualified);
            }
        }
        if self.module_members.contains_key(name) || self.module_params.contains_key(name) {
            return Some(name.to_string());
        }
        None
    }

    fn instantiate_module(
        &mut self,
        source: &str,
        dest: &str,
        mapping: &HashMap<PrimId, PrimArg>,
    ) -> Result<()> {
        let members = self
            .module_members
            .get(source)
            .cloned()
            .unwrap_or_default();
        let mut new_members = Vec::new();

        for (member_name, src_idx) in &members {
            let dest_member = format!("{}.{}", dest, member_name);

            let new_idx = if let Some(src_idx) = src_idx {
                let src_entry = &self.entries[*src_idx];
                let new_body = match &src_entry.body {
                    EntryBody::Cell(cell) => {
                        let new_pure = if mapping.is_empty() {
                            cell.pure.clone()
                        } else {
                            cell.pure.subst(mapping)
                        };
                        EntryBody::Cell(FreeCell::from_pure(&new_pure))
                    }
                    EntryBody::Meta(prim) => EntryBody::Meta(prim.clone()),
                };
                let src_pc = src_entry.param_counts.clone();
                let idx = self.add_entry(dest_member.clone(), Some(src_entry.color), new_body, src_pc);

                if let Some(params) = self.entry_params.get(src_idx).cloned() {
                    self.entry_params.insert(idx, params);
                }

                Some(idx)
            } else {
                None
            };

            new_members.push((member_name.clone(), new_idx));

            let src_member_name = format!("{}.{}", source, member_name);
            if self.module_members.contains_key(&src_member_name) {
                self.instantiate_module(&src_member_name, &dest_member, mapping)?;
            }
        }

        self.module_members.insert(dest.to_string(), new_members);
        Ok(())
    }

    // --- Type evaluation ---

    /// Resolve a type annotation Val to a MetaType.
    fn resolve_meta_type(&self, val: &Val) -> Option<MetaType> {
        if let Val::Path(path) = val {
            let name = path_name(path)?;
            let index = self.resolve_name(&name)?;
            if let EntryBody::Meta(prim) = &self.entries[index].body {
                let seg = path.segments.last()?;
                if seg.0.params.is_empty() {
                    return Some(MetaType(prim.id, vec![]));
                }
                let arg_types: Vec<MetaType> = seg.0.params.iter()
                    .filter_map(|pv| {
                        let v = self.program.val(pv.val);
                        self.resolve_meta_type(&v.0)
                    })
                    .collect();
                return Some(MetaType(prim.id, arg_types));
            }
        }
        None
    }

    /// Type-check a meta value expression, returning its MetaType.
    /// Returns None if the expression is not a valid meta expression.
    fn check_meta_type(&self, val: &Val) -> Option<MetaType> {
        match val {
            Val::Lit(Lit::Number(_)) => {
                let id = self.meta_id("nat")?;
                Some(MetaType(id, vec![]))
            }
            Val::Path(path) => {
                // Try implicit number resolution (e.g., 0.6 → rat)
                if let Some(num) = try_path_as_number(path) {
                    let id = match num {
                        ImplicitNum::Nat(_) => self.meta_id("nat")?,
                        ImplicitNum::Rat(_) => self.meta_id("rat")?,
                    };
                    return Some(MetaType(id, vec![]));
                }

                let name = path_name(path)?;
                let index = self.resolve_name(&name)?;
                let EntryBody::Meta(prim) = &self.entries[index].body else { return None };
                let sig = self.meta_sigs.get(&prim.id)?;

                // Collect argument types from the last segment's params
                let seg = path.segments.last()?;
                let arg_types: Vec<Option<MetaType>> = seg.0.params.iter()
                    .map(|pv| {
                        let v = self.program.val(pv.val);
                        self.check_meta_type(&v.0)
                    })
                    .collect();

                // Arity check
                if arg_types.len() != sig.params.len() {
                    return None;
                }

                // Argument type check
                for (arg_ty, param_ty) in arg_types.iter().zip(sig.params.iter()) {
                    match arg_ty {
                        Some(ty) if ty == param_ty => {}
                        Some(ty) if self.meta_type_coercible(ty, param_ty) => {}
                        _ => return None,
                    }
                }

                Some(sig.ret.clone())
            }
            _ => None,
        }
    }

    fn eval_ty(&self, val: &Val) -> Result<(u8, Ty)> {
        match val {
            Val::Path(path) => {
                let name = path_name(path).ok_or_else(|| "invalid type".to_string())?;
                if name == "*" {
                    return Ok((0, Ty::Zero));
                }
                if let Some(mt) = self.resolve_meta_type(val) {
                    return Ok((0, Ty::Meta(mt)));
                }
                // Bootstrap: keyword 'meta' before base.meta is registered
                if name == "meta" {
                    return Ok((0, Ty::Meta(MetaType(0, vec![]))));
                }
                Err("expected `*` or arrow type".to_string())
            }
            Val::Arrow(ArrowKind::To | ArrowKind::Eq, l_id, r_id) => {
                let l = self.program.val(*l_id);
                let r = self.program.val(*r_id);
                let mut s = self.eval_val(&l.0)?;
                let mut t = self.eval_val(&r.0)?;
                let max = s.pure.dim().in_space.max(t.pure.dim().in_space);
                s = lift_dim(s, max);
                t = lift_dim(t, max);
                check_prim(&s.pure, &t.pure)?;
                Ok((max + 1, Ty::Succ(s, t)))
            }
            Val::Arrow(ArrowKind::Functor, _, _) => Err("expected arrow type".to_string()),
            Val::Hole(_) => Err("hole in type position".to_string()),
            _ => Err("expected `*` or arrow type".to_string()),
        }
    }

    // --- Value evaluation ---

    fn eval_val(&self, val: &Val) -> Result<FreeCell> {
        match val {
            Val::Path(path) => self.resolve_path(path),
            Val::Comp(axis, children) => {
                let mut cells: Vec<FreeCell> = children
                    .iter()
                    .map(|id| {
                        let child = self.program.val(*id);
                        self.eval_val(&child.0)
                    })
                    .collect::<Result<_>>()?;
                let max = cells
                    .iter()
                    .map(|c| c.pure.dim().in_space)
                    .max()
                    .unwrap();
                cells = cells.into_iter().map(|c| lift_dim(c, max)).collect();
                Ok(FreeCell::comp(*axis as u8, cells)?)
            }
            Val::Lit(_) => Err("literal in value position".to_string()),
            Val::CompStar(_) => Err("unsupported operator in value".to_string()),
            Val::Arrow(_, _, _) => Err("unsupported operator in value".to_string()),
            Val::Hole(_) => Err("hole in value position".to_string()),
        }
    }

    // --- Path resolution ---

    fn resolve_path(&self, path: &Path) -> Result<FreeCell> {
        if let Some(app_id) = path.applicand {
            return self.resolve_functor_app(path, app_id);
        }
        let name = path_name(path).ok_or_else(|| "invalid path".to_string())?;
        let index = self
            .resolve_name(&name)
            .ok_or_else(|| format!("unknown variable: {}", name))?;

        let base_cell = match &self.entries[index].body {
            EntryBody::Cell(cell) => cell.clone(),
            EntryBody::Meta(_) => return Err(format!("{} is a meta entry, not a cell", name)),
        };

        let mapping = self.build_path_mapping(path)?;
        if mapping.is_empty() {
            Ok(base_cell)
        } else {
            let new_pure = base_cell.pure.subst(&mapping);
            Ok(FreeCell::from_pure(&new_pure))
        }
    }

    fn resolve_functor_app(&self, path: &Path, app_id: ValId) -> Result<FreeCell> {
        let name = path_name(path).ok_or_else(|| "invalid path".to_string())?;

        // Resolve functor name (with prefix search)
        let functor_name = self.resolve_functor_name(&name)
            .ok_or_else(|| format!("{} is not a functor", name))?;

        let map = self.functor_maps.get(&functor_name)
            .ok_or_else(|| format!("{} is not a functor", name))?;

        // Resolve the applicand
        let app_val = &self.program.val(app_id).0;
        let arg_cell = self.eval_val(app_val)?;

        // Apply functor
        let result_pure = apply_functor(&arg_cell.pure, map)?;
        Ok(FreeCell::from_pure(&result_pure))
    }

    fn resolve_functor_name(&self, name: &str) -> Option<String> {
        for prefix in self.prefixes.iter().rev() {
            let qualified = format!("{}.{}", prefix, name);
            if self.functor_maps.contains_key(&qualified) {
                return Some(qualified);
            }
        }
        if self.functor_maps.contains_key(name) {
            return Some(name.to_string());
        }
        None
    }

    fn resolve_param_arg(&self, pv: &ParamVal, kind: ParamKind) -> Result<PrimArg> {
        match kind {
            ParamKind::Cell => {
                let val_s = self.program.val(pv.val);
                let arg_cell = self.eval_val(&val_s.0)?;
                Ok(PrimArg::Cell(arg_cell.pure.clone()))
            }
            ParamKind::Nat => {
                let n = extract_number(self.program, pv)
                    .ok_or_else(|| "expected a number literal for nat parameter".to_string())?;
                Ok(PrimArg::Nat(n as u64))
            }
            ParamKind::Rat => {
                let r = extract_number(self.program, pv)
                    .ok_or_else(|| "expected a number literal for rat parameter".to_string())?;
                Ok(PrimArg::rat(r))
            }
            ParamKind::Meta => {
                let val_s = self.program.val(pv.val);
                self.eval_meta_val(&val_s.0)
            }
        }
    }

    fn build_path_mapping(&self, path: &Path) -> Result<HashMap<PrimId, PrimArg>> {
        let mut mapping = HashMap::new();

        let mut current = String::new();
        for seg_s in &path.segments {
            let seg = &seg_s.0;
            if current.is_empty() {
                current = seg.name.clone();
            } else {
                current = format!("{}.{}", current, seg.name);
            }

            if seg.params.is_empty() {
                continue;
            }

            // Collect param infos from entry_params or module_params
            let params = self.resolve_name(&current)
                .and_then(|idx| self.entry_params.get(&idx))
                .or_else(|| {
                    self.resolve_module_name(&current)
                        .and_then(|name| self.module_params.get(&name))
                });

            if let Some(params) = params {
                for (i, (_, fresh_id, kind)) in params.iter().enumerate() {
                    if let Some(pv) = seg.params.get(i) {
                        let arg = self.resolve_param_arg(pv, *kind)?;
                        mapping.insert(*fresh_id, arg);
                    }
                }
            }
        }

        Ok(mapping)
    }
}

// --- Helpers ---

fn apply_functor(cell: &PureCell, map: &HashMap<PrimId, PureCell>) -> Result<PureCell> {
    match cell {
        PureCell::Prim(prim, _, dim) => match map.get(&prim.id) {
            Some(replacement) => {
                let mut result = replacement.clone();
                while result.dim().in_space < dim.in_space {
                    result = PureCell::id(result);
                }
                Ok(result)
            }
            None => Err(format!("functor: no mapping for P{}", prim.id)),
        },
        PureCell::Comp(axis, children, _) => {
            let mapped: Vec<PureCell> = children
                .iter()
                .map(|c| apply_functor(c, map))
                .collect::<Result<_>>()?;
            PureCell::comp(*axis, mapped)
        }
    }
}

fn match_ty(x: &Ty, y: &Ty) -> Result<()> {
    match (x, y) {
        (Ty::Zero, Ty::Zero) => Ok(()),
        (Ty::Succ(xs, xt), Ty::Succ(ys, yt)) => {
            if !xs.pure.is_convertible(&ys.pure) {
                return Err("type mismatch in arrow source".to_string());
            }
            if !xt.pure.is_convertible(&yt.pure) {
                return Err("type mismatch in arrow target".to_string());
            }
            Ok(())
        }
        (Ty::Meta(a), Ty::Meta(b)) if a == b => Ok(()),
        _ => Err("type mismatch".to_string()),
    }
}

fn auto_color(index: usize) -> (u8, u8, u8) {
    let pi = std::f64::consts::PI;
    let hue = (index as f64 * 137.5) % 360.0 * (pi / 180.0);
    let color = (hue, hue + 2.0 * pi / 3.0, hue + 4.0 * pi / 3.0);
    let color = (
        color.0.sin() * 0.5 + 0.5,
        color.1.sin() * 0.5 + 0.5,
        color.2.sin() * 0.5 + 0.5,
    );
    (
        (color.0 * 255.0) as u8,
        (color.1 * 255.0) as u8,
        (color.2 * 255.0) as u8,
    )
}

fn lift_dim(mut cell: FreeCell, target: u8) -> FreeCell {
    while cell.pure.dim().in_space < target {
        cell = FreeCell::id(cell);
    }
    cell
}

enum ImplicitNum {
    Nat(u64),
    Rat(f64),
}

fn try_path_as_number(path: &Path) -> Option<ImplicitNum> {
    if path.applicand.is_some() {
        return None;
    }
    if path.segments.iter().any(|s| {
        !s.0.params.is_empty() || !crate::convert::is_number_str(&s.0.name)
    }) {
        return None;
    }
    match path.segments.len() {
        1 => {
            let s = &path.segments[0].0.name;
            let n: u64 = s.parse().ok()?;
            Some(ImplicitNum::Nat(n))
        }
        2 => {
            let s = format!("{}.{}", path.segments[0].0.name, path.segments[1].0.name);
            let r: f64 = s.parse().ok()?;
            Some(ImplicitNum::Rat(r))
        }
        _ => None,
    }
}

fn path_name(path: &Path) -> Option<String> {
    let parts: Vec<_> = path.segments.iter().map(|s| s.0.name.clone()).collect();
    if parts.is_empty() {
        return None;
    }
    Some(parts.join("."))
}

fn extract_number(program: &Program, param: &ParamVal) -> Option<f64> {
    let val = program.val(param.val);
    match &val.0 {
        Val::Lit(Lit::Number(s)) => s.parse().ok(),
        Val::Path(path) => match try_path_as_number(path)? {
            ImplicitNum::Nat(n) => Some(n as f64),
            ImplicitNum::Rat(r) => Some(r),
        },
        _ => None,
    }
}

fn hsv2rgb(h: f64, s: f64, v: f64) -> (u8, u8, u8) {
    let r = (((h + 3.0 / 3.0).fract() * 6.0 - 3.0).abs() - 1.0).clamp(0.0, 1.0);
    let g = (((h + 2.0 / 3.0).fract() * 6.0 - 3.0).abs() - 1.0).clamp(0.0, 1.0);
    let b = (((h + 1.0 / 3.0).fract() * 6.0 - 3.0).abs() - 1.0).clamp(0.0, 1.0);
    let r = (r * s + (1.0 - s)) * v;
    let g = (g * s + (1.0 - s)) * v;
    let b = (b * s + (1.0 - s)) * v;
    ((r * 255.0) as u8, (g * 255.0) as u8, (b * 255.0) as u8)
}

// --- Public API ---

pub fn check(program: &Program, tokens: &[Token]) -> (Env, Vec<Error>) {
    let mut checker = Checker::new(program, tokens);
    checker.check_module(&program.root);
    checker.into_result()
}
