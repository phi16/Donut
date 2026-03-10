use std::collections::HashMap;

use crate::types::token::Token;
use crate::types::common::{Error, TokenSpan};
use crate::types::env::{self, ArrowTy, Meta, Ty};
use crate::types::item::*;
use donut_core::cell::Diagram;
use donut_core::cell::Globular;
use donut_core::common::{Axis, ExtId, Level, Prim, PrimId, PureVal};
use donut_core::pure_cell::PureCell;

struct FunctorEntry {
    param_ext_ids: Vec<ExtId>,
    cell: PureCell,
}

struct Checker<'a> {
    program: &'a Program,
    tokens: &'a [Token<'a>],

    // Output
    env_items: Vec<Option<env::Item>>,
    env_defs: Vec<Option<env::Def>>,
    prim_item: HashMap<PrimId, ItemId>,

    // State
    next_prim: u64,
    prefixes: Vec<String>,
    // Ref → PureVal for checked items/defs
    checked: HashMap<Ref, PureVal>,
    // DefId → (PrimId → FunctorEntry)
    functor_maps: HashMap<DefId, HashMap<PrimId, FunctorEntry>>,

    errors: Vec<Error>,
}

impl<'a> Checker<'a> {
    fn new(program: &'a Program, tokens: &'a [Token<'a>]) -> Self {
        let n_items = program.items.len();
        let n_defs = program.defs.len();
        let mut checker = Checker {
            program,
            tokens,
            env_items: (0..n_items).map(|_| None).collect(),
            env_defs: (0..n_defs).map(|_| None).collect(),
            prim_item: HashMap::new(),
            next_prim: 1, // 0 reserved for meta
            prefixes: Vec::new(),
            checked: HashMap::new(),
            functor_maps: HashMap::new(),
            errors: Vec::new(),
        };
        checker.register_builtins();
        checker
    }

    fn register_builtins(&mut self) {
        let builtins: &[(&str, Ty)] = &[("meta", Ty::Meta), ("*", Ty::Star)];
        for (i, item) in self.program.items.iter().enumerate() {
            if let Some((_, ty)) = builtins.iter().find(|(name, _)| *name == item.lname) {
                let item_id = ItemId(i);
                let pv: PureVal = Meta::Ty(ty.clone()).into();
                self.checked.insert(Ref::Item(item_id), pv);
                self.env_items[i] = Some(env::Item {
                    cname: item.cname.clone(),
                    lname: item.lname.clone(),
                    kind: item.kind,
                    ty: Ty::Meta,
                    prim_id: None,
                    params: vec![],
                });
            }
        }
    }

    fn error_at(&mut self, span: &TokenSpan, msg: impl Into<String>) {
        if let Some(token) = self.tokens.get(span.start) {
            self.errors.push((token.pos.clone(), msg.into()));
        }
    }

    fn fresh_prim_id(&mut self) -> PrimId {
        let id = PrimId(self.next_prim);
        self.next_prim += 1;
        id
    }

    fn current_prefix(&self) -> String {
        self.prefixes.join(".")
    }

    // --- DefTree traversal ---

    fn process_trees(&mut self, trees: &[DefTree]) -> HashMap<String, env::Module> {
        let mut lookup: HashMap<String, env::Module> = HashMap::new();
        for tree in trees {
            let module = self.process_tree(tree);
            let lname = self.program.def(tree.def_id).lname.clone();
            if let Some(existing) = lookup.get_mut(&lname) {
                // += : merge children into existing module
                existing.lookup.extend(module.lookup);
            } else {
                lookup.insert(lname, module);
            }
        }
        lookup
    }

    fn process_tree(&mut self, tree: &DefTree) -> env::Module {
        let def_id = tree.def_id;

        let def = self.program.def(def_id);
        let lname = def.lname.clone();
        let params = def.params.clone();

        self.prefixes.push(lname);

        // Check param Items
        for param in &params {
            let ty = self.eval_ty(param.ty);
            self.check_item(param.item, &ty);
        }

        // Process this def
        self.check_def(def_id);

        // Process children → build lookup
        let lookup = self.process_trees(&tree.children);

        self.prefixes.pop();

        env::Module {
            this: Some(def_id),
            lookup,
        }
    }

    fn check_def(&mut self, def_id: DefId) {
        if self.env_defs[def_id.0].is_some() {
            return; // already checked
        }

        let def = self.program.def(def_id);
        let span = def.span.clone();
        let lname = def.lname.clone();
        let qname = def.qname.clone();
        let origin = def.origin.clone();
        let param_counts = def.param_counts.clone();

        // Evaluate type
        let ty = if let Some(ty_val) = def.ty {
            self.eval_ty(ty_val)
        } else {
            Ty::Star // default; will be refined
        };

        // Evaluate body
        let (item, val) = match &self.program.def(def_id).body {
            DefBody::Decl { item: item_id } => {
                let item_id = *item_id;
                self.check_item(item_id, &ty);
                let pv = self.checked[&Ref::Item(item_id)].clone();
                (Some(item_id), pv)
            }
            DefBody::Alias { val: val_id } => {
                let val_id = *val_id;
                let pv = self.eval_val(val_id);
                (None, pv)
            }
            DefBody::Functor { mappings } => {
                self.check_functor(def_id, &ty, mappings);
                (None, PureVal::App(ExtId(0), vec![]))
            }
            DefBody::None => {
                (None, PureVal::App(ExtId(0), vec![]))
            }
        };

        // Evaluate decorators
        let decos: Vec<PureVal> = self.program.def(def_id).decos
            .iter()
            .map(|&d| self.eval_val(d))
            .collect();

        // Params → ItemIds (already checked in process_tree)
        let params: Vec<ItemId> = self.program.def(def_id).params
            .iter()
            .map(|p| p.item)
            .collect();

        let env_def = env::Def {
            qname,
            lname,
            span,
            item,
            ty: ty.clone(),
            params,
            val,
            decos,
            origin,
            param_counts,
        };
        self.checked.insert(Ref::Def(def_id), env_def.val.clone());
        self.env_defs[def_id.0] = Some(env_def);
    }

    // --- Type evaluation ---

    fn eval_ty(&mut self, val_id: ValId) -> Ty {
        let pv = self.eval_val(val_id);
        self.extract_ty(&pv, val_id)
    }

    fn extract_ty(&mut self, pv: &PureVal, val_id: ValId) -> Ty {
        if let Some(Meta::Ty(ty)) = env::as_meta(pv) {
            ty.clone()
        } else {
            let span = self.program.val_span(val_id);
            self.error_at(span, "expected type expression");
            Ty::Star
        }
    }

    // --- Value evaluation ---

    fn eval_val(&mut self, val_id: ValId) -> PureVal {
        let val = self.program.val(val_id);
        match val {
            Val::Path(path) => self.eval_val_path(path, val_id),
            Val::Lit(lit) => self.eval_lit(lit, val_id),
            Val::Comp(axis, children) => self.eval_comp(*axis, children, val_id),
            Val::CompStar(children) => self.eval_comp_star(children, val_id),
            Val::Arrow(kind, l, r) => {
                let l_val = self.eval_val(*l);
                let r_val = self.eval_val(*r);
                let ty = match kind {
                    ArrowKind::To | ArrowKind::Eq => {
                        let l_level = self.level_of(&l_val);
                        let r_level = self.level_of(&r_val);
                        let level = match (l_level, r_level) {
                            (Some(l), Some(r)) => l.max(r) + 1,
                            (Some(l), None) => l + 1,
                            (None, Some(r)) => r + 1,
                            (None, None) => 1,
                        };
                        let arrow_ty = match kind {
                            ArrowKind::To => ArrowTy::To,
                            _ => ArrowTy::Eq,
                        };
                        Ty::Arrow(level, arrow_ty, l_val, r_val)
                    }
                    ArrowKind::Functor => {
                        Ty::Functor(l_val, r_val)
                    }
                };
                Meta::Ty(ty).into()
            }
            Val::Hole(_) => Meta::Ty(Ty::Hole).into(),
        }
    }

    fn eval_val_path(&mut self, path: &Path, val_id: ValId) -> PureVal {
        let base = if let Some(pv) = self.resolve_ref(path.target) {
            pv
        } else {
            let span = self.program.val_span(val_id);
            self.error_at(span, "unresolved path");
            return PureVal::App(ExtId(0), vec![]);
        };

        // Apply args
        let result = if path.args.is_empty() {
            base
        } else {
            let args: Vec<PureVal> = path.args.iter().map(|&a| self.eval_val(a)).collect();
            match base {
                PureVal::App(id, mut existing) => {
                    existing.extend(args);
                    PureVal::App(id, existing)
                }
                _ => {
                    let span = self.program.val_span(val_id);
                    self.error_at(span, "cannot apply arguments to this value");
                    base
                }
            }
        };

        // Functor application: f(x)
        if let Some(app_val_id) = path.applicand {
            let applicand = self.eval_val(app_val_id);
            let app_cell = match &applicand {
                PureVal::Cell(pc) => pc,
                _ => {
                    let span = self.program.val_span(val_id);
                    self.error_at(span, "functor applicand must be a cell value");
                    return result;
                }
            };
            // Find functor map for this def
            let functor_def_id = match path.target {
                Ref::Def(did) => did,
                Ref::Item(_) => {
                    let span = self.program.val_span(val_id);
                    self.error_at(span, "functor application target must be a definition");
                    return result;
                }
            };
            if let Some(fmap) = self.functor_maps.get(&functor_def_id) {
                match apply_functor(app_cell, fmap) {
                    Ok(pc) => PureVal::Cell(pc),
                    Err(e) => {
                        let span = self.program.val_span(val_id);
                        self.error_at(span, format_functor_error(&e));
                        result
                    }
                }
            } else {
                let span = self.program.val_span(val_id);
                self.error_at(span, "not a functor");
                result
            }
        } else {
            result
        }
    }

    fn eval_lit(&mut self, lit: &Lit, val_id: ValId) -> PureVal {
        match lit {
            Lit::Number(s) => {
                // Try as nat
                if let Ok(n) = s.parse::<u64>() {
                    Meta::Nat(n).into()
                } else if let Ok(f) = s.parse::<f64>() {
                    Meta::Rat(f).into()
                } else {
                    let span = self.program.val_span(val_id);
                    self.error_at(span, format!("invalid number `{}`", s));
                    Meta::Nat(0).into()
                }
            }
            Lit::String(_s) => {
                // String literals → used for import paths etc.
                PureVal::App(ExtId(0), vec![])
            }
            Lit::Array(_) | Lit::Object(_) => {
                // TODO
                PureVal::App(ExtId(0), vec![])
            }
        }
    }

    fn eval_comp(&mut self, axis: Axis, children: &[ValId], val_id: ValId) -> PureVal {
        let child_vals: Vec<PureVal> = children.iter().map(|&c| self.eval_val(c)).collect();
        let cells: Vec<PureCell> = child_vals
            .into_iter()
            .filter_map(|pv| match pv {
                PureVal::Cell(pc) => Some(pc),
                _ => None,
            })
            .collect();
        if cells.len() != children.len() {
            let span = self.program.val_span(val_id);
            self.error_at(span, "composition requires cell values");
            return PureVal::App(ExtId(0), vec![]);
        }
        match PureCell::comp(axis, cells) {
            Ok(pc) => PureVal::Cell(pc),
            Err(e) => {
                let span = self.program.val_span(val_id);
                self.error_at(span, format!("{}", e));
                PureVal::App(ExtId(0), vec![])
            }
        }
    }

    fn eval_comp_star(&mut self, _children: &[ValId], _val_id: ValId) -> PureVal {
        PureVal::App(ExtId(0), vec![])
    }

    // --- Level inference ---

    fn level_of_ty(&self, ty: &Ty) -> Option<Level> {
        match ty {
            Ty::Star | Ty::Meta | Ty::Nat | Ty::Rat | Ty::Color | Ty::Deco => Some(0),
            Ty::Arrow(level, _, _, _) => Some(*level),
            Ty::Functor(_, _) | Ty::Hole => None,
        }
    }

    fn level_of(&self, pv: &PureVal) -> Option<Level> {
        match pv {
            PureVal::Cell(pc) => Some(pc.dim().in_space),
            PureVal::App(ext_id, _) => {
                let idx = ext_id.0 as usize;
                self.env_items.get(idx)?.as_ref().and_then(|item| self.level_of_ty(&item.ty))
            }
            _ => match env::as_meta(pv)? {
                Meta::Ty(ty) => self.level_of_ty(ty),
                _ => Some(0),
            }
        }
    }

    // --- Functor checking ---

    fn check_functor(&mut self, def_id: DefId, ty: &Ty, mappings: &[FunctorMapping]) {
        // Type must be Functor(src, tgt)
        let (src_val, tgt_val) = match ty {
            Ty::Functor(s, t) => (s.clone(), t.clone()),
            _ => {
                let span = self.program.def(def_id).span.clone();
                self.error_at(&span, "functor must have type `A ~> B`");
                return;
            }
        };

        // Extract PureCells from src/tgt
        let (src_cell, tgt_cell) = match (&src_val, &tgt_val) {
            (PureVal::Cell(s), PureVal::Cell(t)) => (s.clone(), t.clone()),
            _ => {
                let span = self.program.def(def_id).span.clone();
                self.error_at(&span, "functor source and target must be cell values");
                return;
            }
        };

        // Extract source PrimId for base case
        let src_prim_id = match src_cell.extract_prim_id() {
            Some(id) => id,
            None => {
                let span = self.program.def(def_id).span.clone();
                self.error_at(&span, "functor source must be a primitive cell");
                return;
            }
        };

        // Base case: source → target
        let mut functor_map: HashMap<PrimId, FunctorEntry> = HashMap::new();
        functor_map.insert(src_prim_id, FunctorEntry {
            param_ext_ids: vec![],
            cell: tgt_cell.clone(),
        });

        // Process each mapping
        for mapping in mappings {
            // Check mapping params
            let param_ext_ids: Vec<ExtId> = mapping.params.iter().map(|param| {
                let ty = self.eval_ty(param.ty);
                self.check_item(param.item, &ty);
                ExtId(param.item.0 as u64)
            }).collect();

            // Eval applicand
            let app_val = self.eval_val(mapping.applicand);
            let app_cell = match &app_val {
                PureVal::Cell(pc) => pc.clone(),
                _ => {
                    let span = self.program.val_span(mapping.applicand);
                    self.error_at(span, "functor mapping applicand must be a cell value");
                    continue;
                }
            };

            let app_prim_id = match app_cell.extract_prim_id() {
                Some(id) => id,
                None => {
                    let span = self.program.val_span(mapping.applicand);
                    self.error_at(span, "functor mapping applicand must be a primitive cell");
                    continue;
                }
            };

            // Eval val (right-hand side)
            let val_val = self.eval_val(mapping.val);
            let mut val_cell = match &val_val {
                PureVal::Cell(pc) => pc.clone(),
                _ => {
                    let span = self.program.val_span(mapping.val);
                    self.error_at(span, "functor mapping value must be a cell value");
                    continue;
                }
            };

            let app_dim = app_cell.dim().in_space;

            // 0-cell: check consistency with base case
            if app_dim == 0 {
                if app_prim_id == src_prim_id {
                    if !val_cell.is_convertible(&tgt_cell) {
                        let span = self.program.val_span(mapping.val);
                        self.error_at(span, "functor 0-cell mapping contradicts base case");
                    }
                }
                continue;
            }

            // Lift val to app dimension if needed
            while val_cell.dim().in_space < app_dim {
                val_cell = PureCell::id(val_cell);
            }

            // Functoriality check: apply_functor(app.s) == val.s, apply_functor(app.t) == val.t
            let expected_s = apply_functor(&app_cell.s(), &functor_map);
            let expected_t = apply_functor(&app_cell.t(), &functor_map);

            match (expected_s, expected_t) {
                (Ok(es), Ok(et)) => {
                    if !es.is_convertible(&val_cell.s()) {
                        let span = self.program.val_span(mapping.val);
                        self.error_at(span, "functor mapping source mismatch");
                        continue;
                    }
                    if !et.is_convertible(&val_cell.t()) {
                        let span = self.program.val_span(mapping.val);
                        self.error_at(span, "functor mapping target mismatch");
                        continue;
                    }
                }
                (Err(e), _) | (_, Err(e)) => {
                    let span = self.program.val_span(mapping.applicand);
                    self.error_at(span, format_functor_error(&e));
                    continue;
                }
            }

            functor_map.insert(app_prim_id, FunctorEntry {
                param_ext_ids,
                cell: val_cell,
            });
        }

        self.functor_maps.insert(def_id, functor_map);
    }

    // --- Path resolution ---

    fn resolve_ref(&self, target: Ref) -> Option<PureVal> {
        self.checked.get(&target).cloned()
    }

    // --- Item checking ---

    fn check_item(&mut self, item_id: ItemId, ty: &Ty) {
        if self.checked.contains_key(&Ref::Item(item_id)) {
            return;
        }
        let item = self.program.item(item_id);
        let prim_id = self.fresh_prim_id();
        let ext_id = ExtId(item_id.0 as u64);

        let pv = PureVal::App(ext_id, vec![]);

        self.checked.insert(Ref::Item(item_id), pv);
        self.prim_item.insert(prim_id, item_id);

        let env_item = env::Item {
            cname: item.cname.clone(),
            lname: item.lname.clone(),
            kind: item.kind,
            ty: ty.clone(),
            prim_id: Some(prim_id),
            params: item.params.iter().map(|p| p.item).collect(),
        };
        self.env_items[item_id.0] = Some(env_item);
    }

    // --- Finalize ---

    fn into_result(self, root_lookup: HashMap<String, env::Module>) -> (env::Env, Vec<Error>) {
        let root = env::Module {
            this: None,
            lookup: root_lookup,
        };

        // Fill unchecked entries with defaults
        let items: Vec<env::Item> = self.env_items.into_iter().enumerate().map(|(i, o)| {
            o.unwrap_or_else(|| {
                let item = self.program.item(ItemId(i));
                env::Item {
                    cname: item.cname.clone(),
                    lname: item.lname.clone(),
                    kind: item.kind,
                    ty: Ty::Star,
                    prim_id: None,
                    params: vec![],
                }
            })
        }).collect();
        let defs: Vec<env::Def> = self.env_defs.into_iter().enumerate().map(|(i, o)| {
            o.unwrap_or_else(|| {
                let def = self.program.def(DefId(i));
                env::Def {
                    qname: def.qname.clone(),
                    lname: def.lname.clone(),
                    span: def.span.clone(),
                    item: None,
                    ty: Ty::Star,
                    params: vec![],
                    val: PureVal::App(ExtId(0), vec![]),
                    decos: vec![],
                    origin: def.origin.clone(),
                    param_counts: def.param_counts.clone(),
                }
            })
        }).collect();

        let env = env::Env {
            items,
            defs,
            prim_item: self.prim_item,
            root,
        };
        (env, self.errors)
    }
}

// --- Functor application ---

enum FunctorError {
    NoMapping(Prim),
    CompError(donut_core::common::Error),
}

fn apply_functor(cell: &PureCell, map: &HashMap<PrimId, FunctorEntry>) -> Result<PureCell, FunctorError> {
    match cell {
        PureCell::Prim(prim, _, dim) => {
            match map.get(&prim.id) {
                Some(entry) => {
                    let mut result = entry.cell.clone();
                    // Substitute params with actual args
                    if !entry.param_ext_ids.is_empty() && !prim.args.is_empty() {
                        let subst_map: HashMap<ExtId, PureVal> = entry.param_ext_ids.iter()
                            .zip(prim.args.iter())
                            .map(|(&eid, arg)| (eid, arg.clone()))
                            .collect();
                        result = result.subst(&subst_map);
                    }
                    // Lift to target dimension
                    while result.dim().in_space < dim.in_space {
                        result = PureCell::id(result);
                    }
                    Ok(result)
                }
                None => Err(FunctorError::NoMapping(prim.clone())),
            }
        }
        PureCell::Comp(axis, children, _) => {
            let mapped: Vec<PureCell> = children
                .iter()
                .map(|c| apply_functor(c, map))
                .collect::<Result<_, _>>()?;
            PureCell::comp(*axis, mapped).map_err(FunctorError::CompError)
        }
    }
}

fn format_functor_error(e: &FunctorError) -> String {
    match e {
        FunctorError::NoMapping(prim) => format!("functor: no mapping for prim {:?}", prim.id),
        FunctorError::CompError(e) => format!("functor composition error: {}", e),
    }
}

// --- Public API ---

pub fn check(program: &Program, tokens: &[Token]) -> (env::Env, Vec<Error>) {
    let mut checker = Checker::new(program, tokens);
    let root_lookup = checker.process_trees(&program.def_order);
    checker.into_result(root_lookup)
}
