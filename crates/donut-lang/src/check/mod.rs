use std::collections::HashMap;

use crate::types::common::{Error, TokenSpan};
use crate::types::env::{self, ArrowTy, Meta, Ty};
use crate::types::item::*;
use crate::types::token::Token;
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

    // --- Display helpers ---

    fn prim_name(&self, id: PrimId) -> String {
        if let Some(&item_id) = self.prim_item.get(&id) {
            self.program.item(item_id).cname.clone()
        } else {
            format!("P{}", id.0)
        }
    }

    fn item_name(&self, id: ItemId) -> String {
        self.program.item(id).cname.clone()
    }

    fn display_pure_val(&self, pv: &PureVal) -> String {
        match pv {
            PureVal::Cell(cell) => self.display_cell(cell),
            PureVal::App(ext_id, args) => {
                let name = self.item_name(ItemId(ext_id.0 as usize));
                if args.is_empty() {
                    name
                } else {
                    let args_str: Vec<String> =
                        args.iter().map(|a| self.display_pure_val(a)).collect();
                    format!("{}[{}]", name, args_str.join(", "))
                }
            }
            PureVal::Any(any) => self.display_meta(any.inner::<Meta>()),
        }
    }

    fn display_cell(&self, cell: &PureCell) -> String {
        match cell {
            PureCell::Prim(prim, _, dim) => {
                let name = self.prim_name(prim.id);
                if !prim.args.is_empty() {
                    let args_str: Vec<String> =
                        prim.args.iter().map(|a| self.display_pure_val(a)).collect();
                    format!("{}[{}]", name, args_str.join(", "))
                } else if dim.in_space == 0 {
                    name
                } else {
                    name
                }
            }
            PureCell::Comp(axis, children, _) => {
                let sep = match axis {
                    0 => " ",
                    1 => "; ",
                    _ => ", ",
                };
                let parts: Vec<String> = children.iter().map(|c| self.display_cell(c)).collect();
                if *axis >= 2 {
                    format!("[{}: {}]", axis, parts.join(sep))
                } else {
                    parts.join(sep)
                }
            }
        }
    }

    fn display_meta(&self, meta: &Meta) -> String {
        match meta {
            Meta::Ty(ty) => self.display_ty(ty),
            Meta::Nat(n) => format!("{}", n),
            Meta::Rat(r) => format!("{}", r),
            Meta::Color(c) => format!("rgb({}, {}, {})", c.0, c.1, c.2),
            Meta::Deco(d) => format!("{:?}", d),
            Meta::Error => "<error>".to_string(),
        }
    }

    fn display_ty(&self, ty: &Ty) -> String {
        match ty {
            Ty::Meta => "meta".to_string(),
            Ty::Star => "*".to_string(),
            Ty::Arrow(_, arrow_ty, src, tgt) => {
                let op = match arrow_ty {
                    ArrowTy::To => "→",
                    ArrowTy::Eq => "~",
                };
                format!(
                    "{} {} {}",
                    self.display_pure_val(src),
                    op,
                    self.display_pure_val(tgt)
                )
            }
            Ty::Functor(src, tgt) => {
                format!(
                    "{} ~> {}",
                    self.display_pure_val(src),
                    self.display_pure_val(tgt)
                )
            }
            Ty::Nat => "nat".to_string(),
            Ty::Rat => "rat".to_string(),
            Ty::Color => "color".to_string(),
            Ty::Deco => "decorator".to_string(),
            Ty::Hole => "_".to_string(),
        }
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
        let def_span = self.program.def(def_id).span.clone();
        for param in &params {
            let ty = self.eval_ty(param.ty);
            self.check_item(param.item, &ty, &def_span);
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

        // Ensure params are checked
        let params = self.program.def(def_id).params.clone();
        let def_span = self.program.def(def_id).span.clone();
        for param in &params {
            let ty = self.eval_ty(param.ty);
            self.check_item(param.item, &ty, &def_span);
        }

        let def = self.program.def(def_id);
        let span = def.span.clone();
        let lname = def.lname.clone();
        let qname = def.qname.clone();
        let origin = def.origin.clone();
        let param_counts = def.param_counts.clone();

        // Evaluate declared type (if any)
        let declared_ty = def.ty.map(|ty_val| self.eval_ty(ty_val));

        // Evaluate body
        let (item, val, ty) = match &self.program.def(def_id).body {
            DefBody::Decl { item: item_id } => {
                let item_id = *item_id;
                let ty = declared_ty.unwrap_or(Ty::Star);
                self.check_item(item_id, &ty, &span);
                let pv = self.checked[&Ref::Item(item_id)].clone();
                (Some(item_id), pv, ty)
            }
            DefBody::Alias { val: val_id } => {
                let val_id = *val_id;
                let mut pv = self.eval_val(val_id);
                let ty = if let Some(declared) = declared_ty {
                    pv = self.coerce_val_to_ty(pv, &declared, &span);
                    declared
                } else {
                    self.infer_ty(&pv)
                };
                (None, pv, ty)
            }
            DefBody::Functor { mappings } => {
                let ty = declared_ty.unwrap_or(Ty::Hole);
                self.check_functor(def_id, &ty, mappings);
                (None, Meta::Error.into(), ty)
            }
            DefBody::None => {
                let ty = declared_ty.unwrap_or(Ty::Star);
                (None, Meta::Error.into(), ty)
            }
        };

        // Evaluate and validate decorators
        let deco_ids: Vec<ValId> = self.program.def(def_id).decos.clone();
        let decos: Vec<PureVal> = deco_ids
            .iter()
            .map(|&d| {
                let pv = self.eval_val(d);
                let deco_span = self.program.val_span(d);
                if !self.check_meta_val(&pv, Ty::Deco) {
                    self.error_at(deco_span, "decorator must be a decorator value");
                }
                pv
            })
            .collect();

        // Params → ItemIds (already checked in process_tree)
        let params: Vec<ItemId> = self
            .program
            .def(def_id)
            .params
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

    fn infer_ty(&self, pv: &PureVal) -> Ty {
        match pv {
            PureVal::Cell(pc) => {
                let dim = pc.dim().in_space;
                if dim == 0 {
                    Ty::Star
                } else {
                    Ty::Arrow(
                        dim,
                        ArrowTy::To,
                        PureVal::Cell(pc.s()),
                        PureVal::Cell(pc.t()),
                    )
                }
            }
            PureVal::App(ext_id, args) => {
                let idx = ext_id.0 as usize;
                if let Some(Some(item)) = self.env_items.get(idx) {
                    if args.is_empty() {
                        item.ty.clone()
                    } else {
                        // Substitute param ExtIds with actual args
                        let subst_map: HashMap<ExtId, PureVal> = item
                            .params
                            .iter()
                            .zip(args.iter())
                            .map(|(param_item_id, arg)| {
                                (ExtId(param_item_id.0 as u64), arg.clone())
                            })
                            .collect();
                        subst_ty(&item.ty, &subst_map, &self.prim_item)
                    }
                } else {
                    Ty::Hole
                }
            }
            _ => match env::as_meta(pv) {
                Some(Meta::Ty(_)) => Ty::Meta,
                Some(Meta::Nat(_)) => Ty::Nat,
                Some(Meta::Rat(_)) => Ty::Rat,
                Some(Meta::Color(_)) => Ty::Color,
                Some(Meta::Deco(_)) => Ty::Deco,
                Some(Meta::Error) => Ty::Hole,
                None => Ty::Hole,
            },
        }
    }

    fn extract_ty(&mut self, pv: &PureVal, val_id: ValId) -> Ty {
        if let Some(Meta::Ty(ty)) = env::as_meta(pv) {
            ty.clone()
        } else {
            let span = self.program.val_span(val_id);
            self.error_at(
                span,
                format!(
                    "expected type expression, got `{}`",
                    self.display_pure_val(pv)
                ),
            );
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
                    ArrowKind::Functor => Ty::Functor(l_val, r_val),
                };
                Meta::Ty(ty).into()
            }
            Val::Hole(_) => Meta::Ty(Ty::Hole).into(),
            Val::Subst(inner_id, mapping) => {
                let inner_id = *inner_id;
                let mapping: HashMap<ItemId, ValId> = mapping.clone();
                let inner_pv = self.eval_val(inner_id);
                // Convert HashMap<ItemId, ValId> to HashMap<ExtId, PureVal>
                let subst_map: HashMap<ExtId, PureVal> = mapping
                    .iter()
                    .map(|(item_id, &val_id)| {
                        let ext_id = ExtId(item_id.0 as u64);
                        let pv = self.eval_val(val_id);
                        (ext_id, pv)
                    })
                    .collect();
                subst_pv(&inner_pv, &subst_map, &self.prim_item)
            }
        }
    }

    fn eval_val_path(&mut self, path: &Path, val_id: ValId) -> PureVal {
        let base = if let Some(pv) = self.resolve_ref(path.target) {
            pv
        } else {
            let span = self.program.val_span(val_id);
            let target_name = match path.target {
                Ref::Def(def_id) => format!("`{}` (unchecked)", self.program.def(def_id).qname),
                Ref::Item(item_id) => format!("`{}` (item)", self.program.item(item_id).cname),
            };
            self.error_at(span, format!("unresolved path: {}", target_name));
            return Meta::Error.into();
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
                    // Substitute param ExtIds with actual args (Cell, Any/Meta, etc.)
                    let param_ids = self.param_ext_ids(path.target);
                    let subst_map: HashMap<ExtId, PureVal> =
                        param_ids.into_iter().zip(args.into_iter()).collect();
                    subst_pv(&base, &subst_map, &self.prim_item)
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
                match apply_functor(app_cell, fmap, &self.prim_item) {
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
                Meta::Error.into()
            }
            Lit::Array(_) | Lit::Object(_) => {
                // TODO
                Meta::Error.into()
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
            return Meta::Error.into();
        }
        // Lift all cells to the max dimension
        let max_dim = cells.iter().map(|c| c.dim().in_space).max().unwrap_or(0);
        let cells: Vec<PureCell> = cells
            .into_iter()
            .map(|mut c| {
                while c.dim().in_space < max_dim {
                    c = PureCell::id(c);
                }
                c
            })
            .collect();
        match PureCell::comp(axis, cells) {
            Ok(pc) => PureVal::Cell(pc),
            Err(e) => {
                let span = self.program.val_span(val_id);
                self.error_at(span, format!("{}", e));
                Meta::Error.into()
            }
        }
    }

    fn eval_comp_star(&mut self, _children: &[ValId], _val_id: ValId) -> PureVal {
        Meta::Error.into()
    }

    // --- Level inference ---

    fn level_of_ty(&self, ty: &Ty) -> Option<Level> {
        match ty {
            Ty::Star => Some(0),
            Ty::Arrow(level, _, _, _) => Some(*level),
            Ty::Meta | Ty::Nat | Ty::Rat | Ty::Color | Ty::Deco => None,
            Ty::Functor(_, _) | Ty::Hole => None,
        }
    }

    fn level_of(&self, pv: &PureVal) -> Option<Level> {
        match pv {
            PureVal::Cell(pc) => Some(pc.dim().in_space),
            PureVal::App(ext_id, _) => {
                let idx = ext_id.0 as usize;
                self.env_items
                    .get(idx)?
                    .as_ref()
                    .and_then(|item| self.level_of_ty(&item.ty))
            }
            _ => match env::as_meta(pv)? {
                Meta::Ty(ty) => self.level_of_ty(ty),
                _ => None,
            },
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
        functor_map.insert(
            src_prim_id,
            FunctorEntry {
                param_ext_ids: vec![],
                cell: tgt_cell.clone(),
            },
        );

        // Process each mapping
        for mapping in mappings {
            // Check mapping params
            let mapping_span = self.program.val_span(mapping.applicand).clone();
            let param_ext_ids: Vec<ExtId> = mapping
                .params
                .iter()
                .map(|param| {
                    let ty = self.eval_ty(param.ty);
                    self.check_item(param.item, &ty, &mapping_span);
                    ExtId(param.item.0 as u64)
                })
                .collect();

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
            let expected_s = apply_functor(&app_cell.s(), &functor_map, &self.prim_item);
            let expected_t = apply_functor(&app_cell.t(), &functor_map, &self.prim_item);

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

            functor_map.insert(
                app_prim_id,
                FunctorEntry {
                    param_ext_ids,
                    cell: val_cell,
                },
            );
        }

        self.functor_maps.insert(def_id, functor_map);
    }

    // --- Path resolution ---

    fn resolve_ref(&mut self, target: Ref) -> Option<PureVal> {
        self.checked.get(&target).cloned()
    }

    fn param_ext_ids(&self, target: Ref) -> Vec<ExtId> {
        match target {
            Ref::Item(item_id) => self
                .program
                .item(item_id)
                .params
                .iter()
                .map(|p| ExtId(p.item.0 as u64))
                .collect(),
            Ref::Def(def_id) => self
                .program
                .def(def_id)
                .params
                .iter()
                .map(|p| ExtId(p.item.0 as u64))
                .collect(),
        }
    }

    // --- Item checking ---

    fn check_item(&mut self, item_id: ItemId, ty: &Ty, span: &TokenSpan) {
        if self.checked.contains_key(&Ref::Item(item_id)) {
            return;
        }
        let item = self.program.item(item_id);

        let (pv, prim_id) = match ty {
            Ty::Star => {
                let prim_id = self.fresh_prim_id();
                let prim = Prim::new(prim_id.0);
                (PureVal::Cell(PureCell::zero(prim)), Some(prim_id))
            }
            Ty::Arrow(level, _, src_val, tgt_val) => {
                let prim_id = self.fresh_prim_id();
                let prim = Prim::new(prim_id.0);
                match (Self::extract_cell(src_val), Self::extract_cell(tgt_val)) {
                    (Some(mut src), Some(mut tgt)) => {
                        let target_dim = *level - 1;
                        while src.dim().in_space < target_dim {
                            src = PureCell::id(src);
                        }
                        while tgt.dim().in_space < target_dim {
                            tgt = PureCell::id(tgt);
                        }
                        match PureCell::prim(prim, src, tgt) {
                            Ok(pc) => (PureVal::Cell(pc), Some(prim_id)),
                            Err(e) => {
                                self.error_at(span, format!("cell construction error: {}", e));
                                (Meta::Error.into(), Some(prim_id))
                            }
                        }
                    }
                    _ => {
                        self.error_at(span, "arrow source/target must be cell values");
                        (Meta::Error.into(), Some(prim_id))
                    }
                }
            }
            Ty::Meta => {
                let ty = match item.cname.as_str() {
                    "base::nat" => Some(Ty::Nat),
                    "base::rat" => Some(Ty::Rat),
                    "base::color" => Some(Ty::Color),
                    "base::decorator" => Some(Ty::Deco),
                    _ => None,
                };
                let pv = match ty {
                    Some(ty) => Meta::Ty(ty).into(),
                    None => {
                        self.error_at(span, format!("unknown meta type `{}`", item.cname));
                        Meta::Error.into()
                    }
                };
                (pv, None)
            }
            Ty::Nat | Ty::Rat | Ty::Color | Ty::Deco => {
                (PureVal::App(ExtId(item_id.0 as u64), vec![]), None)
            }
            Ty::Functor(_, _) => {
                unreachable!()
            }
            Ty::Hole => {
                // error has already been reported
                (Meta::Error.into(), None)
            }
        };

        self.checked.insert(Ref::Item(item_id), pv);
        if let Some(pid) = prim_id {
            self.prim_item.insert(pid, item_id);
        }

        let env_item = env::Item {
            cname: item.cname.clone(),
            lname: item.lname.clone(),
            kind: item.kind,
            ty: ty.clone(),
            prim_id,
            params: item.params.iter().map(|p| p.item).collect(),
        };
        self.env_items[item_id.0] = Some(env_item);
    }

    fn extract_cell(pv: &PureVal) -> Option<PureCell> {
        match pv {
            PureVal::Cell(pc) => Some(pc.clone()),
            _ => None,
        }
    }

    fn coerce_val_to_ty(&mut self, pv: PureVal, ty: &Ty, span: &TokenSpan) -> PureVal {
        match ty {
            Ty::Star => {
                if let PureVal::Cell(pc) = &pv {
                    if pc.dim().in_space != 0 {
                        self.error_at(
                            span,
                            format!("expected 0-cell, got {}-cell", pc.dim().in_space),
                        );
                    }
                } else if !matches!(env::as_meta(&pv), Some(Meta::Ty(Ty::Star))) {
                    self.error_at(span, "expected a 0-cell value");
                }
                pv
            }
            Ty::Arrow(level, _, src_val, tgt_val) => {
                if let PureVal::Cell(mut pc) = pv {
                    // Dim lift if needed
                    while pc.dim().in_space < *level {
                        pc = PureCell::id(pc);
                    }
                    if pc.dim().in_space != *level {
                        self.error_at(
                            span,
                            format!("expected {}-cell, got {}-cell", level, pc.dim().in_space),
                        );
                    } else {
                        // Check source/target compatibility
                        if let Some(src) = Self::extract_cell(src_val) {
                            let mut expected_src = src;
                            while expected_src.dim().in_space < *level - 1 {
                                expected_src = PureCell::id(expected_src);
                            }
                            if !pc.s().is_convertible(&expected_src) {
                                self.error_at(
                                    span,
                                    "value source does not match declared type source",
                                );
                            }
                        }
                        if let Some(tgt) = Self::extract_cell(tgt_val) {
                            let mut expected_tgt = tgt;
                            while expected_tgt.dim().in_space < *level - 1 {
                                expected_tgt = PureCell::id(expected_tgt);
                            }
                            if !pc.t().is_convertible(&expected_tgt) {
                                self.error_at(
                                    span,
                                    "value target does not match declared type target",
                                );
                            }
                        }
                    }
                    PureVal::Cell(pc)
                } else {
                    self.error_at(span, format!("expected a {}-cell value", level));
                    pv
                }
            }
            Ty::Meta => {
                if env::as_meta(&pv).is_none() {
                    self.error_at(span, "expected a meta value");
                }
                pv
            }
            Ty::Nat => {
                if !self.check_meta_val(&pv, Ty::Nat) {
                    self.error_at(span, "expected a nat value");
                }
                pv
            }
            Ty::Rat => {
                if !self.check_meta_val(&pv, Ty::Rat) && !self.check_meta_val(&pv, Ty::Nat) {
                    self.error_at(span, "expected a rat value");
                }
                pv
            }
            Ty::Color => {
                if !self.check_meta_val(&pv, Ty::Color) {
                    self.error_at(span, "expected a color value");
                }
                pv
            }
            Ty::Deco => {
                if !self.check_meta_val(&pv, Ty::Deco) {
                    self.error_at(span, "expected a decorator value");
                }
                pv
            }
            Ty::Functor(_, _) | Ty::Hole => pv,
        }
    }

    fn check_meta_val(&self, pv: &PureVal, expected: Ty) -> bool {
        self.infer_ty(pv) == expected
    }

    // --- Finalize ---

    fn into_result(self, root_lookup: HashMap<String, env::Module>) -> (env::Env, Vec<Error>) {
        let root = env::Module {
            this: None,
            lookup: root_lookup,
        };

        // Fill unchecked entries with defaults
        let items: Vec<env::Item> = self
            .env_items
            .into_iter()
            .enumerate()
            .map(|(i, o)| {
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
            })
            .collect();
        let defs: Vec<env::Def> = self
            .env_defs
            .into_iter()
            .enumerate()
            .map(|(i, o)| {
                o.unwrap_or_else(|| {
                    let def = self.program.def(DefId(i));
                    env::Def {
                        qname: def.qname.clone(),
                        lname: def.lname.clone(),
                        span: def.span.clone(),
                        item: None,
                        ty: Ty::Star,
                        params: vec![],
                        val: Meta::Error.into(),
                        decos: vec![],
                        origin: def.origin.clone(),
                        param_counts: def.param_counts.clone(),
                    }
                })
            })
            .collect();

        let env = env::Env {
            items,
            defs,
            prim_item: self.prim_item,
            root,
        };
        (env, self.errors)
    }
}

// --- Substitution with Meta/Ty awareness ---

fn subst_any_handler(
    pv: &PureVal,
    map: &HashMap<ExtId, PureVal>,
    prim_item: &HashMap<PrimId, ItemId>,
) -> PureVal {
    if let Some(meta) = env::as_meta(pv) {
        let new_meta = match meta {
            Meta::Ty(ty) => Meta::Ty(subst_ty(ty, map, prim_item)),
            _ => return pv.clone(),
        };
        new_meta.into()
    } else {
        pv.clone()
    }
}

fn subst_pv(
    pv: &PureVal,
    map: &HashMap<ExtId, PureVal>,
    prim_item: &HashMap<PrimId, ItemId>,
) -> PureVal {
    match pv {
        PureVal::Cell(pc) => PureVal::Cell(subst_cell(pc, map, prim_item)),
        _ => pv.subst_with(map, &|v, m| subst_any_handler(v, m, prim_item)),
    }
}

fn subst_cell(
    pc: &PureCell,
    map: &HashMap<ExtId, PureVal>,
    prim_item: &HashMap<PrimId, ItemId>,
) -> PureCell {
    let prim_handler = |prim_id: PrimId, mapping: &HashMap<ExtId, PureVal>| -> Option<PureCell> {
        let item_id = prim_item.get(&prim_id)?;
        let ext_id = ExtId(item_id.0 as u64);
        let replacement = mapping.get(&ext_id)?;
        match replacement {
            PureVal::Cell(cell) => Some(cell.clone()),
            _ => None,
        }
    };
    pc.subst_with_prim(
        map,
        &|v, m| subst_any_handler(v, m, prim_item),
        &prim_handler,
    )
}

fn subst_ty(ty: &Ty, map: &HashMap<ExtId, PureVal>, prim_item: &HashMap<PrimId, ItemId>) -> Ty {
    if map.is_empty() {
        return ty.clone();
    }
    match ty {
        Ty::Arrow(level, arrow_ty, src, tgt) => Ty::Arrow(
            *level,
            arrow_ty.clone(),
            subst_pv(src, map, prim_item),
            subst_pv(tgt, map, prim_item),
        ),
        Ty::Functor(src, tgt) => {
            Ty::Functor(subst_pv(src, map, prim_item), subst_pv(tgt, map, prim_item))
        }
        _ => ty.clone(),
    }
}

// --- Functor application ---

enum FunctorError {
    NoMapping(Prim),
    CompError(donut_core::common::Error),
}

fn apply_functor(
    cell: &PureCell,
    map: &HashMap<PrimId, FunctorEntry>,
    prim_item: &HashMap<PrimId, ItemId>,
) -> Result<PureCell, FunctorError> {
    match cell {
        PureCell::Prim(prim, _, dim) => {
            match map.get(&prim.id) {
                Some(entry) => {
                    let mut result = entry.cell.clone();
                    // Substitute params with actual args
                    if !entry.param_ext_ids.is_empty() && !prim.args.is_empty() {
                        let subst_map: HashMap<ExtId, PureVal> = entry
                            .param_ext_ids
                            .iter()
                            .zip(prim.args.iter())
                            .map(|(&eid, arg)| (eid, arg.clone()))
                            .collect();
                        result = subst_cell(&result, &subst_map, prim_item);
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
                .map(|c| apply_functor(c, map, prim_item))
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
