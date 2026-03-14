use std::collections::HashMap;

use crate::types::common::{SpanError, TokenSpan};
use crate::types::env::{self, ArrowTy, DisplayContext, Meta, Ty};
use crate::types::item::*;
use donut_core::cell::Diagram;
use donut_core::cell::Globular;
use donut_core::common::{
    AnyBox, Axis, BoundId, ExtType, Level, Prim, PrimId, PureVal, RefId, SubstMap,
};
use donut_core::pure_cell::{PureCell, Shape};

#[derive(Clone)]
struct FunctorEntry {
    param_bound_entries: Vec<(BoundId, ExtType)>,
    cell: PureCell,
}

struct FunctorMap {
    entries: HashMap<PrimId, FunctorEntry>,
    /// tgt_dim - src_dim (mapped cells shift dimension by this amount)
    dim_shift: i32,
}

type Error = SpanError;

struct Checker<'a> {
    program: &'a Program,

    // Output
    env_refs: Vec<Option<env::Item>>,
    env_bounds: Vec<Option<env::Item>>,
    env_defs: Vec<Option<env::Def>>,
    prim_owner: HashMap<PrimId, env::PrimOwner>,

    // State
    next_prim: u64,
    prefixes: Vec<String>,
    // Ref → PureVal for checked items/defs
    checked: HashMap<Entry, PureVal>,
    // DefId → (PrimId → FunctorEntry)
    functor_maps: HashMap<DefId, FunctorMap>,
    // DeclDef expansion: PrimId of `x` → PureCell of `y`
    def_subs: HashMap<PrimId, PureCell>,
    // Constraints accumulated during check_def
    current_reqs: Vec<env::FunctorReq>,
    // Cache: (functor DefId, arg PrimId) → generated fresh PureCell
    constraint_cache: HashMap<(DefId, PrimId), PureCell>,
    // PrimIds that may generate constraints (param prims + constraint-generated fresh prims)
    constrainable: HashSet<PrimId>,
    errors: Vec<Error>,
}

impl<'a> DisplayContext for Checker<'a> {
    fn ref_name(&self, ref_id: RefId) -> &str {
        if ref_id.0 < self.program.items.len() {
            &self.program.item(ref_id).cname
        } else if let Some(Some(env_item)) = self.env_refs.get(ref_id.0) {
            &env_item.cname
        } else {
            "?"
        }
    }

    fn bound_name(&self, bound_id: BoundId) -> &str {
        if bound_id.0 < self.program.bounds.len() {
            &self.program.bound(bound_id).lname
        } else if let Some(Some(env_item)) = self.env_bounds.get(bound_id.0) {
            &env_item.lname
        } else {
            "?"
        }
    }

    fn prim_name(&self, prim_id: PrimId) -> &str {
        if let Some(&owner) = self.prim_owner.get(&prim_id) {
            match owner {
                env::PrimOwner::Ref(ref_id) => {
                    if ref_id.0 < self.program.items.len() {
                        &self.program.item(ref_id).cname
                    } else if let Some(Some(env_item)) = self.env_refs.get(ref_id.0) {
                        &env_item.cname
                    } else {
                        "?"
                    }
                }
                env::PrimOwner::Bound(bound_id) => {
                    if bound_id.0 < self.program.bounds.len() {
                        &self.program.bound(bound_id).lname
                    } else if let Some(Some(env_item)) = self.env_bounds.get(bound_id.0) {
                        &env_item.lname
                    } else {
                        "?"
                    }
                }
            }
        } else {
            "?"
        }
    }
}

impl<'a> Checker<'a> {
    fn new(program: &'a Program) -> Self {
        let n_items = program.items.len();
        let n_bounds = program.bounds.len();
        let n_defs = program.defs.len();
        let mut checker = Checker {
            program,
            env_refs: (0..n_items).map(|_| None).collect(),
            env_bounds: (0..n_bounds).map(|_| None).collect(),
            env_defs: (0..n_defs).map(|_| None).collect(),
            prim_owner: HashMap::new(),
            next_prim: 1, // 0 reserved for meta
            prefixes: Vec::new(),
            checked: HashMap::new(),
            functor_maps: HashMap::new(),
            def_subs: HashMap::new(),
            current_reqs: Vec::new(),
            constraint_cache: HashMap::new(),
            constrainable: HashSet::new(),
            errors: Vec::new(),
        };
        checker.register_builtins();
        checker
    }

    fn register_builtins(&mut self) {
        let builtins: &[(&str, Ty)] = &[("meta", Ty::Meta), ("*", Ty::Star)];
        for (i, item) in self.program.items.iter().enumerate() {
            if let Some((_, ty)) = builtins.iter().find(|(name, _)| *name == item.lname) {
                let ref_id = RefId(i);
                let pv: PureVal = Meta::Ty(ty.clone()).into();
                self.checked.insert(Entry::Ref(ref_id), pv);
                self.env_refs[i] = Some(env::Item {
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
        self.errors.push((span.clone(), msg.into()));
    }

    fn fresh_prim_id(&mut self) -> PrimId {
        let id = PrimId(self.next_prim);
        self.next_prim += 1;
        id
    }

    fn display_pure_val(&self, pv: &PureVal) -> String {
        env::display_pure_val(self, pv)
    }

    fn display_cell(&self, pc: &PureCell) -> String {
        env::display_cell(self, pc)
    }

    fn display_ty(&self, ty: &Ty) -> String {
        env::display_ty(self, ty)
    }

    fn format_comp_error(&self, e: &donut_core::common::Error) -> String {
        match e {
            donut_core::common::Error::NotConvertible(a, b) => {
                format!(
                    "`{}` is not convertible to `{}`",
                    self.display_cell(a),
                    self.display_cell(b)
                )
            }
            donut_core::common::Error::IncompatibleDimension {
                expected,
                got_dim,
                got,
            } => {
                format!(
                    "expected {}-cell, got {}-cell `{}`",
                    expected,
                    got_dim,
                    self.display_cell(got)
                )
            }
        }
    }

    fn format_functor_error(&self, def_id: DefId, e: &FunctorError) -> String {
        let name = &self.program.def(def_id).lname;
        match e {
            FunctorError::NoMapping(prim) => format!(
                "functor {}: no mapping for {}",
                name,
                self.prim_name(prim.id)
            ),
            FunctorError::CompError(e) => format!(
                "functor {} composition error: {}",
                name,
                self.format_comp_error(e)
            ),
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

        // Check param Items (bound variables)
        let def_span = self.program.def(def_id).span.clone();
        for param in &params {
            let ty = self.eval_ty(param.ty);
            self.check_bound(param.bound, &ty, &def_span);
        }

        // Extend constrainable with this def's param prims (save for restore)
        let saved_constrainable = self.constrainable.clone();
        self.constrainable.extend(self.current_param_prims(def_id));

        // Process `before` trees (where clauses) — must be checked before this def
        let mut lookup = self.process_trees(&tree.before);

        // Process this def
        self.check_def(def_id);

        // Process `after` trees (module members, with clauses)
        lookup.extend(self.process_trees(&tree.after));

        // Restore constrainable
        self.constrainable = saved_constrainable;
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

        // Save and restore constraint collection state for this def
        let saved_reqs = std::mem::take(&mut self.current_reqs);
        let saved_cache = std::mem::take(&mut self.constraint_cache);
        let err_count_at_start = self.errors.len();

        // Ensure params are checked
        let params = self.program.def(def_id).params.clone();
        let def_span = self.program.def(def_id).span.clone();
        for param in &params {
            let ty = self.eval_ty(param.ty);
            self.check_bound(param.bound, &ty, &def_span);
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
            DefBody::Decl {
                item: ref_id,
                def_val,
            } => {
                let ref_id = *ref_id;
                let def_val = *def_val;
                let def_pv = def_val.map(|id| self.eval_val(id));
                let ty = if let Some(ref pv) = def_pv {
                    // DeclDef: infer type from body if not explicitly declared
                    match declared_ty {
                        Some(Ty::Hole) | None => self.infer_ty(pv),
                        Some(ty) => ty,
                    }
                } else {
                    declared_ty.unwrap_or(Ty::Star)
                };
                self.check_ref_item(ref_id, &ty, &span);
                // Record DeclDef substitution: x's PrimId → y's cell
                if let Some(PureVal::Cell(target_cell)) = &def_pv {
                    if let Some(Some(env_item)) = self.env_refs.get(ref_id.0) {
                        if let Some(prim_id) = env_item.prim_id {
                            // Transitively resolve through existing subs
                            let resolved = env::expand_defs(target_cell, &self.def_subs);
                            self.def_subs.insert(prim_id, resolved);
                        }
                    }
                }
                let pv = self.checked[&Entry::Ref(ref_id)].clone();
                (Some(ref_id), pv, ty)
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
                    self.error_at(
                        deco_span,
                        format!("expected decorator, got `{}`", self.display_pure_val(&pv),),
                    );
                }
                pv
            })
            .collect();

        // Params → BoundIds (already checked in process_tree)
        let params: Vec<BoundId> = self
            .program
            .def(def_id)
            .params
            .iter()
            .map(|p| p.bound)
            .collect();

        // Validate values before storing (only if no errors in this def)
        // TODO: re-enable after fixing pre-existing validation issues
        // if self.errors.len() == err_count_at_start {
        //     val.validate(&validate_meta);
        // }

        let env_def = env::Def {
            qname,
            lname,
            span,
            item,
            ty: ty.clone(),
            params,
            val,
            decos,
            reqs: std::mem::take(&mut self.current_reqs),
            style: env::DefStyle {
                color: env::Color::gray(),
            },
            origin,
            param_counts,
        };
        self.checked.insert(Entry::Def(def_id), env_def.val.clone());
        self.env_defs[def_id.0] = Some(env_def);

        // Restore constraint collection state
        self.current_reqs = saved_reqs;
        self.constraint_cache = saved_cache;
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
            PureVal::Ref(ref_id, args) => {
                let idx = ref_id.0;
                if let Some(Some(item)) = self.env_refs.get(idx) {
                    if args.is_empty() {
                        item.ty.clone()
                    } else {
                        // Substitute bound vars with actual args
                        let subst_map: SubstMap = item
                            .params
                            .iter()
                            .zip(args.iter())
                            .map(|(&bound_id, arg)| {
                                let ext_type = self.ext_type_for_bound(bound_id);
                                (bound_id, (arg.clone(), ext_type))
                            })
                            .collect();
                        subst_ty(&item.ty, &subst_map, &self.prim_owner)
                    }
                } else {
                    Ty::Hole
                }
            }
            PureVal::Bound(bound_id) => {
                let idx = bound_id.0;
                if let Some(Some(item)) = self.env_bounds.get(idx) {
                    item.ty.clone()
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
        match env::as_meta(pv) {
            Some(Meta::Ty(ty)) => ty.clone(),
            Some(Meta::Error) => Ty::Hole,
            _ => {
                let span = self.program.val_span(val_id);
                self.error_at(
                    span,
                    format!(
                        "expected type expression, got `{}`",
                        self.display_pure_val(pv)
                    ),
                );
                Ty::Hole
            }
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
                let mapping: HashMap<BoundId, ValId> = mapping.clone();
                let inner_pv = self.eval_val(inner_id);
                // Convert HashMap<BoundId, ValId> to SubstMap
                let subst_map: SubstMap = mapping
                    .iter()
                    .map(|(&bound_id, &val_id)| {
                        let pv = self.eval_val(val_id);
                        let ext_type = self.ext_type_for_bound(bound_id);
                        (bound_id, (pv, ext_type))
                    })
                    .collect();
                subst_pv(&inner_pv, &subst_map, &self.prim_owner)
            }
        }
    }

    fn eval_val_path(&mut self, path: &Path, val_id: ValId) -> PureVal {
        let base = if let Some(pv) = self.resolve_ref(path.target) {
            pv
        } else {
            let span = self.program.val_span(val_id);
            match path.target {
                Entry::Def(def_id) => {
                    let qname = &self.program.def(def_id).qname;
                    self.error_at(span, format!("self-referencing definition: `{}`", qname));
                }
                Entry::Ref(ref_id) => {
                    let cname = &self.program.item(ref_id).cname;
                    self.error_at(span, format!("unresolved item: `{}`", cname));
                }
                Entry::Bound(bound_id) => {
                    let lname = &self.program.bound(bound_id).lname;
                    self.error_at(span, format!("unresolved bound variable: `{}`", lname));
                }
            };
            return Meta::Error.into();
        };

        // Check arg count (only for Def targets; Item refs may appear
        // internally via Subst without args)
        if let Entry::Def(def_id) = path.target {
            let expected_params = self.program.def(def_id).params.len();
            if path.args.len() != expected_params {
                let name = &self.program.def(def_id).qname;
                let span = self.program.val_span(val_id);
                self.error_at(
                    span,
                    format!(
                        "`{}` expects {} parameters, but {} were given",
                        name,
                        expected_params,
                        path.args.len()
                    ),
                );
            }
        }

        // Apply args
        let mut result = if path.args.is_empty() {
            base
        } else {
            let mut args: Vec<PureVal> = path.args.iter().map(|&a| self.eval_val(a)).collect();

            // Type-check args against parameter types (and lift dimensions)
            if let Entry::Def(def_id) = path.target {
                let params: Vec<_> = self
                    .program
                    .def(def_id)
                    .params
                    .iter()
                    .map(|p| (p.bound, p.ty))
                    .collect();
                let span = self.program.val_span(val_id);
                let err_count = self.errors.len();
                let mut param_subst: SubstMap = HashMap::new();
                for (i, arg) in args.iter_mut().enumerate() {
                    if let Some(&(bound_id, ty_val_id)) = params.get(i) {
                        let mut param_ty = self.eval_ty(ty_val_id);
                        if !param_subst.is_empty() {
                            param_ty = subst_ty(&param_ty, &param_subst, &self.prim_owner);
                        }
                        *arg = self.coerce_val_to_ty(arg.clone(), &param_ty, &span);
                        let ext_type = self.ext_type_for_bound(bound_id);
                        param_subst.insert(bound_id, (arg.clone(), ext_type));
                    }
                }
                if self.errors.len() > err_count {
                    return Meta::Error.into();
                }
            }
            // Decl defs (with item) and bare Items accumulate args into App;
            // Alias defs substitute param placeholders with actual args.
            let is_alias = match path.target {
                Entry::Ref(_) | Entry::Bound(_) => false,
                Entry::Def(def_id) => {
                    matches!(self.program.def(def_id).body, DefBody::Alias { .. })
                }
            };
            if is_alias {
                let param_entries = self.param_bound_entries(path.target);
                let subst_map: SubstMap = param_entries
                    .into_iter()
                    .zip(args.into_iter())
                    .map(|((ext_id, ext_type), arg)| (ext_id, (arg, ext_type)))
                    .collect();
                subst_pv(&base, &subst_map, &self.prim_owner)
            } else {
                match base {
                    PureVal::Ref(id, mut existing) => {
                        existing.extend(args);
                        PureVal::Ref(id, existing)
                    }
                    _ => {
                        let param_entries = self.param_bound_entries(path.target);
                        let subst_map: SubstMap = param_entries
                            .into_iter()
                            .zip(args.into_iter())
                            .map(|((ext_id, ext_type), arg)| (ext_id, (arg, ext_type)))
                            .collect();
                        subst_pv(&base, &subst_map, &self.prim_owner)
                    }
                }
            }
        };

        // Verify/propagate functor constraints from target def
        if let Entry::Def(def_id) = path.target {
            if !path.args.is_empty() {
                if let Some(Some(env_def)) = self.env_defs.get(def_id.0) {
                    let reqs = env_def.reqs.clone();
                    if !reqs.is_empty() {
                        // Build substitution map: bound vars → actual arg values
                        let param_entries = self.param_bound_entries(path.target);
                        let args: Vec<PureVal> =
                            path.args.iter().map(|&a| self.eval_val(a)).collect();
                        let mut subst_map: SubstMap = param_entries
                            .into_iter()
                            .zip(args.into_iter())
                            .map(|((ext_id, ext_type), arg)| (ext_id, (arg, ext_type)))
                            .collect();

                        // Process reqs in order, growing subst_map with results
                        for req in &reqs {
                            let sub_arg = match subst_cell(&req.arg, &subst_map, &self.prim_owner) {
                                Ok(cell) => cell,
                                Err(e) => {
                                    let span = self.program.val_span(val_id);
                                    self.error_at(
                                        span,
                                        format!(
                                            "functor constraint substitution failed: {}",
                                            self.format_comp_error(&e)
                                        ),
                                    );
                                    continue;
                                }
                            };
                            if let Some(fmap) = self.functor_maps.get(&req.functor) {
                                let entries = fmap.entries.clone();
                                let dim_shift = fmap.dim_shift;
                                match self.apply_functor_constrained(
                                    &sub_arg,
                                    req.functor,
                                    &entries,
                                    dim_shift,
                                ) {
                                    Ok(result_cell) => {
                                        let bound_id = BoundId(req.result.0);
                                        let result_dim = result_cell.dim().in_space;
                                        subst_map.insert(
                                            bound_id,
                                            (PureVal::Cell(result_cell), ExtType::Cell(result_dim)),
                                        );
                                    }
                                    Err(e) => {
                                        let span = self.program.val_span(val_id);
                                        self.error_at(
                                            span,
                                            self.format_functor_error(req.functor, &e),
                                        );
                                    }
                                }
                            }
                        }

                        // Re-substitute result with the full map (including constraint results)
                        result = subst_pv(&result, &subst_map, &self.prim_owner);
                    }
                }
            }
        }

        // Functor application: f(x)
        if let Some(app_val_id) = path.applicand {
            let applicand = self.eval_val(app_val_id);
            let app_cell = match &applicand {
                PureVal::Cell(pc) => pc,
                _ => {
                    let span = self.program.val_span(val_id);
                    self.error_at(
                        span,
                        format!(
                            "functor applicand must be a cell, got `{}`",
                            self.display_pure_val(&applicand)
                        ),
                    );
                    return result;
                }
            };
            // Find functor map for this def
            let functor_def_id = match path.target {
                Entry::Def(did) => did,
                Entry::Ref(_) | Entry::Bound(_) => {
                    let span = self.program.val_span(val_id);
                    self.error_at(span, "functor application target must be a definition");
                    return result;
                }
            };
            if let Some(fmap) = self.functor_maps.get(&functor_def_id) {
                let entries = fmap.entries.clone();
                let dim_shift = fmap.dim_shift;
                match self.apply_functor_constrained(app_cell, functor_def_id, &entries, dim_shift)
                {
                    Ok(pc) => PureVal::Cell(pc),
                    Err(e) => {
                        let span = self.program.val_span(val_id);
                        self.error_at(span, self.format_functor_error(functor_def_id, &e));
                        result
                    }
                }
            } else {
                let span = self.program.val_span(val_id);
                let name = &self.program.def(functor_def_id).lname;
                self.error_at(span, format!("`{}` is not a functor", name));
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
        let mut non_cell = None;
        let cells: Vec<PureCell> = child_vals
            .into_iter()
            .filter_map(|pv| match pv {
                PureVal::Cell(pc) => Some(pc),
                _ => {
                    non_cell = Some(pv);
                    None
                }
            })
            .collect();
        if cells.len() != children.len() {
            let span = self.program.val_span(val_id);
            if let Some(bad) = non_cell {
                self.error_at(
                    span,
                    format!(
                        "composition requires cell values, got `{}`",
                        self.display_pure_val(&bad)
                    ),
                );
            }
            return Meta::Error.into();
        }
        match PureCell::comp(axis, cells) {
            Ok(pc) => PureVal::Cell(pc),
            Err(e) => {
                let span = self.program.val_span(val_id);
                self.error_at(span, self.format_comp_error(&e));
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
            PureVal::Ref(RefId(idx), _) => {
                let idx = *idx;
                self.env_refs
                    .get(idx)?
                    .as_ref()
                    .and_then(|item| self.level_of_ty(&item.ty))
            }
            PureVal::Bound(BoundId(idx)) => {
                let idx = *idx;
                self.env_bounds
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
                self.error_at(
                    &span,
                    format!(
                        "functor must have type `A ~> B`, got `{}`",
                        self.display_ty(ty)
                    ),
                );
                return;
            }
        };

        // Extract PureCells from src/tgt
        let (src_cell, tgt_cell) = match (&src_val, &tgt_val) {
            (PureVal::Cell(s), PureVal::Cell(t)) => (s.clone(), t.clone()),
            _ => {
                let span = self.program.def(def_id).span.clone();
                self.error_at(
                    &span,
                    format!(
                        "functor source and target must be cell values, got `{}` ~> `{}`",
                        self.display_pure_val(&src_val),
                        self.display_pure_val(&tgt_val),
                    ),
                );
                return;
            }
        };

        let dim_shift = tgt_cell.dim().in_space as i32 - src_cell.dim().in_space as i32;

        // Extract source PrimId for base case
        let src_prim_id = match src_cell.extract_prim_id() {
            Some(id) => id,
            None => {
                let span = self.program.def(def_id).span.clone();
                self.error_at(
                    &span,
                    format!(
                        "functor source must be a primitive cell, got `{}`",
                        self.display_cell(&src_cell),
                    ),
                );
                return;
            }
        };

        // Base case: source → target
        let mut functor_map: HashMap<PrimId, FunctorEntry> = HashMap::new();
        functor_map.insert(
            src_prim_id,
            FunctorEntry {
                param_bound_entries: vec![],
                cell: tgt_cell.clone(),
            },
        );

        // Process each mapping
        for mapping in mappings {
            // Check mapping params
            let mapping_span = self.program.val_span(mapping.applicand).clone();
            let param_bound_entries: Vec<(BoundId, ExtType)> = mapping
                .params
                .iter()
                .map(|param| {
                    let ty = self.eval_ty(param.ty);
                    self.check_bound(param.bound, &ty, &mapping_span);
                    (param.bound, self.ext_type_for_bound(param.bound))
                })
                .collect();

            // Eval applicand
            let app_val = self.eval_val(mapping.applicand);
            let app_cell = match &app_val {
                PureVal::Cell(pc) => pc.clone(),
                _ => {
                    let span = self.program.val_span(mapping.applicand);
                    self.error_at(
                        span,
                        format!(
                            "functor mapping applicand must be a cell, got `{}`",
                            self.display_pure_val(&app_val),
                        ),
                    );
                    continue;
                }
            };

            let app_prim_id = match app_cell.extract_prim_id() {
                Some(id) => id,
                None => {
                    let span = self.program.val_span(mapping.applicand);
                    self.error_at(
                        span,
                        format!(
                            "functor mapping applicand must be a primitive cell, got `{}`",
                            self.display_cell(&app_cell),
                        ),
                    );
                    continue;
                }
            };

            // Eval val (right-hand side)
            let val_val = self.eval_val(mapping.val);
            let mut val_cell = match &val_val {
                PureVal::Cell(pc) => pc.clone(),
                _ => {
                    let span = self.program.val_span(mapping.val);
                    self.error_at(
                        span,
                        format!(
                            "functor mapping value must be a cell, got `{}`",
                            self.display_pure_val(&val_val),
                        ),
                    );
                    continue;
                }
            };

            let app_dim = app_cell.dim().in_space;

            // Base cell: check consistency with base case
            if app_prim_id == src_prim_id {
                if !val_cell.is_convertible(&tgt_cell) {
                    let span = self.program.val_span(mapping.val);
                    self.error_at(span, format!(
                        "functor base mapping contradicts base case: `{}` is not convertible to `{}`",
                        self.display_cell(&val_cell), self.display_cell(&tgt_cell),
                    ));
                }
                continue;
            }

            // Expected dimension of mapped result
            let expected_dim = app_dim as i32 + dim_shift;
            if expected_dim < 0 {
                let span = self.program.val_span(mapping.applicand);
                self.error_at(
                    span,
                    format!(
                        "functor mapping `{}` ({}-cell) is below functor source dimension",
                        self.display_cell(&app_cell),
                        app_dim,
                    ),
                );
                continue;
            }

            // Lift val to expected dimension if needed
            while (val_cell.dim().in_space as i32) < expected_dim {
                val_cell = PureCell::id(val_cell);
            }

            // Functoriality check: apply_functor(app.s) == val.s, apply_functor(app.t) == val.t
            let expected_s =
                apply_functor(&app_cell.s(), &functor_map, &self.prim_owner, dim_shift);
            let expected_t =
                apply_functor(&app_cell.t(), &functor_map, &self.prim_owner, dim_shift);

            match (expected_s, expected_t) {
                (Ok(es), Ok(et)) => {
                    if !es.is_convertible(&val_cell.s()) {
                        let span = self.program.val_span(mapping.val);
                        self.error_at(
                            span,
                            format!(
                                "functor mapping source mismatch: expected `{}`, got `{}`",
                                self.display_cell(&es),
                                self.display_cell(&val_cell.s()),
                            ),
                        );
                        continue;
                    }
                    if !et.is_convertible(&val_cell.t()) {
                        let span = self.program.val_span(mapping.val);
                        self.error_at(
                            span,
                            format!(
                                "functor mapping target mismatch: expected `{}`, got `{}`",
                                self.display_cell(&et),
                                self.display_cell(&val_cell.t()),
                            ),
                        );
                        continue;
                    }
                }
                (Err(e), _) | (_, Err(e)) => {
                    let span = self.program.val_span(mapping.applicand);
                    self.error_at(span, self.format_functor_error(def_id, &e));
                    continue;
                }
            }

            functor_map.insert(
                app_prim_id,
                FunctorEntry {
                    param_bound_entries,
                    cell: val_cell,
                },
            );
        }

        self.functor_maps.insert(
            def_id,
            FunctorMap {
                entries: functor_map,
                dim_shift,
            },
        );
    }

    // --- Path resolution ---

    fn resolve_ref(&mut self, target: Entry) -> Option<PureVal> {
        self.checked.get(&target).cloned()
    }

    fn ext_type_for_ref(&self, ref_id: RefId) -> ExtType {
        if let Some(Some(item)) = self.env_refs.get(ref_id.0) {
            Self::ext_type_from_ty(&item.ty)
        } else {
            ExtType::NonCell
        }
    }

    fn ext_type_for_bound(&self, bound_id: BoundId) -> ExtType {
        if let Some(Some(item)) = self.env_bounds.get(bound_id.0) {
            Self::ext_type_from_ty(&item.ty)
        } else {
            ExtType::NonCell
        }
    }

    fn ext_type_from_ty(ty: &Ty) -> ExtType {
        match ty {
            Ty::Star => ExtType::Cell(0),
            Ty::Arrow(level, _, _, _) => ExtType::Cell(*level),
            _ => ExtType::NonCell,
        }
    }

    fn param_bound_entries(&self, target: Entry) -> Vec<(BoundId, ExtType)> {
        let params: &[_] = match target {
            Entry::Ref(ref_id) => &self.program.item(ref_id).params,
            Entry::Bound(_) => return vec![],
            Entry::Def(def_id) => &self.program.def(def_id).params,
        };
        params
            .iter()
            .map(|p| (p.bound, self.ext_type_for_bound(p.bound)))
            .collect()
    }

    // --- Item checking ---

    /// Build (PureVal, Option<PrimId>) for an item given its type and param_args.
    fn build_item_val(
        &mut self,
        item: &Item,
        ty: &Ty,
        span: &TokenSpan,
        param_args: Vec<PureVal>,
        is_param: bool,
        raw_id: usize,
    ) -> (PureVal, Option<PrimId>) {
        match ty {
            Ty::Star => {
                let prim_id = self.fresh_prim_id();
                let prim = Prim::with_id_args(PrimId(prim_id.0), param_args);
                (PureVal::Cell(PureCell::zero(prim)), Some(prim_id))
            }
            Ty::Arrow(level, _, src_val, tgt_val) => {
                let prim_id = self.fresh_prim_id();
                let prim = Prim::with_id_args(PrimId(prim_id.0), param_args);
                match (Self::extract_cell(src_val), Self::extract_cell(tgt_val)) {
                    (Some(src), Some(tgt)) => match PureCell::prim(prim, src, tgt) {
                        Ok(pc) => (PureVal::Cell(pc), Some(prim_id)),
                        Err(e) => {
                            self.error_at(
                                span,
                                format!("cell construction error: {}", self.format_comp_error(&e)),
                            );
                            (Meta::Error.into(), Some(prim_id))
                        }
                    },
                    _ => {
                        self.error_at(
                            span,
                            format!(
                                "arrow source/target must be cell values in type `{}`",
                                self.display_ty(ty),
                            ),
                        );
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
                let pv = if is_param {
                    PureVal::Bound(BoundId(raw_id))
                } else {
                    PureVal::Ref(RefId(raw_id), vec![])
                };
                (pv, None)
            }
            Ty::Functor(_, _) => {
                unreachable!()
            }
            Ty::Hole => {
                // error has already been reported
                (Meta::Error.into(), None)
            }
        }
    }

    fn check_ref_item(&mut self, ref_id: RefId, ty: &Ty, span: &TokenSpan) {
        if self.checked.contains_key(&Entry::Ref(ref_id)) {
            return;
        }
        let item = self.program.item(ref_id);
        let param_args: Vec<PureVal> = item
            .params
            .iter()
            .map(|p| PureVal::Bound(p.bound))
            .collect();

        let (pv, prim_id) = self.build_item_val(item, ty, span, param_args, false, ref_id.0);

        self.checked.insert(Entry::Ref(ref_id), pv);
        if let Some(pid) = prim_id {
            self.prim_owner.insert(pid, env::PrimOwner::Ref(ref_id));
        }

        let env_item = env::Item {
            cname: item.cname.clone(),
            lname: item.lname.clone(),
            kind: item.kind,
            ty: ty.clone(),
            prim_id,
            params: item.params.iter().map(|p| p.bound).collect(),
        };
        self.env_refs[ref_id.0] = Some(env_item);
    }

    fn check_bound(&mut self, bound_id: BoundId, ty: &Ty, span: &TokenSpan) {
        if self.checked.contains_key(&Entry::Bound(bound_id)) {
            return;
        }
        let item = self.program.bound(bound_id);
        let param_args: Vec<PureVal> = item
            .params
            .iter()
            .map(|p| PureVal::Bound(p.bound))
            .collect();

        let (pv, prim_id) = self.build_item_val(item, ty, span, param_args, true, bound_id.0);

        self.checked.insert(Entry::Bound(bound_id), pv);
        if let Some(pid) = prim_id {
            self.prim_owner.insert(pid, env::PrimOwner::Bound(bound_id));
        }

        let env_item = env::Item {
            cname: item.cname.clone(),
            lname: item.lname.clone(),
            kind: item.kind,
            ty: ty.clone(),
            prim_id,
            params: item.params.iter().map(|p| p.bound).collect(),
        };
        self.env_bounds[bound_id.0] = Some(env_item);
    }

    fn extract_cell(pv: &PureVal) -> Option<PureCell> {
        match pv {
            PureVal::Cell(pc) => Some(pc.clone()),
            _ => None,
        }
    }

    fn coerce_val_to_ty(&mut self, pv: PureVal, ty: &Ty, span: &TokenSpan) -> PureVal {
        let error_val = || -> PureVal { Meta::Error.into() };
        match ty {
            Ty::Star => {
                if let PureVal::Cell(pc) = &pv {
                    if pc.dim().in_space != 0 {
                        self.error_at(
                            span,
                            format!(
                                "expected `*` (0-cell), got {}-cell `{}`",
                                pc.dim().in_space,
                                self.display_cell(pc),
                            ),
                        );
                        return error_val();
                    }
                } else if !matches!(env::as_meta(&pv), Some(Meta::Ty(Ty::Star))) {
                    self.error_at(
                        span,
                        format!(
                            "expected `*` (0-cell), got `{}`",
                            self.display_pure_val(&pv),
                        ),
                    );
                    return error_val();
                }
                pv
            }
            Ty::Arrow(level, _, src_val, tgt_val) => {
                if let PureVal::Cell(pc) = pv {
                    let pc = pc.lift_to(*level);
                    if pc.dim().in_space != *level {
                        self.error_at(
                            span,
                            format!(
                                "expected `{}` ({}-cell), got {}-cell `{}`",
                                self.display_ty(ty),
                                level,
                                pc.dim().in_space,
                                self.display_cell(&pc),
                            ),
                        );
                        return error_val();
                    }
                    let mut ok = true;
                    if let Some(src) = Self::extract_cell(src_val) {
                        if !pc.s().is_convertible(&src) {
                            self.error_at(
                                span,
                                format!(
                                    "source mismatch: `{}` has source `{}`, expected `{}`",
                                    self.display_cell(&pc),
                                    self.display_cell(&pc.s()),
                                    self.display_cell(&src),
                                ),
                            );
                            ok = false;
                        }
                    }
                    if let Some(tgt) = Self::extract_cell(tgt_val) {
                        if !pc.t().is_convertible(&tgt) {
                            self.error_at(
                                span,
                                format!(
                                    "target mismatch: `{}` has target `{}`, expected `{}`",
                                    self.display_cell(&pc),
                                    self.display_cell(&pc.t()),
                                    self.display_cell(&tgt),
                                ),
                            );
                            ok = false;
                        }
                    }
                    if ok { PureVal::Cell(pc) } else { error_val() }
                } else {
                    self.error_at(
                        span,
                        format!(
                            "expected `{}` ({}-cell), got `{}`",
                            self.display_ty(ty),
                            level,
                            self.display_pure_val(&pv),
                        ),
                    );
                    error_val()
                }
            }
            Ty::Meta => {
                if env::as_meta(&pv).is_none() {
                    self.error_at(
                        span,
                        format!("expected meta value, got `{}`", self.display_pure_val(&pv)),
                    );
                    return error_val();
                }
                pv
            }
            Ty::Nat => {
                if !self.check_meta_val(&pv, Ty::Nat) {
                    self.error_at(
                        span,
                        format!("expected nat, got `{}`", self.display_pure_val(&pv)),
                    );
                    return error_val();
                }
                pv
            }
            Ty::Rat => {
                if !self.check_meta_val(&pv, Ty::Rat) && !self.check_meta_val(&pv, Ty::Nat) {
                    self.error_at(
                        span,
                        format!("expected rat, got `{}`", self.display_pure_val(&pv)),
                    );
                    return error_val();
                }
                pv
            }
            Ty::Color => {
                if !self.check_meta_val(&pv, Ty::Color) {
                    self.error_at(
                        span,
                        format!("expected color, got `{}`", self.display_pure_val(&pv)),
                    );
                    return error_val();
                }
                pv
            }
            Ty::Deco => {
                if !self.check_meta_val(&pv, Ty::Deco) {
                    self.error_at(
                        span,
                        format!("expected decorator, got `{}`", self.display_pure_val(&pv)),
                    );
                    return error_val();
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
        let n_program_items = self.program.items.len();
        let items: Vec<env::Item> = self
            .env_refs
            .into_iter()
            .enumerate()
            .map(|(i, o)| {
                o.unwrap_or_else(|| {
                    if i < n_program_items {
                        let item = self.program.item(RefId(i));
                        env::Item {
                            cname: item.cname.clone(),
                            lname: item.lname.clone(),
                            kind: item.kind,
                            ty: Ty::Star,
                            prim_id: None,
                            params: vec![],
                        }
                    } else {
                        // Constraint-generated item (should already be Some)
                        env::Item {
                            cname: "?".to_string(),
                            lname: "?".to_string(),
                            kind: ItemKind::Decl,
                            ty: Ty::Hole,
                            prim_id: None,
                            params: vec![],
                        }
                    }
                })
            })
            .collect();
        let n_program_bounds = self.program.bounds.len();
        let bounds: Vec<env::Item> = self
            .env_bounds
            .into_iter()
            .enumerate()
            .map(|(i, o)| {
                o.unwrap_or_else(|| {
                    if i < n_program_bounds {
                        let item = self.program.bound(BoundId(i));
                        env::Item {
                            cname: item.cname.clone(),
                            lname: item.lname.clone(),
                            kind: item.kind,
                            ty: Ty::Star,
                            prim_id: None,
                            params: vec![],
                        }
                    } else {
                        // Constraint-generated bound (should already be Some)
                        env::Item {
                            cname: "?".to_string(),
                            lname: "?".to_string(),
                            kind: ItemKind::Param,
                            ty: Ty::Hole,
                            prim_id: None,
                            params: vec![],
                        }
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
                        reqs: vec![],
                        style: env::DefStyle {
                            color: env::Color::gray(),
                        },
                        origin: def.origin.clone(),
                        param_counts: def.param_counts.clone(),
                    }
                })
            })
            .collect();

        let env = env::Env {
            refs: items,
            bounds,
            defs,
            prim_owner: self.prim_owner,
            root,
            def_subs: self.def_subs,
        };
        (env, self.errors)
    }
}

// --- Substitution with Meta/Ty awareness ---

fn subst_any_handler(
    pv: &PureVal,
    map: &SubstMap,
    prim_owner: &HashMap<PrimId, env::PrimOwner>,
) -> PureVal {
    if let Some(meta) = env::as_meta(pv) {
        let new_meta = match meta {
            Meta::Ty(ty) => Meta::Ty(subst_ty(ty, map, prim_owner)),
            _ => return pv.clone(),
        };
        new_meta.into()
    } else {
        pv.clone()
    }
}

fn make_prim_handler(
    prim_owner: &HashMap<PrimId, env::PrimOwner>,
) -> impl Fn(PrimId, &SubstMap) -> Option<PureCell> + '_ {
    move |prim_id: PrimId, mapping: &SubstMap| -> Option<PureCell> {
        let owner = prim_owner.get(&prim_id)?;
        let bound_id = match owner {
            env::PrimOwner::Bound(bid) => *bid,
            env::PrimOwner::Ref(_) => return None,
        };
        let (replacement, _) = mapping.get(&bound_id)?;
        match replacement {
            PureVal::Cell(cell) => Some(cell.clone()),
            _ => None,
        }
    }
}

fn subst_pv(pv: &PureVal, map: &SubstMap, prim_owner: &HashMap<PrimId, env::PrimOwner>) -> PureVal {
    pv.subst(
        map,
        &|v, m| subst_any_handler(v, m, prim_owner),
        &make_prim_handler(prim_owner),
    )
    .unwrap_or_else(|_| Meta::Error.into())
}

fn subst_cell(
    pc: &PureCell,
    map: &SubstMap,
    prim_owner: &HashMap<PrimId, env::PrimOwner>,
) -> Result<PureCell, donut_core::common::Error> {
    pc.subst(
        map,
        &|v, m| subst_any_handler(v, m, prim_owner),
        &make_prim_handler(prim_owner),
    )
}

fn subst_ty(ty: &Ty, map: &SubstMap, prim_owner: &HashMap<PrimId, env::PrimOwner>) -> Ty {
    if map.is_empty() {
        return ty.clone();
    }
    match ty {
        Ty::Arrow(level, arrow_ty, src, tgt) => Ty::Arrow(
            *level,
            arrow_ty.clone(),
            subst_pv(src, map, prim_owner),
            subst_pv(tgt, map, prim_owner),
        ),
        Ty::Functor(src, tgt) => Ty::Functor(
            subst_pv(src, map, prim_owner),
            subst_pv(tgt, map, prim_owner),
        ),
        _ => ty.clone(),
    }
}

fn validate_meta(any: &AnyBox) {
    let meta = any.inner::<env::Meta>();
    match meta {
        env::Meta::Ty(ty) => validate_ty(ty),
        _ => {}
    }
}

fn validate_ty(ty: &Ty) {
    match ty {
        Ty::Arrow(_, _, src, tgt) => {
            src.validate(&validate_meta);
            tgt.validate(&validate_meta);
        }
        Ty::Functor(src, tgt) => {
            src.validate(&validate_meta);
            tgt.validate(&validate_meta);
        }
        _ => {}
    }
}

// --- Functor application ---

enum FunctorError {
    NoMapping(Prim),
    CompError(donut_core::common::Error),
}

use std::collections::HashSet;

impl<'a> Checker<'a> {
    /// Collect PrimIds of all parameter items for the current def's params.
    fn current_param_prims(&self, def_id: DefId) -> HashSet<PrimId> {
        let mut prims = HashSet::new();
        for param in &self.program.def(def_id).params {
            if let Some(Some(env_item)) = self.env_bounds.get(param.bound.0) {
                if let Some(prim_id) = env_item.prim_id {
                    prims.insert(prim_id);
                }
            }
        }
        prims
    }

    /// Apply functor with constraint generation.
    /// Unknown prims in `self.constrainable` generate a FunctorReq + fresh Item.
    /// Unknown prims NOT in `self.constrainable` return NoMapping error.
    fn apply_functor_constrained(
        &mut self,
        cell: &PureCell,
        functor_def_id: DefId,
        map: &HashMap<PrimId, FunctorEntry>,
        dim_shift: i32,
    ) -> Result<PureCell, FunctorError> {
        match cell {
            PureCell::Prim(prim, shape, dim) => {
                if let Some(entry) = map.get(&prim.id) {
                    // Known mapping — same as apply_functor
                    let mut result = entry.cell.clone();
                    if !entry.param_bound_entries.is_empty() && !prim.args.is_empty() {
                        let subst_map: SubstMap = entry
                            .param_bound_entries
                            .iter()
                            .zip(prim.args.iter())
                            .map(|(&(eid, ext_type), arg)| (eid, (arg.clone(), ext_type)))
                            .collect();
                        result = subst_cell(&result, &subst_map, &self.prim_owner)
                            .map_err(FunctorError::CompError)?;
                    }
                    let target_dim = dim.in_space as i32 + dim_shift;
                    while (result.dim().in_space as i32) < target_dim {
                        result = PureCell::id(result);
                    }
                    Ok(result)
                } else if let Some(cached) = self.constraint_cache.get(&(functor_def_id, prim.id)) {
                    // Already generated a fresh prim for this constraint
                    let mut result = cached.clone();
                    let target_dim = dim.in_space as i32 + dim_shift;
                    while (result.dim().in_space as i32) < target_dim {
                        result = PureCell::id(result);
                    }
                    Ok(result)
                } else if self.constrainable.contains(&prim.id) {
                    // Constrainable prim — generate constraint + fresh prim as Item
                    let fresh_id = self.fresh_prim_id();
                    let fresh_prim = Prim::with_id_args(fresh_id, prim.args.clone());

                    // Add fresh prim to constrainable (for f(g(m)) patterns)
                    self.constrainable.insert(fresh_id);

                    let result = match shape {
                        Shape::Zero => PureCell::zero(fresh_prim),
                        Shape::Succ { source, target } => {
                            let mapped_src = self.apply_functor_constrained(
                                source,
                                functor_def_id,
                                map,
                                dim_shift,
                            )?;
                            let mapped_tgt = self.apply_functor_constrained(
                                target,
                                functor_def_id,
                                map,
                                dim_shift,
                            )?;
                            PureCell::prim(fresh_prim, mapped_src, mapped_tgt)
                                .map_err(FunctorError::CompError)?
                        }
                    };

                    // Cache
                    self.constraint_cache
                        .insert((functor_def_id, prim.id), result.clone());

                    // Create Item for the fresh prim (for bound-variable substitution)
                    let functor_name = self.program.def(functor_def_id).lname.clone();
                    let arg_name = if let Some(&owner) = self.prim_owner.get(&prim.id) {
                        match owner {
                            env::PrimOwner::Ref(ref_id) => {
                                if ref_id.0 < self.program.items.len() {
                                    self.program.item(ref_id).lname.clone()
                                } else if let Some(Some(env_item)) = self.env_refs.get(ref_id.0) {
                                    env_item.lname.clone()
                                } else {
                                    "?".to_string()
                                }
                            }
                            env::PrimOwner::Bound(bound_id) => {
                                if bound_id.0 < self.program.bounds.len() {
                                    self.program.bound(bound_id).lname.clone()
                                } else if let Some(Some(env_item)) = self.env_bounds.get(bound_id.0)
                                {
                                    env_item.lname.clone()
                                } else {
                                    "?".to_string()
                                }
                            }
                        }
                    } else {
                        "?".to_string()
                    };
                    let display_name = format!("{}({})", functor_name, arg_name);

                    let result_bound_id = BoundId(self.env_bounds.len());
                    self.env_bounds.push(Some(env::Item {
                        cname: display_name.clone(),
                        lname: display_name,
                        kind: ItemKind::Param,
                        ty: Ty::Hole, // placeholder
                        prim_id: Some(fresh_id),
                        params: vec![],
                    }));
                    self.prim_owner
                        .insert(fresh_id, env::PrimOwner::Bound(result_bound_id));

                    // Record constraint with result bound
                    let arg_cell = PureCell::Prim(prim.clone(), shape.clone(), *dim);
                    self.current_reqs.push(env::FunctorReq {
                        functor: functor_def_id,
                        arg: arg_cell,
                        result: result_bound_id,
                    });

                    Ok(result)
                } else {
                    // Not constrainable — strict error
                    Err(FunctorError::NoMapping(prim.clone()))
                }
            }
            PureCell::Comp(axis, children, _) => {
                let mapped: Vec<PureCell> = children
                    .iter()
                    .map(|c| self.apply_functor_constrained(c, functor_def_id, map, dim_shift))
                    .collect::<Result<_, _>>()?;
                PureCell::comp(*axis, mapped).map_err(FunctorError::CompError)
            }
        }
    }
}

fn apply_functor(
    cell: &PureCell,
    map: &HashMap<PrimId, FunctorEntry>,
    prim_owner: &HashMap<PrimId, env::PrimOwner>,
    dim_shift: i32,
) -> Result<PureCell, FunctorError> {
    match cell {
        PureCell::Prim(prim, _, dim) => {
            match map.get(&prim.id) {
                Some(entry) => {
                    let mut result = entry.cell.clone();
                    // Substitute params with actual args
                    if !entry.param_bound_entries.is_empty() && !prim.args.is_empty() {
                        let subst_map: SubstMap = entry
                            .param_bound_entries
                            .iter()
                            .zip(prim.args.iter())
                            .map(|(&(eid, ext_type), arg)| (eid, (arg.clone(), ext_type)))
                            .collect();
                        result = subst_cell(&result, &subst_map, prim_owner)
                            .map_err(FunctorError::CompError)?;
                    }
                    // Lift to target dimension (accounting for dimension shift)
                    let target_dim = dim.in_space as i32 + dim_shift;
                    while (result.dim().in_space as i32) < target_dim {
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
                .map(|c| apply_functor(c, map, prim_owner, dim_shift))
                .collect::<Result<_, _>>()?;
            PureCell::comp(*axis, mapped).map_err(FunctorError::CompError)
        }
    }
}

// --- Public API ---

pub fn check(program: &Program) -> (env::Env, Vec<SpanError>) {
    let mut checker = Checker::new(program);
    let root_lookup = checker.process_trees(&program.def_order);
    checker.into_result(root_lookup)
}
