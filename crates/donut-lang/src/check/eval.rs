use super::*;

// --- Checker methods: evaluation ---

impl<'a> Checker<'a> {
    // --- Meta evaluation ---

    pub(super) fn eval_meta_val(&self, val: &Val) -> Result<PrimArg> {
        match val {
            Val::Path(path) => {
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
                        let mapping = self.build_path_mapping(path)?;

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
                    EntryBody::Type(_, _) => {
                        Err(format!("{} is a type alias, not a meta value", name))
                    }
                }
            }
            Val::Lit(Lit::Number(s)) => {
                if s.contains('.') {
                    let r: f64 = s.parse().map_err(|e: std::num::ParseFloatError| e.to_string())?;
                    Ok(PrimArg::rat(r))
                } else {
                    let n: u64 = s.parse().map_err(|_| format!("invalid nat: {}", s))?;
                    Ok(PrimArg::Nat(n))
                }
            }
            _ => Err("unsupported in meta context".to_string()),
        }
    }

    pub(super) fn meta_type_coercible(&self, from: &MetaType, to: &MetaType) -> bool {
        if let (Some(nat_id), Some(rat_id)) = (self.meta_id("nat"), self.meta_id("rat")) {
            *from == MetaType(nat_id, vec![]) && *to == MetaType(rat_id, vec![])
        } else {
            false
        }
    }

    pub(super) fn detect_param_kind(&self, val: &Val) -> ParamKind {
        match self.eval_ty(val) {
            Ok((_, Ty::Meta(mt))) => {
                if self.meta_id("nat") == Some(mt.0) {
                    ParamKind::Nat
                } else if self.meta_id("rat") == Some(mt.0) {
                    ParamKind::Rat
                } else {
                    ParamKind::Meta(mt)
                }
            }
            _ => ParamKind::Cell,
        }
    }

    pub(super) fn resolve_meta_type(&self, val: &Val) -> Option<MetaType> {
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

    pub(super) fn check_meta_type(&self, val: &Val) -> Option<MetaType> {
        match val {
            Val::Lit(Lit::Number(_)) => {
                let id = self.meta_id("nat")?;
                Some(MetaType(id, vec![]))
            }
            Val::Path(path) => {
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

                let seg = path.segments.last()?;
                let arg_types: Vec<Option<MetaType>> = seg.0.params.iter()
                    .map(|pv| {
                        let v = self.program.val(pv.val);
                        self.check_meta_type(&v.0)
                    })
                    .collect();

                if arg_types.len() != sig.params.len() {
                    return None;
                }

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

    // --- Type evaluation ---

    pub(super) fn eval_ty(&self, val: &Val) -> Result<(u8, Ty)> {
        match val {
            Val::Path(path) => {
                let name = path_name(path).ok_or_else(|| "invalid type".to_string())?;
                if name == "*" {
                    return Ok((0, Ty::Zero));
                }
                if let Some(idx) = self.resolve_name(&name) {
                    if let EntryBody::Type(dim, ref ty) = self.entries[idx].body {
                        let mapping = self.build_path_mapping(path)?;
                        let base = (dim, ty.clone());
                        if mapping.is_empty() {
                            return Ok(base);
                        } else {
                            return Ok(subst_ty(&base, &mapping));
                        }
                    }
                }
                if name == "meta" {
                    let id = self.meta_id("meta").unwrap();
                    return Ok((0, Ty::Meta(MetaType(id, vec![]))));
                }
                if let Some(mt) = self.resolve_meta_type(val) {
                    return Ok((0, Ty::Meta(mt)));
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

    pub(super) fn eval_val(&self, val: &Val) -> Result<FreeCell> {
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
            EntryBody::Type(_, _) => return Err(format!("{} is a type alias, not a cell", name)),
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

        let functor_name = self.resolve_functor_name(&name)
            .ok_or_else(|| format!("{} is not a functor", name))?;

        let map = self.functor_maps.get(&functor_name)
            .ok_or_else(|| format!("{} is not a functor", name))?;

        let app_val = &self.program.val(app_id).0;
        let arg_cell = self.eval_val(app_val)?;

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

    pub(super) fn resolve_param_arg(&self, pv: &ParamVal, kind: &ParamKind) -> Result<PrimArg> {
        let val_s = self.program.val(pv.val);
        match kind {
            ParamKind::Cell => {
                let arg_cell = self.eval_val(&val_s.0)?;
                Ok(PrimArg::Cell(arg_cell.pure.clone()))
            }
            ParamKind::Nat | ParamKind::Rat | ParamKind::Meta(_) => {
                let mut arg = self.eval_meta_val(&val_s.0)?;
                if *kind == ParamKind::Rat {
                    if let PrimArg::Nat(n) = arg {
                        arg = PrimArg::rat(n as f64);
                    }
                }
                Ok(arg)
            }
        }
    }

    pub(super) fn build_path_mapping(&self, path: &Path) -> Result<HashMap<PrimId, PrimArg>> {
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

            let params = self.resolve_name(&current)
                .and_then(|idx| self.entry_params.get(&idx))
                .or_else(|| {
                    self.resolve_module_name(&current)
                        .and_then(|name| self.module_params.get(&name))
                });

            if let Some(params) = params {
                for (i, (_, fresh_id, kind)) in params.iter().enumerate() {
                    if let Some(pv) = seg.params.get(i) {
                        let arg = self.resolve_param_arg(pv, kind)?;
                        mapping.insert(*fresh_id, arg);
                    }
                }
            }
        }

        Ok(mapping)
    }

    // --- Module alias / instantiation ---

    pub(super) fn try_module_alias(
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
                        let arg = self.resolve_param_arg(pv, kind)?;
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

    pub(super) fn instantiate_module(
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
                    EntryBody::Type(dim, ty) => {
                        let base = (*dim, ty.clone());
                        let (d, t) = if mapping.is_empty() {
                            base
                        } else {
                            subst_ty(&base, mapping)
                        };
                        EntryBody::Type(d, t)
                    }
                };
                let src_pc = src_entry.param_counts.clone();
                let idx = self.add_entry(
                    dest_member.clone(),
                    src_entry.color,
                    new_body, src_pc,
                );

                if let Some(params) = self.entry_params.get(src_idx).cloned() {
                    self.entry_params.insert(idx, params);
                }

                if let Some(val) = self.meta_values.get(src_idx).cloned() {
                    let new_val = if mapping.is_empty() {
                        val
                    } else {
                        val.subst(mapping)
                    };
                    self.meta_values.insert(idx, new_val);
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

    pub(super) fn resolve_item_param_types(&self, params: &[Param]) -> Vec<MetaType> {
        params.iter()
            .filter_map(|p| {
                let ty_s = self.program.val(p.ty);
                self.resolve_meta_type(&ty_s.0)
            })
            .collect()
    }
}

// --- Free functions ---

pub(super) fn apply_functor(cell: &PureCell, map: &HashMap<PrimId, FunctorEntry>) -> Result<PureCell> {
    match cell {
        PureCell::Prim(prim, _, dim) => match map.get(&prim.id) {
            Some(entry) => {
                let mut result = entry.cell.clone();
                if !entry.param_prims.is_empty() && !prim.args.is_empty() {
                    let subst_map: HashMap<PrimId, PrimArg> = entry.param_prims.iter()
                        .zip(prim.args.iter())
                        .map(|(&param_id, arg)| (param_id, arg.clone()))
                        .collect();
                    result = result.subst(&subst_map);
                }
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

fn subst_ty(ty: &(u8, Ty), mapping: &HashMap<PrimId, PrimArg>) -> (u8, Ty) {
    let (dim, t) = ty;
    let new_t = match t {
        Ty::Zero => Ty::Zero,
        Ty::Succ(s, t) => Ty::Succ(
            FreeCell::from_pure(&s.pure.subst(mapping)),
            FreeCell::from_pure(&t.pure.subst(mapping)),
        ),
        Ty::Meta(mt) => Ty::Meta(mt.clone()),
    };
    (*dim, new_t)
}

pub(super) fn make_cell(prim: Prim, ty: &Ty) -> Result<FreeCell> {
    match ty {
        Ty::Zero => Ok(FreeCell::zero(prim)),
        Ty::Succ(s, t) => FreeCell::prim(prim, s.clone(), t.clone()),
        Ty::Meta(_) => Err("meta type cannot be used as cell type".to_string()),
    }
}

pub(super) fn match_ty(x: &Ty, y: &Ty) -> Result<()> {
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
