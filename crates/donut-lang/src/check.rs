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
}

#[derive(Debug, Clone)]
pub struct Entry {
    pub name: String,
    pub color: (u8, u8, u8),
    pub cell: FreeCell,
}

#[derive(Debug, Clone)]
pub struct Env {
    pub entries: Vec<Entry>,
    pub lookup: HashMap<String, usize>,
}

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
    module_params: HashMap<String, Vec<(String, PrimId)>>,
    module_members: HashMap<String, Vec<(String, Option<usize>)>>,
    entry_params: HashMap<usize, Vec<(String, PrimId)>>,
    accumulated_args: Vec<PrimArg>,
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

    fn add_entry(&mut self, name: String, color: Option<(u8, u8, u8)>, cell: FreeCell) -> usize {
        let color = color.unwrap_or_else(|| auto_color(self.entries.len()));
        let idx = self.entries.len();
        self.lookup.insert(name.clone(), idx);
        self.entries.push(Entry { name, color, cell });
        idx
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

        let color = extract_color(self.program, &item.decos);
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
                        let ty_s = self.program.val(*ty_id);
                        match self.eval_ty(&ty_s.0) {
                            Ok((_, ty)) => {
                                let prim = self.make_prim();
                                let cell = match &ty {
                                    Ty::Zero => Ok(FreeCell::zero(prim)),
                                    Ty::Succ(s, t) => {
                                        FreeCell::prim(prim, s.clone(), t.clone())
                                    }
                                };
                                match cell {
                                    Ok(cell) => {
                                        let idx = self.add_entry(qname.clone(), color, cell);
                                        if has_params {
                                            self.entry_params
                                                .insert(idx, param_freshes.clone());
                                        }
                                    }
                                    Err(msg) => self.error_at(span, msg),
                                }
                            }
                            Err(msg) => self.error_at(span, msg),
                        }
                    }
                    (dim_ty, Some(body_id)) => {
                        let body_s = self.program.val(*body_id);
                        match self.eval_val(&body_s.0) {
                            Ok(body) => {
                                let dim = body.pure.dim().in_space;

                                if let Some(ty_id) = dim_ty {
                                    let ty_s = self.program.val(*ty_id);
                                    match self.eval_ty(&ty_s.0) {
                                        Ok((declared_dim, declared_ty)) => {
                                            if declared_dim != dim {
                                                self.error_at(
                                                    span,
                                                    "declared type dimension does not match body",
                                                );
                                            } else {
                                                let body_ty = if dim == 0 {
                                                    Ty::Zero
                                                } else {
                                                    Ty::Succ(
                                                        FreeCell::from_pure(&body.pure.s()),
                                                        FreeCell::from_pure(&body.pure.t()),
                                                    )
                                                };
                                                if let Err(msg) =
                                                    match_ty(&declared_ty, &body_ty)
                                                {
                                                    self.error_at(span, msg);
                                                }
                                            }
                                        }
                                        Err(msg) => self.error_at(span, msg),
                                    }
                                }

                                let idx = self.add_entry(qname.clone(), color, body);
                                if has_params {
                                    self.entry_params.insert(idx, param_freshes.clone());
                                }
                            }
                            Err(msg) => self.error_at(span, msg),
                        }
                    }
                    (None, None) => {}
                }

                if !members.entries.is_empty() {
                    self.check_members(&qname, members, has_params);
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

    fn check_functor(&mut self, item: &Item, _qname: &str, mappings: &[FunctorMapping]) {
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

        let src_prim_id = match extract_prim_id(&src_cell.pure) {
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
            let app_cell = match self.eval_val(app_val) {
                Ok(c) => c,
                Err(msg) => {
                    self.error_at(&app_span, msg);
                    continue;
                }
            };
            let app_prim_id = match extract_prim_id(&app_cell.pure) {
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
            if dim != val_cell.pure.dim().in_space {
                self.error_at(&val_span, "functor mapping dimension mismatch");
                continue;
            }

            let expected_s = apply_functor(&app_cell.pure.s(), &functor_map);
            let expected_t = apply_functor(&app_cell.pure.t(), &functor_map);

            match (expected_s, expected_t) {
                (Ok(es), Ok(et)) => {
                    let actual_s = val_cell.pure.s();
                    let actual_t = val_cell.pure.t();
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

            functor_map.insert(app_prim_id, val_cell.pure.clone());
        }
    }

    fn check_members(&mut self, prefix: &str, module: &Module, parent_has_params: bool) {
        self.prefixes.push(prefix.to_string());
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
        self.prefixes.pop();

        if parent_has_params || !members.is_empty() {
            self.module_members.insert(prefix.to_string(), members);
        }
    }

    // --- Params ---

    fn enter_params(&mut self, params: &[Param]) -> Result<Vec<(String, PrimId)>> {
        let mut freshes = Vec::new();
        for param in params {
            let ty_s = self.program.val(param.ty);
            let (_, ty) = self.eval_ty(&ty_s.0)?;
            let fresh_id = self.fresh_prim_id();
            let prim = Prim::new(fresh_id);
            let cell = match &ty {
                Ty::Zero => FreeCell::zero(prim),
                Ty::Succ(s, t) => FreeCell::prim(prim, s.clone(), t.clone())?,
            };
            let param_name = self.qualified_name(&param.name);
            self.add_entry(param_name, None, cell.clone());
            self.accumulated_args.push(PrimArg::Cell(cell.pure.clone()));
            freshes.push((param.name.clone(), fresh_id));
        }
        Ok(freshes)
    }

    fn exit_params(&mut self, freshes: &[(String, PrimId)]) {
        for _ in freshes {
            self.accumulated_args.pop();
        }
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
                for (i, (_, fresh_id)) in params.iter().enumerate() {
                    if let Some(pv) = seg.params.get(i) {
                        let val_s = self.program.val(pv.val);
                        let arg_cell = self.eval_val(&val_s.0)?;
                        mapping.insert(*fresh_id, PrimArg::Cell(arg_cell.pure.clone()));
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
                let new_pure = if mapping.is_empty() {
                    src_entry.cell.pure.clone()
                } else {
                    src_entry.cell.pure.subst(mapping)
                };
                let new_cell = FreeCell::from_pure(&new_pure);
                let idx = self.add_entry(dest_member.clone(), Some(src_entry.color), new_cell);

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

    fn eval_ty(&self, val: &Val) -> Result<(u8, Ty)> {
        match val {
            Val::Path(path) => {
                if path_name(path).as_deref() == Some("*") {
                    return Ok((0, Ty::Zero));
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
        if path.applicand.is_some() {
            return Err("functor application cannot be used in applicand".to_string());
        }
        let name = path_name(path).ok_or_else(|| "invalid path".to_string())?;
        let index = self
            .resolve_name(&name)
            .ok_or_else(|| format!("unknown variable: {}", name))?;

        let base_cell = self.entries[index].cell.clone();

        let mapping = self.build_path_mapping(path)?;
        if mapping.is_empty() {
            Ok(base_cell)
        } else {
            let new_pure = base_cell.pure.subst(&mapping);
            Ok(FreeCell::from_pure(&new_pure))
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

            let resolved = self
                .resolve_name(&current)
                .ok_or_else(|| format!("unknown: {}", current))?;

            if let Some(params) = self.entry_params.get(&resolved) {
                for (i, (_, fresh_id)) in params.iter().enumerate() {
                    if let Some(pv) = seg.params.get(i) {
                        let val_s = self.program.val(pv.val);
                        let arg_cell = self.eval_val(&val_s.0)?;
                        mapping.insert(*fresh_id, PrimArg::Cell(arg_cell.pure.clone()));
                    }
                }
            }

            if let Some(resolved_name) = self.resolve_module_name(&current) {
                if let Some(params) = self.module_params.get(&resolved_name) {
                    for (i, (_, fresh_id)) in params.iter().enumerate() {
                        if let Some(pv) = seg.params.get(i) {
                            let val_s = self.program.val(pv.val);
                            let arg_cell = self.eval_val(&val_s.0)?;
                            mapping.insert(*fresh_id, PrimArg::Cell(arg_cell.pure.clone()));
                        }
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

fn extract_prim_id(cell: &PureCell) -> Option<PrimId> {
    match cell {
        PureCell::Prim(prim, _, dim) if dim.effective == dim.in_space => Some(prim.id),
        _ => None,
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

fn path_name(path: &Path) -> Option<String> {
    let parts: Vec<_> = path.segments.iter().map(|s| s.0.name.clone()).collect();
    if parts.is_empty() {
        return None;
    }
    Some(parts.join("."))
}

fn extract_color(program: &Program, decos: &[ValId]) -> Option<(u8, u8, u8)> {
    for &deco_id in decos {
        let deco = program.val(deco_id);
        let Val::Path(path) = &deco.0 else {
            continue;
        };
        if path.segments.len() != 1 {
            continue;
        }
        let seg = &path.segments[0].0;
        let name = &seg.name;
        let args = &seg.params;

        match name.as_str() {
            "gray" if args.len() == 1 => {
                let g = extract_number(program, &args[0])? as u8;
                return Some((g, g, g));
            }
            "rgb" if args.len() == 3 => {
                let r = extract_number(program, &args[0])? as u8;
                let g = extract_number(program, &args[1])? as u8;
                let b = extract_number(program, &args[2])? as u8;
                return Some((r, g, b));
            }
            "hsv" if !args.is_empty() => {
                let h = extract_rational(program, &args[0])?;
                let s = args
                    .get(1)
                    .map_or(Some(1.0), |a| extract_rational(program, a))?;
                let v = args
                    .get(2)
                    .map_or(Some(1.0), |a| extract_rational(program, a))?;
                return Some(hsv2rgb(h, s, v));
            }
            _ => {}
        }
    }
    None
}

fn extract_number(program: &Program, param: &ParamVal) -> Option<f64> {
    let val = program.val(param.val);
    match &val.0 {
        Val::Lit(Lit::Number(s)) => s.parse().ok(),
        _ => None,
    }
}

fn extract_rational(program: &Program, param: &ParamVal) -> Option<f64> {
    let val = program.val(param.val);
    match &val.0 {
        Val::Lit(Lit::Number(s)) => s.parse().ok(),
        Val::Lit(Lit::String(s)) => match s.as_str() {
            "1-" => Some(0.01),
            "1" => Some(0.05),
            "1+" => Some(0.09),
            "2+" => Some(0.52),
            "2" => Some(0.56),
            "2-" => Some(0.60),
            "orange" => Some(0.1),
            "yellow" => Some(0.16),
            "green" => Some(0.33),
            "cyan" => Some(0.5),
            "blue" => Some(0.6),
            "purple" => Some(0.75),
            "pink" => Some(0.9),
            _ => None,
        },
        Val::Path(path) => {
            if path.applicand.is_some() {
                return None;
            }
            let parts: Option<Vec<_>> = path
                .segments
                .iter()
                .map(|seg_s| {
                    if !seg_s.0.params.is_empty() {
                        return None;
                    }
                    let name = &seg_s.0.name;
                    crate::convert::is_number_str(name).then_some(name.as_str())
                })
                .collect();
            parts?.join(".").parse().ok()
        }
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

fn dedent(code: &str) -> String {
    let lines: Vec<&str> = code.lines().collect();
    let min_indent = lines
        .iter()
        .filter(|l| !l.trim().is_empty())
        .map(|l| l.len() - l.trim_start().len())
        .min()
        .unwrap_or(0);
    lines
        .iter()
        .map(|l| {
            if l.len() >= min_indent {
                &l[min_indent..]
            } else {
                l.trim()
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

// --- Public API ---

pub fn check(program: &Program, tokens: &[Token]) -> (Env, Vec<Error>) {
    let mut checker = Checker::new(program, tokens);
    checker.check_module(&program.root);
    checker.into_result()
}

pub fn check_source(code: &str) -> std::result::Result<Env, String> {
    let code = dedent(code.trim_matches('\n'));
    let (tokens, _, tok_errors) = crate::tokenize::tokenize(&code);
    if !tok_errors.is_empty() {
        return Err(format!("tokenize errors: {:?}", tok_errors));
    }
    let (program, parse_errors) = crate::parse::parse(&tokens);
    if !parse_errors.is_empty() {
        return Err(format!("parse errors: {:?}", parse_errors));
    }
    let (sem_prog, conv_errors) = crate::convert::convert(program, &tokens);
    if !conv_errors.is_empty() {
        return Err(format!("convert errors: {:?}", conv_errors));
    }
    let (resolved, resolve_errors) = crate::resolve::resolve(sem_prog, &tokens);
    if !resolve_errors.is_empty() {
        return Err(format!("check errors: {:?}", resolve_errors));
    }
    let (env, check_errors) = check(&resolved, &tokens);
    if !check_errors.is_empty() {
        return Err(format!(
            "{}",
            check_errors
                .iter()
                .map(|(_, msg)| msg.as_str())
                .collect::<Vec<_>>()
                .join("\n")
        ));
    }
    Ok(env)
}
