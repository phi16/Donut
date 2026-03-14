use std::collections::HashMap;

use crate::types::common::TokenSpan;
use crate::types::item::{DefId, ItemId, ItemKind};
use donut_core::common::{AnyBox, ExtId, Level, PrimId, PureVal};
use donut_core::pure_cell::{PureCell, Shape};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Color(pub u8, pub u8, pub u8);

impl Color {
    pub fn new(r: u8, g: u8, b: u8) -> Self {
        Color(r, g, b)
    }

    pub fn gray() -> Self {
        Color(128, 128, 128)
    }

    pub fn r(&self) -> u8 { self.0 }
    pub fn g(&self) -> u8 { self.1 }
    pub fn b(&self) -> u8 { self.2 }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Style {
    Color(Color),
    Lighten(f64),
    Darken(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decorator {
    Style(Style),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArrowTy {
    To,
    Eq,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Meta,
    Star,
    Arrow(Level /* non-zero */, ArrowTy, PureVal, PureVal),
    Functor(PureVal, PureVal),
    Nat,
    Rat,
    Color,
    Deco,
    Hole,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Meta {
    Ty(Ty),
    Nat(u64),
    Rat(f64),
    Color(Color),
    Deco(Decorator),
    Error,
}

impl From<Meta> for PureVal {
    fn from(m: Meta) -> Self {
        PureVal::Any(AnyBox::new(m))
    }
}

pub fn as_meta(val: &PureVal) -> Option<&Meta> {
    match val {
        PureVal::Any(v) => Some(v.inner::<Meta>()),
        _ => None,
    }
}

// Note: ItemId ≈ ExtId

// --- Entry ---

#[derive(Debug)]
pub struct Item {
    pub cname: String,
    pub lname: String,
    pub kind: ItemKind,
    pub ty: Ty,
    pub prim_id: Option<PrimId>,
    pub params: Vec<ItemId>,
}

#[derive(Debug)]
pub struct DefStyle {
    pub color: Color,
}

/// Constraint: functor `functor` must be defined on `arg`.
/// `arg` may contain parameter prims and fresh prims from prior constraints.
/// `result` is the Item created for the fresh prim representing the functor application result.
#[derive(Debug, Clone)]
pub struct FunctorReq {
    pub functor: DefId,
    pub arg: PureCell,
    pub result: ItemId,
}

#[derive(Debug)]
pub struct Def {
    pub qname: String,
    pub lname: String,
    pub span: TokenSpan,
    pub item: Option<ItemId>,
    pub ty: Ty,
    pub params: Vec<ItemId>,
    pub val: PureVal,
    pub decos: Vec<PureVal>,
    pub reqs: Vec<FunctorReq>,
    pub style: DefStyle,
    pub origin: Option<String>,
    pub param_counts: Vec<usize>,
}

#[derive(Debug)]
pub struct Module {
    pub this: Option<DefId>,
    pub lookup: HashMap<String, Module>,
}

// --- Env ---

#[derive(Debug)]
pub struct Env {
    pub items: Vec<Item>,
    pub defs: Vec<Def>,
    pub prim_item: HashMap<PrimId, ItemId>,
    pub root: Module,
    /// DeclDef expansion: PrimId of `x` → PureCell of `y` for `x := y`.
    pub def_subs: HashMap<PrimId, PureCell>,
}

// --- Display ---

/// Name-resolution context for display functions.
pub trait DisplayContext {
    fn item_cname(&self, ext_id: ExtId) -> &str;
    fn prim_name(&self, prim_id: PrimId) -> &str;
}

pub fn display_pure_val(ctx: &impl DisplayContext, pv: &PureVal) -> String {
    match pv {
        PureVal::Cell(cell) => display_cell(ctx, cell),
        PureVal::App(ext_id, args) => {
            let name = ctx.item_cname(*ext_id);
            if args.is_empty() {
                name.to_string()
            } else {
                let args_str: Vec<String> =
                    args.iter().map(|a| display_pure_val(ctx, a)).collect();
                format!("{}[{}]", name, args_str.join(", "))
            }
        }
        PureVal::Any(any) => display_meta(ctx, any.inner::<Meta>()),
    }
}

pub fn display_cell(ctx: &impl DisplayContext, cell: &PureCell) -> String {
    match cell {
        PureCell::Prim(prim, _, _) => {
            let name = ctx.prim_name(prim.id);
            if prim.args.is_empty() {
                name.to_string()
            } else {
                let args_str: Vec<String> =
                    prim.args.iter().map(|a| display_pure_val(ctx, a)).collect();
                format!("{}[{}]", name, args_str.join(", "))
            }
        }
        PureCell::Comp(axis, children, _) => {
            let sep = match axis {
                0 => " ",
                1 => "; ",
                _ => ", ",
            };
            let parts: Vec<String> =
                children.iter().map(|c| display_cell(ctx, c)).collect();
            if *axis >= 2 {
                format!("[{}: {}]", axis, parts.join(sep))
            } else {
                parts.join(sep)
            }
        }
    }
}

pub fn display_meta(ctx: &impl DisplayContext, meta: &Meta) -> String {
    match meta {
        Meta::Ty(ty) => display_ty(ctx, ty),
        Meta::Nat(n) => format!("{}", n),
        Meta::Rat(r) => format!("{}", r),
        Meta::Color(c) => format!("rgb({}, {}, {})", c.0, c.1, c.2),
        Meta::Deco(d) => format!("{:?}", d),
        Meta::Error => "<error>".to_string(),
    }
}

pub fn display_ty(ctx: &impl DisplayContext, ty: &Ty) -> String {
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
                display_pure_val(ctx, src),
                op,
                display_pure_val(ctx, tgt)
            )
        }
        Ty::Functor(src, tgt) => {
            format!(
                "{} ~> {}",
                display_pure_val(ctx, src),
                display_pure_val(ctx, tgt)
            )
        }
        Ty::Nat => "nat".to_string(),
        Ty::Rat => "rat".to_string(),
        Ty::Color => "color".to_string(),
        Ty::Deco => "decorator".to_string(),
        Ty::Hole => "_".to_string(),
    }
}

// --- Env as DisplayContext ---

impl DisplayContext for Env {
    fn item_cname(&self, ext_id: ExtId) -> &str {
        &self.items[ext_id.0 as usize].cname
    }

    fn prim_name(&self, prim_id: PrimId) -> &str {
        if let Some(&item_id) = self.prim_item.get(&prim_id) {
            &self.items[item_id.0].cname
        } else {
            "?"
        }
    }
}

struct DefDisplayContext<'a> {
    env: &'a Env,
    params: &'a [ItemId],
}

impl<'a> DefDisplayContext<'a> {
    fn from_def(env: &'a Env, def: &'a Def) -> Self {
        Self { env, params: &def.params }
    }
}

impl<'a> DisplayContext for DefDisplayContext<'a> {
    fn item_cname(&self, ext_id: ExtId) -> &str {
        let item_id = ItemId(ext_id.0 as usize);
        if self.params.contains(&item_id) {
            &self.env.items[item_id.0].lname
        } else {
            &self.env.items[item_id.0].cname
        }
    }

    fn prim_name(&self, prim_id: PrimId) -> &str {
        if let Some(&item_id) = self.env.prim_item.get(&prim_id) {
            if self.params.contains(&item_id) {
                &self.env.items[item_id.0].lname
            } else {
                &self.env.items[item_id.0].cname
            }
        } else {
            "?"
        }
    }
}

impl Env {
    pub fn display_pure_val(&self, pv: &PureVal) -> String {
        display_pure_val(self, pv)
    }

    pub fn display_cell(&self, cell: &PureCell) -> String {
        display_cell(self, cell)
    }

    pub fn display_ty(&self, ty: &Ty) -> String {
        display_ty(self, ty)
    }

    /// Format params as `[x: *, y: x → x]` or `[x: * | f(x)]` with constraints.
    fn format_params(&self, ctx: &impl DisplayContext, params: &[ItemId], reqs: &[FunctorReq]) -> String {
        if params.is_empty() && reqs.is_empty() {
            return String::new();
        }
        let params_str: Vec<String> = params.iter().map(|&item_id| {
            let item = &self.items[item_id.0];
            format!("{}: {}", item.lname, display_ty(ctx, &item.ty))
        }).collect();
        if reqs.is_empty() {
            format!("[{}]", params_str.join(", "))
        } else {
            let reqs_str: Vec<String> = reqs.iter().map(|req| {
                let functor_name = &self.defs[req.functor.0].lname;
                let arg_str = display_cell(ctx, &req.arg);
                format!("{}({})", functor_name, arg_str)
            }).collect();
            format!("[{} | {}]", params_str.join(", "), reqs_str.join(", "))
        }
    }

    /// Display a def's own parameters: `[x: C → C, f: x → x]`
    pub fn display_params(&self, def: &Def) -> String {
        let ctx = DefDisplayContext::from_def(self, def);
        self.format_params(&ctx, &def.params, &def.reqs)
    }

    /// Display type using def's parameter context (lname for params)
    pub fn display_def_ty(&self, def: &Def) -> String {
        let ctx = DefDisplayContext::from_def(self, def);
        display_ty(&ctx, &def.ty)
    }

    /// Display any Ty using def's parameter context
    pub fn display_ty_in_def(&self, ty: &Ty, def: &Def) -> String {
        let ctx = DefDisplayContext::from_def(self, def);
        display_ty(&ctx, ty)
    }

    /// Collect all parameters for a def, including ancestor params,
    /// by walking the module tree along the def's qname path.
    pub fn collect_all_params(&self, def: &Def) -> Vec<ItemId> {
        let mut all_params = Vec::new();
        let mut current = &self.root;
        for seg in def.qname.split('.') {
            if let Some(child) = current.lookup.get(seg) {
                if let Some(def_id) = child.this {
                    all_params.extend_from_slice(&self.defs[def_id.0].params);
                }
                current = child;
            }
        }
        all_params
    }

    /// Display all parameters (own + ancestors): `[x: C → C]`
    pub fn display_all_params(&self, def: &Def) -> String {
        let all_params = self.collect_all_params(def);
        let all_reqs = self.collect_all_reqs(def);
        let ctx = DefDisplayContext { env: self, params: &all_params };
        self.format_params(&ctx, &all_params, &all_reqs)
    }

    /// Collect all functor constraints for a def, including ancestor reqs.
    pub fn collect_all_reqs(&self, def: &Def) -> Vec<FunctorReq> {
        let mut all_reqs = Vec::new();
        let mut current = &self.root;
        for seg in def.qname.split('.') {
            if let Some(child) = current.lookup.get(seg) {
                if let Some(def_id) = child.this {
                    all_reqs.extend_from_slice(&self.defs[def_id.0].reqs);
                }
                current = child;
            }
        }
        all_reqs
    }

    /// Whether a def is parametric (own or ancestor has parameters).
    pub fn is_parametric(&self, def: &Def) -> bool {
        !self.collect_all_params(def).is_empty()
    }

    /// Display full def signature: `M[x: *].a: *`
    pub fn display_def_signature(&self, def: &Def) -> String {
        let all_params = self.collect_all_params(def);
        let segments: Vec<&str> = def.qname.split('.').collect();

        let ctx = DefDisplayContext { env: self, params: &all_params };

        // Build name: M[x: *].a
        let mut name = String::new();
        let mut current = &self.root;
        for (i, &seg) in segments.iter().enumerate() {
            if i > 0 {
                name.push('.');
            }
            name.push_str(seg);
            if let Some(child) = current.lookup.get(seg) {
                if let Some(def_id) = child.this {
                    let d = &self.defs[def_id.0];
                    name.push_str(&self.format_params(&ctx, &d.params, &d.reqs));
                }
                current = child;
            }
        }

        format!("{}: {}", name, display_ty(&ctx, &def.ty))
    }

    pub fn def_color<'a>(&self, def: &'a Def) -> &'a Color {
        &def.style.color
    }

    /// Expand DeclDef substitutions in a PureCell.
    pub fn expand_defs(&self, pc: &PureCell) -> PureCell {
        expand_defs(pc, &self.def_subs)
    }
}

// --- DeclDef expansion ---

/// Recursively replace PrimIds in `pc` using the substitution map.
pub fn expand_defs(pc: &PureCell, subs: &HashMap<PrimId, PureCell>) -> PureCell {
    if subs.is_empty() {
        return pc.clone();
    }
    match pc {
        PureCell::Prim(prim, shape, dim) => {
            if prim.args.is_empty() && dim.effective == dim.in_space {
                if let Some(replacement) = subs.get(&prim.id) {
                    return replacement.clone();
                }
            }
            let new_shape = expand_defs_shape(shape, subs);
            PureCell::Prim(prim.clone(), new_shape, *dim)
        }
        PureCell::Comp(axis, children, dim) => {
            let new_children = children.iter().map(|c| expand_defs(c, subs)).collect();
            PureCell::Comp(*axis, new_children, *dim)
        }
    }
}

fn expand_defs_shape(shape: &Shape, subs: &HashMap<PrimId, PureCell>) -> Shape {
    match shape {
        Shape::Zero => Shape::Zero,
        Shape::Succ { source, target } => Shape::Succ {
            source: Box::new(expand_defs(source, subs)),
            target: Box::new(expand_defs(target, subs)),
        },
    }
}
