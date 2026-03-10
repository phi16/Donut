use std::collections::HashMap;

use crate::types::common::TokenSpan;
use crate::types::item::{DefId, ItemId, ItemKind};
use donut_core::common::{AnyBox, Level, PrimId, PureVal};
use donut_core::pure_cell::PureCell;

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
pub struct Def {
    pub qname: String,
    pub lname: String,
    pub span: TokenSpan,
    pub item: Option<ItemId>,
    pub ty: Ty,
    pub params: Vec<ItemId>,
    pub val: PureVal,
    pub decos: Vec<PureVal>,
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
}

// --- Display ---

impl Env {
    pub fn prim_name(&self, id: PrimId) -> &str {
        if let Some(&item_id) = self.prim_item.get(&id) {
            &self.items[item_id.0].cname
        } else {
            "?"
        }
    }

    pub fn display_pure_val(&self, pv: &PureVal) -> String {
        match pv {
            PureVal::Cell(cell) => self.display_cell(cell),
            PureVal::App(ext_id, args) => {
                let name = &self.items[ext_id.0 as usize].cname;
                if args.is_empty() {
                    name.clone()
                } else {
                    let args_str: Vec<String> =
                        args.iter().map(|a| self.display_pure_val(a)).collect();
                    format!("{}[{}]", name, args_str.join(", "))
                }
            }
            PureVal::Any(any) => self.display_meta(any.inner::<Meta>()),
        }
    }

    pub fn display_cell(&self, cell: &PureCell) -> String {
        match cell {
            PureCell::Prim(prim, _, _) => {
                let name = self.prim_name(prim.id);
                if !prim.args.is_empty() {
                    let args_str: Vec<String> =
                        prim.args.iter().map(|a| self.display_pure_val(a)).collect();
                    format!("{}[{}]", name, args_str.join(", "))
                } else {
                    name.to_string()
                }
            }
            PureCell::Comp(axis, children, _) => {
                let sep = match axis {
                    0 => " ",
                    1 => "; ",
                    _ => ", ",
                };
                let parts: Vec<String> =
                    children.iter().map(|c| self.display_cell(c)).collect();
                if *axis >= 2 {
                    format!("[{}: {}]", axis, parts.join(sep))
                } else {
                    parts.join(sep)
                }
            }
        }
    }

    pub fn display_meta(&self, meta: &Meta) -> String {
        match meta {
            Meta::Ty(ty) => self.display_ty(ty),
            Meta::Nat(n) => format!("{}", n),
            Meta::Rat(r) => format!("{}", r),
            Meta::Color(c) => format!("rgb({}, {}, {})", c.0, c.1, c.2),
            Meta::Deco(d) => format!("{:?}", d),
            Meta::Error => "<error>".to_string(),
        }
    }

    pub fn display_ty(&self, ty: &Ty) -> String {
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

    pub fn display_params(&self, def: &Def) -> String {
        if def.params.is_empty() {
            return String::new();
        }
        let params: Vec<String> = def.params.iter().map(|&item_id| {
            let item = &self.items[item_id.0];
            format!("{}: {}", item.lname, self.display_ty(&item.ty))
        }).collect();
        format!("[{}]", params.join(", "))
    }

    pub fn def_color<'a>(&self, def: &'a Def) -> Option<&'a Color> {
        for deco in &def.decos {
            if let Some(Meta::Deco(Decorator::Style(Style::Color(c)))) = as_meta(deco) {
                return Some(c);
            }
        }
        None
    }
}
