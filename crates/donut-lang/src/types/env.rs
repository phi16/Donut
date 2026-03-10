use std::collections::HashMap;

use crate::types::common::TokenSpan;
use crate::types::item::{DefId, ItemId, ItemKind};
use donut_core::common::{AnyBox, Level, PrimId, PureVal};

#[derive(Debug, Clone, PartialEq)]
pub struct Color(pub u8, pub u8, pub u8);

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
