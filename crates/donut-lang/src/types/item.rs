use crate::types::common::{S, TokenSpan};
use donut_core::common::Axis;
use std::collections::HashMap;

// --- Index types ---

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ValId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ItemId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(pub usize);

/// A scope entry: either a direct Item (e.g. parameter) or a Def (named definition).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Ref {
    Item(ItemId),
    Def(DefId),
}

// --- Val types ---

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ArrowKind {
    To,
    Eq,
    Functor,
}

#[derive(Debug)]
pub struct Path {
    pub target: Ref,
    pub args: Vec<ValId>,
    pub applicand: Option<ValId>,
}

#[derive(Debug)]
pub enum Lit {
    Number(String),
    String(String),
    Array(Vec<ValId>),
    Object(Vec<(String, ValId)>),
}

#[derive(Debug, Clone)]
pub enum Hole {
    Any,
    Named(String),
}

#[derive(Debug)]
pub enum Val {
    Path(Path),
    Lit(Lit),
    Comp(Axis, Vec<ValId>),
    CompStar(Vec<ValId>),
    Arrow(ArrowKind, ValId, ValId),
    Hole(Hole),
}

// --- Param ---

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: ValId,
}

// --- Item ---
// The actual entity. Identified by cname. ItemId ≈ ExtId.

#[derive(Debug, Clone, Copy)]
pub enum ItemKind {
    Decl,
    Def, // TODO
    Param,
}

#[derive(Debug)]
pub struct Item {
    pub cname: String,
    pub lname: String,
    pub kind: ItemKind,
    pub ty: ValId,
    pub params: Vec<Param>,
}

// --- Def ---
// Named definition. Has body, members, decorators.

#[derive(Debug)]
pub struct FunctorMapping {
    pub params: Vec<Param>,
    pub applicand: ValId,
    pub val: ValId,
}

#[derive(Debug)]
pub enum DefBody {
    None,
    Decl { item: ItemId },
    Alias { val: ValId },
    Functor { mappings: Vec<FunctorMapping> },
}

#[derive(Debug)]
pub struct Def {
    pub qname: String,
    pub lname: String,
    pub span: TokenSpan,
    pub ty: Option<ValId>,
    pub params: Vec<Param>,
    pub body: DefBody,
    pub members: Module,
    pub decos: Vec<ValId>,
    pub origin: Option<String>,
    pub param_counts: Vec<usize>,
}

// --- Module ---

#[derive(Debug, Clone)]
pub struct Module {
    pub entries: Vec<DefId>,
    index: HashMap<String, usize>,
}

// --- DefTree ---

#[derive(Debug, Clone)]
pub enum DefTree {
    Def(DefId),
    Scope {
        def_id: DefId,
        children: Vec<DefTree>,
    },
}

// --- Program ---

pub struct Program {
    pub root: Module,
    pub items: Vec<Item>,
    pub defs: Vec<Def>,
    pub vals: Vec<S<Val>>,
    pub def_order: Vec<DefTree>,
}

impl Program {
    pub fn val(&self, id: ValId) -> &Val {
        &self.vals[id.0].0
    }
    pub fn val_span(&self, id: ValId) -> &TokenSpan {
        &self.vals[id.0].1
    }
    pub fn item(&self, id: ItemId) -> &Item {
        &self.items[id.0]
    }
    pub fn def(&self, id: DefId) -> &Def {
        &self.defs[id.0]
    }
}

impl Module {
    pub fn new() -> Self {
        Module {
            entries: Vec::new(),
            index: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, def: DefId) -> Option<DefId> {
        if let Some(&idx) = self.index.get(&name) {
            Some(self.entries[idx])
        } else {
            let idx = self.entries.len();
            self.index.insert(name, idx);
            self.entries.push(def);
            None
        }
    }

    pub fn get(&self, name: &str) -> Option<DefId> {
        let &idx = self.index.get(name)?;
        Some(self.entries[idx])
    }

    pub fn contains_key(&self, name: &str) -> bool {
        self.index.contains_key(name)
    }

    /// Merge entries from another Module. Returns conflicting lnames.
    pub fn merge_from(&mut self, source: &Module, defs: &[Def]) -> Vec<String> {
        let mut conflicts = Vec::new();
        for &def_id in &source.entries {
            let lname = defs[def_id.0].lname.clone();
            if self.define(lname.clone(), def_id).is_some() {
                conflicts.push(lname);
            }
        }
        conflicts
    }
}

impl Def {
    pub fn val(&self) -> Option<ValId> {
        match &self.body {
            DefBody::Alias { val } => Some(*val),
            _ => None,
        }
    }
}
