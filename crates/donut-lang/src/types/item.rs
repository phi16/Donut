pub use crate::types::common::TokenSpan;
use donut_core::common::Axis;
use std::collections::HashMap;

// --- Index types ---

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ValId(pub usize);

/// Generator ID. Structurally equivalent to PrimId but kept as a separate type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GenId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ItemId(pub usize);

// --- Val types ---

#[derive(Debug, Clone, Copy)]
pub enum ArrowKind {
    To,
    Eq,
    Functor,
}

#[derive(Debug)]
pub struct ParamVal {
    pub name: Option<String>,
    pub val: ValId,
}

#[derive(Debug)]
pub struct Segment {
    pub name: String,
    pub params: Vec<ParamVal>,
}

#[derive(Debug)]
pub struct Path {
    pub segments: Vec<Segment>,
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

// --- Gen (Generator) ---
// Creates a new prim. Identified by cname. GenId ≈ PrimId.

#[derive(Debug, Clone, Copy)]
pub enum GenKind {
    Decl,
    Def,
    Param,
}

#[derive(Debug)]
pub struct Gen {
    pub cname: String,
    pub lname: String,
    pub kind: GenKind,
    pub ty: ValId,
    pub params: Vec<Param>,
}

// --- Item ---
// Named reference. Body references Gens via Val expressions.

#[derive(Debug)]
pub struct FunctorMapping {
    pub params: Vec<Param>,
    pub applicand: ValId,
    pub val: ValId,
}

#[derive(Debug)]
pub enum ItemBody {
    Value {
        val: Option<ValId>,
        members: Module,
    },
    Functor {
        mappings: Vec<FunctorMapping>,
    },
}

#[derive(Debug)]
pub struct Item {
    pub qname: String,
    pub lname: String,
    pub span: TokenSpan,
    pub ty: Option<ValId>,
    pub params: Vec<Param>,
    pub body: ItemBody,
    pub decos: Vec<ValId>,
    pub origin: Option<String>,
    pub param_counts: Vec<usize>,
}

// --- Module ---

#[derive(Debug, Clone)]
pub struct Module {
    pub entries: Vec<ItemId>,
    index: HashMap<String, usize>,
}

// --- Program ---

pub struct Program {
    pub root: Module,
    pub gens: Vec<Gen>,
    pub items: Vec<Item>,
    pub vals: Vec<Val>,
    pub val_spans: Vec<Option<TokenSpan>>,
}

impl Program {
    pub fn val(&self, id: ValId) -> &Val {
        &self.vals[id.0]
    }
    pub fn val_span(&self, id: ValId) -> Option<&TokenSpan> {
        self.val_spans[id.0].as_ref()
    }
    pub fn generator(&self, id: GenId) -> &Gen {
        &self.gens[id.0]
    }
    pub fn item(&self, id: ItemId) -> &Item {
        &self.items[id.0]
    }
}

impl Module {
    pub fn new() -> Self {
        Module {
            entries: Vec::new(),
            index: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, item: ItemId) -> Option<ItemId> {
        if let Some(&idx) = self.index.get(&name) {
            Some(self.entries[idx])
        } else {
            let idx = self.entries.len();
            self.index.insert(name, idx);
            self.entries.push(item);
            None
        }
    }

    pub fn get(&self, name: &str) -> Option<ItemId> {
        let &idx = self.index.get(name)?;
        Some(self.entries[idx])
    }

    pub fn contains_key(&self, name: &str) -> bool {
        self.index.contains_key(name)
    }

    /// Merge entries from another Module. Returns conflicting lnames.
    /// Requires access to items array to get lnames from ItemIds.
    pub fn merge_from(&mut self, source: &Module, items: &[Item]) -> Vec<String> {
        let mut conflicts = Vec::new();
        for &item_id in &source.entries {
            let lname = items[item_id.0].lname.clone();
            if self.define(lname.clone(), item_id).is_some() {
                conflicts.push(lname);
            }
        }
        conflicts
    }
}

impl Item {
    pub fn members(&self) -> Option<&Module> {
        match &self.body {
            ItemBody::Value { members, .. } => Some(members),
            _ => None,
        }
    }

    pub fn members_mut(&mut self) -> Option<&mut Module> {
        match &mut self.body {
            ItemBody::Value { members, .. } => Some(members),
            _ => None,
        }
    }

    pub fn val(&self) -> Option<ValId> {
        match &self.body {
            ItemBody::Value { val, .. } => *val,
            _ => None,
        }
    }
}
