pub use crate::types::common::S;
pub use crate::types::common::TokenSpan;
use std::collections::HashMap;

// --- Index types ---

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ValId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ItemId(pub usize);

// --- Val 型 (semtree::Val に対応、A なし、演算子 flat) ---

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
    pub segments: Vec<S<Segment>>,
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
    Comp(u32, Vec<ValId>),
    CompStar(Vec<ValId>),
    Arrow(ArrowKind, ValId, ValId),
    Hole(Hole),
}

// --- Item 型 ---

#[derive(Debug, Clone, Copy)]
pub enum ItemKind {
    Decl,
    Alias,
    Def,
    Param,
}

#[derive(Debug)]
pub struct Param {
    pub name: String,
    pub ty: ValId,
}

#[derive(Debug)]
pub struct FunctorMapping {
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
    pub span: TokenSpan,
    pub kind: Option<ItemKind>,
    pub ty: Option<ValId>,
    pub params: Vec<Param>,
    pub body: ItemBody,
    pub decos: Vec<ValId>,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub entries: Vec<(String, ItemId)>,
    index: HashMap<String, usize>,
}

// --- Program ---

pub struct Program {
    pub root: Module,
    pub items: Vec<Item>,
    pub vals: Vec<S<Val>>,
}

impl Program {
    pub fn val(&self, id: ValId) -> &S<Val> {
        &self.vals[id.0]
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

    pub fn define(&mut self, name: String, item: ItemId) {
        if let Some(&idx) = self.index.get(&name) {
            self.entries[idx].1 = item;
        } else {
            let idx = self.entries.len();
            self.index.insert(name.clone(), idx);
            self.entries.push((name, item));
        }
    }

    pub fn get(&self, name: &str) -> Option<ItemId> {
        let &idx = self.index.get(name)?;
        Some(self.entries[idx].1)
    }

    pub fn contains_key(&self, name: &str) -> bool {
        self.index.contains_key(name)
    }

    /// Merge source entries into self. Returns conflicting key names.
    /// Duplicate entries with the same ItemId are silently skipped.
    pub fn merge(&mut self, source: Module) -> Vec<String> {
        let mut conflicts = Vec::new();
        for (key, item) in source.entries {
            if let Some(existing) = self.get(&key) {
                if existing != item {
                    conflicts.push(key);
                }
            } else {
                self.define(key, item);
            }
        }
        conflicts
    }
}

impl Item {
    pub fn new(kind: Option<ItemKind>, span: TokenSpan) -> Self {
        Item {
            span,
            kind,
            ty: None,
            params: Vec::new(),
            body: ItemBody::Value {
                val: None,
                members: Module::new(),
            },
            decos: Vec::new(),
        }
    }

    pub fn param(ty: ValId, span: TokenSpan) -> Self {
        Item {
            span,
            kind: Some(ItemKind::Param),
            ty: Some(ty),
            params: Vec::new(),
            body: ItemBody::Value {
                val: None,
                members: Module::new(),
            },
            decos: Vec::new(),
        }
    }

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
