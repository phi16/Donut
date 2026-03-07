pub use crate::types::common::S;
pub use crate::types::common::TokenSpan;
use std::collections::{HashMap, HashSet};

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

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: ValId,
}

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
    /// For imported modules, the source name (e.g. "sys") used as canonical prefix.
    pub origin: Option<String>,
    /// Names added via `use` (available for resolution but not re-exported).
    used: HashSet<String>,
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
            origin: None,
            used: HashSet::new(),
        }
    }

    /// Insert a new entry. Returns the existing ItemId if already defined as non-used.
    /// Used entries can be shadowed by new definitions.
    pub fn define(&mut self, name: String, item: ItemId) -> Option<ItemId> {
        if let Some(&idx) = self.index.get(&name) {
            if self.used.contains(&name) {
                // Shadow the used entry: add new entry, update index
                let new_idx = self.entries.len();
                self.index.insert(name.clone(), new_idx);
                self.entries.push((name, item));
                None
            } else {
                Some(self.entries[idx].1)
            }
        } else {
            let idx = self.entries.len();
            self.index.insert(name.clone(), idx);
            self.entries.push((name, item));
            None
        }
    }

    /// Replace an existing entry's ItemId (e.g., forward ref → real item).
    /// Panics if the name does not exist.
    pub fn replace(&mut self, name: &str, item: ItemId) {
        let &idx = self.index.get(name).expect("replace: name not found");
        self.entries[idx].1 = item;
    }

    pub fn get(&self, name: &str) -> Option<ItemId> {
        let &idx = self.index.get(name)?;
        Some(self.entries[idx].1)
    }

    pub fn contains_key(&self, name: &str) -> bool {
        self.index.contains_key(name)
    }

    /// Merge source into self. Used entries from source are propagated as used.
    /// Returns conflicting key names (non-used only).
    pub fn merge(&mut self, source: Module) -> Vec<String> {
        let mut conflicts = Vec::new();
        let source_used = source.used;
        for (key, item) in source.entries {
            if source_used.contains(&key) {
                // Propagate used entries as used (skip if any entry already exists)
                if !self.has_entry(&key) {
                    self.used.insert(key.clone());
                    let idx = self.entries.len();
                    self.index.insert(key.clone(), idx);
                    self.entries.push((key, item));
                }
            } else if let Some(existing) = self.get(&key) {
                if existing != item {
                    conflicts.push(key);
                }
            } else {
                self.define(key, item);
            }
        }
        conflicts
    }

    /// Merge all source entries as "used" (internal only, not visible via `get()`).
    pub fn merge_used(&mut self, source: Module) -> Vec<String> {
        let mut conflicts = Vec::new();
        for (key, item) in source.entries {
            if self.has_entry(&key) {
                // Already exists (used or non-used); check for conflict on non-used
                if let Some(existing) = self.get(&key) {
                    if existing != item {
                        conflicts.push(key);
                    }
                }
            } else {
                self.used.insert(key.clone());
                let idx = self.entries.len();
                self.index.insert(key.clone(), idx);
                self.entries.push((key, item));
            }
        }
        conflicts
    }

    /// Check if a name exists in entries (including used).
    fn has_entry(&self, name: &str) -> bool {
        self.index.contains_key(name)
    }

    /// Check if a name is in the used set.
    fn is_used_name(&self, name: &str) -> bool {
        self.used.contains(name)
    }

    /// Check if an entry at the given index is exported (non-used and active in index).
    pub fn is_exported(&self, idx: usize) -> bool {
        let (name, _) = &self.entries[idx];
        !self.used.contains(name) && self.index.get(name) == Some(&idx)
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
