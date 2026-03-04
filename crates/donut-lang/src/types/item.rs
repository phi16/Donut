pub use crate::types::common::S;
use std::collections::HashMap;

// --- Val 型 (semtree::Val に対応、A なし、演算子 flat) ---

#[derive(Debug, Clone, Copy)]
pub enum ArrowKind {
    To,
    Eq,
    Functor,
}

#[derive(Debug, Clone)]
pub struct ParamVal {
    pub name: Option<String>,
    pub val: S<Val>,
}

#[derive(Debug, Clone)]
pub struct Segment {
    pub name: String,
    pub params: Vec<ParamVal>,
}

#[derive(Debug, Clone)]
pub struct Path {
    pub segments: Vec<S<Segment>>,
    pub applicand: Option<Box<S<Val>>>,
}

#[derive(Debug, Clone)]
pub enum Lit {
    Number(String),
    String(String),
    Array(Vec<S<Val>>),
    Object(Vec<(String, S<Val>)>),
}

#[derive(Debug, Clone)]
pub enum Val {
    Path(Path),
    Lit(Lit),
    Comp(u32, Vec<S<Val>>),
    CompStar(Vec<S<Val>>),
    Arrow(ArrowKind, Box<S<Val>>, Box<S<Val>>),
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
    pub ty: S<Val>,
}

#[derive(Debug, Clone)]
pub struct FunctorMapping {
    pub applicand: S<Val>,
    pub val: S<Val>,
}

#[derive(Debug, Clone)]
pub enum ItemBody {
    Value {
        val: Option<S<Val>>,
        members: Module,
    },
    Functor {
        mappings: Vec<FunctorMapping>,
    },
}

#[derive(Debug, Clone)]
pub struct Item {
    pub kind: Option<ItemKind>,
    pub ty: Option<S<Val>>,
    pub params: Vec<Param>,
    pub body: ItemBody,
    pub decos: Vec<S<Val>>,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub entries: Vec<(String, Item)>,
    index: HashMap<String, usize>,
}

impl Module {
    pub fn new() -> Self {
        Module {
            entries: Vec::new(),
            index: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, item: Item) {
        if let Some(&idx) = self.index.get(&name) {
            self.entries[idx].1 = item;
        } else {
            let idx = self.entries.len();
            self.index.insert(name.clone(), idx);
            self.entries.push((name, item));
        }
    }

    pub fn get(&self, name: &str) -> Option<&Item> {
        let &idx = self.index.get(name)?;
        Some(&self.entries[idx].1)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Item> {
        let &idx = self.index.get(name)?;
        Some(&mut self.entries[idx].1)
    }

    pub fn contains_key(&self, name: &str) -> bool {
        self.index.contains_key(name)
    }

    /// Merge source entries into self. Returns conflicting key names.
    pub fn merge(&mut self, source: Module) -> Vec<String> {
        let mut conflicts = Vec::new();
        for (key, item) in source.entries {
            if self.contains_key(&key) {
                conflicts.push(key);
            } else {
                self.define(key, item);
            }
        }
        conflicts
    }
}

impl Item {
    pub fn new(kind: Option<ItemKind>) -> Self {
        Item {
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

    pub fn param(ty: S<Val>) -> Self {
        Item {
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

    pub fn val(&self) -> Option<&S<Val>> {
        match &self.body {
            ItemBody::Value { val, .. } => val.as_ref(),
            _ => None,
        }
    }
}
