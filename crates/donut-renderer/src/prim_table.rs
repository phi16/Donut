use donut_core::common::{Level, Prim, PrimId};
use std::collections::HashMap;

pub struct PrimEntry {
    pub name: String,
    pub level: Level,
    pub color: (u8, u8, u8),
}

pub struct PrimTable {
    table: HashMap<PrimId, PrimEntry>,
}

impl PrimTable {
    pub fn new() -> Self {
        PrimTable {
            table: HashMap::new(),
        }
    }

    pub fn insert(&mut self, prim: Prim, name: &str, level: Level, color: (u8, u8, u8)) {
        let name = name.to_string();
        let entry = PrimEntry { name, level, color };
        self.table.insert(prim.id, entry);
    }

    pub fn get(&self, prim: &Prim) -> Option<&PrimEntry> {
        self.table.get(&prim.id)
    }
}
