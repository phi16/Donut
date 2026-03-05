use donut_core::free_cell::FreeCell;
use std::collections::HashMap;

type Result<T> = std::result::Result<T, String>;

#[derive(Debug, Clone)]
pub struct Element {
    pub name: String,
    pub color: (u8, u8, u8),
    pub cell: FreeCell,
}

#[derive(Debug, Clone)]
pub struct Table {
    pub elements: Vec<Element>,
    pub lookup: HashMap<String, usize>,
}

pub fn load(code: &str) -> Result<Table> {
    let env = crate::check::check_source(code)?;
    Ok(Table {
        elements: env
            .entries
            .into_iter()
            .map(|e| Element {
                name: e.name,
                color: e.color,
                cell: e.cell,
            })
            .collect(),
        lookup: env.lookup,
    })
}
