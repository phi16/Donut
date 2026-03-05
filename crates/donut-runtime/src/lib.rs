pub mod env;

use donut_core::common::PrimId;
use donut_core::free_cell::{Cell, CellF, FreeCell};
use donut_core::pure_cell::PureCell;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    U32(u32),
    F32(f64),
    Bool(bool),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::U32(v) => write!(f, "{}", v),
            Value::F32(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
        }
    }
}

pub fn format_values(values: &[Value]) -> String {
    values
        .iter()
        .map(|v| v.to_string())
        .collect::<Vec<_>>()
        .join(", ")
}

pub fn extract_prim_id(pure: &PureCell) -> Option<PrimId> {
    match pure {
        PureCell::Prim(prim, _, dim) if dim.effective == dim.in_space => Some(prim.id),
        _ => None,
    }
}

pub type EvalFn = Box<dyn Fn(&[Value]) -> Vec<Value>>;

pub struct Runtime {
    ops: HashMap<PrimId, EvalFn>,
}

impl Runtime {
    pub fn new() -> Self {
        Runtime {
            ops: HashMap::new(),
        }
    }

    pub fn register(&mut self, id: PrimId, f: impl Fn(&[Value]) -> Vec<Value> + 'static) {
        self.ops.insert(id, Box::new(f));
    }

    pub fn eval(&self, cell: &FreeCell, input: &[Value]) -> Result<Vec<Value>, String> {
        self.eval_cell(&cell.cell, input)
    }

    fn eval_cell(&self, cell: &Cell, input: &[Value]) -> Result<Vec<Value>, String> {
        match cell.0.as_ref() {
            CellF::Prim(prim, src, _) => {
                let expected = width_1cell(src);
                if input.len() != expected {
                    return Err(format!(
                        "prim {}: expected {} inputs, got {}",
                        prim.id, expected, input.len()
                    ));
                }
                let f = self
                    .ops
                    .get(&prim.id)
                    .ok_or_else(|| format!("no eval function for prim {}", prim.id))?;
                Ok(f(input))
            }
            CellF::Id(_) => Ok(input.to_vec()),
            CellF::Comp(1, children) => {
                let mut current = input.to_vec();
                for child in children {
                    current = self.eval_cell(child, &current)?;
                }
                Ok(current)
            }
            CellF::Comp(0, children) => {
                let mut offset = 0;
                let mut result = Vec::new();
                for child in children {
                    let w = source_width(child);
                    if offset + w > input.len() {
                        return Err(format!(
                            "input too short: need {} values at offset {}, have {}",
                            w,
                            offset,
                            input.len()
                        ));
                    }
                    let child_input = &input[offset..offset + w];
                    let child_output = self.eval_cell(child, child_input)?;
                    result.extend(child_output);
                    offset += w;
                }
                Ok(result)
            }
            CellF::Zero(_) => Err("cannot evaluate a 0-cell".into()),
            CellF::Comp(axis, _) => Err(format!("unsupported composition axis {} for evaluation", axis)),
        }
    }
}

/// Compute the number of input value slots for a 2-cell.
fn source_width(cell: &Cell) -> usize {
    match cell.0.as_ref() {
        CellF::Prim(_, src, _) => width_1cell(src),
        CellF::Id(x) => width_1cell(x),
        CellF::Comp(1, children) => children.first().map_or(0, source_width),
        CellF::Comp(0, children) => children.iter().map(source_width).sum(),
        CellF::Zero(_) => 0,
        CellF::Comp(_, _) => 0,
    }
}

/// Compute the number of value slots for a 1-cell (type).
/// - Prim (atomic type like u32) → 1 slot
/// - Id (identity/unit type) → 0 slots
/// - Comp (product type) → sum of children
/// - Zero (0-cell, base object) → 0 slots
fn width_1cell(cell: &Cell) -> usize {
    match cell.0.as_ref() {
        CellF::Zero(_) => 0,
        CellF::Prim(_, _, _) => 1,
        CellF::Id(_) => 0,
        CellF::Comp(_, children) => children.iter().map(width_1cell).sum(),
    }
}

pub use env::ENV_SOURCE;

#[cfg(test)]
mod tests;
