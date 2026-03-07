pub mod env;

use donut_core::cell::Globular;
use donut_core::common::{PrimArg, PrimId};
use donut_core::free_cell::{Cell, CellF, FreeCell};
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    U32(u32),
    F32(f64),
    Bool(bool),
    F2(f64, f64),
    F3(f64, f64, f64),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::U32(v) => write!(f, "{}", v),
            Value::F32(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::F2(x, y) => write!(f, "({}, {})", x, y),
            Value::F3(x, y, z) => write!(f, "({}, {}, {})", x, y, z),
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

type EvalFn = Box<dyn Fn(&[PrimArg], &[Value]) -> Result<Vec<Value>, String>>;

pub struct Runtime {
    ops: HashMap<PrimId, EvalFn>,
    base: Option<PrimId>,
}

impl Runtime {
    pub fn new() -> Self {
        Runtime {
            ops: HashMap::new(),
            base: None,
        }
    }

    pub fn set_base(&mut self, id: PrimId) {
        self.base = Some(id);
    }

    pub fn register(&mut self, id: PrimId, f: impl Fn(&[PrimArg], &[Value]) -> Result<Vec<Value>, String> + 'static) {
        self.ops.insert(id, Box::new(f));
    }

    /// Check if a cell can be evaluated with no input.
    /// 型だけで「実行すべきか」を判定: sys.C 上の閉じた 2-cell か？
    pub fn is_evaluable(&self, cell: &FreeCell) -> bool {
        let dim = cell.pure.dim().in_space;
        if dim != 2 {
            return false;
        }
        if source_width(&cell.cell) > 0 {
            return false;
        }
        // 0-cell 境界が base (sys.C) と一致するか
        let Some(base) = self.base else { return false };
        let zero_cell = cell.pure.s().s();
        zero_cell.extract_prim_id() == Some(base)
    }

    /// Returns None if evaluable, or Some(reason) if not.
    pub fn eval_check(&self, cell: &FreeCell, prim_names: &HashMap<PrimId, String>) -> Option<String> {
        let dim = cell.pure.dim().in_space;
        if dim != 2 {
            return Some(format!("{}-cell", dim));
        }
        // 0-cell 境界チェック
        if let Some(base) = self.base {
            let zero_cell = cell.pure.s().s();
            if zero_cell.extract_prim_id() != Some(base) {
                let base_name = prim_names.get(&base).cloned().unwrap_or_else(|| format!("prim#{}", base));
                return Some(format!("not on {}", base_name));
            }
        }
        let sw = source_width(&cell.cell);
        if sw > 0 {
            return Some(format!("needs {} input(s)", sw));
        }
        let mut missing = Vec::new();
        self.collect_missing_ops(&cell.cell, &mut missing, prim_names);
        if !missing.is_empty() {
            missing.sort();
            missing.dedup();
            return Some(format!("missing ops: {}", missing.join(", ")));
        }
        None
    }

    fn collect_missing_ops(&self, cell: &Cell, missing: &mut Vec<String>, prim_names: &HashMap<PrimId, String>) {
        match cell.0.as_ref() {
            CellF::Prim(prim, _, _) => {
                if !self.ops.contains_key(&prim.id) {
                    let name = prim_names.get(&prim.id)
                        .cloned()
                        .unwrap_or_else(|| format!("prim#{}", prim.id));
                    missing.push(name);
                }
            }
            CellF::Id(_) => {}
            CellF::Comp(0 | 1, children) => {
                for c in children {
                    self.collect_missing_ops(c, missing, prim_names);
                }
            }
            CellF::Comp(_, _) | CellF::Zero(_) => {}
        }
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
                f(&prim.args, input)
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

#[cfg(test)]
mod tests;
