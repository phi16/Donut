use donut_core::common::{Level, Prim, PrimArg, PrimId};
use donut_core::pure_cell::PureCell;
use std::collections::HashMap;

pub struct PrimEntry {
    pub name: String,
    pub level: Level,
    pub color: (u8, u8, u8),
    pub param_counts: Vec<usize>,
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

    pub fn insert(
        &mut self,
        prim: Prim,
        name: &str,
        level: Level,
        color: (u8, u8, u8),
        param_counts: Vec<usize>,
    ) {
        let name = name.to_string();
        let entry = PrimEntry {
            name,
            level,
            color,
            param_counts,
        };
        self.table.insert(prim.id, entry);
    }

    pub fn get(&self, prim: &Prim) -> Option<&PrimEntry> {
        self.table.get(&prim.id)
    }

    pub fn format_prim(&self, prim: &Prim) -> String {
        let entry = self.table.get(&prim.id);
        let base = entry.map(|e| e.name.as_str()).unwrap_or("?");
        if prim.args.is_empty() {
            return base.to_string();
        }
        let param_counts = entry.map(|e| &e.param_counts[..]).unwrap_or(&[]);
        let segments: Vec<&str> = base.split('.').collect();
        let mut arg_idx = 0;
        let mut parts = Vec::new();
        for (i, seg) in segments.iter().enumerate() {
            let n = param_counts.get(i).copied().unwrap_or(0);
            if n > 0 && arg_idx + n <= prim.args.len() {
                let args: Vec<String> = prim.args[arg_idx..arg_idx + n]
                    .iter()
                    .map(|a| self.format_arg(a))
                    .collect();
                parts.push(format!("{}[{}]", seg, args.join(", ")));
                arg_idx += n;
            } else {
                parts.push(seg.to_string());
            }
        }
        // Any remaining args attach to the last segment
        if arg_idx < prim.args.len() {
            let last = parts.pop().unwrap_or_default();
            let args: Vec<String> = prim.args[arg_idx..]
                .iter()
                .map(|a| self.format_arg(a))
                .collect();
            parts.push(format!("{}[{}]", last, args.join(", ")));
        }
        parts.join(".")
    }

    fn format_arg(&self, arg: &PrimArg) -> String {
        match arg {
            PrimArg::Cell(PureCell::Prim(prim, _, _)) => self.format_prim(prim),
            PrimArg::Cell(_) => "..".to_string(),
            PrimArg::Nat(n) => n.to_string(),
            PrimArg::App(id, args) => {
                let base = self
                    .table
                    .get(id)
                    .map(|e| e.name.as_str())
                    .unwrap_or("?");
                if args.is_empty() {
                    base.to_string()
                } else {
                    let args: Vec<String> = args.iter().map(|a| self.format_arg(a)).collect();
                    format!("{}[{}]", base, args.join(", "))
                }
            }
        }
    }
}
