use donut_core::cell::Globular;
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

    pub fn format_cell_type(&self, pure: &PureCell) -> String {
        let dim = pure.dim().in_space;
        match dim {
            0 => "*".to_string(),
            1 => {
                let src = self.format_0cell(&pure.s());
                let tgt = self.format_0cell(&pure.t());
                format!("{} → {}", src, tgt)
            }
            _ => {
                let src = self.format_1cell(&pure.s());
                let tgt = self.format_1cell(&pure.t());
                format!("{} → {}", src, tgt)
            }
        }
    }

    fn format_0cell(&self, pure: &PureCell) -> String {
        match pure {
            PureCell::Prim(prim, _, _) => self.format_prim(prim),
            PureCell::Comp(_, children, _) => {
                let parts: Vec<String> = children.iter().map(|c| self.format_0cell(c)).collect();
                if parts.is_empty() { "·".to_string() } else { parts.join(" ") }
            }
        }
    }

    fn format_1cell(&self, pure: &PureCell) -> String {
        let parts = self.collect_1cell_parts(pure);
        if parts.is_empty() {
            let base = self.format_0cell(&pure.s());
            format!("id[{}]", base)
        } else {
            parts.join(" ")
        }
    }

    fn collect_1cell_parts(&self, pure: &PureCell) -> Vec<String> {
        match pure {
            PureCell::Prim(prim, _, dim) => {
                if dim.effective < dim.in_space {
                    vec![]
                } else {
                    vec![self.format_prim(prim)]
                }
            }
            PureCell::Comp(_, children, _) => {
                children.iter().flat_map(|c| self.collect_1cell_parts(c)).collect()
            }
        }
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
