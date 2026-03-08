use donut_core::cell::Globular;
use donut_core::common::{Prim, PrimId};
use donut_core::pure_cell::PureCell;
use donut_lang::check::{display_prim, display_pure_cell, PrimDecl};
use std::collections::HashMap;

pub struct PrimTable {
    prim_decls: HashMap<PrimId, PrimDecl>,
}

impl PrimTable {
    pub fn new(prim_decls: HashMap<PrimId, PrimDecl>) -> Self {
        PrimTable { prim_decls }
    }

    pub fn get(&self, prim: &Prim) -> Option<&PrimDecl> {
        self.prim_decls.get(&prim.id)
    }

    pub fn format_prim(&self, prim: &Prim) -> String {
        display_prim(prim, &self.prim_decls)
    }

    pub fn format_cell_type(&self, pure: &PureCell) -> String {
        let dim = pure.dim().in_space;
        match dim {
            0 => "*".to_string(),
            1 => {
                let src = display_pure_cell(&pure.s(), &self.prim_decls);
                let tgt = display_pure_cell(&pure.t(), &self.prim_decls);
                format!("{} → {}", src, tgt)
            }
            _ => {
                let src = self.format_1cell(&pure.s());
                let tgt = self.format_1cell(&pure.t());
                format!("{} → {}", src, tgt)
            }
        }
    }

    fn format_1cell(&self, pure: &PureCell) -> String {
        let parts = self.collect_1cell_parts(pure);
        if parts.is_empty() {
            let base = display_pure_cell(&pure.s(), &self.prim_decls);
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
                    vec![display_prim(prim, &self.prim_decls)]
                }
            }
            PureCell::Comp(_, children, _) => {
                children.iter().flat_map(|c| self.collect_1cell_parts(c)).collect()
            }
        }
    }
}
