use donut_core::common::{Prim, PrimId};
use donut_lang::check::{display_cell_type, display_prim, PrimDecl};
use donut_core::pure_cell::PureCell;
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
        display_cell_type(pure, &self.prim_decls)
    }
}
