use crate::geometry::*;
use donut_core::cell::{Cell, LayoutCell, Level, PrimId, Shape, Vec1};
use std::rc::Rc;

struct Builder {}

impl Builder {
    fn new() -> Self {
        Self {}
    }

    fn prim(&self, prim_id: PrimId, level: Level, shape: &Shape) -> Block {
        unimplemented!()
    }

    fn cell(&self, pc: &LayoutCell) -> Block {
        let layout = &pc.1;
        let dim = layout.size.len();
        let size = pc.size();
        match &pc.0.as_ref() {
            Cell::Prim(prim_id, shape) => self.prim(*prim_id, dim as Level, shape),
            Cell::Id(inner) => {
                let bound = size[dim - 1];
                let inner = inner.extend(&pc.1.pad);
                self.cell(&inner).shift(bound)
            }
            Cell::Comp(level, children, _) => {
                let cs = children.iter().map(|c| self.cell(c)).collect::<Vec<_>>();
                unimplemented!()
            }
        }
    }
}

pub fn extract_geometry(cell: &LayoutCell) -> Block {
    let builder = Builder::new();
    builder.cell(cell)
}
