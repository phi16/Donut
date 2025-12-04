/*
use crate::geometry::*;
use donut_core::cell::{Cell, LayoutCell, Level, LayoutCell, PrimId, Shape, Vec1};
use std::rc::Rc;

struct Builder {}

impl Builder {
    fn new() -> Self {
        Self {}
    }

    fn prim(&self, prim_id: PrimId, level: Level, shape: &Shape) -> Geometry {}

    fn cell(&self, pc: &LayoutCell) -> Geometry {
        let layout = &pc.cell.1;
        let dim = layout.size.len();
        let size = pc.size();
        match &pc.cell.0 {
            Cell::Prim(prim_id, shape) => self.prim(*prim_id, dim as Level, shape),
            Cell::Id(inner) => {
                let bound = size[dim - 1];
                let inner_pc = LayoutCell {
                    cell: Rc::clone(inner),
                    pad: pc.pad.clone(),
                };
                self.cell(&inner_pc).shift(bound)
            }
            Cell::Comp(children, level, _) => {
                let cs = children.iter().map(|c| self.cell(c)).collect::<Vec<_>>();
                unimplemented!()
            }
        }
    }
}

pub fn extract_geometry(cell: &LayoutCell) -> Geometry {
    let builder = Builder::new();
    builder.cell(&LayoutCell::from_cell(Rc::clone(cell)))
}

*/
