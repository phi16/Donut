/*
use crate::geometry::*;
use donut_core::cell::{CellF, LayoutCell, Level, PaddedCell, PrimId, ShapeF, Vec1};
use std::rc::Rc;

struct Builder {}

impl Builder {
    fn new() -> Self {
        Self {}
    }

    fn prim(&self, prim_id: PrimId, level: Level, shape: &ShapeF<PaddedCell>) -> Geometry {}

    fn cell(&self, pc: &PaddedCell) -> Geometry {
        let layout = &pc.cell.1;
        let dim = layout.size.len();
        let size = pc.size();
        match &pc.cell.0 {
            CellF::Prim(prim_id, shape) => self.prim(*prim_id, dim as Level, shape),
            CellF::Id(inner) => {
                let bound = size[dim - 1];
                let inner_pc = PaddedCell {
                    cell: Rc::clone(inner),
                    pad: pc.pad.clone(),
                };
                self.cell(&inner_pc).shift(bound)
            }
            CellF::Comp(children, level, _) => {
                let cs = children.iter().map(|c| self.cell(c)).collect::<Vec<_>>();
                unimplemented!()
            }
        }
    }
}

pub fn extract_geometry(cell: &Rc<LayoutCell>) -> Geometry {
    let builder = Builder::new();
    builder.cell(&PaddedCell::from_cell(Rc::clone(cell)))
}

*/
