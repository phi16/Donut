use crate::geometry::*;
use donut_core::cell::{Cell, LayoutCell, Level, PrimId, Vec1};

struct Builder {}

impl Builder {
    fn new() -> Self {
        Self {}
    }

    fn cell(&self, c: &LayoutCell) -> Block {
        let size = &c.1.size;
        let dim = size.len();
        let mut b = match c.0.as_ref() {
            Cell::Prim(prim_id, shape) => {
                unimplemented!()
            }
            Cell::Id(inner) => {
                let mut inner = self.cell(inner);
                inner.shift(size[dim - 1]);
                inner
            }
            Cell::Comp(l, cs, inner_pads) => {
                let cs = cs.iter().map(|c| self.cell(c)).collect::<Vec<_>>();
                unimplemented!()
            }
        };
        b.pad(&c.1.pad);
        b
    }
}

pub fn extract_geometry(cell: &LayoutCell) -> Block {
    let builder = Builder::new();
    builder.cell(cell)
}
