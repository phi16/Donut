use crate::geometry::*;
use donut_core::cell::{Cell, Coord, LayoutCell, Level, PrimId, Shape, Vec1};

/*

struct Builder {}

impl Builder {
    fn new() -> Self {
        Self {}
    }

    fn shape(&self, s: &Shape) -> Unit {
        unimplemented!()
    }

    fn cell(&self, c: &LayoutCell) -> Geometry {
        let layout = &c.1;
        let mut g = match c.0.as_ref() {
            Cell::Prim(prim_id, shape) => {
                let unit = self.shape(shape);
                Geometry::Prim(*prim_id, unit)
            }
            Cell::Id(inner) => {
                let mut inner = self.cell(inner);
                inner.shift(layout.size[layout.size.len() - 1]);
                inner
            }
            Cell::Comp(axis, cs, inner_pads) => {
                assert!(cs.len() >= 2);
                let cs = cs.iter().map(|c| self.cell(c)).collect::<Vec<_>>();
                let mut ps = Vec::new();
                for i in 0..(cs.len() - 1) {
                    ps.push(Geometry::bridge(
                        cs[i].t(*axis),
                        cs[i + 1].s(*axis),
                        inner_pads[i],
                    ));
                }
                Geometry::comp(*axis, cs, ps)
            }
        };
        g.extend(&layout.pad);
        g
    }
}

pub fn extract_geometry(cell: &LayoutCell) -> Geometry {
    let builder = Builder::new();
    builder.cell(cell)
}

*/
