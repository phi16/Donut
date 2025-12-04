use crate::geometry_2::*;
use donut_core::cell;
use donut_core::cell::{Cell, LayoutCell, Level, PrimId, Vec1};
use std::rc::Rc;

struct Builder {}

impl Builder {
    fn new() -> Self {
        Self {}
    }

    fn prim_geo(&self, prim_id: PrimId, s: Geometry, t: Geometry, w: u32) -> Geometry {
        unimplemented!()
    }

    fn cell(&self, c: &LayoutCell) -> (Block, Type) {
        let size = &c.1.size;
        let dim = size.len();
        let (mut b, ty) = match c.0.as_ref() {
            Cell::Prim(prim_id, shape) => {
                let (geometry, shape) = match shape {
                    cell::Shape::Zero => (Geometry::point(*prim_id), Shape::Zero),
                    cell::Shape::Succ(s, t) => {
                        let s = self.cell(s);
                        let t = self.cell(t);
                        let g =
                            self.prim_geo(*prim_id, s.0.into_geo(), t.0.into_geo(), size[dim - 1]);
                        let sh = Shape::Succ(Box::new(s.1), Box::new(t.1));
                        (g, sh)
                    }
                };
                let prim = Prim {
                    prim_id: *prim_id,
                    shape,
                    dim: dim as Level,
                };
                let unit = Unit {
                    ty: unimplemented!(),
                    geometry,
                    size: size.clone(),
                };
                let b = Block::Prim(prim.clone(), unit);
                let ty = Type::Prim(prim);
                (b, ty)
            }
            Cell::Id(inner) => {
                let (mut inner, mut ty) = self.cell(inner);
                inner.shift(size[dim - 1]);
                ty.shift();
                (inner, ty)
            }
            Cell::Comp(l, cs, inner_pads) => {
                let (cs, ts): (Vec<_>, Vec<_>) = cs.iter().map(|c| self.cell(c)).unzip();
                unimplemented!()
            }
        };
        b.pad(&c.1.pad);
        (b, ty)
    }
}

pub fn extract_geometry(cell: &LayoutCell) -> Block {
    let builder = Builder::new();
    builder.cell(cell).0
}
