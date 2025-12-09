use crate::geometry_3::*;
use donut_core::cell::{Cell, Coord, LayoutCell, Level, PrimId, Shape, Vec1};

struct Builder {}

impl Builder {
    fn new() -> Self {
        Self {}
    }

    fn ty(&self, shape: &Shape) -> (Type, Level) {
        match shape {
            Shape::Zero => (Type::Zero(Cube::point()), 0),
            Shape::Succ(s, t) => {
                let l = s.1.size.len() as Level;
                let s = self.cell(s);
                let t = self.cell(t);
                let half = Q::new(1, 2);
                let fs = Block::lerp(&s.s(), &t.s(), half);
                let ft = Block::lerp(&s.t(), &t.t(), half);

                // shrink s/t into like [fs, ft] and create bridges
                let source = unimplemented!();
                let target = unimplemented!();
                let face = unimplemented!();

                let ty = Type::Succ(l, Box::new(source), Box::new(face), Box::new(target));
                (ty, l)
            }
        }
    }

    fn cell(&self, c: &LayoutCell) -> Block {
        let size = &c.1.size;
        let dim = size.len();
        let mut b = match c.0.as_ref() {
            Cell::Prim(prim_id, shape) => {
                let (ty, edim) = self.ty(shape);
                let prim = Prim {
                    prim_id: *prim_id,
                    edim,
                    ty,
                    size: size.iter().map(|&x| Q::from(x as i32)).collect(),
                };
                Block::Prim(prim)
            }
            Cell::Id(inner) => {
                let mut inner = self.cell(inner);
                inner.shift(size[dim - 1]);
                inner
            }
            Cell::Comp(axis, cs, inner_pads) => {
                let cs = cs.iter().map(|c| self.cell(c)).collect::<Vec<_>>();
                Block::comp(*axis, cs, inner_pads)
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
