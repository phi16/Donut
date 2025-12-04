use donut_core::cell::{Coord, Level, PrimId, Vec1};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Cube {
    Unit(Coord),
    Bridge(Rc<Cube>, Rc<Cube>, i32, u32),
    Comp(Level, Vec1<Rc<Cube>>),
}

impl Cube {
    pub fn dim(&self) -> Level {
        match self {
            Cube::Unit(s) => s.len() as Level,
            Cube::Bridge(c0, c1, _, _) => {
                assert_eq!(c0.dim(), c1.dim());
                c0.dim() + 1
            }
            Cube::Comp(_, cs) => {
                let d = cs.first().dim();
                assert!(cs.iter().all(|c| c.dim() == d));
                d
            }
        }
    }

    pub fn valid(&self) -> bool {
        match self {
            Cube::Unit(_) => true,
            Cube::Bridge(c0, c1, _, _) => c0.valid() && c1.valid() && c0.dim() == c1.dim(),
            Cube::Comp(_, cs) => {
                let d = cs.first().dim();
                // TODO: check boundaries
                cs.iter().all(|c| c.valid() && c.dim() == d)
            }
        }
    }

    pub fn shift(self, w: u32) -> Self {
        let face = Rc::new(self);
        Cube::Bridge(face.clone(), face, 0, w)
    }
}
