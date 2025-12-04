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

#[derive(Debug, Clone)]
pub enum Geometry {
    Prim(PrimId, Cube),
    Comp(Level, Vec1<Rc<Geometry>>),
}

impl Geometry {
    pub fn shift(self, w: u32) -> Self {
        match self {
            Geometry::Prim(prim_id, cube) => {
                let cube = cube.shift(w);
                Geometry::Prim(prim_id, cube)
            }
            Geometry::Comp(level, children) => {
                let cs = children
                    .into_iter()
                    .map(|c| Rc::new(c.as_ref().clone().shift(w)))
                    .collect::<Vec<_>>();
                let children = Vec1::from_vec(cs).unwrap();
                Geometry::Comp(level, children)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Box {
    pub prim_id: PrimId,
    pub geometry: Geometry,
    pub size: Coord,
}

impl Box {
    pub fn shift(self, w: u32) -> Self {
        let geometry = self.geometry.shift(w);
        Box {
            prim_id: self.prim_id,
            geometry,
            size: self.size,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Block {
    Unit(Box),
    Comp(Level, Vec1<Box>),
}

impl Block {
    pub fn shift(self, w: u32) -> Self {
        match self {
            Block::Unit(b) => {
                let geometry = b.geometry.shift(w);
                let b = Box {
                    prim_id: b.prim_id,
                    geometry,
                    size: b.size,
                };
                Block::Unit(b)
            }
            Block::Comp(level, children) => {
                let cs = children.into_iter().map(|c| c.shift(w)).collect::<Vec<_>>();
                let children = Vec1::from_vec(cs).unwrap();
                Block::Comp(level, children)
            }
        }
    }
}
