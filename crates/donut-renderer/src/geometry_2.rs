use donut_core::cell::{Coord, Level, Padding, PrimId, Vec1};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Cube {
    Point(Coord),
    Bridge(Rc<Cube>, u32, Rc<Cube>, u32),
    Comp(Level, Vec1<Rc<Cube>>),
}

impl Cube {
    pub fn dim(&self) -> Level {
        match self {
            Cube::Point(s) => s.len() as Level,
            Cube::Bridge(c0, _, c1, _) => {
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
            Cube::Point(_) => true,
            Cube::Bridge(c0, _, c1, _) => c0.valid() && c1.valid() && c0.dim() == c1.dim(),
            Cube::Comp(_, cs) => {
                let d = cs.first().dim();
                // TODO: check boundaries
                cs.iter().all(|c| c.valid() && c.dim() == d)
            }
        }
    }

    pub fn shift(self, x0: u32, x1: u32) -> Self {
        let face = Rc::new(self);
        Cube::Bridge(face.clone(), x0, face, x1)
    }
}

#[derive(Debug, Clone)]
pub enum Geometry {
    Prim(PrimId, Cube),
    Comp(Level, Vec1<Box<Geometry>>),
}

impl Geometry {
    pub fn point(prim_id: PrimId) -> Self {
        Geometry::Prim(prim_id, Cube::Point(vec![]))
    }
    pub fn shift(&mut self, x0: u32, x1: u32) {
        match self {
            Geometry::Prim(_, cube) => {
                *cube = cube.clone().shift(x0, x1);
            }
            Geometry::Comp(_, children) => {
                for child in children.iter_mut() {
                    child.shift(x0, x1);
                }
            }
        }
    }
    pub fn comp(level: Level, elements: Vec<Geometry>) -> Self {
        unimplemented!()
    }
}

#[derive(Debug, Clone)]
pub enum Shape {
    Zero,
    Succ(Box<Type>, Box<Type>),
}

#[derive(Debug, Clone)]
pub struct Prim {
    pub prim_id: PrimId,
    pub shape: Shape,
    pub dim: Level,
}

impl Prim {
    pub fn shift(&mut self) {
        self.dim += 1;
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Prim(Prim),
    Comp(Level, Vec1<Box<Type>>),
}

impl Type {
    pub fn shift(&mut self) {
        match self {
            Type::Prim(p) => {
                p.shift();
            }
            Type::Comp(_, cs) => {
                for c in cs.iter_mut() {
                    c.shift();
                }
            }
        }
    }

    pub fn dim(&self) -> Level {
        match self {
            Type::Prim(p) => p.dim,
            Type::Comp(_, cs) => {
                let d = cs.first().dim();
                assert!(cs.iter().all(|c| c.dim() == d));
                d
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Unit {
    pub ty: Type,
    pub geometry: Geometry,
    pub size: Coord,
}

impl Unit {
    pub fn shift(&mut self, w: u32) {
        self.geometry.shift(0, w);
        self.size.push(w);
    }
}

#[derive(Debug, Clone)]
pub enum Block {
    Prim(Prim, Unit),
    Comp(Level, Vec1<Box<Block>>, Vec<Unit>),
}

impl Block {
    pub fn shift(&mut self, w: u32) {
        match self {
            Block::Prim(p, u) => {
                p.shift();
                u.geometry.shift(0, w);
            }
            Block::Comp(_, cs, ps) => {
                for c in cs.iter_mut() {
                    c.shift(w);
                }
                for p in ps.iter_mut() {
                    p.shift(w);
                }
            }
        }
    }

    pub fn pad(&mut self, pad: &Padding) {
        unimplemented!()
    }

    pub fn into_geo(self) -> Geometry {
        match self {
            Block::Prim(_, u) => u.geometry,
            Block::Comp(l, cs, ps) => {
                let es = {
                    let mut elements = vec![];
                    let mut ps = ps;
                    // add a dummy unit
                    ps.push(Unit {
                        ty: unimplemented!(),
                        geometry: Geometry::point(0),
                        size: vec![],
                    });
                    for (c, p) in cs.into_iter().zip(ps.into_iter()) {
                        elements.push(c.into_geo());
                        elements.push(p.geometry);
                    }
                    elements.pop(); // remove the dummy unit
                    elements
                };
                Geometry::comp(l, es)
            }
        }
    }
}
