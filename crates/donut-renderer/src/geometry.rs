use donut_core::cell::{Coord, Level, Padding, PrimId, Vec1};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Cube {
    Point(Coord),
    Flat(Rc<Cube>, u32),
    Bridge {
        f0: Rc<Cube>,
        x0: u32,
        f1: Rc<Cube>,
        x1: u32,
    },
}

#[derive(Debug, Clone)]
pub enum Type {
    Zero(Cube),
    Succ(Level, Box<Block>, Box<Type>, Box<Block>),
}

#[derive(Debug, Clone)]
pub struct Prim {
    pub prim_id: PrimId,
    pub ty: Type,
    pub size: Coord,
}

#[derive(Debug, Clone)]
pub enum Block {
    Prim(Prim),
    Comp(Level, Vec1<Box<Block>>),
}

impl Cube {
    pub fn shift(&self, x0: u32, x1: u32) -> Self {
        let face = Rc::new(self.clone());
        Cube::Bridge {
            f0: face.clone(),
            x0,
            f1: face,
            x1,
        }
    }
}

impl Type {
    pub fn shift(&mut self, w: u32) {
        match self {
            Type::Zero(cube) => {
                *cube = cube.shift(0, w);
            }
            Type::Succ(_, s, f, t) => {
                s.shift(w);
                f.shift(w);
                t.shift(w);
            }
        }
    }
}

impl Prim {
    pub fn shift(&mut self, w: u32) {
        self.ty.shift(w);
    }
}

impl Block {
    pub fn pad(&mut self, pad: &Padding) {
        unimplemented!()
    }

    pub fn shift(&mut self, w: u32) {
        match self {
            Block::Prim(prim) => {
                prim.shift(w);
            }
            Block::Comp(_, children) => {
                for child in children.iter_mut() {
                    child.shift(w);
                }
            }
        }
    }
}
