use donut_core::cell::{Level, Padding, PrimId, Vec1};
use std::rc::Rc;

pub type Q = num_rational::Rational32;
pub type CoordQ = Vec<Q>;

#[derive(Debug, Clone)]
pub enum Cube {
    Point(CoordQ),
    Bridge {
        f0: Rc<Cube>,
        x0: Q,
        f1: Rc<Cube>,
        x1: Q,
    },
    Comp(Level, Vec1<Rc<Cube>>),
}

#[derive(Debug, Clone)]
pub enum Type {
    Zero(Cube), // ...?
    Succ(Level, Box<Block>, Box<Type>, Box<Block>),
}

#[derive(Debug, Clone)]
pub struct Prim {
    pub prim_id: PrimId,
    pub ty: Type,
    pub edim: Level,
    pub size: CoordQ,
}

#[derive(Debug, Clone)]
pub enum Block {
    Prim(Prim),
    Comp(Level, Vec1<Box<Block>>),
}

impl Cube {
    pub fn shift(&self, x0: Q, x1: Q) -> Self {
        let face = Rc::new(self.clone());
        Cube::Bridge {
            f0: face.clone(),
            x0,
            f1: face,
            x1,
        }
    }

    pub fn pad(&self, pad: &Padding) -> Self {
        unimplemented!()
    }

    pub fn dim(&self) -> Level {
        match self {
            Cube::Point(_) => 0,
            Cube::Bridge { f0, f1, .. } => {
                assert_eq!(f0.dim(), f1.dim());
                f0.dim() + 1
            }
            Cube::Comp(_, cs) => {
                let d = cs.first().dim();
                for c in cs.iter() {
                    assert_eq!(c.dim(), d);
                }
                d
            }
        }
    }

    pub fn point() -> Self {
        Cube::Point(vec![])
    }
}

impl Type {
    pub fn shift(&mut self, w: u32) {
        match self {
            Type::Zero(cube) => {
                *cube = cube.shift(0.into(), (w as i32).into());
            }
            Type::Succ(_, s, f, t) => {
                s.shift(w);
                f.shift(w);
                t.shift(w);
            }
        }
    }

    pub fn pad(&mut self, pad: &Padding) {
        match self {
            Type::Zero(cube) => {
                *cube = cube.pad(pad);
            }
            Type::Succ(l, s, f, t) => {
                let mut pad_inside = pad.clone();
                pad_inside.min[*l as usize] = 0;
                pad_inside.max[*l as usize] = 0;
                if !pad_inside.is_zero() {
                    s.pad(&pad_inside);
                    f.pad(&pad_inside);
                    t.pad(&pad_inside);
                }

                let min_end = pad.min[*l as usize];
                let max_end = pad.max[*l as usize];
                let mut end_pad = Padding {
                    min: vec![0; pad.min.len()],
                    max: vec![0; pad.max.len()],
                };
                if min_end != 0 {
                    end_pad.min[*l as usize] = min_end;
                    s.pad(&end_pad);
                    end_pad.min[*l as usize] = 0;
                }
                if max_end != 0 {
                    end_pad.max[*l as usize] = max_end;
                    t.pad(&end_pad);
                    end_pad.max[*l as usize] = 0;
                }
            }
        }
    }

    pub fn edim(&self) -> Level {
        match self {
            Type::Zero(_) => 0,
            Type::Succ(l, _, f, _) => {
                let d = *l;
                assert_eq!(f.edim() + 1, d);
                d
            }
        }
    }

    pub fn sdim(&self) -> Level {
        match self {
            Type::Zero(cube) => cube.dim(),
            Type::Succ(_, s, f, t) => {
                let d = s.sdim();
                assert_eq!(f.sdim(), d);
                assert_eq!(t.sdim(), d);
                d
            }
        }
    }
}

impl Prim {
    pub fn shift(&mut self, w: u32) {
        self.ty.shift(w);
    }

    pub fn pad(&mut self, pad: &Padding) {
        assert!(self.size.len() == pad.min.len());
        for i in 0..self.size.len() {
            self.size[i] += Q::from((pad.min[i] + pad.max[i]) as i32);
        }
        self.ty.pad(pad);
    }
}

impl Block {
    pub fn shift(&mut self, w: u32) {
        match self {
            Block::Prim(prim) => {
                prim.shift(w);
            }
            Block::Comp(_, cs) => {
                for c in cs.iter_mut() {
                    c.shift(w);
                }
            }
        }
    }

    pub fn pad(&mut self, pad: &Padding) {
        match self {
            Block::Prim(prim) => {
                prim.pad(pad);
            }
            Block::Comp(axis, cs) => {
                let n = cs.len();
                assert!(n >= 2);
                let mut pad_inside = pad.clone();
                pad_inside.min[*axis as usize] = 0;
                pad_inside.max[*axis as usize] = 0;
                if !pad_inside.is_zero() {
                    for c in cs.iter_mut() {
                        c.pad(&pad_inside);
                    }
                }

                let min_end = pad.min[*axis as usize];
                let max_end = pad.max[*axis as usize];
                let mut end_pad = Padding {
                    min: vec![0; pad.min.len()],
                    max: vec![0; pad.max.len()],
                };
                if min_end != 0 {
                    end_pad.min[*axis as usize] = min_end;
                    cs[0].pad(&end_pad);
                    end_pad.min[*axis as usize] = 0;
                }
                if max_end != 0 {
                    end_pad.max[*axis as usize] = max_end;
                    cs[n - 1].pad(&end_pad);
                    end_pad.max[*axis as usize] = 0;
                }
            }
        }
    }

    pub fn edim(&self) -> Level {
        match self {
            Block::Prim(prim) => prim.edim,
            Block::Comp(_, cs) => cs.iter().map(|c| c.edim()).max().unwrap(),
        }
    }

    pub fn sdim(&self) -> Level {
        match self {
            Block::Prim(prim) => {
                let d = prim.size.len() as Level;
                assert_eq!(prim.ty.sdim(), d);
                d
            }
            Block::Comp(_, cs) => {
                let d = cs.first().sdim();
                for c in cs.iter() {
                    assert_eq!(c.sdim(), d);
                }
                d
            }
        }
    }

    pub fn bridge(&self, f0: &Block, f1: &Block, x0: u32, x1: u32) -> Self {
        unimplemented!()
    }

    pub fn comp(axis: Level, cs: Vec<Block>, inner_pads: &Vec<u32>) -> Self {
        unimplemented!()
    }

    pub fn s(&self) -> Self {
        unimplemented!()
    }

    pub fn t(&self) -> Self {
        unimplemented!()
    }

    pub fn lerp(b0: &Self, b1: &Self, a: Q) -> Self {
        unimplemented!()
    }
}
