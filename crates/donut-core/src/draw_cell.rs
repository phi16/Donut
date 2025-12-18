use crate::common::*;
use crate::layout_cell;
use crate::layout_cell::{LayoutCell, RawCell};
use std::cell::RefCell;
use std::rc::Rc;

pub type Q = num_rational::Rational32;
pub type CoordQ = Vec<Q>;
pub type Cot = i8; // -1, 0, 1

type X = Rc<RefCell<Q>>;

fn clone_x(x: &X) -> X {
    Rc::new(RefCell::new(*x.borrow()))
}

#[derive(Debug, Clone)]
pub enum Cube {
    Point(Vec<X>),
    Bridge {
        source: (Box<Cube>, X),
        target: (Box<Cube>, X),
    },
}

#[derive(Debug, Clone)]
pub enum Shape {
    Zero(Cube),
    Succ {
        source: Box<DrawCell>,
        target: Box<DrawCell>,
    },
}

#[derive(Debug, Clone)]
pub enum DrawCell {
    Prim(Prim, Shape, Dim),
    Comp(Axis, Vec2<DrawCell>, Dim),
}

impl Cube {
    fn shift(&mut self, s: Q, t: Q) {
        let mut face = Cube::Point(vec![]);
        std::mem::swap(self, &mut face);
        let s = Rc::new(RefCell::new(s));
        let t = Rc::new(RefCell::new(t));
        let source_face = face.deep_clone();
        let target_face = face;
        *self = Cube::Bridge {
            source: (Box::new(source_face), s),
            target: (Box::new(target_face), t),
        };
    }

    fn deep_clone(&self) -> Self {
        match self {
            Cube::Point(coords) => Cube::Point(coords.iter().map(clone_x).collect()),
            Cube::Bridge { source, target } => Cube::Bridge {
                source: (Box::new(source.0.deep_clone()), clone_x(&source.1)),
                target: (Box::new(target.0.deep_clone()), clone_x(&target.1)),
            },
        }
    }
}

enum ShapeSlice {
    IdSection(Shape),
    PrimSection(DrawCell),
}

impl Shape {
    fn shift(&mut self, width: Q) {
        match self {
            Shape::Zero(cube) => {
                cube.shift(Q::from(0), width);
            }
            Shape::Succ { source, target } => {
                source.shift(width);
                target.shift(width);
            }
        }
    }

    fn s(&self, a: Axis) -> ShapeSlice {
        unimplemented!()
    }
    fn t(&self, a: Axis) -> ShapeSlice {
        unimplemented!()
    }
}

impl DrawCell {
    fn shift(&mut self, width: Q) {
        match self {
            DrawCell::Prim(_, shape, dim) => {
                shape.shift(width);
                *dim = dim.shifted();
            }
            DrawCell::Comp(_, children, dim) => {
                for child in children.iter_mut() {
                    child.shift(width);
                }
                *dim = dim.shifted();
            }
        }
    }

    fn shifted(mut self, width: Q) -> Self {
        self.shift(width);
        self
    }

    fn comp(axis: Axis, children: Vec2<DrawCell>) -> Option<Self> {
        unimplemented!()
    }

    fn s(&self, a: Axis) -> Self {
        match self {
            DrawCell::Prim(prim, shape, dim) => match shape.s(a) {
                ShapeSlice::PrimSection(cell) => cell,
                ShapeSlice::IdSection(shape) => DrawCell::Prim(prim.clone(), shape, dim.clone()),
            },
            DrawCell::Comp(axis, children, _) => {
                if axis == &a {
                    children.first().unwrap().s(a)
                } else {
                    let cs = children.iter().map(|c| c.s(a)).collect::<Vec2<_>>();
                    DrawCell::comp(*axis, cs).unwrap()
                }
            }
        }
    }
    fn t(&self, a: Axis) -> Self {
        match self {
            DrawCell::Prim(prim, shape, dim) => match shape.t(a) {
                ShapeSlice::PrimSection(cell) => cell,
                ShapeSlice::IdSection(shape) => DrawCell::Prim(prim.clone(), shape, dim.clone()),
            },
            DrawCell::Comp(axis, children, _) => {
                if axis == &a {
                    children.last().unwrap().t(a)
                } else {
                    let cs = children.iter().map(|c| c.t(a)).collect::<Vec2<_>>();
                    DrawCell::comp(*axis, cs).unwrap()
                }
            }
        }
    }

    fn slice_s(&self) -> Self {
        match self {
            DrawCell::Prim(prim, shape, dim) if dim.effective < dim.in_space => {
                DrawCell::Prim(prim.clone(), ?, dim.sliced())
            }
            DrawCell::Prim(_, shape, _) => match shape {
                Shape::Zero(_) => panic!("zero-cell has no source"),
                Shape::Succ { source, .. } => source.slice_s(),
            },
            DrawCell::Comp(axis, children, dim) => {
                if *axis == dim.in_space - 1 {
                    children.first().unwrap().slice_s()
                } else {
                    let cs = children.iter().map(|c| c.slice_s()).collect::<Vec2<_>>();
                    DrawCell::comp(*axis, cs).unwrap()
                }
            }
        }
    }
    fn slice_t(&self) -> Self {
        unimplemented!()
    }

    fn fuse(left: &Self, right: &Self) -> Option<()> {
        unimplemented!()
    }

    fn shrink(&self, size: &CoordN) {
        unimplemented!()
    }

    pub fn from_layout_cell(cell: &LayoutCell) -> Self {
        let dim = cell.1.dim.clone();
        let size = &cell.1.size;
        match cell.0.as_ref() {
            RawCell::Prim(prim, shape) => match shape {
                layout_cell::Shape::Zero => {
                    assert_eq!(dim, Dim::zero());
                    DrawCell::Prim(prim.clone(), Shape::Zero(Cube::Point(vec![])), dim)
                }
                layout_cell::Shape::Succ {
                    source_extend,
                    source,
                    width,
                    target,
                    target_extend,
                } => {
                    let source_extend = Q::from(*source_extend as i32);
                    let width = Q::from(*width as i32);
                    let target_extend = Q::from(*target_extend as i32);

                    let source =
                        DrawCell::from_layout_cell(source).shifted(source_extend + width / 2);
                    let target =
                        DrawCell::from_layout_cell(target).shifted(target_extend + width / 2);

                    source.slice_t().shrink(size);
                    target.slice_s().shrink(size);

                    DrawCell::Prim(
                        prim.clone(),
                        Shape::Succ {
                            source: Box::new(source),
                            target: Box::new(target),
                        },
                        dim,
                    )
                }
            },
            RawCell::Id(inner, width) => {
                let cell = DrawCell::from_layout_cell(inner);
                cell.shifted(Q::from(*width as i32))
            }
            RawCell::Comp(axis, children) => {
                let cs = children
                    .iter()
                    .map(|c| DrawCell::from_layout_cell(c))
                    .collect::<Vec2<_>>();
                DrawCell::comp(*axis, cs).unwrap()
            }
        }
    }
}
