use std::vec;

use donut_core::cell::*;
use donut_core::common::*;
use donut_core::pure_cell;
use donut_core::pure_cell::PureCell;

use donut_util::println;

pub type X = crate::lins::X;

#[derive(Debug, Clone)]
pub enum Shape {
    Zero,
    Succ {
        source: (LayoutCell, X),
        center_doubled: Vec<X>,
        target: (LayoutCell, X),
    },
}

#[derive(Debug, Clone)]
pub struct Cube {
    pub mins: Vec<X>,
    pub maxs: Vec<X>,
}

#[derive(Debug, Clone)]
pub enum RawCell {
    Prim(Prim, Shape, Cube),
    Comp(Axis, Vec2<LayoutCell>),
}

#[derive(Debug, Clone)]
pub struct Layout {
    pub dim: Dim,
    pub cube: Cube,
    pub vars: Vec<X>,
}

#[derive(Debug, Clone)]
pub struct LayoutCell(pub Box<RawCell>, pub Layout);

impl Cube {
    pub fn zero() -> Self {
        Cube {
            mins: vec![],
            maxs: vec![],
        }
    }

    pub fn sliced(&self) -> Self {
        let mut cube = self.clone();
        cube.mins.pop();
        cube.maxs.pop();
        cube
    }

    pub fn shift(&mut self, s: &X, t: &X) {
        self.mins.push(s.clone());
        self.maxs.push(t.clone());
    }

    pub fn squash(&mut self, axis: Axis, s: &X, t: &X) {
        self.mins[axis as usize] = s.clone();
        self.maxs[axis as usize] = t.clone();
    }

    pub fn move_face_to(&mut self, axis: Axis, side: Side, x: &X) {
        match side {
            Side::Source => self.mins[axis as usize] = x.clone(),
            Side::Target => self.maxs[axis as usize] = x.clone(),
        }
    }
}

impl Layout {
    pub fn zero() -> Self {
        Layout {
            dim: Dim::zero(),
            cube: Cube::zero(),
            vars: vec![],
        }
    }

    pub fn shift(&mut self, s: &X, t: &X) {
        self.dim.in_space += 1;
        self.cube.shift(s, t);
    }
}

impl LayoutCell {
    pub fn face_at(&self, a: Axis, side: Side, s: &X, t: &X) -> Self {
        let dim = &self.1.dim;
        match self.0.as_ref() {
            RawCell::Prim(prim, shape, cube) if dim.effective <= a => {
                let mut cube = cube.clone();
                cube.squash(a - dim.effective, s, t);
                let cell = RawCell::Prim(prim.clone(), shape.clone(), cube);
                let mut layout = self.1.clone();
                layout.cube.squash(a, s, t);
                LayoutCell(Box::new(cell), layout)
            }
            RawCell::Prim(_, shape, cube) => {
                let mut cell = if dim.effective == a + 1 {
                    let face = match (side, shape) {
                        (Side::Source, Shape::Succ { source, .. }) => &source.0,
                        (Side::Target, Shape::Succ { target, .. }) => &target.0,
                        _ => unreachable!(),
                    };
                    let mut face = face.clone();
                    face.shift(s, t);
                    face
                } else {
                    match shape {
                        Shape::Zero => panic!("zero-cell has no faces"),
                        Shape::Succ { source, .. } => {
                            let mut face = source.0.face_at(a, side, s, t);
                            face.shift(
                                &self.1.cube.mins[dim.effective as usize - 1],
                                &self.1.cube.maxs[dim.effective as usize - 1],
                            );
                            face
                        }
                    }
                };
                for (min, max) in cube.mins.iter().zip(cube.maxs.iter()) {
                    cell.shift(min, max);
                }
                cell
            }
            RawCell::Comp(axis, cs) => {
                if *axis == a {
                    match side {
                        Side::Source => cs.first().unwrap().face_at(a, side, s, t),
                        Side::Target => cs.last().unwrap().face_at(a, side, s, t),
                    }
                } else {
                    let children = cs
                        .iter()
                        .map(|child| child.face_at(a, side, s, t))
                        .collect::<Vec<_>>();
                    let (cell, dim) = LayoutCell::comp_unchecked(*axis, children);
                    let mut layout = self.1.clone();
                    layout.dim = dim;
                    layout.cube.squash(a, s, t);
                    LayoutCell(cell, layout)
                }
            }
        }
    }

    pub fn face(&self, axis: Axis, side: Side) -> Self {
        let x = match side {
            Side::Source => self.1.cube.mins[axis as usize].clone(),
            Side::Target => self.1.cube.maxs[axis as usize].clone(),
        };
        let face = self.face_at(axis, side, &x, &x);
        assert_eq!(self.1.dim.in_space, face.1.dim.in_space);
        face
    }

    pub fn move_face_to(&mut self, a: Axis, side: Side, x: &X) {
        let dim = &self.1.dim;
        match self.0.as_mut() {
            RawCell::Prim(_, _, cube) if dim.effective <= a => {
                cube.move_face_to(a - dim.effective, side, x);
            }
            RawCell::Prim(_, shape, _) => {
                if dim.effective == a + 1 {
                    // do nothing
                } else {
                    match shape {
                        Shape::Zero => panic!("zero-cell has no faces"),
                        Shape::Succ { source, target, .. } => {
                            source.0.move_face_to(a, side, x);
                            target.0.move_face_to(a, side, x);
                        }
                    }
                }
            }
            RawCell::Comp(axis, cs) => {
                if *axis == a {
                    match side {
                        Side::Source => cs.first_mut().unwrap().move_face_to(a, side, x),
                        Side::Target => cs.last_mut().unwrap().move_face_to(a, side, x),
                    }
                } else {
                    for child in cs.iter_mut() {
                        child.move_face_to(a, side, x);
                    }
                }
            }
        }
        self.1.cube.move_face_to(a, side, x);
    }

    pub fn shift(&mut self, s: &X, t: &X) {
        match self.0.as_mut() {
            RawCell::Prim(_, _, cube) => {
                cube.shift(s, t);
            }
            RawCell::Comp(_, children) => {
                for child in children.iter_mut() {
                    child.shift(s, t);
                }
            }
        }
        self.1.shift(s, t);
    }

    pub fn comp_unchecked(axis: Axis, children: Vec2<Self>) -> (Box<RawCell>, Dim) {
        let n = children.len();
        assert!(n >= 1);

        let last_x = children[n - 1].1.cube.maxs[axis as usize].clone();

        let mut cs: Vec<LayoutCell> = vec![];
        let mut first_id = None;
        let mut dim = children.first().unwrap().dim();
        for mut c in children {
            let d = c.dim();
            assert_eq!(d.in_space, dim.in_space);
            dim.effective = dim.effective.max(d.effective);
            if d.effective <= axis {
                // id
                match cs.last_mut() {
                    Some(l) => {
                        // unites to the last element
                        l.move_face_to(axis, Side::Target, &c.1.cube.maxs[axis as usize]);
                    }
                    None => {
                        if first_id.is_none() {
                            first_id = Some(c);
                        } else {
                            // do nothing
                        }
                    }
                }
            } else {
                match c.0.as_mut() {
                    RawCell::Comp(a, cc) if *a == axis => {
                        cs.append(cc);
                    }
                    _ => {
                        cs.push(c);
                    }
                }
            }
        }

        if let Some(first) = first_id {
            let first_x = first.1.cube.mins[axis as usize].clone();
            match cs.first_mut() {
                Some(f) => {
                    // unites to the first element
                    f.move_face_to(axis, Side::Source, &first_x);
                }
                None => {
                    // id only case
                    let mut first = first;
                    first.move_face_to(axis, Side::Target, &last_x);
                    return (first.0, first.1.dim);
                }
            }
        }

        let cell = match cs.len() {
            0 => unreachable!(),
            1 => cs.into_iter().next().unwrap().0,
            _ => Box::new(RawCell::Comp(axis, cs)),
        };
        (cell, dim)
    }
}

impl Globular for LayoutCell {
    fn dim(&self) -> Dim {
        self.1.dim
    }

    fn s(&self) -> Self {
        let dim = &self.1.dim;
        match self.0.as_ref() {
            RawCell::Prim(prim, shape, cube) if dim.effective < dim.in_space => {
                let cell = RawCell::Prim(prim.clone(), shape.clone(), cube.sliced());
                LayoutCell(
                    Box::new(cell),
                    Layout {
                        dim: dim.sliced(),
                        cube: self.1.cube.sliced(),
                        vars: self.1.vars.clone(),
                    },
                )
            }
            RawCell::Prim(_, shape, _) => match shape {
                Shape::Zero => panic!("zero-cell has no source"),
                Shape::Succ { source, .. } => source.0.clone(),
            },
            RawCell::Comp(axis, children) => {
                if *axis == dim.in_space - 1 {
                    children.first().unwrap().s()
                } else {
                    let children = children.iter().map(|child| child.s()).collect::<Vec<_>>();
                    let mut layout = self.1.clone();
                    let (cell, dim) = LayoutCell::comp_unchecked(*axis, children);
                    layout.dim = dim;
                    layout.cube = layout.cube.sliced();
                    LayoutCell(cell, layout)
                }
            }
        }
    }

    fn t(&self) -> Self {
        let dim = &self.1.dim;
        match self.0.as_ref() {
            RawCell::Prim(prim, shape, cube) if dim.effective < dim.in_space => {
                let cell = RawCell::Prim(prim.clone(), shape.clone(), cube.sliced());
                LayoutCell(
                    Box::new(cell),
                    Layout {
                        dim: dim.sliced(),
                        cube: self.1.cube.sliced(),
                        vars: self.1.vars.clone(),
                    },
                )
            }
            RawCell::Prim(_, shape, _) => match shape {
                Shape::Zero => panic!("zero-cell has no target"),
                Shape::Succ { target, .. } => target.0.clone(),
            },
            RawCell::Comp(axis, children) => {
                if *axis == dim.in_space - 1 {
                    children.last().unwrap().t()
                } else {
                    let children = children.iter().map(|child| child.t()).collect::<Vec<_>>();
                    let mut layout = self.1.clone();
                    let (cell, dim) = LayoutCell::comp_unchecked(*axis, children);
                    layout.dim = dim;
                    layout.cube = layout.cube.sliced();
                    LayoutCell(cell, layout)
                }
            }
        }
    }

    fn to_pure(&self) -> PureCell {
        let dim = &self.1.dim;
        match self.0.as_ref() {
            RawCell::Prim(prim, shape, _) => {
                let pure_shape = match shape {
                    Shape::Zero => pure_cell::Shape::Zero,
                    Shape::Succ { source, target, .. } => pure_cell::Shape::Succ {
                        source: Box::new(source.0.to_pure()),
                        target: Box::new(target.0.to_pure()),
                    },
                };
                PureCell::Prim(prim.clone(), pure_shape, *dim)
            }
            RawCell::Comp(axis, children) => PureCell::Comp(
                *axis,
                children.iter().map(|child| child.to_pure()).collect(),
                *dim,
            ),
        }
    }
}
