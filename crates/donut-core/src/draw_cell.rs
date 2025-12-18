use crate::common::*;
use crate::layout_cell;
use crate::layout_cell::{LayoutCell, RawCell};
use core::fmt;
use std::cell::RefCell;
use std::rc::Rc;

pub type Cot = i8; // -1, 0, 1

#[derive(Debug, Clone)]
pub enum X {
    X(Rc<RefCell<Rc<RefCell<Q>>>>),
}

trait Fusable {
    fn fuse(left: &Self, right: &Self, action: fn(lx: &X, rx: &X)) -> Option<()>;
}

impl X {
    fn from(q: Q) -> Self {
        X::X(Rc::new(RefCell::new(Rc::new(RefCell::new(q)))))
    }

    pub fn eval(&self) -> Q {
        match self {
            X::X(r) => {
                let r = r.borrow();
                let r = r.borrow();
                *r
            }
        }
    }

    fn shrink(&self, min: u32, max: u32) {
        let v = self.eval();
        if v == Q::from(min as i32) || v == Q::from(max as i32) {
            return;
        }
        let c = Q::from((min + max) as i32) / 2;
        let X::X(r) = self;
        let r = r.borrow_mut();
        let mut r = r.borrow_mut();
        *r = c;
    }

    fn deep_clone(&self) -> Self {
        match self {
            X::X(r) => {
                let r = r.borrow();
                let r = r.borrow();
                X::from(*r)
            }
        }
    }
}

impl Fusable for X {
    fn fuse(left: &Self, right: &Self, action: fn(lx: &X, rx: &X)) -> Option<()> {
        action(left, right);
        Some(())
        /* let X::X(l) = left;
        let X::X(r) = right;
        if Rc::ptr_eq(l, r) {
            return Some(());
        }
        let mut l = l.borrow_mut();
        let mut r = r.borrow_mut();
        if Rc::ptr_eq(&*l, &*r) {
            return Some(());
        }
        let h = {
            let lv = l.borrow();
            let rv = r.borrow();
            let hv = (*lv + *rv) / 2;
            Rc::new(RefCell::new(hv))
        };

        *l = h.clone();
        *r = h;
        Some(()) */
    }
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
    Zero,
    Succ {
        source: Box<DrawCell>,
        target: Box<DrawCell>,
    },
}

#[derive(Debug, Clone)]
pub enum DrawCell {
    Prim(Prim, Shape, Cube, Dim),
    Comp(Axis, Vec2<DrawCell>, Dim),
}

#[derive(Debug, Clone)]
pub enum Slice {
    Unit(DrawCell),
    Pair(Box<Slice>, Box<Slice>),
}

impl Cube {
    fn shift(&mut self, s: Q, t: Q) {
        let mut face = Cube::Point(vec![]);
        std::mem::swap(self, &mut face);
        let s = X::from(s);
        let t = X::from(t);
        let source_face = face.deep_clone();
        let target_face = face;
        *self = Cube::Bridge {
            source: (Box::new(source_face), s),
            target: (Box::new(target_face), t),
        };
    }
    fn unshift(&mut self, side: Side) {
        match self {
            Cube::Point(_) => panic!("cannot unshift a point"),
            Cube::Bridge { source, target } => {
                let mut face = Cube::Point(vec![]);
                let face_ref = match side {
                    Side::Source => source.0.as_mut(),
                    Side::Target => target.0.as_mut(),
                };
                std::mem::swap(face_ref, &mut face);
                *self = face;
            }
        }
    }

    fn shrink(&self, origin: &[u32], size: &[u32]) {
        assert_eq!(origin.len(), size.len());
        match self {
            Cube::Point(coords) => {
                assert_eq!(coords.len(), size.len());
                for (coord, (min, width)) in coords.iter().zip(origin.iter().zip(size.iter())) {
                    coord.shrink(*min, *min + *width);
                }
            }
            Cube::Bridge { source, target } => {
                let min = *origin.last().unwrap();
                let width = size.last().unwrap();
                let max = min + width;
                source.1.shrink(min, max);
                target.1.shrink(min, max);
                let n = origin.len() - 1;
                source.0.shrink(&origin[..n], &size[..n]);
                target.0.shrink(&origin[..n], &size[..n]);
            }
        }
    }

    fn deep_clone(&self) -> Self {
        match self {
            Cube::Point(coords) => Cube::Point(coords.iter().map(|x| x.deep_clone()).collect()),
            Cube::Bridge { source, target } => Cube::Bridge {
                source: (Box::new(source.0.deep_clone()), source.1.deep_clone()),
                target: (Box::new(target.0.deep_clone()), target.1.deep_clone()),
            },
        }
    }
}

impl Fusable for Cube {
    fn fuse(left: &Self, right: &Self, action: fn(lx: &X, rx: &X)) -> Option<()> {
        match (left, right) {
            (Cube::Point(lcoords), Cube::Point(rcoords)) => {
                if lcoords.len() != rcoords.len() {
                    return None;
                }
                for (lc, rc) in lcoords.iter().zip(rcoords.iter()) {
                    X::fuse(lc, rc, action)?;
                }
                Some(())
            }
            (
                Cube::Bridge {
                    source: (ls, lsx),
                    target: (lt, ltx),
                },
                Cube::Bridge {
                    source: (rs, rsx),
                    target: (rt, rtx),
                },
            ) => {
                Cube::fuse(ls, rs, action)?;
                Cube::fuse(lt, rt, action)?;
                X::fuse(lsx, rsx, action)?;
                X::fuse(ltx, rtx, action)
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Side {
    Source,
    Target,
}

impl Shape {
    fn shift(&mut self, s: Q, t: Q) {
        match self {
            Shape::Zero => {}
            Shape::Succ { source, target } => {
                source.shift(s, t);
                target.shift(s, t);
            }
        }
    }
    fn unshift(&mut self, side: Side) {
        match self {
            Shape::Zero => {}
            Shape::Succ { source, target } => {
                source.unshift(side);
                target.unshift(side);
            }
        }
    }
}

impl Fusable for Shape {
    fn fuse(left: &Self, right: &Self, action: fn(lx: &X, rx: &X)) -> Option<()> {
        match (left, right) {
            (Shape::Zero, Shape::Zero) => Some(()),
            (
                Shape::Succ {
                    source: ls,
                    target: lt,
                },
                Shape::Succ {
                    source: rs,
                    target: rt,
                },
            ) => {
                DrawCell::fuse(ls, rs, action)?;
                DrawCell::fuse(lt, rt, action)
            }
            _ => None,
        }
    }
}

impl DrawCell {
    fn dim(&self) -> Dim {
        match self {
            DrawCell::Prim(_, _, _, dim) => dim.clone(),
            DrawCell::Comp(_, _, dim) => dim.clone(),
        }
    }

    fn shift(&mut self, s: Q, t: Q) {
        match self {
            DrawCell::Prim(_, shape, cube, dim) => {
                shape.shift(s, t);
                cube.shift(s, t);
                *dim = dim.shifted();
            }
            DrawCell::Comp(_, children, dim) => {
                for child in children.iter_mut() {
                    child.shift(s, t);
                }
                *dim = dim.shifted();
            }
        }
    }
    fn unshift(&mut self, side: Side) {
        match self {
            DrawCell::Prim(_, shape, cube, dim) => {
                shape.unshift(side);
                cube.unshift(side);
                *dim = dim.sliced();
            }
            DrawCell::Comp(_, children, dim) => {
                for child in children.iter_mut() {
                    child.unshift(side);
                }
                *dim = dim.sliced();
            }
        }
    }

    fn s(&self) -> Self {
        match self {
            DrawCell::Prim(prim, shape, cube, dim) if dim.effective < dim.in_space => {
                let mut shape = shape.clone();
                shape.unshift(Side::Source);
                let mut cube = cube.clone();
                cube.unshift(Side::Source);
                DrawCell::Prim(prim.clone(), shape, cube, dim.sliced())
            }
            DrawCell::Prim(_, shape, _, _) => match shape {
                Shape::Zero => panic!("zero-cell has no source"),
                Shape::Succ { source, .. } => source.s(),
            },
            DrawCell::Comp(axis, children, dim) => {
                if *axis == dim.in_space - 1 {
                    children.first().unwrap().s()
                } else {
                    let cs = children.iter().map(|c| c.s()).collect::<Vec2<_>>();
                    DrawCell::comp(*axis, cs)
                }
            }
        }
    }
    fn t(&self) -> Self {
        match self {
            DrawCell::Prim(prim, shape, cube, dim) if dim.effective < dim.in_space => {
                let mut shape = shape.clone();
                shape.unshift(Side::Target);
                let mut cube = cube.clone();
                cube.unshift(Side::Target);
                DrawCell::Prim(prim.clone(), shape, cube, dim.sliced())
            }
            DrawCell::Prim(_, shape, _, _) => match shape {
                Shape::Zero => panic!("zero-cell has no target"),
                Shape::Succ { target, .. } => target.t(),
            },
            DrawCell::Comp(axis, children, dim) => {
                if *axis == dim.in_space - 1 {
                    children.last().unwrap().t()
                } else {
                    let cs = children.iter().map(|c| c.t()).collect::<Vec2<_>>();
                    DrawCell::comp(*axis, cs)
                }
            }
        }
    }

    fn slice_s(&self, axis: Axis) -> Slice {
        let dim = self.dim();
        assert!(axis < dim.in_space);
        if axis == dim.in_space - 1 {
            Slice::Unit(self.s())
        } else {
            let s = self.s().slice_s(axis);
            let t = self.t().slice_s(axis);
            Slice::Pair(Box::new(s), Box::new(t))
        }
    }
    fn slice_t(&self, axis: Axis) -> Slice {
        let dim = self.dim();
        assert!(axis < dim.in_space);
        if axis == dim.in_space - 1 {
            Slice::Unit(self.t())
        } else {
            let s = self.s().slice_t(axis);
            let t = self.t().slice_t(axis);
            Slice::Pair(Box::new(s), Box::new(t))
        }
    }

    fn comp(axis: Axis, children: Vec2<DrawCell>) -> Self {
        let n = children.len();
        assert!(n >= 1);

        let mut first_ids = vec![];
        let mut cs = vec![];
        let mut dim = children.first().unwrap().dim();
        for c in children {
            let d = c.dim();
            assert_eq!(d.in_space, dim.in_space);
            dim.effective = dim.effective.max(d.effective);
            match c {
                DrawCell::Comp(a, mut cc, _) if a == axis => {
                    cs.append(&mut cc);
                }
                DrawCell::Prim(_, _, _, dim) if dim.effective < axis => match cs.last_mut() {
                    None => first_ids.push(c),
                    Some(last) => last.slice_t(axis).weld(c.slice_t(axis)),
                },
                _ => {
                    cs.push(c);
                }
            }
        }

        for i in 0..cs.len() - 1 {
            let t = cs[i].slice_t(axis);
            let s = cs[i + 1].slice_s(axis);
            Slice::fuse(&t, &s, |lx, rx| {
                let c = (lx.eval() + rx.eval()) / 2;
                let X::X(l) = lx;
                let X::X(r) = rx;
                let l = l.borrow_mut();
                let r = r.borrow_mut();
                let mut l = l.borrow_mut();
                let mut r = r.borrow_mut();
                *l = c;
                *r = c;
            })
            .unwrap();
        }

        if !first_ids.is_empty() {
            match cs.first_mut() {
                None => {
                    let mut ids = first_ids.into_iter();
                    let first = ids.next().unwrap();
                    match ids.last() {
                        None => {}
                        Some(last) => {
                            first.slice_t(axis).weld(last.slice_t(axis));
                        }
                    }
                    cs.push(first.clone());
                }
                Some(c) => {
                    let first = first_ids.first().unwrap();
                    c.slice_s(axis).weld(first.slice_s(axis));
                }
            }
        }

        match cs.len() {
            0 => unreachable!(),
            1 => cs.into_iter().next().unwrap(),
            _ => DrawCell::Comp(axis, cs, dim),
        }
    }

    fn shrink(&self, origin: &CoordN, size: &CoordN) {
        match self {
            DrawCell::Prim(_, shape, cube, _) => {
                match shape {
                    Shape::Zero => {}
                    Shape::Succ { source, target } => {
                        source.shrink(origin, size);
                        target.shrink(origin, size);
                    }
                }
                cube.shrink(origin, size);
            }
            DrawCell::Comp(_, children, _) => {
                for child in children.iter() {
                    child.shrink(origin, size);
                }
            }
        }
    }

    pub fn from_layout_cell(cell: &LayoutCell) -> Self {
        let dim = cell.1.dim.clone();
        let size = &cell.1.size;
        match cell.0.as_ref() {
            RawCell::Prim(prim, shape) => match shape {
                layout_cell::Shape::Zero => {
                    assert_eq!(dim, Dim::zero());
                    let shape = Shape::Zero;
                    let cube = Cube::Point(vec![]);
                    DrawCell::Prim(prim.clone(), shape, cube, dim)
                }
                layout_cell::Shape::Succ {
                    source_limit,
                    source,
                    source_coord,
                    coord,
                    target_coord,
                    target,
                    target_limit,
                } => {
                    assert_eq!(dim.effective, dim.in_space);
                    let source_limit = Q::from(*source_limit as i32);
                    let target_limit = Q::from(*target_limit as i32);
                    let source_coord = Q::from(*source_coord as i32);
                    let target_coord = Q::from(*target_coord as i32);
                    let center = (source_coord + target_coord) / 2;
                    let mut source = DrawCell::from_layout_cell(source);
                    let mut target = DrawCell::from_layout_cell(target);
                    source.shift(source_limit, center);
                    target.shift(center, target_limit);

                    let mut face_origin = cell.1.origin.clone();
                    face_origin.pop();
                    let mut face_size = size.clone();
                    face_size.pop();
                    source.t().shrink(&face_origin, &face_size);
                    target.s().shrink(&face_origin, &face_size);

                    let shape = Shape::Succ {
                        source: Box::new(source),
                        target: Box::new(target),
                    };
                    let coord = coord.iter().map(|x| X::from(*x)).collect::<Vec<_>>();
                    let cube = Cube::Point(coord);
                    DrawCell::Prim(prim.clone(), shape, cube, dim)
                }
            },
            RawCell::Id(inner, s, t) => {
                let mut cell = DrawCell::from_layout_cell(inner);
                cell.shift(Q::from(*s as i32), Q::from(*t as i32));
                cell
            }
            RawCell::Comp(axis, children) => {
                let cs = children
                    .iter()
                    .map(|c| DrawCell::from_layout_cell(c))
                    .collect::<Vec2<_>>();
                DrawCell::comp(*axis, cs)
            }
        }
    }
}

impl Fusable for DrawCell {
    fn fuse(left: &Self, right: &Self, action: fn(lx: &X, rx: &X)) -> Option<()> {
        match (left, right) {
            (DrawCell::Prim(_, ls, lc, _), DrawCell::Prim(_, rs, rc, _)) => {
                Shape::fuse(ls, rs, action)?;
                Cube::fuse(lc, rc, action)
            }
            (DrawCell::Comp(_, lchildren, _), DrawCell::Comp(_, rchildren, _)) => {
                if lchildren.len() != rchildren.len() {
                    return None;
                }
                for (lc, rc) in lchildren.iter().zip(rchildren.iter()) {
                    DrawCell::fuse(lc, rc, action)?;
                }
                Some(())
            }
            _ => None,
        }
    }
}

impl Slice {
    fn weld(&self, other: Self) {
        Slice::fuse(self, &other, |lx, rx| {
            // unimplemented!()
            // do nothing
        })
        .unwrap()
    }
}

impl Fusable for Slice {
    fn fuse(left: &Self, right: &Self, action: fn(lx: &X, rx: &X)) -> Option<()> {
        match (left, right) {
            (Slice::Unit(lc), Slice::Unit(rc)) => DrawCell::fuse(lc, rc, action),
            (Slice::Pair(ls, lt), Slice::Pair(rs, rt)) => {
                Slice::fuse(ls, rs, action)?;
                Slice::fuse(lt, rt, action)
            }
            _ => None,
        }
    }
}

impl fmt::Display for X {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let r = self.eval();
        write!(f, "{}", r)
    }
}

impl fmt::Display for Cube {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Cube::Point(coords) => {
                write!(f, "[")?;
                for (i, coord) in coords.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", coord)?;
                }
                write!(f, "]")
            }
            Cube::Bridge { source, target } => {
                write!(f, "[{}:{} - {}:{}]", source.1, source.0, target.0, target.1)
            }
        }
    }
}

impl fmt::Display for DrawCell {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn go(cell: &DrawCell, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
            let pad = " ".repeat(indent);
            write!(f, "{}", pad)?;
            match cell {
                DrawCell::Prim(prim, shape, cube, dim) => {
                    write!(
                        f,
                        "({}/{}) Prim {:?} {}",
                        dim.effective, dim.in_space, prim.id, cube
                    )?;
                    match shape {
                        Shape::Zero => write!(f, " {{}}")?,
                        Shape::Succ { source, target } => {
                            writeln!(f, " {{",)?;
                            go(source, f, indent + 2)?;
                            writeln!(f, ",")?;
                            go(target, f, indent + 2)?;
                            writeln!(f, "")?;
                            write!(f, "{}}}", pad)?;
                        }
                    }
                }
                DrawCell::Comp(axis, children, dim) => {
                    writeln!(
                        f,
                        "({}/{}) Comp<{:?}> {{",
                        dim.effective, dim.in_space, axis
                    )?;
                    for child in children.iter() {
                        go(child, f, indent + 2)?;
                        writeln!(f, ",")?;
                    }
                    write!(f, "{}}}", pad)?;
                }
            }
            Ok(())
        }
        go(self, f, 0)?;
        writeln!(f, "")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn padded_cell_assoc() {
        let assoc = crate::cell::tests::assoc::<crate::padded_cell::PaddedCell>();
        let assoc = assoc.resolve_pad();
        let assoc = DrawCell::from_layout_cell(&assoc);

        eprintln!("{}", assoc);
        assert!(false);
    }
}
