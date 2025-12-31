use std::vec;

use crate::cell::{CellFactory, CellLike};
use crate::common::*;
use crate::draw_cell;
use crate::draw_cell::DrawCell;
use crate::lins::{Cloner, Lins, Solution};
use crate::pure_cell;
use crate::pure_cell::PureCell;

type X = crate::lins::X;

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
    mins: Vec<X>,
    maxs: Vec<X>,
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
pub struct LayoutCell(Box<RawCell>, Layout);

pub struct LayoutCellFactory(Lins);

#[derive(Debug, Clone, Copy)]
enum Side {
    Source,
    Target,
}

impl Cube {
    fn zero() -> Self {
        Cube {
            mins: vec![],
            maxs: vec![],
        }
    }

    fn sliced(&self) -> Self {
        let mut cube = self.clone();
        cube.mins.pop();
        cube.maxs.pop();
        cube
    }

    fn shift(&mut self, s: &X, t: &X) {
        self.mins.push(s.clone());
        self.maxs.push(t.clone());
    }

    fn squash(&mut self, axis: Axis, s: &X, t: &X) {
        self.mins[axis as usize] = s.clone();
        self.maxs[axis as usize] = t.clone();
    }

    fn move_face_to(&mut self, axis: Axis, side: Side, x: &X) {
        match side {
            Side::Source => self.mins[axis as usize] = x.clone(),
            Side::Target => self.maxs[axis as usize] = x.clone(),
        }
    }
}

impl Layout {
    fn zero() -> Self {
        Layout {
            dim: Dim::zero(),
            cube: Cube::zero(),
            vars: vec![],
        }
    }

    fn shift(&mut self, s: &X, t: &X) {
        self.dim.in_space += 1;
        self.cube.shift(s, t);
    }
}

impl LayoutCell {
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

    fn face_at(&self, a: Axis, side: Side, s: &X, t: &X) -> Self {
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
                        Shape::Succ { source, target, .. } => {
                            let mut face = source.0.face_at(a, side, s, t);
                            face.shift(&source.1, &target.1);
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

    fn face(&self, axis: Axis, side: Side) -> Self {
        let x = match side {
            Side::Source => self.1.cube.mins[axis as usize].clone(),
            Side::Target => self.1.cube.maxs[axis as usize].clone(),
        };
        let face = self.face_at(axis, side, &x, &x);
        assert_eq!(self.1.dim.in_space, face.1.dim.in_space);
        face
    }

    fn move_face_to(&mut self, a: Axis, side: Side, x: &X) {
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

    fn shift(&mut self, s: &X, t: &X) {
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

    fn comp_unchecked(axis: Axis, children: Vec2<Self>) -> (Box<RawCell>, Dim) {
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
            if d.effective < axis {
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
                    let zero_cylinder = first.face_at(axis, Side::Source, &first_x, &last_x);
                    return (zero_cylinder.0, dim);
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

impl CellLike for LayoutCell {
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
                    layout.cube.sliced();
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
                    layout.cube.sliced();
                    LayoutCell(cell, layout)
                }
            }
        }
    }

    fn is_convertible(&self, other: &Self) -> bool {
        if self.dim() != other.dim() {
            return false;
        }
        let c0 = self.to_pure();
        let c1 = other.to_pure();
        c0.is_convertible(&c1)
    }
}

impl LayoutCellFactory {
    pub fn new() -> Self {
        LayoutCellFactory(Lins::new())
    }

    fn var(&mut self, name: String) -> X {
        X::var(self.0.fresh_var(name))
    }

    fn eq(&mut self, a: &X, b: &X) {
        self.0.add_constraint(a, b);
    }

    fn eqs(&mut self, va: &Vec<X>, vb: &Vec<X>) {
        assert_eq!(va.len(), vb.len());
        va.iter().zip(vb.iter()).for_each(|(a, b)| {
            self.eq(a, b);
        });
    }

    fn fuse_cube(&mut self, a: &Cube, b: &Cube) {
        self.eqs(&a.mins, &b.mins);
        self.eqs(&a.maxs, &b.maxs);
    }

    fn fuse_layout(&mut self, a: &Layout, b: &Layout) {
        assert_eq!(a.dim, b.dim);
        self.fuse_cube(&a.cube, &b.cube);
    }

    fn fuse(&mut self, a: &LayoutCell, b: &LayoutCell) {
        self.fuse_layout(&a.1, &b.1);
        match (a.0.as_ref(), b.0.as_ref()) {
            (RawCell::Prim(pa, sa, ca), RawCell::Prim(pb, sb, cb)) => {
                assert_eq!(pa, pb);
                match (sa, sb) {
                    (Shape::Zero, Shape::Zero) => {}
                    (
                        Shape::Succ {
                            source: sa,
                            center_doubled: ca,
                            target: ta,
                        },
                        Shape::Succ {
                            source: sb,
                            center_doubled: cb,
                            target: tb,
                        },
                    ) => {
                        self.fuse(&sa.0, &sb.0);
                        self.eqs(&ca, &cb);
                        self.fuse(&ta.0, &tb.0);
                    }
                    _ => unreachable!(),
                }
                self.fuse_cube(ca, cb);
            }
            (RawCell::Comp(aa, csa), RawCell::Comp(ab, csb)) => {
                assert_eq!(aa, ab);
                assert_eq!(csa.len(), csb.len());
                for (ca, cb) in csa.iter().zip(csb.iter()) {
                    self.fuse(ca, cb);
                }
            }
            _ => unreachable!(),
        }
    }

    fn collect_vars(&self, cell: &LayoutCell) -> X {
        let mut x = X::zero();
        fn collect(x: &mut X, cell: &LayoutCell) {
            for v in &cell.1.vars {
                *x = x.add(v);
            }
            match cell.0.as_ref() {
                RawCell::Prim(_, shape, _) => match shape {
                    Shape::Zero => {}
                    Shape::Succ { source, target, .. } => {
                        collect(x, &source.0);
                        collect(x, &target.0);
                    }
                },
                RawCell::Comp(_, children) => {
                    for child in children {
                        collect(x, child);
                    }
                }
            }
        }
        collect(&mut x, cell);
        x
    }

    fn convert(&self, cell: &LayoutCell, sol: &Solution) -> DrawCell {
        fn convert_cube(cube: &Cube, sol: &Solution) -> draw_cell::Cube {
            let mins = cube.mins.iter().map(|x| sol.eval(x)).collect();
            let maxs = cube.maxs.iter().map(|x| sol.eval(x)).collect();
            draw_cell::Cube { mins, maxs }
        }
        fn convert_cell(cell: &LayoutCell, sol: &Solution) -> DrawCell {
            let layout = draw_cell::Layout {
                dim: cell.1.dim,
                cube: convert_cube(&cell.1.cube, sol),
            };
            let cell = match cell.0.as_ref() {
                RawCell::Prim(prim, shape, cube) => {
                    let draw_shape = match shape {
                        Shape::Zero => draw_cell::Shape::Zero,
                        Shape::Succ {
                            source,
                            center_doubled,
                            target,
                        } => draw_cell::Shape::Succ {
                            source: (convert_cell(&source.0, sol), sol.eval(&source.1)),
                            center: center_doubled.iter().map(|x| sol.eval(x) / 2).collect(),
                            target: (convert_cell(&target.0, sol), sol.eval(&target.1)),
                        },
                    };
                    let draw_cube = convert_cube(cube, sol);
                    draw_cell::RawCell::Prim(prim.clone(), draw_shape, draw_cube)
                }
                RawCell::Comp(axis, children) => {
                    let draw_children = children
                        .iter()
                        .map(|child| convert_cell(child, sol))
                        .collect();
                    draw_cell::RawCell::Comp(*axis, draw_children)
                }
            };
            DrawCell(Box::new(cell), layout)
        }
        convert_cell(cell, sol)
    }

    fn solve(&mut self, cell: &LayoutCell) -> DrawCell {
        let d = cell.1.dim.in_space;
        let ws = (0..d)
            .map(|i| self.var(format!("Mw{}", i)))
            .collect::<Vec<_>>();
        for i in 0..d as usize {
            let min = X::zero();
            let w = &ws[i];
            let base = X::one();
            let max = min.add(&base).add(w);
            self.eq(&cell.1.cube.mins[i], &min);
            self.eq(&cell.1.cube.maxs[i], &max);
        }
        let vars = self.collect_vars(cell);
        let mut lins = Lins::new();
        std::mem::swap(&mut self.0, &mut lins);
        let sol = lins.solve(&vars).unwrap();

        self.convert(cell, &sol)
    }
}

impl CellFactory for LayoutCellFactory {
    type Cell = LayoutCell;

    fn clone(&mut self, cell: &LayoutCell) -> LayoutCell {
        let mut cloner = self.0.cloner();

        fn clone_xs(cloner: &mut Cloner, xs: &Vec<X>) -> Vec<X> {
            xs.iter().map(|x| cloner.clone(x)).collect::<Vec<_>>()
        }
        fn clone_cube(cloner: &mut Cloner, cube: &Cube) -> Cube {
            let mins = clone_xs(cloner, &cube.mins);
            let maxs = clone_xs(cloner, &cube.maxs);
            Cube { mins, maxs }
        }
        fn clone_layout(cloner: &mut Cloner, layout: &Layout) -> Layout {
            let cube = clone_cube(cloner, &layout.cube);
            let vars = clone_xs(cloner, &layout.vars);
            Layout {
                dim: layout.dim,
                cube,
                vars,
            }
        }
        fn clone_cell(cloner: &mut Cloner, cell: &LayoutCell) -> LayoutCell {
            let layout = clone_layout(cloner, &cell.1);
            let cell = match cell.0.as_ref() {
                RawCell::Prim(prim, shape, cube) => {
                    let new_shape = match shape {
                        Shape::Zero => Shape::Zero,
                        Shape::Succ {
                            source,
                            center_doubled,
                            target,
                        } => Shape::Succ {
                            source: (clone_cell(cloner, &source.0), cloner.clone(&source.1)),
                            center_doubled: clone_xs(cloner, center_doubled),
                            target: (clone_cell(cloner, &target.0), cloner.clone(&target.1)),
                        },
                    };
                    let new_cube = clone_cube(cloner, cube);
                    RawCell::Prim(prim.clone(), new_shape, new_cube)
                }
                RawCell::Comp(axis, children) => {
                    let new_children = children
                        .iter()
                        .map(|child| clone_cell(cloner, child))
                        .collect::<Vec2<_>>();
                    RawCell::Comp(*axis, new_children)
                }
            };
            LayoutCell(Box::new(cell), layout)
        }
        let c = clone_cell(&mut cloner, cell);
        cloner.drop();
        c
    }

    fn zero(&mut self, prim: Prim) -> Self::Cell {
        let cell = RawCell::Prim(prim, Shape::Zero, Cube::zero());
        LayoutCell(Box::new(cell), Layout::zero())
    }

    fn prim(&mut self, prim: Prim, source: Self::Cell, target: Self::Cell) -> Self::Cell {
        self.fuse_layout(&source.1, &target.1);
        let d = source.dim().in_space;
        let dim = Dim::new(d + 1, d + 1);

        // s |--u+w--> x <----> y <--v+w--| t

        let s = self.var(format!("P[{}]s{}", prim.id, d));
        let u = self.var(format!("P[{}]u{}", prim.id, d));
        let v = self.var(format!("P[{}]v{}", prim.id, d));
        let w = self.var(format!("P[{}]w{}", prim.id, d));
        let x = s.add(&u.add(&w));
        let y = x.add(&X::one());
        let t = y.add(&v.add(&w));

        let mut cube = source.1.cube.clone();
        let center_doubled = cube // TODO: this is not an appropriate center, maybe
            .mins
            .iter()
            .zip(cube.maxs.iter())
            .map(|(s, t)| s.add(t))
            .collect::<Vec<_>>();

        let shape = Shape::Succ {
            source: (source, x),
            center_doubled,
            target: (target, y),
        };

        let cell = RawCell::Prim(prim, shape, Cube::zero());
        cube.shift(&s, &t);
        let vars = vec![s, u, v, w];
        LayoutCell(Box::new(cell), Layout { dim, cube, vars })
    }

    fn id(&mut self, face: Self::Cell) -> Self::Cell {
        let d = face.dim().in_space;
        let s = self.var(format!("Is{}", d));
        let w = self.var(format!("Iw{}", d));
        let t = s.add(&w);
        let mut cell = face;
        cell.shift(&s, &t);
        cell.1.vars.push(s);
        cell.1.vars.push(w);
        cell
    }

    fn comp(&mut self, axis: Axis, children: Vec2<Self::Cell>) -> Option<Self::Cell> {
        let n = children.len();
        for i in 0..n - 1 {
            let t = children[i].face(axis, Side::Target);
            let s = children[i + 1].face(axis, Side::Source);
            if !t.is_convertible(&s) {
                eprintln!(
                    "{:?}\n is not convertible to\n{:?}",
                    t.to_pure(),
                    s.to_pure()
                );
                return None;
            }
            self.fuse(&t, &s);
        }
        let mins = children[0].1.cube.mins.clone();
        let maxs = children[n - 1].1.cube.maxs.clone();
        let (cell, dim) = LayoutCell::comp_unchecked(axis, children);
        let cube = Cube { mins, maxs };
        let layout = Layout {
            dim,
            cube,
            vars: vec![],
        };
        Some(LayoutCell(cell, layout))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assoc_layout() {
        use crate::cell::tests::assoc;
        let mut cb = LayoutCellFactory::new();
        let a = assoc(&mut cb);
        let cell = cb.solve(&a);
        eprintln!("assoc layout:\n{}", cell);
        assert!(false);
    }
}
