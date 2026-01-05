use crate::common::*;
use crate::free_cell::FreeCell;
use crate::pure_cell::PureCell;

pub trait Globular {
    fn dim(&self) -> Dim;

    // cell.s().s() ~ cell.t().s()
    // cell.s().t() ~ cell.t().t()

    fn s(&self) -> Self;
    fn t(&self) -> Self;
    fn to_pure(&self) -> PureCell;

    fn is_convertible(&self, other: &Self) -> bool {
        self.to_pure() == other.to_pure()
    }
}

pub trait Diagram: Clone {
    fn zero(prim: Prim) -> Self;
    fn prim(prim: Prim, source: Self, target: Self) -> Result<Self>;
    fn id(face: Self) -> Self;
    fn comp(axis: Axis, children: Vec2<Self>) -> Result<Self>;

    fn prim_c(prim: Prim, source: &Self, target: &Self) -> Self {
        Self::prim(prim, source.clone(), target.clone()).unwrap()
    }
    fn id_c(face: &Self) -> Self {
        Self::id(face.clone())
    }
    fn comp_c(axis: Axis, children: Vec2<&Self>) -> Self {
        let children: Vec2<Self> = children.into_iter().map(|child| child.clone()).collect();
        Self::comp(axis, children).unwrap()
    }
}

pub trait DiagramMap {
    type Cell: Globular;

    fn zero(&mut self, prim: Prim) -> Self::Cell;
    fn prim(&mut self, prim: Prim, source: Self::Cell, target: Self::Cell) -> Self::Cell;
    fn id(&mut self, face: Self::Cell) -> Self::Cell;
    fn comp(&mut self, axis: Axis, children: Vec2<Self::Cell>) -> Self::Cell;

    fn from_free(&mut self, cell: FreeCell) -> Self::Cell {
        use crate::free_cell::{Cell, CellF};
        fn go<M: DiagramMap + ?Sized>(f: &mut M, cell: &Cell) -> M::Cell {
            match cell.0.as_ref() {
                CellF::Zero(prim) => f.zero(prim.clone()),
                CellF::Prim(prim, source, target) => {
                    let source = go(f, source);
                    let target = go(f, target);
                    f.prim(prim.clone(), source, target)
                }
                CellF::Id(face) => {
                    let face = go(f, face);
                    f.id(face)
                }
                CellF::Comp(axis, children) => {
                    let children = children
                        .into_iter()
                        .map(|child| go(f, child))
                        .collect::<Vec2<_>>();
                    f.comp(*axis, children)
                }
            }
        }
        go(self, &cell.cell)
    }
}

pub fn compatible<T: Globular>(a: &T, b: &T) -> Result<()> {
    if a.is_convertible(b) {
        Ok(())
    } else {
        return Err(format!(
            "{}\n is not convertible to\n{}",
            a.to_pure(),
            b.to_pure()
        ));
    }
}

pub fn check_prim<T: Globular>(s: &T, t: &T) -> Result<()> {
    let sd = s.dim().in_space;
    let td = t.dim().in_space;
    if sd == 0 && td == 0 {
        Ok(())
    } else if sd == 0 || td == 0 {
        Err("incompatible".to_string())
    } else {
        compatible(&s.s(), &t.s())?;
        compatible(&s.t(), &t.t())?;
        Ok(())
    }
}

pub fn source_face<T: Globular>(cell: &T, axis: Axis) -> T {
    let d = cell.dim().in_space;
    assert!(axis < d);
    let mut cell = cell.s();
    for _ in 0..(d - axis - 1) {
        cell = cell.s();
    }
    cell
}

pub fn target_face<T: Globular>(cell: &T, axis: Axis) -> T {
    let d = cell.dim().in_space;
    assert!(axis < d);
    let mut cell = cell.t();
    for _ in 0..(d - axis - 1) {
        cell = cell.t();
    }
    cell
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    pub fn assoc<T: Diagram>() -> T {
        let a = T::zero(Prim::new(0));
        let x = T::prim_c(Prim::new(1), &a, &a);
        let xx = T::comp_c(0, vec![&x, &x]);
        let m = T::prim_c(Prim::new(2), &xx, &x);
        let xi = T::id_c(&x);
        let mx = T::comp_c(0, vec![&m, &xi]);
        let xm = T::comp_c(0, vec![&xi, &m]);
        let mm_l = T::comp_c(1, vec![&mx, &m]);
        let mm_r = T::comp_c(1, vec![&xm, &m]);
        let assoc = T::prim_c(Prim::new(3), &mm_l, &mm_r);
        assoc
    }

    pub fn pentagon<T: Diagram>() -> T {
        let a = T::zero(Prim::new(0));
        let x = T::prim_c(Prim::new(1), &a, &a);
        let xx = T::comp_c(0, vec![&x, &x]);
        let m = T::prim_c(Prim::new(2), &xx, &x);
        let xi = T::id_c(&x);
        let mx = T::comp_c(0, vec![&m, &xi]);
        let xm = T::comp_c(0, vec![&xi, &m]);
        let mm_l = T::comp_c(1, vec![&mx, &m]);
        let mm_r = T::comp_c(1, vec![&xm, &m]);
        let assoc = T::prim_c(Prim::new(3), &mm_l, &mm_r);
        let xii = T::id_c(&xi);
        let ax = T::comp_c(0, vec![&assoc, &xii]);

        let chl = {
            let mmx = T::comp_c(0, vec![&mm_r, &xi]);
            let xmx = T::comp_c(0, vec![&xi, &m, &xi]);
            let mmx2 = T::comp_c(1, vec![&xmx, &mx]);
            let ch = T::prim_c(Prim::new(4), &mmx, &mmx2);
            let mi = T::id_c(&m);
            T::comp_c(1, vec![&ch, &mi])
        };

        let chr = {
            let xmm = T::comp_c(0, vec![&xi, &mm_l]);
            let xmx = T::comp_c(0, vec![&xi, &m, &xi]);
            let xmm2 = T::comp_c(1, vec![&xmx, &xm]);
            let ch = T::prim_c(Prim::new(5), &xmm2, &xmm);
            let mi = T::id_c(&m);
            T::comp_c(1, vec![&ch, &mi])
        };

        let mi = T::id_c(&m);
        let am = T::comp_c(1, vec![&ax, &mi]);
        let xmx = T::comp_c(0, vec![&xi, &m, &xi]);
        let xmxi = T::id_c(&xmx);
        let ma = T::comp_c(1, vec![&xmxi, &assoc]);

        let xa = T::comp_c(0, vec![&xii, &assoc]);
        let am2 = T::comp_c(1, vec![&xa, &mi]);

        let aaa = T::comp_c(2, vec![&am, &chl, &ma, &chr, &am2]);

        let mxx = T::comp_c(0, vec![&m, &xi, &xi]);
        let mxxi = T::id_c(&mxx);
        let oa = T::comp_c(1, vec![&mxxi, &assoc]);

        let xxm = T::comp_c(0, vec![&xi, &xi, &m]);
        let xxmi = T::id_c(&xxm);
        let ao = T::comp_c(1, vec![&xxmi, &assoc]);

        let chx = {
            let vl = T::comp_c(1, vec![&mxx, &xm]);
            let vx = T::comp_c(0, vec![&m, &m]);
            let vr = T::comp_c(1, vec![&xxm, &mx]);
            let ch0 = T::prim_c(Prim::new(6), &vl, &vx);
            let ch1 = T::prim_c(Prim::new(7), &vx, &vr);
            let cc = T::comp_c(2, vec![&ch0, &ch1]);
            T::comp_c(1, vec![&cc, &mi])
        };

        let ichl = {
            let k1 = T::comp_c(0, vec![&mm_l, &xi]);
            let k2 = T::comp_c(1, vec![&mxx, &mx]);
            let k = T::prim_c(Prim::new(8), &k1, &k2);
            T::comp_c(1, vec![&k, &mi])
        };
        let ichr = {
            let k1 = T::comp_c(0, vec![&xi, &mm_r]);
            let k2 = T::comp_c(1, vec![&xxm, &xm]);
            let k = T::prim_c(Prim::new(9), &k2, &k1);
            T::comp_c(1, vec![&k, &mi])
        };

        let oao = T::comp_c(2, vec![&ichl, &oa, &chx, &ao, &ichr]);

        T::prim_c(Prim::new(10), &aaa, &oao)
    }
}
