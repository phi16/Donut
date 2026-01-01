use crate::common::*;

pub trait CellLike {
    fn dim(&self) -> Dim;

    // cell.s().s() ~ cell.t().s()
    // cell.s().t() ~ cell.t().t()

    fn s(&self) -> Self;
    fn t(&self) -> Self;
    fn is_convertible(&self, other: &Self) -> bool;
}

pub fn check_prim<T: CellLike>(s: &T, t: &T) -> Result<()> {
    if s.dim().in_space == 0 {
        Ok(())
    } else if s.s().is_convertible(&t.s()) && s.t().is_convertible(&t.t()) {
        Ok(())
    } else {
        Err("incompatible".to_string())
    }
}

pub trait CellFactory {
    type Cell: CellLike;

    fn clone(&mut self, cell: &Self::Cell) -> Self::Cell;
    fn zero(&mut self, prim: Prim) -> Self::Cell;
    fn prim(&mut self, prim: Prim, source: Self::Cell, target: Self::Cell) -> Result<Self::Cell>;
    fn id(&mut self, face: Self::Cell) -> Self::Cell;
    fn comp(&mut self, axis: Axis, children: Vec2<Self::Cell>) -> Result<Self::Cell>;

    fn prim_c(&mut self, prim: Prim, source: &Self::Cell, target: &Self::Cell) -> Self::Cell {
        let source = self.clone(source);
        let target = self.clone(target);
        self.prim(prim, source, target).unwrap()
    }

    fn id_c(&mut self, face: &Self::Cell) -> Self::Cell {
        let face = self.clone(face);
        self.id(face)
    }

    fn comp_c(&mut self, axis: Axis, children: Vec2<&Self::Cell>) -> Self::Cell {
        let children: Vec2<Self::Cell> = children.iter().map(|child| self.clone(child)).collect();
        self.comp(axis, children).unwrap()
    }
}

pub fn source_face<T: CellLike>(cell: &T, axis: Axis) -> T {
    let d = cell.dim().in_space;
    assert!(axis < d);
    let mut cell = cell.s();
    for _ in 0..(d - axis - 1) {
        cell = cell.s();
    }
    cell
}

pub fn target_face<T: CellLike>(cell: &T, axis: Axis) -> T {
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

    pub fn assoc<T: CellFactory>(f: &mut T) -> T::Cell {
        let a = f.zero(Prim::new(0));
        let x = f.prim_c(Prim::new(1), &a, &a);
        let xx = f.comp_c(0, vec![&x, &x]);
        let m = f.prim_c(Prim::new(2), &xx, &x);
        let xi = f.id_c(&x);
        let mx = f.comp_c(0, vec![&m, &xi]);
        let xm = f.comp_c(0, vec![&xi, &m]);
        let mm_l = f.comp_c(1, vec![&mx, &m]);
        let mm_r = f.comp_c(1, vec![&xm, &m]);
        let assoc = f.prim_c(Prim::new(3), &mm_l, &mm_r);
        assoc
    }

    pub fn assoc2<T: CellFactory>(f: &mut T) -> T::Cell {
        let a = f.zero(Prim::new(0));
        let x = f.prim_c(Prim::new(1), &a, &a);
        let xx = f.comp_c(0, vec![&x, &x]);
        let m = f.prim_c(Prim::new(2), &xx, &x);
        let xi = f.id_c(&x);
        let mx = f.comp_c(0, vec![&m, &xi]);
        let xm = f.comp_c(0, vec![&xi, &m]);
        let mm_l = f.comp_c(1, vec![&mx, &m]);
        let mm_r = f.comp_c(1, vec![&xm, &m]);
        let assoc = f.prim_c(Prim::new(3), &mm_l, &mm_r);
        let xii = f.id_c(&xi);
        let ax = f.comp_c(0, vec![&assoc, &xii]);

        let interchange = {
            let mmx = f.comp_c(0, vec![&mm_r, &xi]);
            let xmx = f.comp_c(0, vec![&xi, &m, &xi]);
            let mmx2 = f.comp_c(1, vec![&xmx, &mx]);
            let ch = f.prim_c(Prim::new(4), &mmx, &mmx2);
            let mi = f.id_c(&m);
            f.comp_c(1, vec![&ch, &mi])
        };

        let mi = f.id_c(&m);
        let am = f.comp_c(1, vec![&ax, &mi]);
        let xmx = f.comp_c(0, vec![&xi, &m, &xi]);
        let xmxi = f.id_c(&xmx);
        let ma = f.comp_c(1, vec![&xmxi, &assoc]);
        f.comp_c(2, vec![&am, &interchange, &ma])
    }
}
