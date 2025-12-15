use crate::common::*;

pub trait Cellular: Clone {
    fn dim(&self) -> Dim;
    fn zero(prim: Prim) -> Self;
    fn prim(prim: Prim, source: Self, target: Self) -> Self;
    fn id(face: Self) -> Self;
    fn comp(axis: Axis, children: Vec2<Self>) -> Option<Self>;

    // cell.s().s() ~ cell.t().s()
    // cell.s().t() ~ cell.t().t()

    fn s(&self) -> Self;
    fn t(&self) -> Self;
    fn is_convertible(&self, other: &Self) -> bool;
}

pub fn source_face<T: Cellular>(cell: &T, axis: Level) -> T {
    let d = cell.dim().in_space;
    assert!(axis < d);
    let mut cell = cell.s();
    for _ in 0..(d - axis - 1) {
        cell = cell.s();
    }
    cell
}

pub fn target_face<T: Cellular>(cell: &T, axis: Level) -> T {
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

    pub fn assoc<T: Cellular>() -> T {
        let a = T::zero(Prim::new(0));
        let x = T::prim(Prim::new(1), a.clone(), a.clone());
        let xx = T::comp(0, vec![x.clone(), x.clone()]).unwrap();
        let m = T::prim(Prim::new(2), xx.clone(), x.clone());
        let xi = T::id(x.clone());
        let mx = T::comp(0, vec![m.clone(), xi.clone()]).unwrap();
        let xm = T::comp(0, vec![xi.clone(), m.clone()]).unwrap();
        let mm_l = T::comp(1, vec![mx, m.clone()]).unwrap();
        let mm_r = T::comp(1, vec![xm, m.clone()]).unwrap();
        let assoc = T::prim(Prim::new(3), mm_l, mm_r);
        assoc
    }
}
