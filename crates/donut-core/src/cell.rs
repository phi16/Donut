use crate::common::*;

pub trait Cellular: Clone + Sized {
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
