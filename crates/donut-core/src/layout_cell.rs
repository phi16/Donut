use crate::common::*;

#[derive(Debug, Clone)]
enum Shape {
    Zero,
    Succ {
        source_extend: N,
        source: LayoutCell,
        width: N,
        target: LayoutCell,
        target_extend: N,
    },
}

#[derive(Debug, Clone)]
enum RawCell {
    Prim(Prim, Shape),
    Id(LayoutCell, N),
    Comp(Axis, Vec2<LayoutCell>, Vec1<N>),
}

#[derive(Debug, Clone)]
struct Layout {
    dim: Dim,
    size: CoordN,
}

#[derive(Debug, Clone)]
pub struct LayoutCell(Box<RawCell>, Layout);
