use crate::common::*;

#[derive(Debug, Clone)]
struct FaceBlock {
    pub cell: LayoutCell,
    pub width: N,
    pub extend: N,
}

#[derive(Debug, Clone)]
enum Shape {
    Zero,
    Succ {
        source: FaceBlock,
        shrink_point: CoordN,
        target: FaceBlock,
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
