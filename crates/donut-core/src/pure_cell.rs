use crate::cell::*;
use crate::common::*;

#[derive(Debug, Clone)]
pub enum Shape {
    Zero,
    Succ {
        source: Box<PureCell>,
        target: Box<PureCell>,
    },
}

#[derive(Debug, Clone)]
pub enum PureCell {
    Prim(Prim, Shape, Dim),
    Comp(Axis, Vec2<PureCell>),
}

impl Cellular for PureCell {
    fn dim(&self) -> Dim {
        match self {
            PureCell::Prim(_, _, dim) => *dim,
            PureCell::Comp(_, children) => children[0].dim().shifted(),
        }
    }

    fn zero(prim: Prim) -> Self {
        PureCell::Prim(prim, Shape::Zero, Dim::new(0, 0))
    }

    fn prim(prim: Prim, source: Self, target: Self) -> Self {
        let d = source.dim().in_space;
        assert_eq!(d, target.dim().in_space);
        PureCell::Prim(
            prim,
            Shape::Succ {
                source: Box::new(source),
                target: Box::new(target),
            },
            Dim::new(d, d),
        )
    }

    fn is_convertible(&self, other: &Self) -> bool {
        unimplemented!()
    }
}
