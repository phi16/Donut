use crate::cell::*;
use crate::common::*;
use crate::pure_cell::{PureCell, Shape};
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum CellF<T> {
    Zero(Prim),
    Prim(Prim, T, T),
    Id(T),
    Comp(Axis, Vec<T>),
}

#[derive(Debug, Clone)]
pub struct Cell(pub Rc<CellF<Cell>>);

#[derive(Debug, Clone)]
pub struct FreeCell {
    pub cell: Cell,
    pub pure: PureCell,
}

impl Diagram for FreeCell {
    fn zero(prim: Prim) -> Self {
        let cell = Cell(Rc::new(CellF::Zero(prim.clone())));
        let pure = PureCell::Prim(prim, Shape::Zero, Dim::new(0, 0));
        FreeCell { cell, pure }
    }

    fn prim(prim: Prim, source: Self, target: Self) -> Result<Self> {
        let cell = Cell(Rc::new(CellF::Prim(prim.clone(), source.cell, target.cell)));
        let pure = PureCell::prim(prim, source.pure, target.pure)?;
        Ok(FreeCell { cell, pure })
    }

    fn id(face: Self) -> Self {
        let cell = Cell(Rc::new(CellF::Id(face.cell)));
        let pure = PureCell::id(face.pure);
        FreeCell { cell, pure }
    }

    fn comp(axis: Axis, children: Vec2<Self>) -> Result<Self> {
        let (cells, pures) = children
            .into_iter()
            .map(|child| (child.cell, child.pure))
            .unzip();
        let cell = Cell(Rc::new(CellF::Comp(axis, cells)));
        let pure = PureCell::comp(axis, pures)?;
        Ok(FreeCell { cell, pure })
    }
}

impl fmt::Display for FreeCell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.pure)
    }
}
