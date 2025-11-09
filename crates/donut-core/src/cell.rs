use nonempty::NonEmpty;
use std::rc::Rc;

pub type Level = u8;
pub type Vec1<T> = NonEmpty<T>;
pub type PrimId = u64;

#[derive(Clone)]
pub enum ShapeF<C> {
    Zero,
    Succ(C, C),
}

#[derive(Clone)]
pub enum CellF<C> {
    Prim(PrimId, ShapeF<C>),
    Id(C),
    Comp(Vec1<C>, Level),
}

pub struct Cell(pub CellF<Rc<Cell>>);
pub type Shape = ShapeF<Rc<Cell>>;

// Layouts

pub type Coord = Vec<u32>;
#[derive(Clone)]
pub struct Bounds {
    pub min: Coord,
    pub max: Coord,
}

#[derive(Clone)]
pub struct Layout {
    pub size: Coord,
    pub children: Vec<Bounds>,
}

impl Layout {
    pub fn zero() -> Self {
        Self {
            size: vec![],
            children: vec![],
        }
    }
}

pub struct LayoutCell(pub CellF<Rc<LayoutCell>>, pub Layout);
pub type LayoutShape = ShapeF<Rc<LayoutCell>>;
