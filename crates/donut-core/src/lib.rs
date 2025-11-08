use nonempty::NonEmpty;
use std::rc::Rc;

type Level = u8;
type Vec1<T> = NonEmpty<T>;
type PrimId = u64;

pub enum ShapeF<C> {
    Zero,
    Succ(C, C),
}

pub enum CellF<C> {
    Prim(PrimId, ShapeF<C>),
    Id(C),
    Comp(Vec1<C>, Level),
}

pub struct Cell(pub CellF<Rc<Cell>>);
pub type Shape = ShapeF<Rc<Cell>>;

// Layouts

pub type Coord = Vec<f32>;
pub struct Bounds {
    pub min: Coord,
    pub max: Coord,
}

pub struct Layout {
    pub size: Coord,
    pub children: Vec<Bounds>,
}

pub struct LayoutCell(pub CellF<Rc<LayoutCell>>, pub Layout);
pub type LayoutShape = ShapeF<Rc<LayoutCell>>;

pub struct Prim {
    pub name: String,
    pub level: Level,
    pub color: (u8, u8, u8, u8),
    pub base_size: Coord,
    pub base_shape: LayoutShape,
}
