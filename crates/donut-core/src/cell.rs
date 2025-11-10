use nonempty::NonEmpty;
use std::rc::Rc;

pub type Level = u8;
pub type Vec1<T> = NonEmpty<T>;
pub type PrimId = u64;

#[derive(Debug, Clone)]
pub enum ShapeF<C> {
    Zero,
    Succ(C, C),
}

#[derive(Debug, Clone)]
pub enum CellF<C> {
    Prim(PrimId, ShapeF<C>),
    Id(C),
    Comp(Vec1<C>, Level),
}

pub struct Cell(pub CellF<Rc<Cell>>);

// Layouts

pub type Coord = Vec<u32>;

#[derive(Debug, Clone)]
pub struct Layout {
    pub size: Coord,
    pub min_pad: Coord,
    pub max_pad: Coord,
    pub bounds: Coord, // min_pad + size + max_pad
}

impl Layout {
    pub fn zero() -> Self {
        Self {
            size: vec![],
            min_pad: vec![],
            max_pad: vec![],
            bounds: vec![],
        }
    }
    pub fn push(&mut self, len: u32) {
        self.size.push(len);
        self.min_pad.push(0);
        self.max_pad.push(0);
        self.bounds.push(len);
    }
    pub fn pop(&mut self) {
        self.size.pop();
        self.min_pad.pop();
        self.max_pad.pop();
        self.bounds.pop();
    }
}

#[derive(Debug, Clone)]
pub struct LayoutCell(pub CellF<Rc<LayoutCell>>, pub Layout);

impl LayoutCell {
    pub fn dim(&self) -> usize {
        self.1.size.len()
    }

    // cell.s().s() == cell.t().s()
    // cell.s().t() == cell.t().t()

    // Note: s(), t() forgets the original layout

    pub fn s(&self) -> Rc<LayoutCell> {
        let dim = self.dim();
        match &self.0 {
            CellF::Prim(_, shape) => match shape {
                ShapeF::Zero => panic!("zero cell has no source"),
                ShapeF::Succ(s, _) => Rc::clone(s),
            },
            CellF::Id(inner) => Rc::clone(inner),
            CellF::Comp(children, level) => {
                if *level as usize + 1 == dim {
                    children.first().s()
                } else {
                    let cs = children
                        .iter()
                        .map(|c| c.s())
                        .collect::<Vec<Rc<LayoutCell>>>();
                    let mut layout = self.1.clone();
                    layout.pop();
                    Rc::new(LayoutCell(
                        CellF::Comp(NonEmpty::from_vec(cs).unwrap(), *level),
                        layout,
                    ))
                }
            }
        }
    }
    pub fn t(&self) -> Rc<LayoutCell> {
        let dim = self.dim();
        match &self.0 {
            CellF::Prim(_, shape) => match shape {
                ShapeF::Zero => panic!("zero cell has no target"),
                ShapeF::Succ(_, t) => Rc::clone(t),
            },
            CellF::Id(inner) => Rc::clone(inner),
            CellF::Comp(children, level) => {
                if *level as usize + 1 == dim {
                    children.last().t()
                } else {
                    let cs = children
                        .iter()
                        .map(|c| c.t())
                        .collect::<Vec<Rc<LayoutCell>>>();
                    let mut layout = self.1.clone();
                    layout.pop();
                    Rc::new(LayoutCell(
                        CellF::Comp(NonEmpty::from_vec(cs).unwrap(), *level),
                        layout,
                    ))
                }
            }
        }
    }

    pub fn source(self: &Rc<Self>, level: Level) -> Rc<LayoutCell> {
        let mut dim = self.dim();
        let mut cell = Rc::clone(self);
        while (level as usize) + 1 < dim {
            cell = cell.s(); // s or t
            dim -= 1;
        }
        cell.s()
    }
    pub fn target(self: &Rc<Self>, level: Level) -> Rc<LayoutCell> {
        let mut dim = self.dim();
        let mut cell = Rc::clone(self);
        while (level as usize) + 1 < dim {
            cell = cell.s(); // s or t
            dim -= 1;
        }
        cell.t()
    }
}
