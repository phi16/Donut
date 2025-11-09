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

impl Bounds {
    pub fn empty() -> Self {
        Self {
            min: vec![],
            max: vec![],
        }
    }
    pub fn new(min: Coord, max: Coord) -> Self {
        Self { min, max }
    }
    pub fn push(&mut self, len: u32) {
        self.min.push(0);
        self.max.push(len);
    }
    pub fn pop(&mut self) {
        self.min.pop();
        self.max.pop();
    }
}

#[derive(Clone)]
pub struct Layout {
    pub size: Coord,
    pub offset: Coord,
    pub bounds: Coord,
    pub children: Vec<Bounds>,
}

impl Layout {
    pub fn zero() -> Self {
        Self {
            size: vec![],
            offset: vec![],
            bounds: vec![],
            children: vec![],
        }
    }
    pub fn push(&mut self, len: u32) {
        self.size.push(len);
        self.offset.push(0);
        self.bounds.push(len);
        for b in &mut self.children {
            b.push(len);
        }
    }
    pub fn pop(&mut self) {
        self.size.pop();
        self.offset.pop();
        self.bounds.pop();
        for b in &mut self.children {
            b.pop();
        }
    }
}

#[derive(Clone)]
pub struct LayoutCell(pub CellF<Rc<LayoutCell>>, pub Layout);
pub type LayoutShape = ShapeF<Rc<LayoutCell>>;

impl LayoutCell {
    pub fn dim(&self) -> usize {
        self.1.size.len()
    }

    // cell.s().s() == cell.t().s()
    // cell.s().t() == cell.t().t()

    // TODO: don't forget the original layout

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
                    let mut cell = children.first().as_ref().clone();
                    cell.1.pop();
                    Rc::new(cell)
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
                    let mut cell = children.last().as_ref().clone();
                    cell.1.pop();
                    Rc::new(cell)
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
