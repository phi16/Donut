use nonempty::NonEmpty;
use std::rc::Rc;

pub type Level = u8;
pub type Vec1<T> = NonEmpty<T>;
pub type PrimId = u64;

#[derive(Debug, Clone)]
pub enum ShapeF<P> {
    Zero,
    Succ(P, P),
}

#[derive(Debug, Clone)]
pub enum CellF<C, P> {
    Prim(PrimId, ShapeF<P>),
    Id(C),
    Comp(Vec1<P>, Level, Vec<u32>),
}

// Layouts

pub type Coord = Vec<u32>;

#[derive(Debug, Clone)]
pub struct Padding {
    pub min: Coord,
    pub max: Coord,
}

impl Padding {
    pub fn zero(dim: usize) -> Self {
        Self {
            min: vec![0; dim],
            max: vec![0; dim],
        }
    }

    pub fn centered(pad: Coord) -> Self {
        Self {
            min: pad.clone(),
            max: pad,
        }
    }

    pub fn pop(&mut self) {
        self.min.pop();
        self.max.pop();
    }
}

#[derive(Debug, Clone)]
pub struct Layout {
    pub size: Coord,
}

impl Layout {
    pub fn zero() -> Self {
        Self { size: vec![] }
    }
}

#[derive(Debug, Clone)]
pub struct LayoutCell(pub CellF<Rc<LayoutCell>, PaddedCell>, pub Layout);

#[derive(Debug, Clone)]
pub struct PaddedCell {
    pub cell: Rc<LayoutCell>,
    pub pad: Padding,
}

impl LayoutCell {
    pub fn dim(&self) -> usize {
        self.1.size.len()
    }
}

impl PaddedCell {
    pub fn from_cell(cell: Rc<LayoutCell>) -> Self {
        let dim = cell.dim();
        Self {
            cell,
            pad: Padding::zero(dim),
        }
    }

    fn dim(&self) -> usize {
        self.cell.dim()
    }

    pub fn size(&self) -> Coord {
        let base_size = &self.cell.1.size;
        base_size
            .iter()
            .enumerate()
            .map(|(i, s)| s + self.pad.min[i] + self.pad.max[i])
            .collect()
    }

    pub fn extend(&self, pad: &Padding) -> Self {
        // accepts higher padding
        assert!(self.pad.min.len() <= pad.min.len());
        assert!(self.pad.max.len() <= pad.max.len());

        let mut new_pad = self.pad.clone();
        for i in 0..new_pad.min.len() {
            new_pad.min[i] += pad.min[i];
            new_pad.max[i] += pad.max[i];
        }
        PaddedCell {
            cell: Rc::clone(&self.cell),
            pad: new_pad,
        }
    }

    // cell.s().s() == cell.t().s()
    // cell.s().t() == cell.t().t()

    pub fn s(&self) -> Self {
        let dim = self.dim();
        let cell = match &self.cell.0 {
            CellF::Prim(_, shape) => match shape {
                ShapeF::Zero => panic!("zero cell has no source"),
                ShapeF::Succ(s, _) => return s.extend(&self.pad),
            },
            CellF::Id(inner) => Rc::clone(inner),
            CellF::Comp(children, level, inner_pads) => {
                if *level as usize + 1 == dim {
                    return children.first().s().extend(&self.pad);
                } else {
                    let cs = children.iter().map(|c| c.s()).collect::<Vec<_>>();
                    let mut layout = self.cell.1.clone();
                    layout.size.pop();
                    Rc::new(LayoutCell(
                        CellF::Comp(NonEmpty::from_vec(cs).unwrap(), *level, inner_pads.clone()),
                        layout,
                    ))
                }
            }
        };
        let mut pad = self.pad.clone();
        pad.pop();
        PaddedCell { cell, pad }
    }
    pub fn t(&self) -> Self {
        let dim = self.dim();
        let cell = match &self.cell.0 {
            CellF::Prim(_, shape) => match shape {
                ShapeF::Zero => panic!("zero cell has no target"),
                ShapeF::Succ(_, t) => return t.extend(&self.pad),
            },
            CellF::Id(inner) => Rc::clone(inner),
            CellF::Comp(children, level, inner_pads) => {
                if *level as usize + 1 == dim {
                    return children.last().t().extend(&self.pad);
                } else {
                    let cs = children.iter().map(|c| c.t()).collect::<Vec<_>>();
                    let mut layout = self.cell.1.clone();
                    layout.size.pop();
                    Rc::new(LayoutCell(
                        CellF::Comp(NonEmpty::from_vec(cs).unwrap(), *level, inner_pads.clone()),
                        layout,
                    ))
                }
            }
        };
        let mut pad = self.pad.clone();
        pad.pop();
        PaddedCell { cell, pad }
    }

    pub fn source(&self, level: Level) -> Self {
        let mut dim = self.dim();
        let mut cell = self.clone();
        while (level as usize) + 1 < dim {
            cell = cell.s(); // s or t
            dim -= 1;
        }
        cell.s()
    }
    pub fn target(&self, level: Level) -> Self {
        let mut dim = self.dim();
        let mut cell = self.clone();
        while (level as usize) + 1 < dim {
            cell = cell.s(); // s or t
            dim -= 1;
        }
        cell.t()
    }
}
