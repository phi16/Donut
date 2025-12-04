use nonempty::NonEmpty;
use std::rc::Rc;

pub type Level = u8;
pub type Vec1<T> = NonEmpty<T>;
pub type PrimId = u64;

#[derive(Debug, Clone)]
pub enum Shape {
    Zero,
    Succ(LayoutCell, LayoutCell),
}

#[derive(Debug, Clone)]
pub enum Cell {
    Prim(PrimId, Shape),
    Id(LayoutCell),
    Comp(Vec1<LayoutCell>, Level, Vec<u32>),
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
    pub pad: Padding,
}

impl Layout {
    pub fn zero() -> Self {
        Self {
            size: vec![],
            pad: Padding::zero(0),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LayoutCell(pub Rc<Cell>, pub Layout);

impl LayoutCell {
    pub fn dim(&self) -> usize {
        self.1.size.len()
    }

    pub fn with_zero_pad(cell: Rc<Cell>, layout: Layout) -> Self {
        let dim = layout.size.len();
        Self(
            cell,
            Layout {
                size: layout.size,
                pad: Padding::zero(dim),
            },
        )
    }

    pub fn size(&self) -> Coord {
        let base_size = &self.1.size;
        base_size
            .iter()
            .enumerate()
            .map(|(i, s)| s + self.1.pad.min[i] + self.1.pad.max[i])
            .collect()
    }

    pub fn extend(&self, pad: &Padding) -> Self {
        // accepts higher padding
        assert!(self.1.pad.min.len() <= pad.min.len());
        assert!(self.1.pad.max.len() <= pad.max.len());

        let mut new_pad = self.1.pad.clone();
        for i in 0..new_pad.min.len() {
            new_pad.min[i] += pad.min[i];
            new_pad.max[i] += pad.max[i];
        }
        let mut new_layout = self.1.clone();
        new_layout.pad = new_pad;
        LayoutCell(Rc::clone(&self.0), new_layout)
    }

    // cell.s().s() == cell.t().s()
    // cell.s().t() == cell.t().t()

    pub fn s(&self) -> Self {
        let dim = self.dim();
        let cell = match self.0.as_ref() {
            Cell::Prim(_, shape) => match shape {
                Shape::Zero => panic!("zero cell has no source"),
                Shape::Succ(s, _) => return s.extend(&self.1.pad),
            },
            Cell::Id(inner) => Rc::clone(&inner.0),
            Cell::Comp(children, level, inner_pads) => {
                if *level as usize + 1 == dim {
                    return children.first().s().extend(&self.1.pad);
                } else {
                    let cs = children.iter().map(|c| c.s()).collect::<Vec<_>>();
                    let mut layout = self.1.clone();
                    layout.size.pop();
                    Rc::new(Cell::Comp(
                        NonEmpty::from_vec(cs).unwrap(),
                        *level,
                        inner_pads.clone(),
                    ))
                }
            }
        };
        let mut layout = self.1.clone();
        layout.pad.pop();
        layout.size.pop();
        LayoutCell(cell, layout)
    }
    pub fn t(&self) -> Self {
        let dim = self.dim();
        let cell = match self.0.as_ref() {
            Cell::Prim(_, shape) => match shape {
                Shape::Zero => panic!("zero cell has no target"),
                Shape::Succ(_, t) => return t.extend(&self.1.pad),
            },
            Cell::Id(inner) => Rc::clone(&inner.0),
            Cell::Comp(children, level, inner_pads) => {
                if *level as usize + 1 == dim {
                    return children.last().t().extend(&self.1.pad);
                } else {
                    let cs = children.iter().map(|c| c.t()).collect::<Vec<_>>();
                    let mut layout = self.1.clone();
                    layout.size.pop();
                    Rc::new(Cell::Comp(
                        NonEmpty::from_vec(cs).unwrap(),
                        *level,
                        inner_pads.clone(),
                    ))
                }
            }
        };
        let mut layout = self.1.clone();
        layout.pad.pop();
        layout.size.pop();
        LayoutCell(cell, layout)
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
