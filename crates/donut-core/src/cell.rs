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
    Comp(Level, Vec1<LayoutCell>, Vec<u32>),
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

    pub fn pop(&mut self) -> Option<(u32, u32)> {
        let min_val = self.min.pop()?;
        let max_val = self.max.pop()?;
        Some((min_val, max_val))
    }

    pub fn dim(&self) -> Level {
        assert_eq!(self.min.len(), self.max.len());
        self.min.len() as Level
    }

    pub fn is_zero(&self) -> bool {
        self.min.iter().all(|&x| x == 0) && self.max.iter().all(|&x| x == 0)
    }
}

#[derive(Debug, Clone)]
pub struct Layout {
    pub size: Coord,
    pub pad: Padding,
}

impl Layout {
    pub fn zero(dim: Level) -> Self {
        Self {
            size: vec![],
            pad: Padding::zero(dim as usize),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LayoutCell(pub Rc<Cell>, pub Layout);

impl LayoutCell {
    pub fn new(cell: Rc<Cell>, layout: Layout) -> Self {
        Self(cell, layout)
    }

    pub fn dim(&self) -> usize {
        self.1.size.len()
    }

    pub fn full_size(&self) -> Coord {
        let base_size = &self.1.size;
        base_size
            .iter()
            .enumerate()
            .map(|(i, s)| s + self.1.pad.min[i] + self.1.pad.max[i])
            .collect()
    }

    pub fn extend(&mut self, pad: &Padding) {
        assert!(self.1.pad.min.len() == pad.min.len());
        assert!(self.1.pad.max.len() == pad.max.len());

        for i in 0..pad.min.len() {
            self.1.pad.min[i] += pad.min[i];
            self.1.pad.max[i] += pad.max[i];
        }
    }

    pub fn extended(mut self, pad: &Padding) -> Self {
        self.extend(pad);
        self
    }

    // cell.s().s() ~ cell.t().s()
    // cell.s().t() ~ cell.t().t()

    pub fn s(&self) -> Self {
        let dim = self.dim();
        let cell = match self.0.as_ref() {
            Cell::Prim(_, shape) => match shape {
                Shape::Zero => panic!("zero cell has no source"),
                Shape::Succ(s, _) => return s.clone().extended(&self.1.pad),
            },
            Cell::Id(inner) => Rc::clone(&inner.0),
            Cell::Comp(level, children, inner_pads) => {
                if *level as usize + 1 == dim {
                    return children.first().s().extended(&self.1.pad);
                } else {
                    let cs = children.iter().map(|c| c.s()).collect::<Vec<_>>();
                    let mut layout = self.1.clone();
                    layout.size.pop();
                    Rc::new(Cell::Comp(
                        *level,
                        NonEmpty::from_vec(cs).unwrap(),
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
                Shape::Succ(_, t) => return t.clone().extended(&self.1.pad),
            },
            Cell::Id(inner) => Rc::clone(&inner.0),
            Cell::Comp(level, children, inner_pads) => {
                if *level as usize + 1 == dim {
                    return children.last().t().extended(&self.1.pad);
                } else {
                    let cs = children.iter().map(|c| c.t()).collect::<Vec<_>>();
                    let mut layout = self.1.clone();
                    layout.size.pop();
                    Rc::new(Cell::Comp(
                        *level,
                        NonEmpty::from_vec(cs).unwrap(),
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
}
