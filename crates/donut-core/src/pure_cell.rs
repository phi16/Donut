use crate::cell::*;
use crate::common::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Shape {
    Zero,
    Succ {
        source: Box<PureCell>,
        target: Box<PureCell>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PureCell {
    Prim(Prim, Shape, Dim),
    Comp(Axis, Vec2<PureCell>, Dim),
}

pub struct PureCellFactory;

impl CellLike for PureCell {
    fn dim(&self) -> Dim {
        match self {
            PureCell::Prim(_, _, dim) => dim.clone(),
            PureCell::Comp(_, _, dim) => dim.clone(),
        }
    }
    fn s(&self) -> Self {
        match self {
            PureCell::Prim(prim, shape, dim) if dim.effective < dim.in_space => {
                PureCell::Prim(prim.clone(), shape.clone(), dim.sliced())
            }
            PureCell::Prim(_, shape, _) => match shape {
                Shape::Zero => panic!("zero-cell has no source"),
                Shape::Succ { source, .. } => source.as_ref().clone(),
            },
            PureCell::Comp(axis, children, dim) => {
                if *axis == dim.in_space - 1 {
                    children.first().unwrap().s()
                } else {
                    let children = children.iter().map(|child| child.s()).collect::<Vec<_>>();
                    PureCellFactory {}.comp(*axis, children).unwrap()
                }
            }
        }
    }

    fn t(&self) -> Self {
        match self {
            PureCell::Prim(prim, shape, dim) if dim.effective < dim.in_space => {
                PureCell::Prim(prim.clone(), shape.clone(), dim.sliced())
            }
            PureCell::Prim(_, shape, _) => match shape {
                Shape::Zero => panic!("zero-cell has no target"),
                Shape::Succ { target, .. } => target.as_ref().clone(),
            },
            PureCell::Comp(axis, children, dim) => {
                if *axis == dim.in_space - 1 {
                    children.last().unwrap().t()
                } else {
                    let children = children.iter().map(|child| child.t()).collect::<Vec<_>>();
                    PureCellFactory {}.comp(*axis, children).unwrap()
                }
            }
        }
    }

    fn is_convertible(&self, other: &Self) -> bool {
        self == other
    }
}

impl CellFactory for PureCellFactory {
    type Cell = PureCell;

    fn clone(&mut self, cell: &Self::Cell) -> Self::Cell {
        cell.clone()
    }

    fn zero(&mut self, prim: Prim) -> Self::Cell {
        PureCell::Prim(prim, Shape::Zero, Dim::new(0, 0))
    }

    fn prim(&mut self, prim: Prim, source: Self::Cell, target: Self::Cell) -> Self::Cell {
        let d = source.dim().in_space;
        assert_eq!(d, target.dim().in_space);
        PureCell::Prim(
            prim,
            Shape::Succ {
                source: Box::new(source),
                target: Box::new(target),
            },
            Dim::new(d + 1, d + 1),
        )
    }

    fn id(&mut self, face: Self::Cell) -> Self::Cell {
        match face {
            PureCell::Prim(prim, shape, dim) => PureCell::Prim(prim, shape, dim.shifted()),
            PureCell::Comp(axis, children, dim) => PureCell::Comp(
                axis,
                children.into_iter().map(|child| self.id(child)).collect(),
                dim.shifted(),
            ),
        }
    }

    fn comp(&mut self, axis: Axis, children: Vec2<Self::Cell>) -> Option<Self::Cell> {
        let n = children.len();
        if n == 0 {
            return None;
        }
        assert!(n >= 1);

        let first_source_face = source_face(&children[0], axis);
        let last_target_face = target_face(&children[n - 1], axis);

        for i in 0..n - 1 {
            let t = target_face(&children[i], axis);
            let s = source_face(&children[i + 1], axis);
            if !t.is_convertible(&s) {
                return None;
            }
        }

        let mut cs = vec![];
        let mut dim = children.first().unwrap().dim();
        for c in children {
            let d = c.dim();
            assert_eq!(d.in_space, dim.in_space);
            dim.effective = dim.effective.max(d.effective);
            match c {
                PureCell::Comp(a, mut cc, _) if a == axis => {
                    cs.append(&mut cc);
                }
                PureCell::Prim(_, _, dim) if dim.effective < axis => {
                    // do nothing
                }
                _ => {
                    cs.push(c);
                }
            }
        }

        Some(match cs.len() {
            0 => {
                assert!(first_source_face.is_convertible(&last_target_face));
                self.id(first_source_face)
            }
            1 => cs.into_iter().next().unwrap(),
            _ => PureCell::Comp(axis, cs, dim),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pure_cell_assoc() {
        let mut builder = PureCellFactory {};
        let assoc = crate::cell::tests::assoc(&mut builder);
        assert_eq!(assoc.s().s(), assoc.t().s());
        assert_eq!(assoc.s().t(), assoc.t().t());
    }
}
