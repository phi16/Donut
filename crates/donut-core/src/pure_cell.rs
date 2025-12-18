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

impl Cellular for PureCell {
    fn dim(&self) -> Dim {
        match self {
            PureCell::Prim(_, _, dim) => dim.clone(),
            PureCell::Comp(_, _, dim) => dim.clone(),
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
            Dim::new(d + 1, d + 1),
        )
    }

    fn id(face: Self) -> Self {
        match face {
            PureCell::Prim(prim, shape, dim) => PureCell::Prim(prim, shape, dim.shifted()),
            PureCell::Comp(axis, children, dim) => PureCell::Comp(
                axis,
                children.into_iter().map(|child| Self::id(child)).collect(),
                dim.shifted(),
            ),
        }
    }

    fn comp(axis: Axis, children: Vec2<Self>) -> Option<Self> {
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
                PureCell::id(first_source_face)
            }
            1 => cs.into_iter().next().unwrap(),
            _ => PureCell::Comp(axis, cs, dim),
        })
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
                    PureCell::comp(*axis, children).unwrap()
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
                    PureCell::comp(*axis, children).unwrap()
                }
            }
        }
    }

    fn is_convertible(&self, other: &Self) -> bool {
        self == other
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pure_cell_assoc() {
        let assoc = crate::cell::tests::assoc::<PureCell>();
        assert_eq!(assoc.s().s(), assoc.t().s());
        assert_eq!(assoc.s().t(), assoc.t().t());
    }
}
