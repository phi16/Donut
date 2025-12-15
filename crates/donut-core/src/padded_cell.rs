use crate::cell::*;
use crate::common::*;
use crate::layout_cell;
use crate::pure_cell::PureCell;

const BLOCK_WIDTH: N = 100;

#[derive(Debug, Clone)]
enum Shape {
    Zero,
    Succ {
        source: PaddedCell,
        width: N,
        target: PaddedCell,
    },
}

#[derive(Debug, Clone)]
enum RawCell {
    Prim(Prim, Shape),
    Id(PaddedCell, N),
    Comp(Axis, Vec2<PaddedCell>),
}

#[derive(Debug, Clone)]
struct Pad {
    min_pad: CoordN,
    max_pad: CoordN,
}

impl Pad {
    fn zero(dim: Level) -> Self {
        Pad {
            min_pad: vec![0; dim as usize],
            max_pad: vec![0; dim as usize],
        }
    }

    fn pop(&mut self) -> Option<(N, N)> {
        let min = self.min_pad.pop()?;
        let max = self.max_pad.pop()?;
        Some((min, max))
    }
}

#[derive(Debug, Clone)]
struct Layout {
    dim: Dim,
    inner_size: CoordN,
    full_size: CoordN,
    pad: Pad,
}

impl Layout {
    fn new(dim: Dim, size: CoordN) -> Self {
        assert_eq!(dim.in_space as usize, size.len());
        Layout {
            dim,
            inner_size: size.clone(),
            full_size: size,
            pad: Pad::zero(dim.in_space),
        }
    }
}

#[derive(Debug, Clone)]
pub struct PaddedCell(Box<RawCell>, Layout);

impl PaddedCell {
    fn fit(&mut self, new_size: &CoordN) {
        let half_delta = new_size
            .iter()
            .zip(self.1.full_size.iter())
            .map(|(&new, &old)| {
                assert!(new >= old);
                (new - old) / 2
            })
            .collect::<Vec<_>>();
        self.extend(&half_delta, &half_delta);
    }

    fn extend(&mut self, min_pad: &[N], max_pad: &[N]) {
        let d = self.1.dim.in_space as usize;
        assert_eq!(min_pad.len(), d);
        assert_eq!(max_pad.len(), d);
        for (i, &pad) in min_pad.iter().enumerate() {
            self.1.pad.min_pad[i] += pad;
            self.1.full_size[i] += pad;
        }
        for (i, &pad) in max_pad.iter().enumerate() {
            self.1.pad.max_pad[i] += pad;
            self.1.full_size[i] += pad;
        }
    }

    fn to_pure(&self) -> PureCell {
        match self.0.as_ref() {
            RawCell::Prim(prim, shape) => match shape {
                Shape::Zero => PureCell::zero(prim.clone()),
                Shape::Succ { source, target, .. } => {
                    let s = source.to_pure();
                    let t = target.to_pure();
                    PureCell::prim(prim.clone(), s, t)
                }
            },
            RawCell::Id(face, _) => {
                let f = face.to_pure();
                PureCell::id(f)
            }
            RawCell::Comp(axis, children) => {
                let cs = children
                    .iter()
                    .map(|c| c.to_pure())
                    .collect::<Vec2<PureCell>>();
                PureCell::comp(*axis, cs).unwrap()
            }
        }
    }

    pub fn resolve_pad(&self) -> layout_cell::LayoutCell {
        use layout_cell as l;
        use layout_cell::LayoutCell;
        fn resolve(cell: &PaddedCell, mut pad: Pad) -> LayoutCell {
            let dim = cell.1.dim;
            add_coord(&mut pad.min_pad, &cell.1.pad.min_pad);
            add_coord(&mut pad.max_pad, &cell.1.pad.max_pad);
            let (raw_cell, size) = match &cell.0.as_ref() {
                RawCell::Prim(prim, shape) => match shape {
                    Shape::Zero => (l::RawCell::Prim(prim.clone(), l::Shape::Zero), vec![]),
                    Shape::Succ {
                        source,
                        width,
                        target,
                    } => {
                        let extend = pad.pop().unwrap();
                        let s = resolve(source, pad.clone());
                        let t = resolve(target, pad);
                        let mut size = s.1.size.clone();
                        assert_eq!(size, t.1.size);
                        size.push(extend.0 + *width + extend.1);
                        (
                            l::RawCell::Prim(
                                prim.clone(),
                                l::Shape::Succ {
                                    source_extend: extend.0,
                                    source: s,
                                    width: *width,
                                    target: t,
                                    target_extend: extend.1,
                                },
                            ),
                            size,
                        )
                    }
                },
                RawCell::Id(face, width) => {
                    let extend = pad.pop().unwrap();
                    let f = resolve(face, pad);
                    let width = extend.0 + *width + extend.1;
                    let mut size = f.1.size.clone();
                    size.push(width);
                    (l::RawCell::Id(f, width), size)
                }
                RawCell::Comp(axis, children) => {
                    let n = children.len();
                    let cs = children
                        .iter()
                        .enumerate()
                        .map(|(i, c)| {
                            let mut pad = pad.clone();
                            if i != 0 {
                                pad.min_pad[*axis as usize] = 0;
                            }
                            if i != n - 1 {
                                pad.max_pad[*axis as usize] = 0;
                            }
                            resolve(c, pad)
                        })
                        .collect::<Vec2<l::LayoutCell>>();
                    let mut size = cs.first().unwrap().1.size.clone();
                    for c in &cs[1..] {
                        size[*axis as usize] += c.1.size[*axis as usize];
                    }
                    (l::RawCell::Comp(*axis, cs), size)
                }
            };
            LayoutCell(Box::new(raw_cell), l::Layout { dim, size })
        }
        resolve(self, Pad::zero(self.1.dim.in_space))
    }
}

impl Cellular for PaddedCell {
    fn dim(&self) -> Dim {
        self.1.dim
    }

    fn zero(prim: Prim) -> Self {
        let cell = Box::new(RawCell::Prim(prim, Shape::Zero));
        let layout = Layout::new(Dim::new(0, 0), vec![]);
        PaddedCell(cell, layout)
    }

    fn prim(prim: Prim, source: Self, target: Self) -> Self {
        let d = source.dim().in_space;
        assert_eq!(d, target.dim().in_space);
        let mut size = source.1.full_size.clone();
        max_coord(&mut size, &target.1.full_size);
        let mut source = source;
        let mut target = target;
        source.fit(&size);
        target.fit(&size);
        let shape = Shape::Succ {
            source,
            width: BLOCK_WIDTH,
            target,
        };
        size.push(BLOCK_WIDTH);
        let d = d + 1;
        let cell = Box::new(RawCell::Prim(prim, shape));
        let layout = Layout::new(Dim::new(d, d), size);
        PaddedCell(cell, layout)
    }

    fn id(face: Self) -> Self {
        let dim = face.dim().shifted();
        let mut size = face.1.full_size.clone();
        size.push(BLOCK_WIDTH);
        let cell = Box::new(RawCell::Id(face, BLOCK_WIDTH));
        let layout = Layout::new(dim, size);
        PaddedCell(cell, layout)
    }

    fn comp(axis: Axis, children: Vec2<Self>) -> Option<Self> {
        let n = children.len();
        if n == 0 {
            return None;
        }
        if n == 1 {
            return Some(children.into_iter().next().unwrap());
        }
        assert!(n >= 2);
        let mut dim = children[0].dim();
        if axis >= dim.in_space {
            return None;
        }

        for i in 0..n - 1 {
            let t = target_face(&children[i], axis);
            let s = source_face(&children[i + 1], axis);
            if !t.is_convertible(&s) {
                eprintln!("t.is_convertible(&s) failed: ({:?})", t.is_convertible(&s));
                return None;
            }
        }

        let mut max_size = children[0].1.full_size.clone();
        let mut axis_size = children[0].1.full_size[axis as usize];
        for child in &children[1..] {
            assert_eq!(child.dim().in_space, dim.in_space);
            dim.effective = dim.effective.max(child.dim().effective);
            max_coord(&mut max_size, &child.1.full_size);
            axis_size += child.1.full_size[axis as usize];
        }

        let mut size = max_size;
        size[axis as usize] = axis_size;

        let mut children = children;
        let mut new_size = size.clone();
        for c in &mut children {
            new_size[axis as usize] = c.1.full_size[axis as usize];
            c.fit(&new_size);
        }

        let cell = Box::new(RawCell::Comp(axis, children));
        let layout = Layout::new(dim, size);
        Some(PaddedCell(cell, layout))
    }

    fn s(&self) -> Self {
        let dim = self.dim();
        let d = dim.in_space - 1;
        let mut c = match self.0.as_ref() {
            RawCell::Prim(_, ref shape) => match shape {
                Shape::Succ { source, .. } => source.clone(),
                Shape::Zero => panic!("zero-cell has no source"),
            },
            RawCell::Id(ref face, _) => face.clone(),
            RawCell::Comp(axis, ref children) => {
                if axis == &d {
                    children.first().unwrap().s()
                } else {
                    assert!(axis < &d);
                    let cs = children.iter().map(|c| c.s()).collect::<Vec2<PaddedCell>>();
                    let dim = cs
                        .iter()
                        .map(|c| c.dim())
                        .reduce(|d0, d1| {
                            assert_eq!(d0.in_space, d1.in_space);
                            Dim::new(d0.effective.max(d1.effective), d0.in_space)
                        })
                        .unwrap();
                    let cell = Box::new(RawCell::Comp(*axis, cs));
                    let size = self.1.inner_size[..d as usize].to_vec();
                    let layout = Layout::new(dim, size);
                    PaddedCell(cell, layout)
                }
            }
        };
        c.extend(
            &self.1.pad.min_pad[..d as usize],
            &self.1.pad.max_pad[..d as usize],
        );
        c
    }

    fn t(&self) -> Self {
        let dim = self.dim();
        let d = dim.in_space - 1;
        let mut c = match self.0.as_ref() {
            RawCell::Prim(_, ref shape) => match shape {
                Shape::Succ { target, .. } => target.clone(),
                Shape::Zero => panic!("zero-cell has no target"),
            },
            RawCell::Id(ref face, _) => face.clone(),
            RawCell::Comp(axis, ref children) => {
                if axis == &d {
                    children.last().unwrap().t()
                } else {
                    assert!(axis < &d);
                    let cs = children.iter().map(|c| c.t()).collect::<Vec2<PaddedCell>>();
                    let dim = cs
                        .iter()
                        .map(|c| c.dim())
                        .reduce(|d0, d1| {
                            assert_eq!(d0.in_space, d1.in_space);
                            Dim::new(d0.effective.max(d1.effective), d0.in_space)
                        })
                        .unwrap();
                    let cell = Box::new(RawCell::Comp(*axis, cs));
                    let size = self.1.inner_size[..d as usize].to_vec();
                    let layout = Layout::new(dim, size);
                    PaddedCell(cell, layout)
                }
            }
        };
        let d = self.dim().in_space - 1;
        c.extend(
            &self.1.pad.min_pad[..d as usize],
            &self.1.pad.max_pad[..d as usize],
        );
        c
    }

    fn is_convertible(&self, other: &Self) -> bool {
        if self.dim() != other.dim() {
            return false;
        }
        let c0 = self.to_pure();
        let c1 = other.to_pure();
        c0.is_convertible(&c1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn padded_cell_assoc() {
        let assoc = crate::cell::tests::assoc::<PaddedCell>();
        assert!(assoc.s().s().is_convertible(&assoc.t().s()));
        assert!(assoc.s().t().is_convertible(&assoc.t().t()));

        eprintln!("{}", assoc.resolve_pad());
        assert!(false);
    }
}
