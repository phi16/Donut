use crate::common::*;

#[derive(Debug, Clone, PartialEq, Eq)]
enum RawCube {
    Point(CoordQ),
    Bridge(Vec1<(RawCube, Q, Q)>),
    Comp(Level, Vec2<RawCube>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cube {
    d: Dimensions,
    c: RawCube,
}

impl RawCube {
    pub fn slice_s(&self, d: Level) -> Self {
        match self {
            RawCube::Point(_) => panic!(),
            RawCube::Bridge(faces) => faces.first().unwrap().0.clone(),
            RawCube::Comp(axis, cs) => {
                let fd = d - 1;
                if axis == &fd {
                    cs.first().unwrap().slice_s(d)
                } else {
                    let cs = cs.iter().map(|c| c.slice_s(d)).collect();
                    RawCube::comp(*axis, fd, cs)
                }
            }
        }
    }

    pub fn slice_t(&self, d: Level) -> Self {
        match self {
            RawCube::Point(_) => panic!(),
            RawCube::Bridge(faces) => faces.last().unwrap().0.clone(),
            RawCube::Comp(axis, cs) => {
                let fd = d - 1;
                if axis == &fd {
                    cs.last().unwrap().slice_t(d)
                } else {
                    let cs = cs.iter().map(|c| c.slice_t(d)).collect();
                    RawCube::comp(*axis, fd, cs)
                }
            }
        }
    }

    pub fn s(&self, l: Level, d: Level) -> Self {
        assert!(l < d);
        match self {
            RawCube::Point(_) => panic!(),
            RawCube::Bridge(faces) => {
                let fd = d - 1;
                if l == fd {
                    let f = faces.first().unwrap().clone();
                    RawCube::Bridge(vec![(f.0, f.1, f.1)])
                } else {
                    let fs = faces
                        .iter()
                        .map(|(f, x0, x1)| {
                            let source = f.s(l, fd);
                            (source, *x0, *x1)
                        })
                        .collect();
                    RawCube::Bridge(fs)
                }
            }
            RawCube::Comp(axis, cs) => {
                if axis == &l {
                    cs.first().unwrap().s(l, d)
                } else {
                    let cs = cs.iter().map(|c| c.s(l, d)).collect();
                    RawCube::comp(*axis, d, cs)
                }
            }
        }
    }

    pub fn t(&self, l: Level, d: Level) -> Self {
        assert!(l < d);
        match self {
            RawCube::Point(_) => panic!(),
            RawCube::Bridge(faces) => {
                let fd = d - 1;
                if l == fd {
                    let f = faces.last().unwrap().clone();
                    RawCube::Bridge(vec![(f.0, f.2, f.2)])
                } else {
                    let fs = faces
                        .iter()
                        .map(|(f, x0, x1)| {
                            let source = f.t(l, fd);
                            (source, *x0, *x1)
                        })
                        .collect();
                    RawCube::Bridge(fs)
                }
            }
            RawCube::Comp(axis, cs) => {
                if axis == &l {
                    cs.last().unwrap().t(l, d)
                } else {
                    let cs = cs.iter().map(|c| c.t(l, d)).collect();
                    RawCube::comp(*axis, d, cs)
                }
            }
        }
    }

    pub fn comp(axis: Level, _d: Level, cs: Vec2<RawCube>) -> Self {
        // TODO: simplify
        assert!(cs.len() >= 2);
        // TODO: check faces
        RawCube::Comp(axis, cs)
    }
}

impl Cube {
    pub fn zero() -> Self {
        Cube {
            d: Dimensions::zero(),
            c: RawCube::Point(vec![]),
        }
    }

    pub fn point(coord: Coord) -> Self {
        Cube {
            d: Dimensions::from_point(coord.len() as Level),
            c: RawCube::Point(from_coord(coord)),
        }
    }

    pub fn interval(x0: Q, x1: Q) -> Self {
        Cube::zero().shifted(x0, x1)
    }

    pub fn bridge(f0: (Cube, Q), f1: (Cube, Q)) -> Self {
        // TODO: f0.0 / f1.0 should have the same structure
        let d = f0.0.d.clone();
        assert_eq!(f1.0.d, d);
        Cube {
            d: d.shifted(),
            c: if f0.0.c == f1.0.c {
                RawCube::Bridge(vec![(f0.0.c, f0.1, f1.1)])
            } else {
                RawCube::Bridge(vec![(f0.0.c, f0.1, f0.1), (f1.0.c, f1.1, f1.1)])
            },
        }
    }

    pub fn dim(&self) -> Dimensions {
        self.d.clone()
    }

    pub fn shifted(self, x0: Q, x1: Q) -> Self {
        Cube {
            d: self.d.shifted(),
            c: RawCube::Bridge(vec![(self.c, x0, x1)]),
        }
    }

    pub fn suspended(self, x: Q) -> Self {
        Cube {
            d: self.d.shifted(),
            c: RawCube::Bridge(vec![(self.c, x, x)]),
        }
    }

    pub fn extended(mut self, pad: &Padding) -> Self {
        fn go(cube: &mut RawCube, dim: Level, min_pad: &[u32], max_pad: &[u32]) {
            match cube {
                RawCube::Point(_) => {
                    assert!(min_pad.iter().take(dim as usize).all(|&x| x == 0));
                    assert!(max_pad.iter().take(dim as usize).all(|&x| x == 0));
                }
                RawCube::Bridge(faces) => {
                    assert!(dim > 0);
                    let fd = dim - 1;
                    for face in faces.iter_mut() {
                        let c = &mut face.0;
                        go(c, dim - 1, min_pad, max_pad);
                    }
                    faces.first_mut().unwrap().1 -= Q::from(min_pad[fd as usize] as i32);
                    faces.last_mut().unwrap().2 += Q::from(max_pad[fd as usize] as i32);
                }
                RawCube::Comp(axis, cs) => {
                    if &dim < axis {
                        for c in cs.iter_mut() {
                            go(c, dim, min_pad, max_pad);
                        }
                    } else {
                        let mut min_pad = min_pad.to_vec();
                        let mut max_pad = max_pad.to_vec();
                        let axis = *axis as usize;
                        let min_pad_axis = min_pad[axis];
                        let max_pad_axis = max_pad[axis];
                        min_pad[axis] = 0;
                        max_pad[axis] = 0;
                        for c in cs.iter_mut() {
                            go(c, dim, &min_pad, &max_pad);
                        }
                        if min_pad_axis == 0 && max_pad_axis == 0 {
                            return;
                        }
                        let mut zero_min_pad = vec![0; dim as usize];
                        let mut zero_max_pad = vec![0; dim as usize];
                        if min_pad_axis != 0 {
                            zero_min_pad[axis] = min_pad_axis;
                            go(cs.first_mut().unwrap(), dim, &zero_min_pad, &zero_max_pad);
                            zero_min_pad[axis] = 0;
                        }
                        if max_pad_axis != 0 {
                            zero_max_pad[axis] = max_pad_axis;
                            go(cs.last_mut().unwrap(), dim, &zero_min_pad, &zero_max_pad);
                            zero_max_pad[axis] = 0;
                        }
                    }
                }
            }
        }
        go(&mut self.c, self.d.in_space, &pad.min, &pad.max);
        self
    }

    pub fn slice_s(&self) -> Self {
        Cube {
            d: self.d.sliced(),
            c: self.c.slice_s(self.d.in_space),
        }
    }

    pub fn slice_t(&self) -> Self {
        Cube {
            d: self.d.sliced(),
            c: self.c.slice_t(self.d.in_space),
        }
    }

    pub fn s(&self, l: Level) -> Self {
        Cube {
            d: self.d.clone(),
            c: self.c.s(l, self.d.in_space),
        }
    }

    pub fn t(&self, l: Level) -> Self {
        Cube {
            d: self.d.clone(),
            c: self.c.t(l, self.d.in_space),
        }
    }

    pub fn comp(axis: Level, cs: Vec1<Cube>) -> Self {
        assert!(!cs.is_empty());
        let d = cs.first().unwrap().d.clone();
        assert!(axis < d.in_space);
        for c in &cs {
            assert_eq!(c.d, d);
        }
        let fd = d.in_space - 1;

        if axis == fd {
            let mut fss = Vec::new();
            let mut fs: Vec<(RawCube, Q, Q)> = Vec::new();
            let mut last_face = None;
            for c in &cs {
                match &c.c {
                    RawCube::Point(_) => panic!(),
                    RawCube::Bridge(faces) => {
                        let lf = &faces.last().unwrap().0;
                        let mut faces = faces.iter();
                        if let Some(last_face) = last_face {
                            let f0 = faces.next().unwrap();
                            assert_eq!(last_face, &f0.0);
                            assert!(fs.last().unwrap().2 <= f0.1);
                            fs.last_mut().unwrap().2 = f0.2;
                        }
                        fs.extend(faces.cloned());
                        last_face = Some(lf);
                    }
                    RawCube::Comp(_, _) => {
                        fss.push(RawCube::Bridge(fs));
                        fss.push(c.c.clone());
                        fs = Vec::new();
                        last_face = None;
                    }
                }
            }
            Cube {
                d,
                c: if fss.is_empty() {
                    RawCube::Bridge(fs)
                } else {
                    if !fs.is_empty() {
                        fss.push(RawCube::Bridge(fs));
                    }
                    fss.into_iter().next().unwrap()
                },
            }
        } else {
            let c = RawCube::comp(axis, d.in_space, cs.into_iter().map(|c| c.c).collect());
            Cube { d, c }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn point_test() {
        let p = Cube::interval(1.into(), 2.into()).extended(&Padding::centered(vec![1]));
        assert_eq!(
            p,
            Cube {
                d: Dimensions {
                    effective: 0,
                    in_space: 1,
                },
                c: RawCube::Bridge(vec![(RawCube::Point(vec![]), 0.into(), 3.into())])
            }
        );
    }

    #[test]
    fn extend_test() {
        let p = Cube::interval(1.into(), 2.into());
        let q = Cube::interval(2.into(), 3.into());
        let c = Cube {
            d: Dimensions {
                effective: 0,
                in_space: 2,
            },
            c: RawCube::Bridge(vec![(p.c, 1.into(), 1.into()), (q.c, 2.into(), 2.into())]),
        }
        .extended(&Padding::centered(vec![1, 1]));
        assert_eq!(
            c,
            Cube {
                d: Dimensions {
                    effective: 0,
                    in_space: 2,
                },
                c: RawCube::Bridge(vec![
                    (Cube::interval(0.into(), 3.into()).c, 0.into(), 1.into()),
                    (Cube::interval(1.into(), 4.into()).c, 2.into(), 3.into())
                ]),
            }
        );
    }

    #[test]
    fn st_test1() {
        let c = Cube::interval(0.into(), 1.into());
        let s = c.slice_s();
        let t = c.slice_t();
        assert_eq!(s, Cube::zero());
        assert_eq!(t, Cube::zero());
    }

    #[test]
    fn st_test2() {
        let p = Cube::interval(0.into(), 1.into());
        let q = Cube::interval(1.into(), 2.into());
        let c = Cube {
            d: Dimensions {
                effective: 0,
                in_space: 2,
            },
            c: RawCube::Bridge(vec![(p.c, 0.into(), 0.into()), (q.c, 1.into(), 1.into())]),
        };
        assert_eq!(
            c.s(0),
            Cube {
                d: Dimensions {
                    effective: 0,
                    in_space: 2,
                },
                c: RawCube::Bridge(vec![
                    (Cube::zero().suspended(0.into()).c, 0.into(), 0.into()),
                    (Cube::zero().suspended(1.into()).c, 1.into(), 1.into())
                ]),
            }
        );
        assert_eq!(
            c.s(1),
            Cube {
                d: Dimensions {
                    effective: 0,
                    in_space: 2,
                },
                c: RawCube::Bridge(vec![(
                    Cube::zero().shifted(0.into(), 1.into()).c,
                    0.into(),
                    0.into()
                )]),
            }
        );
    }

    #[test]
    fn comp_1d_test() {
        let c1 = Cube::interval(0.into(), 1.into());
        let c2 = Cube::interval(1.into(), 2.into());
        let c = Cube::comp(0, vec![c1, c2]);

        assert_eq!(
            c,
            Cube {
                d: Dimensions {
                    effective: 0,
                    in_space: 1,
                },
                c: RawCube::Bridge(vec![(Cube::zero().c, 0.into(), 2.into())])
            }
        );
    }

    #[test]
    fn comp_2d_test() {
        let base = Cube::interval(0.into(), 1.into());
        let c1 = base.clone().shifted(0.into(), 1.into());
        let c2 = base.clone().shifted(1.into(), 2.into());

        let c = Cube::comp(1, vec![c1, c2]);
        assert_eq!(c.d.in_space, 2);
        assert_eq!(c.d.effective, 0);

        let s = c.s(0);
        assert_eq!(
            s,
            Cube::zero().suspended(0.into()).shifted(0.into(), 2.into())
        );
    }

    #[test]
    fn comp0_extend_test() {
        let c1 = Cube::bridge(
            (Cube::interval(1.into(), 2.into()), 1.into()),
            (Cube::interval(2.into(), 2.into()), 2.into()),
        );
        let c2 = Cube::bridge(
            (Cube::interval(2.into(), 2.into()), 1.into()),
            (Cube::interval(2.into(), 3.into()), 2.into()),
        );

        let c = Cube::comp(0, vec![c1, c2]).extended(&Padding::centered(vec![1, 1]));

        assert_eq!(
            c,
            Cube {
                d: Dimensions {
                    effective: 0,
                    in_space: 2,
                },
                c: RawCube::Comp(
                    0,
                    vec![
                        RawCube::Bridge(vec![
                            (Cube::interval(0.into(), 2.into()).c, 0.into(), 1.into()),
                            (Cube::interval(1.into(), 2.into()).c, 2.into(), 3.into())
                        ]),
                        RawCube::Bridge(vec![
                            (Cube::interval(2.into(), 3.into()).c, 0.into(), 1.into()),
                            (Cube::interval(2.into(), 4.into()).c, 2.into(), 3.into())
                        ]),
                    ]
                ),
            }
        );
    }
}
