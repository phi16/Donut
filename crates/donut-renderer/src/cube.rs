use crate::common::*;

#[derive(Debug, Clone, PartialEq, Eq)]
enum RawCube {
    Point(CoordQ),
    Bridge(Vec1<(RawCube, Q, Q)>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cube {
    d: Dimensions,
    c: RawCube,
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

    pub fn dim(&self) -> Dimensions {
        self.d.clone()
    }

    pub fn shifted(self, x0: Q, x1: Q) -> Self {
        Cube {
            d: self.d.shifted(),
            c: RawCube::Bridge(vec![(self.c, x0, x1)]),
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
            }
        }
        go(&mut self.c, self.d.in_space, &pad.min, &pad.max);
        self
    }

    pub fn comp(axis: Level, cs: Vec2<Cube>) -> Self {
        unimplemented!()
    }

    pub fn s(l: Level) -> Self {
        unimplemented!()
    }

    pub fn t(l: Level) -> Self {
        unimplemented!()
    }
}

// #[cfg(test)]
mod tests {
    #[test]
    fn point_test() {
        use super::*;
        let p = Cube::zero()
            .shifted(1.into(), 2.into())
            .extended(&Padding::centered(vec![1]));
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
        use super::*;
        let p = Cube::zero().shifted(1.into(), 2.into());
        let q = Cube::zero().shifted(2.into(), 3.into());
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
                    (
                        Cube::zero().shifted(0.into(), 3.into()).c,
                        0.into(),
                        1.into()
                    ),
                    (
                        Cube::zero().shifted(1.into(), 4.into()).c,
                        2.into(),
                        3.into()
                    )
                ]),
            }
        );
    }
}
