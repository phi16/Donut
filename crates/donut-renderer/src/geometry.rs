use crate::common::*;
use crate::cube::*;
use donut_core::cell::{Padding, PrimId};

/*

#[derive(Debug, Clone)]
pub struct Unit {
    pub prim_id: PrimId,
    pub dim: Dimensions,
    pub cube: Cube,
    pub source: Box<Geometry>,
    pub target: Box<Geometry>,
}

#[derive(Debug, Clone)]
pub enum Geometry {
    Unit(Unit),
    Comp(Level, Vec2<Geometry>, Vec1<Geometry>),
}

enum UnitSlice {
    IdSection(Unit),
    PrimSection(Geometry),
}

impl Unit {
    pub fn dim(&self) -> Dimensions {
        match self {
            Unit::Zero(l, c) => {
                assert_eq!(*l, c.dim().space);
                Dimensions::from_unit(*l)
            }
            Unit::Succ(dim, s, f, t) => {
                let fd = f.dim();
                assert_eq!(dim.effective, fd.effective + 1);
                assert!(dim.effective > s.dim().effective);
                assert!(dim.effective > t.dim().effective);
                assert_eq!(dim.space, fd.space + 1);
                assert_eq!(dim.space, s.dim().space);
                assert_eq!(dim.space, t.dim().space);
                dim.clone()
            }
        }
    }

    pub fn shift(&mut self, w: u32) {
        match self {
            Unit::Zero(_, cube) => {
                *cube = cube.shift(0.into(), (w as i32).into());
            }
            Unit::Succ(_, s, f, t) => {
                s.shift(w);
                f.shift(w);
                t.shift(w);
            }
        }
    }

    pub fn extend(&mut self, pad: &Padding) {
        match self {
            Unit::Zero(_, cube) => {
                *cube = cube.extend(pad);
            }
            Unit::Succ(dim, s, f, t) => {
                let l = (dim.space - 1) as usize;
                let mut pad_inside = pad.clone();
                pad_inside.min[l] = 0;
                pad_inside.max[l] = 0;
                if !pad_inside.is_zero() {
                    s.extend(&pad_inside);
                    f.extend(&pad_inside);
                    t.extend(&pad_inside);
                }

                let min_end = pad.min[l];
                let max_end = pad.max[l];
                let mut end_pad = Padding {
                    min: vec![0; pad.min.len()],
                    max: vec![0; pad.max.len()],
                };
                if min_end != 0 {
                    end_pad.min[l] = min_end;
                    s.extend(&end_pad);
                    end_pad.min[l] = 0;
                }
                if max_end != 0 {
                    end_pad.max[l] = max_end;
                    t.extend(&end_pad);
                    end_pad.max[l] = 0;
                }
            }
        }
    }

    fn s(&self, l: Level) -> UnitSlice {
        unimplemented!()
    }

    fn t(&self, l: Level) -> UnitSlice {
        unimplemented!()
    }
}

impl Geometry {
    pub fn shift(&mut self, w: u32) {
        match self {
            Geometry::Prim(_, unit) => {
                unit.shift(w);
            }
            Geometry::Comp(_, cs, ps) => {
                for c in cs.iter_mut() {
                    c.shift(w);
                }
                for p in ps.iter_mut() {
                    p.shift(w);
                }
            }
        }
    }

    pub fn extend(&mut self, pad: &Padding) {
        match self {
            Geometry::Prim(_, unit) => {
                unit.extend(pad);
            }
            Geometry::Comp(axis, cs, ps) => {
                let axis = *axis as usize;
                let n = cs.len();
                assert!(n >= 2);
                let mut pad_inside = pad.clone();
                pad_inside.min[axis] = 0;
                pad_inside.max[axis] = 0;
                if !pad_inside.is_zero() {
                    for c in cs.iter_mut() {
                        c.extend(&pad_inside);
                    }
                    for p in ps.iter_mut() {
                        p.extend(&pad_inside);
                    }
                }

                let min_end = pad.min[axis];
                let max_end = pad.max[axis];
                let mut end_pad = Padding {
                    min: vec![0; pad.min.len()],
                    max: vec![0; pad.max.len()],
                };
                if min_end != 0 {
                    end_pad.min[axis] = min_end;
                    cs[0].extend(&end_pad);
                    end_pad.min[axis] = 0;
                }
                if max_end != 0 {
                    end_pad.max[axis] = max_end;
                    cs[n - 1].extend(&end_pad);
                    end_pad.max[axis] = 0;
                }
            }
        }
    }

    pub fn dim(&self) -> Dimensions {
        match self {
            Geometry::Prim(_, unit) => unit.dim(),
            Geometry::Comp(_, cs, ps) => {
                let d = cs
                    .iter()
                    .map(|c| c.dim())
                    .reduce(|a, b| {
                        assert_eq!(a.space, b.space);
                        Dimensions {
                            effective: a.effective.max(b.effective),
                            space: a.space,
                        }
                    })
                    .unwrap();
                for p in ps {
                    let pd = p.dim();
                    assert_eq!(pd.space, d.space);
                    assert!(pd.effective < d.effective);
                }
                d
            }
        }
    }

    pub fn s(&self, l: Level) -> Self {
        match self {
            Geometry::Prim(prim_id, unit) => match unit.s(l) {
                UnitSlice::PrimSection(g) => g,
                UnitSlice::IdSection(u) => Geometry::Prim(*prim_id, u),
            },
            Geometry::Comp(axis, cs, ps) => {
                if *axis == self.dim().space - 1 {
                    cs.first().unwrap().s(l)
                } else {
                    let cs = cs.iter().map(|c| c.s(l)).collect::<Vec<_>>();
                    let ps = ps.iter().map(|p| p.s(l)).collect::<Vec<_>>();
                    Geometry::comp(*axis, cs, ps)
                }
            }
        }
    }

    pub fn t(&self, l: Level) -> Self {
        match self {
            Geometry::Prim(prim_id, unit) => match unit.t(l) {
                UnitSlice::PrimSection(g) => g,
                UnitSlice::IdSection(u) => Geometry::Prim(*prim_id, u),
            },
            Geometry::Comp(axis, cs, ps) => {
                if *axis == self.dim().space - 1 {
                    cs.last().unwrap().t(l)
                } else {
                    let cs = cs.iter().map(|c| c.t(l)).collect::<Vec<_>>();
                    let ps = ps.iter().map(|p| p.t(l)).collect::<Vec<_>>();
                    Geometry::comp(*axis, cs, ps)
                }
            }
        }
    }

    pub fn comp(axis: Level, cs: Vec<Geometry>, ps: Vec<Geometry>) -> Self {
        unimplemented!()
    }

    pub fn bridge(f0: Geometry, f1: Geometry, w: u32) -> Self {
        unimplemented!()
    }

    pub fn lerp(b0: &Geometry, b1: &Geometry, a: Q) -> Self {
        unimplemented!()
    }
}

*/
