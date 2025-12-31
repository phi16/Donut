use crate::common::*;

fn shrink_q(v: &mut Q, center: &Q, min: &Q, max: &Q) -> bool {
    if v == min || v == max {
        false
    } else {
        *v = center.clone();
        true
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tangent {
    Perp,
    Shrink,
}

#[derive(Debug, Clone)]
pub enum Cuboid {
    Point(CoordQ),
    Bridge {
        source: (Box<Cuboid>, Q, Tangent),
        target: (Box<Cuboid>, Q, Tangent),
    },
}

impl Cuboid {
    fn shift(&mut self, s: &Q, t: &Q) {
        let mut face = Cuboid::Point(vec![]);
        std::mem::swap(self, &mut face);
        *self = Cuboid::Bridge {
            source: (Box::new(face.clone()), s.clone(), Tangent::Perp),
            target: (Box::new(face), t.clone(), Tangent::Perp),
        };
    }

    fn shrink(&mut self, center: &[Q], mins: &[Q], maxs: &[Q]) {
        match self {
            Cuboid::Point(p) => {
                assert_eq!(p.len(), center.len());
                for i in 0..p.len() {
                    shrink_q(&mut p[i], &center[i], &mins[i], &maxs[i]);
                }
            }
            Cuboid::Bridge { source, target } => {
                let min = mins.last().unwrap();
                let max = maxs.last().unwrap();
                let c = center.last().unwrap();
                let n = center.len() - 1;
                if shrink_q(&mut source.1, c, min, max) {
                    source.0.shrink(&center[..n], &mins[..n], &maxs[..n]);
                    source.2 = Tangent::Shrink;
                }
                if shrink_q(&mut target.1, c, min, max) {
                    target.0.shrink(&center[..n], &mins[..n], &maxs[..n]);
                    target.2 = Tangent::Shrink;
                }
            }
        }
    }

    fn shrink_face(&mut self, side: Side, center: &CoordQ, mins: &CoordQ, maxs: &CoordQ) {
        assert_eq!(center.len(), mins.len());
        assert_eq!(center.len(), maxs.len());
        match self {
            Cuboid::Point(_) => unreachable!(),
            Cuboid::Bridge { source, target } => match side {
                Side::Source => {
                    source.0.shrink(center, mins, maxs);
                    source.2 = Tangent::Shrink;
                }
                Side::Target => {
                    target.0.shrink(center, mins, maxs);
                    target.2 = Tangent::Shrink;
                }
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct RenderCell {
    pub cubes: Vec<Vec<(Prim, Cuboid)>>,
}

impl RenderCell {
    pub fn new(dim: Level) -> Self {
        Self {
            cubes: vec![vec![]; (dim + 1) as usize],
        }
    }

    pub fn merge(&mut self, other: RenderCell) {
        assert_eq!(self.cubes.len(), other.cubes.len());
        for (i, mut cs) in other.cubes.into_iter().enumerate() {
            self.cubes[i].append(&mut cs);
        }
    }

    pub fn add_point(&mut self, prim: Prim, p: CoordQ) {
        self.cubes
            .last_mut()
            .unwrap()
            .push((prim, Cuboid::Point(p)));
    }

    pub fn shift(&mut self, s: &Q, t: &Q) {
        for cs in &mut self.cubes {
            for (_, c) in cs {
                c.shift(s, t);
            }
        }
        self.cubes.push(vec![]);
    }

    pub fn shifted(&self, s: &Q, t: &Q) -> Self {
        let mut rc = self.clone();
        rc.shift(s, t);
        rc
    }

    pub fn shrink_face(&mut self, side: Side, center: &CoordQ, mins: &CoordQ, maxs: &CoordQ) {
        assert_eq!(center.len(), mins.len());
        assert_eq!(center.len(), maxs.len());
        for cs in &mut self.cubes {
            for (_, c) in cs {
                c.shrink_face(side, center, mins, maxs);
            }
        }
    }
}
