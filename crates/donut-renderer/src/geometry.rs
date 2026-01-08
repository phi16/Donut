use donut_core::common::*;
use donut_layout::geometry as q;
use donut_layout::geometry::Tangent;

pub type R = f64;
pub type CoordR = Vec<R>;

fn to_f64(q: &Q) -> R {
    let numer = *q.numer() as f64;
    let denom = *q.denom() as f64;
    numer / denom * 100.0 // TODO
}

fn lerp(a: R, b: R, t: R) -> R {
    a * (1.0 - t) + b * t
}

fn bezier(x0: R, t0: R, x1: R, t1: R, t: R) -> R {
    let a = x0;
    let b = lerp(x0, x1, t0);
    let c = lerp(x1, x0, t1);
    let d = x1;
    let x = lerp(a, b, t);
    let y = lerp(b, c, t);
    let z = lerp(c, d, t);
    let u = lerp(x, y, t);
    let v = lerp(y, z, t);
    lerp(u, v, t)
}

fn inverse_bezier(x0: R, t0: R, x1: R, t1: R, x: R) -> R {
    let mut a = 0.0;
    let mut b = 1.0;
    for _ in 0..20 {
        let m = (a + b) / 2.0;
        let xm = bezier(x0, t0, x1, t1, m);
        if xm < x {
            a = m;
        } else {
            b = m;
        }
    }
    (a + b) / 2.0
}

fn lerp_v(a: &CoordR, b: &CoordR, t: R) -> CoordR {
    assert_eq!(a.len(), b.len());
    let mut v = vec![];
    for i in 0..a.len() {
        v.push(lerp(a[i], b[i], t));
    }
    v
}

fn from_tangent(t: Tangent, d: Level) -> CoordR {
    match t {
        q::Tangent::Perp => {
            let mut v = vec![0.0; d as usize];
            v.push(0.5);
            v
        }
        q::Tangent::Shrink => {
            let mut v = vec![1.0; d as usize];
            v.push(0.0);
            v
        }
    }
}

#[derive(Debug, Clone)]
pub enum Cuboid {
    Point(Vec<R>),
    Bridge {
        source: (Box<Cuboid>, R, CoordR),
        target: (Box<Cuboid>, R, CoordR),
    },
}

#[derive(Debug, Clone)]
pub struct Geometry {
    pub cubes: Vec<Vec<(Prim, Cuboid)>>,
    pub spheres: Vec<(Prim, CoordR, R)>,
}

impl Cuboid {
    fn from(c: &q::Cuboid) -> (Self, Level) {
        match c {
            q::Cuboid::Point(p) => {
                let d = p.len() as Level;
                let p = p.iter().map(to_f64).collect::<Vec<_>>();
                (Cuboid::Point(p), d)
            }
            q::Cuboid::Bridge {
                source: (s, sq, st),
                target: (t, tq, tt),
            } => {
                let (s, sd) = Cuboid::from(s);
                let (t, td) = Cuboid::from(t);
                assert_eq!(sd, td);
                let d = sd;
                let st = from_tangent(*st, d);
                let tt = from_tangent(*tt, d);
                let c = Cuboid::Bridge {
                    source: (Box::new(s), to_f64(sq), st),
                    target: (Box::new(t), to_f64(tq), tt),
                };
                (c, d + 1)
            }
        }
    }

    fn lerp(a: &Cuboid, b: &Cuboid, t: R) -> Cuboid {
        match (a, b) {
            (Cuboid::Point(pa), Cuboid::Point(pb)) => {
                assert_eq!(pa.len(), pb.len());
                let mut p = vec![];
                for i in 0..pa.len() {
                    p.push(lerp(pa[i], pb[i], t));
                }
                Cuboid::Point(p)
            }
            (
                Cuboid::Bridge {
                    source: sa,
                    target: ta,
                },
                Cuboid::Bridge {
                    source: sb,
                    target: tb,
                },
            ) => {
                let source = (
                    Box::new(Cuboid::lerp(&sa.0, &sb.0, t)),
                    lerp(sa.1, sb.1, t),
                    lerp_v(&sa.2, &sb.2, t),
                );
                let target = (
                    Box::new(Cuboid::lerp(&ta.0, &tb.0, t)),
                    lerp(ta.1, tb.1, t),
                    lerp_v(&ta.2, &tb.2, t),
                );
                Cuboid::Bridge { source, target }
            }
            _ => unreachable!(),
        }
    }

    fn sliced(&self, x: R, prim: &Prim, spheres: &mut Vec<(Prim, CoordR, R)>) -> Option<Self> {
        match self {
            Cuboid::Point(center) => {
                let c = center.last().unwrap();
                let d = *c - x;
                let r = 16.0;
                let r2 = r * r;
                if d * d < r2 {
                    let n = center.len() - 1;
                    spheres.push((prim.clone(), center[..n].to_vec(), r2 - d * d));
                }
                None
            }
            Cuboid::Bridge { source, target } => {
                if x < source.1 || x > target.1 {
                    return None;
                }
                let t = inverse_bezier(
                    source.1,
                    *source.2.last().unwrap(),
                    target.1,
                    *target.2.last().unwrap(),
                    x,
                );
                let u = bezier(
                    0.0,
                    *source.2.first().unwrap(),
                    1.0,
                    *target.2.first().unwrap(),
                    t,
                );
                Some(Cuboid::lerp(&source.0, &target.0, u))
            }
        }
    }

    fn squashed(&self) -> Self {
        match self {
            Cuboid::Point(center) => {
                assert!(center.len() >= 1);
                let center = center.iter().skip(1).cloned().collect::<Vec<_>>();
                Cuboid::Point(center)
            }
            Cuboid::Bridge { source, target } => {
                let source = (
                    Box::new(source.0.squashed()),
                    source.1,
                    source.2.iter().skip(1).cloned().collect(),
                );
                let target = (
                    Box::new(target.0.squashed()),
                    target.1,
                    target.2.iter().skip(1).cloned().collect(),
                );
                Cuboid::Bridge { source, target }
            }
        }
    }
}

impl Geometry {
    pub fn from(cell: &q::Geometry) -> Self {
        let mut cubes = vec![];
        for qcs in &cell.cubes {
            let mut rcs = vec![];
            for (prim, c) in qcs {
                let c = Cuboid::from(c).0;
                rcs.push((prim.clone(), c));
            }
            cubes.push(rcs);
        }
        let spheres = vec![];
        Geometry { cubes, spheres }
    }

    pub fn sliced(&self, x: R) -> Self {
        let mut css = vec![];
        let mut spheres = vec![];
        for (prim, center, r2) in &self.spheres {
            let c = center.last().unwrap();
            let d = *c - x;
            let mut center = center.clone();
            center.pop();
            if d * d < *r2 {
                spheres.push((prim.clone(), center, *r2 - d * d));
            }
        }
        for cubes in &self.cubes {
            let cs = cubes
                .iter()
                .filter_map(|(prim, cube)| {
                    let cube = cube.sliced(x, prim, &mut spheres)?;
                    Some((prim.clone(), cube))
                })
                .collect();
            css.push(cs);
        }
        css.pop();
        Self {
            cubes: css,
            spheres,
        }
    }

    pub fn squashed(&self) -> Self {
        let mut css = vec![];
        let mut spheres = vec![];
        for (prim, center, r2) in &self.spheres {
            let center = center.iter().skip(1).cloned().collect::<Vec<_>>();
            spheres.push((prim.clone(), center, *r2));
        }
        for cubes in self.cubes.iter().skip(1) {
            let cs = cubes
                .iter()
                .filter_map(|(prim, cube)| {
                    let cube = cube.squashed();
                    Some((prim.clone(), cube))
                })
                .collect();
            css.push(cs);
        }
        Self {
            cubes: css,
            spheres,
        }
    }
}
