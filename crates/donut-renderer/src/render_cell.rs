use donut_core::common::*;
use donut_core::render_cell as q;

pub type R = f64;
pub type CoordR = Vec<R>;

fn to_f64(q: &Q) -> R {
    let numer = *q.numer() as f64;
    let denom = *q.denom() as f64;
    numer / denom * 64.0 // TODO
}

fn lerp(a: R, b: R, t: R) -> R {
    a * (1.0 - t) + b * t
}

fn lerp_v(a: &CoordR, b: &CoordR, t: R) -> CoordR {
    assert_eq!(a.len(), b.len());
    let mut v = vec![];
    for i in 0..a.len() {
        v.push(lerp(a[i], b[i], t));
    }
    v
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
pub struct RenderCell {
    pub cubes: Vec<Vec<(Prim, Cuboid)>>,
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
                let st = match st {
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
                };
                let tt = match tt {
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
                };
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

    fn sliced(&self, x: R) -> Option<Self> {
        match self {
            Cuboid::Point(_) => None,
            Cuboid::Bridge { source, target } => {
                if x < source.1 || x > target.1 {
                    return None;
                }
                let t = (x - source.1) / (target.1 - source.1);
                Some(Cuboid::lerp(&source.0, &target.0, t))
            }
        }
    }
}

impl RenderCell {
    pub fn from(cell: &q::RenderCell) -> Self {
        let mut cubes = vec![];
        for qcs in &cell.cubes {
            let mut rcs = vec![];
            for (prim, c) in qcs {
                let c = Cuboid::from(c).0;
                rcs.push((prim.clone(), c));
            }
            cubes.push(rcs);
        }
        RenderCell { cubes }
    }

    pub fn sliced(&self, x: R) -> Self {
        let mut css = vec![];
        for cubes in self.cubes.iter() {
            let cs = cubes
                .iter()
                .filter_map(|(prim, cube)| {
                    let cube = cube.sliced(x)?;
                    Some((prim.clone(), cube))
                })
                .collect();
            css.push(cs);
        }
        css.pop();
        Self { cubes: css }
    }
}
