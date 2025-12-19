use donut_core::common::{Level, Prim};
use donut_core::draw_cell;
use donut_core::draw_cell::{DrawCell, Shape, X};

pub type R = f64;

#[derive(Debug, Clone)]
pub enum Cube {
    Point(Vec<R>),
    Bridge {
        source: (Box<Cube>, R),
        target: (Box<Cube>, R),
    },
}

fn lerp(a: R, b: R, t: R) -> R {
    a + (b - a) * t
}

impl Cube {
    pub fn from_cube(cube: &draw_cell::Cube) -> Self {
        fn to_real(x: &X) -> R {
            let q = x.eval();
            *q.numer() as f64 / *q.denom() as f64
        }
        match cube {
            draw_cell::Cube::Point(p) => {
                let coords = p.iter().map(to_real).collect();
                Cube::Point(coords)
            }
            draw_cell::Cube::Bridge { source, target } => {
                let source_cube = Box::new(Cube::from_cube(&source.0));
                let target_cube = Box::new(Cube::from_cube(&target.0));
                Cube::Bridge {
                    source: (source_cube, to_real(&source.1)),
                    target: (target_cube, to_real(&target.1)),
                }
            }
        }
    }

    fn lerp(a: &Cube, b: &Cube, t: R) -> Cube {
        match (a, b) {
            (Cube::Point(ac), Cube::Point(bc)) => {
                let coords = ac
                    .iter()
                    .zip(bc.iter())
                    .map(|(a, b)| lerp(*a, *b, t))
                    .collect();
                Cube::Point(coords)
            }
            (
                Cube::Bridge {
                    source: (afs, axs),
                    target: (aft, axt),
                },
                Cube::Bridge {
                    source: (bfs, bxs),
                    target: (bft, bxt),
                },
            ) => {
                let source = Box::new(Cube::lerp(afs, bfs, t));
                let target = Box::new(Cube::lerp(aft, bft, t));
                let s = lerp(*axs, *bxs, t);
                let e = lerp(*axt, *bxt, t);
                Cube::Bridge {
                    source: (source, s),
                    target: (target, e),
                }
            }
            _ => unreachable!(),
        }
    }

    fn sliced(&self, x: R) -> Option<Self> {
        match self {
            Cube::Point(_) => None,
            Cube::Bridge { source, target } => {
                if x < source.1 || x > target.1 {
                    return None;
                }
                let t = (x - source.1) / (target.1 - source.1);
                Some(Cube::lerp(&source.0, &target.0, t))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct RenderCell {
    pub cubes: Vec<Vec<(Prim, Cube)>>,
}

impl RenderCell {
    pub fn new(dim: Level) -> Self {
        Self {
            cubes: vec![vec![]; (dim + 1) as usize],
        }
    }

    pub fn from_draw_cell(draw_cell: &DrawCell) -> RenderCell {
        fn go(draw_cell: &DrawCell, rc: &mut RenderCell) {
            match draw_cell {
                DrawCell::Prim(prim, shape, cube, dim) => {
                    match shape {
                        Shape::Zero => {}
                        Shape::Succ { source, target } => {
                            go(source, rc);
                            go(target, rc);
                        }
                    }
                    let cube = Cube::from_cube(cube);
                    let dim = dim.effective;
                    rc.cubes[dim as usize].push((prim.clone(), cube));
                }
                DrawCell::Comp(_, children, _) => {
                    for child in children {
                        go(child, rc);
                    }
                }
            }
        }
        let d = draw_cell.dim().in_space;
        let mut cubes = RenderCell::new(d);
        go(draw_cell, &mut cubes);
        cubes
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
        Self { cubes: css }
    }
}
