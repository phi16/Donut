use crate::common::*;
use std::fmt;

/* type X = crate::lins::X;

#[derive(Debug, Clone, Copy)]
pub enum Tangent {
    Perp,
    Shrink,
}

#[derive(Debug, Clone)]
pub enum Cuboid {
    Point(Vec<X>),
    Bridge {
        source: (Box<Cuboid>, X, Tangent),
        target: (Box<Cuboid>, X, Tangent),
    },
}

impl Cuboid {
    fn s(&self) -> Self {
        match self {
            Cuboid::Point(_) => unreachable!(),
            Cuboid::Bridge { source, .. } => source.0.as_ref().clone(),
        }
    }
    fn t(&self) -> Self {
        match self {
            Cuboid::Point(_) => unreachable!(),
            Cuboid::Bridge { target, .. } => target.0.as_ref().clone(),
        }
    }

    fn shift(&mut self, s: &X, t: &X) {
        let mut face = Cuboid::Point(vec![]);
        std::mem::swap(self, &mut face);
        *self = Cuboid::Bridge {
            source: (Box::new(face.clone()), s.clone(), Tangent::Perp),
            target: (Box::new(face), t.clone(), Tangent::Perp),
        };
    }
} */

#[derive(Debug, Clone)]
pub enum Shape {
    Zero,
    Succ {
        source: (DrawCell, Q),
        center: CoordQ,
        target: (DrawCell, Q),
    },
}

#[derive(Debug, Clone)]
pub struct Cube {
    pub mins: CoordQ,
    pub maxs: CoordQ,
}

#[derive(Debug, Clone)]
pub enum RawCell {
    Prim(Prim, Shape, Cube),
    Comp(Axis, Vec2<DrawCell>),
}

#[derive(Debug, Clone)]
pub struct Layout {
    pub dim: Dim,
    pub cube: Cube,
}

#[derive(Debug, Clone)]
pub struct DrawCell(pub Box<RawCell>, pub Layout);

impl fmt::Display for DrawCell {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn q(q: &Q) -> String {
            let numer = *q.numer();
            let denom = *q.denom();
            if denom == 1 {
                format!("{:?}", numer)
            } else {
                format!("{:?}/{:?}", numer, denom)
            }
        }
        fn coord_to_string(coord: &CoordQ) -> String {
            let mut s = String::new();
            s.push('[');
            for (i, c) in coord.iter().enumerate() {
                if i > 0 {
                    s.push_str(", ");
                }
                s.push_str(&q(c));
            }
            s.push(']');
            s
        }
        fn cube_to_string(cube: &Cube) -> String {
            format!(
                "{}-{}",
                coord_to_string(&cube.mins),
                coord_to_string(&cube.maxs)
            )
        }
        fn go(cell: &DrawCell, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
            let pad = " ".repeat(indent);
            write!(
                f,
                "{}({}/{}){}: ",
                pad,
                cell.1.dim.effective,
                cell.1.dim.in_space,
                cube_to_string(&cell.1.cube)
            )?;
            match cell.0.as_ref() {
                RawCell::Prim(prim, shape, cube) => {
                    write!(f, "Prim {:?} {}", prim.id, cube_to_string(cube))?;
                    match shape {
                        Shape::Zero => {}
                        Shape::Succ {
                            source,
                            center,
                            target,
                        } => {
                            writeln!(
                                f,
                                " [{}-{}-{}] {{",
                                source.1,
                                coord_to_string(center),
                                target.1
                            )?;
                            go(&source.0, f, indent + 2)?;
                            writeln!(f, ",")?;
                            go(&target.0, f, indent + 2)?;
                            writeln!(f, "")?;
                            write!(f, "{}}}", pad)?;
                        }
                    }
                }
                RawCell::Comp(axis, children) => {
                    writeln!(f, "Comp<{:?}> {{", axis)?;
                    for child in children.iter() {
                        go(child, f, indent + 2)?;
                        writeln!(f, ",")?;
                    }
                    write!(f, "{}}}", pad)?;
                }
            }
            Ok(())
        }
        go(self, f, 0)?;
        writeln!(f, "")
    }
}
