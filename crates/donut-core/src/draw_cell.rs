use crate::common::*;
use crate::render_cell::*;
use donut_util::println;
use std::fmt;

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

impl DrawCell {
    pub fn render(&self) -> RenderCell {
        let d = self.1.dim.in_space;
        match self.0.as_ref() {
            RawCell::Prim(prim, shape, cube) => {
                let cur_d = d - cube.mins.len() as Level;
                let mut rc = RenderCell::new(cur_d);
                match shape {
                    Shape::Zero => {
                        rc.add_point(prim.clone(), vec![]);
                    }
                    Shape::Succ {
                        source,
                        center,
                        target,
                    } => {
                        let min = self.1.cube.mins[(cur_d - 1) as usize];
                        let max = self.1.cube.maxs[(cur_d - 1) as usize];
                        let s = source.1;
                        let t = target.1;
                        let c = (s + t) / 2;
                        let mins = (source.0).1.cube.mins.clone();
                        let maxs = (target.0).1.cube.maxs.clone();
                        let mut source = source.0.render();
                        let mut target = target.0.render();

                        let mut source_shrink = source.shifted(&s, &c);
                        let mut target_shrink = target.shifted(&c, &t);
                        source_shrink.shrink_face(Side::Target, center, &mins, &maxs);
                        target_shrink.shrink_face(Side::Source, center, &mins, &maxs);
                        rc.merge(source_shrink);
                        rc.merge(target_shrink);

                        if min < s {
                            source.shift(&min, &s);
                            rc.merge(source);
                        }
                        if t < max {
                            target.shift(&t, &max);
                            rc.merge(target);
                        }

                        let mut center = center.clone();
                        center.push(c);
                        rc.add_point(prim.clone(), center);
                    }
                }
                for (s, t) in cube.mins.iter().zip(cube.maxs.iter()) {
                    rc.shift(s, t);
                }
                rc
            }
            RawCell::Comp(_, children) => {
                let mut rc = RenderCell::new(d);
                for child in children {
                    rc.merge(child.render());
                }
                rc
            }
        }
    }
}

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
                    write!(f, "E {} -> Prim {:?}", cube_to_string(cube), prim.id)?;
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
