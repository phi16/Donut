use core::fmt;

use crate::common::*;

#[derive(Debug, Clone)]
pub enum Shape {
    Zero,
    Succ {
        source_limit: N,
        source: LayoutCell,
        source_coord: N,
        coord: CoordQ,
        target_coord: N,
        target: LayoutCell,
        target_limit: N,
    },
}

#[derive(Debug, Clone)]
pub enum RawCell {
    Prim(Prim, Shape),
    Id(LayoutCell, N, N),
    Comp(Axis, Vec2<LayoutCell>),
}

#[derive(Debug, Clone)]
pub struct Layout {
    pub dim: Dim,
    pub origin: CoordN,
    pub size: CoordN,
}

#[derive(Debug, Clone)]
pub struct LayoutCell(pub Box<RawCell>, pub Layout);

impl fmt::Display for LayoutCell {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn go(cell: &LayoutCell, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
            let pad = " ".repeat(indent);
            write!(
                f,
                "{}({}/{}){:?}: ",
                pad, cell.1.dim.effective, cell.1.dim.in_space, cell.1.size
            )?;
            match cell.0.as_ref() {
                RawCell::Prim(prim, shape) => {
                    write!(f, "Prim {:?}", prim.id)?;
                    match shape {
                        Shape::Zero => {}
                        Shape::Succ {
                            source_limit,
                            source,
                            source_coord,
                            coord,
                            target_coord,
                            target,
                            target_limit,
                        } => {
                            fn coord_to_string(coord: &CoordQ) -> String {
                                let mut s = String::new();
                                s.push('[');
                                for (i, c) in coord.iter().enumerate() {
                                    if i > 0 {
                                        s.push_str(", ");
                                    }
                                    let numer = *c.numer();
                                    let denom = *c.denom();
                                    if denom == 1 {
                                        s.push_str(&format!("{:?}", numer));
                                    } else {
                                        s.push_str(&format!("{:?}/{:?}", numer, denom));
                                    }
                                }
                                s.push(']');
                                s
                            }
                            writeln!(
                                f,
                                " [{}-{}-{}-{}-{}] {{",
                                source_limit,
                                source_coord,
                                coord_to_string(coord),
                                target_coord,
                                target_limit
                            )?;
                            go(source, f, indent + 2)?;
                            writeln!(f, ",")?;
                            go(target, f, indent + 2)?;
                            writeln!(f, "")?;
                            write!(f, "{}}}", pad)?;
                        }
                    }
                }
                RawCell::Id(cell, s, t) => {
                    writeln!(f, "I[{}-{}] {{", s, t)?;
                    go(cell, f, indent + 2)?;
                    writeln!(f, "")?;
                    write!(f, "{}}}", pad)?;
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
