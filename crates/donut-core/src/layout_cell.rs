use core::fmt;

use crate::common::*;

#[derive(Debug, Clone)]
pub enum Shape {
    Zero,
    Succ {
        source_extend: N,
        source: LayoutCell,
        width: N,
        target: LayoutCell,
        target_extend: N,
    },
}

#[derive(Debug, Clone)]
pub enum RawCell {
    Prim(Prim, Shape),
    Id(LayoutCell, N),
    Comp(Axis, Vec2<LayoutCell>),
}

#[derive(Debug, Clone)]
pub struct Layout {
    pub dim: Dim,
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
                            source_extend,
                            source,
                            width,
                            target,
                            target_extend,
                        } => {
                            writeln!(f, " [{}-{}-{}] {{", source_extend, width, target_extend)?;
                            go(source, f, indent + 2)?;
                            writeln!(f, ",")?;
                            go(target, f, indent + 2)?;
                            writeln!(f, "")?;
                            write!(f, "{}}}", pad)?;
                        }
                    }
                }
                RawCell::Id(cell, width) => {
                    writeln!(f, "I[{}] {{", width)?;
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
        go(self, f, 0);
        writeln!(f, "")
    }
}
