use crate::parse::parse_program;
use crate::types::{Arg, Cell, CellType, Decl, Lit};
use donut_core::cell::{Diagram, Globular, check_prim};
use donut_core::common::{Prim, PrimId};
use donut_core::free_cell::FreeCell;
use std::collections::HashMap;

type Result<T> = std::result::Result<T, String>;

#[derive(Debug, Clone)]
pub enum Ty {
    Zero,
    Succ(FreeCell, FreeCell),
}

#[derive(Debug, Clone)]
pub struct Element {
    pub color: (u8, u8, u8),
    pub name: String,
    pub level: u8,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct Table {
    pub elements: Vec<Element>,
    pub lookup: HashMap<String, usize>,
}

impl Table {
    pub fn new() -> Self {
        Table {
            elements: Vec::new(),
            lookup: HashMap::new(),
        }
    }

    fn add(&mut self, e: Element) -> Prim {
        let index = self.elements.len();
        self.lookup.insert(e.name.clone(), index);
        self.elements.push(e);
        Prim::new(index as PrimId)
    }

    fn color(&self, index: usize, dim: u8) -> (u8, u8, u8) {
        let pi = std::f64::consts::PI;
        let hue = (index as f64 * 137.5) % 360.0 * (pi / 180.0);
        let color = (hue, hue + 2.0 * pi / 3.0, hue + 4.0 * pi / 3.0);
        let color = (
            color.0.sin() * 0.5 + 0.5,
            color.1.sin() * 0.5 + 0.5,
            color.2.sin() * 0.5 + 0.5,
        );
        (
            (color.0 * 255.0) as u8,
            (color.1 * 255.0) as u8,
            (color.2 * 255.0) as u8,
        )
    }

    fn nat(&self, a: &Arg) -> Result<u64> {
        match a {
            Arg::Lit(Lit::Nat(n)) => Ok(*n),
            _ => Err("expected natural literal".to_string()),
        }
    }

    fn match_ty(&self, x: &Ty, y: &Ty) -> Result<()> {
        match (x, y) {
            (Ty::Zero, Ty::Zero) => Ok(()),
            (Ty::Succ(xs, xt), Ty::Succ(ys, yt)) => {
                if !xs.pure.is_convertible(&ys.pure) {
                    return Err("type mismatch in arrow source".to_string());
                }
                if !xt.pure.is_convertible(&yt.pure) {
                    return Err("type mismatch in arrow target".to_string());
                }
                Ok(())
            }
            _ => Err("type mismatch".to_string()),
        }
    }

    fn decl(&mut self, d: &Decl) -> Result<()> {
        let names = d.names.clone();
        if names.is_empty() {
            return Err("declaration must have at least one name".to_string());
        }
        let mut color = None;
        for deco in &d.decos {
            if deco.name == "color" {
                if color.is_some() {
                    // return Err("duplicate color decorator".to_string());
                }
                if deco.args.len() != 3 {
                    return Err("color decorator requires three arguments".to_string());
                }
                let r = self.nat(&deco.args[0])? as u8;
                let g = self.nat(&deco.args[1])? as u8;
                let b = self.nat(&deco.args[2])? as u8;
                color = Some((r, g, b));
            }
        }
        let ty = d.ty.as_ref().map(|c| self.ty(c)).transpose()?;
        let body = d.body.as_ref().map(|c| self.cell(c)).transpose()?;
        match (ty, body) {
            (None, None) => {
                return Err("declaration must have either a type or a body".to_string());
            }
            (Some((dim, ty)), None) => {
                for name in names {
                    let color = color
                        .clone()
                        .unwrap_or_else(|| self.color(self.elements.len(), dim));
                    self.add(Element {
                        color,
                        name,
                        level: dim,
                        ty: ty.clone(),
                    });
                }
            }
            (dim_ty, Some(body)) => {
                if names.len() > 1 {
                    return Err("cannot declare multiple names with a body".to_string());
                }
                let name = names.into_iter().next().unwrap();

                let dim = body.pure.dim().in_space;
                let ty = if dim == 0 {
                    Ty::Zero
                } else {
                    let s = FreeCell::from_pure(&body.pure.s());
                    let t = FreeCell::from_pure(&body.pure.t());
                    Ty::Succ(s, t)
                };

                let (dim, ty) = match dim_ty {
                    Some(dt) => {
                        if dt.0 != dim {
                            return Err("declared type dimension does not match body".to_string());
                        }
                        self.match_ty(&dt.1, &ty)?;
                        dt
                    }
                    None => (dim, ty),
                };

                let color = color.unwrap_or_else(|| self.color(self.elements.len(), dim));
                let def_name = format!("{}.def", name);
                let prim = self.add(Element {
                    color,
                    name,
                    level: dim,
                    ty: ty.clone(),
                });
                let c = match &ty {
                    Ty::Zero => FreeCell::zero(prim),
                    Ty::Succ(s, t) => FreeCell::prim(prim, s.clone(), t.clone()).unwrap(),
                };
                self.add(Element {
                    color,
                    name: def_name,
                    level: dim + 1,
                    ty: Ty::Succ(c, body),
                });
            }
        }
        Ok(())
    }

    fn ty(&self, c: &CellType) -> Result<(u8, Ty)> {
        match c {
            CellType::Star => Ok((0u8, Ty::Zero)),
            CellType::Arr(s, t) => {
                let mut s = self.cell(s)?;
                let mut t = self.cell(t)?;
                let mut sd = s.pure.dim().in_space;
                let mut td = t.pure.dim().in_space;
                if sd < td {
                    while sd < td {
                        s = FreeCell::id(s);
                        sd += 1;
                    }
                } else {
                    while td < sd {
                        t = FreeCell::id(t);
                        td += 1;
                    }
                }
                check_prim(&s.pure, &t.pure)?;
                let d = s.pure.dim().in_space + 1;
                Ok((d, Ty::Succ(s, t)))
            }
        }
    }

    fn cell(&self, c: &Cell) -> Result<FreeCell> {
        match c {
            Cell::Var(name, args) => {
                if !args.is_empty() {
                    return Err("variable arguments not implemented".to_string());
                }
                let index = match self.lookup.get(name) {
                    Some(i) => *i,
                    None => {
                        return Err(format!("unknown variable: {}", name));
                    }
                };
                let e = &self.elements[index];
                let prim = Prim::new(index as PrimId);
                match &e.ty {
                    Ty::Zero => Ok(FreeCell::zero(prim)),
                    Ty::Succ(s, t) => Ok(FreeCell::prim(prim, s.clone(), t.clone())?),
                }
            }
            Cell::Comp(axis, children) => {
                let mut children = children
                    .iter()
                    .map(|child| self.cell(child))
                    .collect::<Result<Vec<_>>>()?;
                let dim = children
                    .iter()
                    .map(|c| c.pure.dim().in_space)
                    .max()
                    .unwrap_or(0);
                for child in &mut children {
                    let mut cd = child.pure.dim().in_space;
                    while cd < dim {
                        *child = FreeCell::id(child.clone());
                        cd += 1;
                    }
                }
                Ok(FreeCell::comp(*axis, children)?)
            }
        }
    }
}

pub fn load(input: &str) -> Result<Table> {
    let decls = parse_program(input)?;
    let mut table = Table::new();
    for decl in &decls {
        table
            .decl(decl)
            .map_err(|e| format!("in declaration {:?}: {}", decl.names, e))?;
    }
    Ok(table)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_load() {
        let input = r#"
            u: *
            x: u → u
            m: x x → x
            assoc: x m; m → m x; m
            rm = x m; m
            lm = m x; m
            assoc2: rm → lm
        "#;
        let table = load(input).unwrap();
        eprintln!("{:#?}", table);
        // assert!(false);
    }
}
