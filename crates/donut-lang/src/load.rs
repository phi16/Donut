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
    pub name: String,
    pub color: (u8, u8, u8),
    pub cell: FreeCell,
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

    fn rat(&self, a: &Arg) -> Result<f64> {
        match a {
            Arg::Lit(Lit::Rat(r)) => Ok(*r),
            Arg::Lit(Lit::Nat(n)) => Ok(*n as f64),
            _ => Err("expected rational literal".to_string()),
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
            if deco.name == "rgb" {
                if deco.args.len() != 3 {
                    return Err("rgb decorator requires three arguments".to_string());
                }
                let r = self.nat(&deco.args[0])? as u8;
                let g = self.nat(&deco.args[1])? as u8;
                let b = self.nat(&deco.args[2])? as u8;
                color = Some((r, g, b));
            } else if deco.name == "hsv" {
                if deco.args.len() != 3 {
                    return Err("hsv decorator requires three arguments".to_string());
                }
                let h = self.rat(&deco.args[0])? as f64;
                let s = self.rat(&deco.args[1])? as f64;
                let v = self.rat(&deco.args[2])? as f64;
                fn hsv2rgb(h: f64, s: f64, v: f64) -> (u8, u8, u8) {
                    let r = (((h + 3.0 / 3.0).fract() * 6.0 - 3.0).abs() - 1.0).clamp(0.0, 1.0);
                    let g = (((h + 2.0 / 3.0).fract() * 6.0 - 3.0).abs() - 1.0).clamp(0.0, 1.0);
                    let b = (((h + 1.0 / 3.0).fract() * 6.0 - 3.0).abs() - 1.0).clamp(0.0, 1.0);
                    let r = (r * s + (1.0 - s)) * v;
                    let g = (g * s + (1.0 - s)) * v;
                    let b = (b * s + (1.0 - s)) * v;
                    ((r * 255.0) as u8, (g * 255.0) as u8, (b * 255.0) as u8)
                }
                color = Some(hsv2rgb(h, s, v));
            } else if deco.name == "gray" {
                if deco.args.len() != 1 {
                    return Err("gray decorator requires one argument".to_string());
                }
                let g = self.nat(&deco.args[0])? as u8;
                color = Some((g, g, g));
            } /* else {
            return Err(format!("unknown decorator: {}", deco.name));
            } */
        }
        let ty = d.ty.as_ref().map(|c| self.ty(c)).transpose()?;
        let body = d.body.as_ref().map(|c| self.cell(c)).transpose()?;
        match (ty, body) {
            (None, None) => {
                return Err("declaration must have either a type or a body".to_string());
            }
            (Some((dim, ty)), None) => {
                for name in names {
                    let index = self.elements.len();
                    let color = color.clone().unwrap_or_else(|| self.color(index, dim));
                    let prim = Prim::new(index as PrimId);
                    let cell = match &ty {
                        Ty::Zero => FreeCell::zero(prim),
                        Ty::Succ(s, t) => FreeCell::prim(prim, s.clone(), t.clone())?,
                    };
                    self.add(Element { name, color, cell });
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
                self.add(Element {
                    name,
                    color,
                    cell: body,
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
                Ok(e.cell.clone())
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

    #[test]
    fn test_load2() {
        let input = r#"
            u: *
            x: u → u
            m: x x → x
            a: m x; m → x m; m
            chl: (x m; m) x → x m x; m x
            chr: x m x; x m → x (m x; m)
            aaa =
                a x; m ;;
                chl; m ;;
                x m x; a ;;
                chr; m ;;
                x a; m
            ch0: m x x; x m → m m
            ch1: m m → x x m; m x

            kl: (m x; m) x → m x x; m x
            kr: x x m; x m → x (x m; m)
            oao =
                kl; m ;;
                m x x; a ;;
                (ch0 ;; ch1); m ;;
                x x m; a ;;
                kr; m

            pentagon: aaa → oao
        "#;
        let table = load(input).unwrap();
        eprintln!("{:#?}", table);
    }
}
