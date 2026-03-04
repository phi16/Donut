use crate::check::{Item, ItemBody, ItemKind, Module};
use crate::types::common::*;
use crate::types::semtree;
use donut_core::cell::{check_prim, Diagram, Globular};
use donut_core::common::{Prim, PrimId};
use donut_core::free_cell::FreeCell;
use std::collections::HashMap;
use std::rc::Rc;

type Result<T> = std::result::Result<T, String>;

#[derive(Debug, Clone)]
enum Ty {
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
    prefixes: Vec<String>,
}

impl Table {
    fn new() -> Self {
        Table {
            elements: Vec::new(),
            lookup: HashMap::new(),
            prefixes: Vec::new(),
        }
    }

    fn resolve(&self, name: &str) -> Option<usize> {
        for prefix in self.prefixes.iter().rev() {
            let qualified = format!("{}.{}", prefix, name);
            if let Some(&idx) = self.lookup.get(&qualified) {
                return Some(idx);
            }
        }
        self.lookup.get(name).copied()
    }

    fn fresh_prim(&self) -> Prim {
        Prim::new(self.elements.len() as PrimId)
    }

    fn add(&mut self, name: String, color: Option<(u8, u8, u8)>, cell: FreeCell) {
        let color = color.unwrap_or_else(|| auto_color(self.elements.len()));
        self.lookup.insert(name.clone(), self.elements.len());
        self.elements.push(Element { name, color, cell });
    }

    fn load_module(&mut self, module: &Module) -> Result<()> {
        for (name, item) in &module.entries {
            self.load_item(name, item)?;
        }
        Ok(())
    }

    fn load_item(&mut self, name: &str, item: &Item) -> Result<()> {
        if matches!(item.kind, Some(ItemKind::Param)) {
            return Ok(());
        }

        let color = extract_color(&item.decos);

        match &item.body {
            ItemBody::Value { val, members } => {
                // Def: ignore body, use type only
                let body_val = if matches!(item.kind, Some(ItemKind::Def)) {
                    &None
                } else {
                    val
                };

                match (&item.ty, body_val) {
                    (Some(ty), None) => {
                        let (_, ty) = self.eval_ty(ty)?;
                        let prim = self.fresh_prim();
                        let cell = match &ty {
                            Ty::Zero => FreeCell::zero(prim),
                            Ty::Succ(s, t) => FreeCell::prim(prim, s.clone(), t.clone())?,
                        };
                        self.add(name.to_string(), color, cell);
                    }
                    (dim_ty, Some(body_rc)) => {
                        let body = self.eval_val(body_rc)?;
                        let dim = body.pure.dim().in_space;
                        let body_ty = if dim == 0 {
                            Ty::Zero
                        } else {
                            Ty::Succ(
                                FreeCell::from_pure(&body.pure.s()),
                                FreeCell::from_pure(&body.pure.t()),
                            )
                        };

                        if let Some(ty_rc) = dim_ty {
                            let (declared_dim, declared_ty) = self.eval_ty(ty_rc)?;
                            if declared_dim != dim {
                                return Err(
                                    "declared type dimension does not match body".to_string(),
                                );
                            }
                            match_ty(&declared_ty, &body_ty)?;
                        }

                        self.add(name.to_string(), color, body);
                    }
                    (None, None) => {}
                }

                if !members.entries.is_empty() {
                    self.load_members(name, members)?;
                }
            }
            ItemBody::Functor { .. } => {
                // TODO: functor declarations are not yet loaded as cells
            }
        }

        Ok(())
    }

    fn load_members(&mut self, prefix: &str, module: &Module) -> Result<()> {
        self.prefixes.push(prefix.to_string());
        for (member_name, item) in &module.entries {
            let full_name = format!("{}.{}", prefix, member_name);
            self.load_item(&full_name, item)?;
        }
        self.prefixes.pop();
        Ok(())
    }

    fn eval_ty(&self, val: &A<semtree::Val>) -> Result<(u8, Ty)> {
        let Some(v) = val.inner() else {
            return Err("invalid type expression".to_string());
        };
        match v {
            semtree::Val::Path(path_a) => {
                if path_name(path_a).as_deref() == Some("*") {
                    return Ok((0, Ty::Zero));
                }
                Err("expected `*` or arrow type".to_string())
            }
            semtree::Val::Op(l, op_a, _, r) => {
                if let Some(semtree::Op::Arrow(
                    semtree::ArrowTy::To | semtree::ArrowTy::Eq,
                )) = op_a.inner()
                {
                    let mut s = self.eval_val_inner(l)?;
                    let mut t = self.eval_val_inner(r)?;
                    let max = s.pure.dim().in_space.max(t.pure.dim().in_space);
                    s = lift_dim(s, max);
                    t = lift_dim(t, max);
                    check_prim(&s.pure, &t.pure)?;
                    Ok((max + 1, Ty::Succ(s, t)))
                } else {
                    Err("expected arrow type".to_string())
                }
            }
            _ => Err("expected `*` or arrow type".to_string()),
        }
    }

    fn eval_val(&self, val: &A<semtree::Val>) -> Result<FreeCell> {
        let Some(v) = val.inner() else {
            return Err("invalid value expression".to_string());
        };
        self.eval_val_inner(v)
    }

    fn eval_val_inner(&self, val: &semtree::Val) -> Result<FreeCell> {
        match val {
            semtree::Val::Path(path_a) => {
                let name = path_name(path_a).ok_or_else(|| "invalid path".to_string())?;
                let index = self
                    .resolve(&name)
                    .ok_or_else(|| format!("unknown variable: {}", name))?;
                Ok(self.elements[index].cell.clone())
            }
            semtree::Val::Op(l, op_a, _, r) => {
                let Some(op) = op_a.inner() else {
                    return Err("invalid operator".to_string());
                };
                match op {
                    semtree::Op::Comp(n) => {
                        let mut children = Vec::new();
                        collect_comp_children(l, *n, &mut children, self)?;
                        collect_comp_children(r, *n, &mut children, self)?;
                        let max = children
                            .iter()
                            .map(|c| c.pure.dim().in_space)
                            .max()
                            .unwrap();
                        let children = children
                            .into_iter()
                            .map(|c| lift_dim(c, max))
                            .collect();
                        Ok(FreeCell::comp(*n as u8, children)?)
                    }
                    _ => Err("unsupported operator in value".to_string()),
                }
            }
            semtree::Val::Lit(_) => Err("literal in value position".to_string()),
        }
    }
}

fn collect_comp_children(
    val: &semtree::Val,
    axis: u32,
    out: &mut Vec<FreeCell>,
    table: &Table,
) -> Result<()> {
    if let semtree::Val::Op(l, op_a, _, r) = val {
        if matches!(op_a.inner(), Some(semtree::Op::Comp(n)) if *n == axis) {
            collect_comp_children(l, axis, out, table)?;
            collect_comp_children(r, axis, out, table)?;
            return Ok(());
        }
    }
    out.push(table.eval_val_inner(val)?);
    Ok(())
}

fn match_ty(x: &Ty, y: &Ty) -> Result<()> {
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

fn auto_color(index: usize) -> (u8, u8, u8) {
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

fn lift_dim(mut cell: FreeCell, target: u8) -> FreeCell {
    while cell.pure.dim().in_space < target {
        cell = FreeCell::id(cell);
    }
    cell
}

fn path_name(path_a: &A<semtree::Path>) -> Option<String> {
    let path = path_a.inner()?;
    let parts: Option<Vec<_>> = path
        .0
        .iter()
        .map(|seg_a| Some(seg_a.inner()?.0.inner()?.0.clone()))
        .collect();
    let parts = parts?;
    if parts.is_empty() {
        return None;
    }
    Some(parts.join("."))
}

fn extract_color(decos: &[Rc<A<semtree::Val>>]) -> Option<(u8, u8, u8)> {
    for deco in decos {
        let Some(semtree::Val::Path(path_a)) = deco.inner() else {
            continue;
        };
        let Some(path) = path_a.inner() else {
            continue;
        };
        if path.0.len() != 1 {
            continue;
        }
        let Some(seg) = path.0[0].inner() else {
            continue;
        };
        let Some(name) = seg.0.inner() else {
            continue;
        };
        let args = &seg.1 .0;

        match name.0.as_str() {
            "gray" if args.len() == 1 => {
                let g = extract_number(&args[0])? as u8;
                return Some((g, g, g));
            }
            "rgb" if args.len() == 3 => {
                let r = extract_number(&args[0])? as u8;
                let g = extract_number(&args[1])? as u8;
                let b = extract_number(&args[2])? as u8;
                return Some((r, g, b));
            }
            "hsv" if !args.is_empty() => {
                let h = extract_rational(&args[0])?;
                let s = args.get(1).map_or(Some(1.0), extract_rational)?;
                let v = args.get(2).map_or(Some(1.0), extract_rational)?;
                return Some(hsv2rgb(h, s, v));
            }
            _ => {}
        }
    }
    None
}

fn extract_number(param_a: &A<semtree::ParamVal>) -> Option<f64> {
    let val = param_a.inner()?.val.inner()?;
    match val {
        semtree::Val::Lit(lit_a) => match lit_a.inner()? {
            semtree::Lit::Number(s) => s.parse().ok(),
            _ => None,
        },
        _ => None,
    }
}

fn extract_rational(param_a: &A<semtree::ParamVal>) -> Option<f64> {
    let val = param_a.inner()?.val.inner()?;
    match val {
        semtree::Val::Lit(lit_a) => match lit_a.inner()? {
            semtree::Lit::Number(s) => s.parse().ok(),
            semtree::Lit::String(s) => match s.as_str() {
                "1-" => Some(0.01),
                "1" => Some(0.05),
                "1+" => Some(0.09),
                "2+" => Some(0.52),
                "2" => Some(0.56),
                "2-" => Some(0.60),
                "orange" => Some(0.1),
                "yellow" => Some(0.16),
                "green" => Some(0.33),
                "cyan" => Some(0.5),
                "blue" => Some(0.6),
                "purple" => Some(0.75),
                "pink" => Some(0.9),
                _ => None,
            },
            _ => None,
        },
        // Dotted number path: 0.6 → Path([Seg("0"), Seg("6")])
        semtree::Val::Path(path_a) => {
            let path = path_a.inner()?;
            if path.1.is_some() {
                return None;
            }
            let parts: Option<Vec<_>> = path
                .0
                .iter()
                .map(|seg_a| {
                    let seg = seg_a.inner()?;
                    if !seg.1 .0.is_empty() {
                        return None;
                    }
                    let name = seg.0.inner()?;
                    crate::convert::is_number_str(&name.0).then_some(name.0.as_str())
                })
                .collect();
            parts?.join(".").parse().ok()
        }
        _ => None,
    }
}

fn hsv2rgb(h: f64, s: f64, v: f64) -> (u8, u8, u8) {
    let r = (((h + 3.0 / 3.0).fract() * 6.0 - 3.0).abs() - 1.0).clamp(0.0, 1.0);
    let g = (((h + 2.0 / 3.0).fract() * 6.0 - 3.0).abs() - 1.0).clamp(0.0, 1.0);
    let b = (((h + 1.0 / 3.0).fract() * 6.0 - 3.0).abs() - 1.0).clamp(0.0, 1.0);
    let r = (r * s + (1.0 - s)) * v;
    let g = (g * s + (1.0 - s)) * v;
    let b = (b * s + (1.0 - s)) * v;
    ((r * 255.0) as u8, (g * 255.0) as u8, (b * 255.0) as u8)
}

fn dedent(code: &str) -> String {
    let lines: Vec<&str> = code.lines().collect();
    let min_indent = lines
        .iter()
        .filter(|l| !l.trim().is_empty())
        .map(|l| l.len() - l.trim_start().len())
        .min()
        .unwrap_or(0);
    lines
        .iter()
        .map(|l| {
            if l.len() >= min_indent {
                &l[min_indent..]
            } else {
                l.trim()
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

pub fn load(code: &str) -> Result<Table> {
    let code = dedent(code.trim_matches('\n'));
    let (tokens, _, tok_errors) = crate::tokenize::tokenize(&code);
    if !tok_errors.is_empty() {
        return Err(format!("tokenize errors: {:?}", tok_errors));
    }
    let (program, parse_errors) = crate::parse::parse(&tokens);
    if !parse_errors.is_empty() {
        return Err(format!("parse errors: {:?}", parse_errors));
    }
    let (sem_prog, conv_errors) = crate::convert::convert(program, &tokens);
    if !conv_errors.is_empty() {
        return Err(format!("convert errors: {:?}", conv_errors));
    }
    let (module, check_errors) = crate::check::check(sem_prog, &tokens);
    if !check_errors.is_empty() {
        return Err(format!("check errors: {:?}", check_errors));
    }
    let mut table = Table::new();
    table.load_module(&module)?;
    Ok(table)
}
