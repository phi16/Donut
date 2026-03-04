use crate::check::{Item, ItemKind, Module};
use crate::types::common::*;
use crate::types::semtree;
use donut_core::cell::{check_prim, Diagram, Globular};
use donut_core::common::{Prim, PrimId};
use donut_core::free_cell::FreeCell;
use std::collections::HashMap;
use std::rc::Rc;

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
    fn new() -> Self {
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

    fn auto_color(&self, index: usize) -> (u8, u8, u8) {
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

        let color = self.extract_color(&item.decos);

        // Def: ignore body, use type only
        let (ty_val, body_val) = if matches!(item.kind, Some(ItemKind::Def)) {
            (&item.ty, &None)
        } else {
            (&item.ty, &item.val)
        };

        match (ty_val, body_val) {
            (Some(ty), None) => {
                let (_, ty) = self.eval_ty(ty)?;
                let index = self.elements.len();
                let color = color.unwrap_or_else(|| self.auto_color(index));
                let prim = Prim::new(index as PrimId);
                let cell = match &ty {
                    Ty::Zero => FreeCell::zero(prim),
                    Ty::Succ(s, t) => FreeCell::prim(prim, s.clone(), t.clone())?,
                };
                self.add(Element {
                    name: name.to_string(),
                    color,
                    cell,
                });
            }
            (dim_ty, Some(body_rc)) => {
                let body = self.eval_val(body_rc)?;
                let dim = body.pure.dim().in_space;
                let body_ty = if dim == 0 {
                    Ty::Zero
                } else {
                    let s = FreeCell::from_pure(&body.pure.s());
                    let t = FreeCell::from_pure(&body.pure.t());
                    Ty::Succ(s, t)
                };

                if let Some(ty_rc) = dim_ty {
                    let (declared_dim, declared_ty) = self.eval_ty(ty_rc)?;
                    if declared_dim != dim {
                        return Err("declared type dimension does not match body".to_string());
                    }
                    self.match_ty(&declared_ty, &body_ty)?;
                }

                let color = color.unwrap_or_else(|| self.auto_color(self.elements.len()));
                self.add(Element {
                    name: name.to_string(),
                    color,
                    cell: body,
                });
            }
            (None, None) => {
                // skip
            }
        }

        // Recurse into members
        if !item.members.entries.is_empty() {
            self.load_members(name, &item.members)?;
        }

        Ok(())
    }

    fn load_members(&mut self, prefix: &str, module: &Module) -> Result<()> {
        // Register short name aliases so inner references resolve
        let short_names: Vec<String> = module.entries.iter().map(|(n, _)| n.clone()).collect();
        for (member_name, item) in &module.entries {
            let full_name = format!("{}.{}", prefix, member_name);
            self.load_item(&full_name, item)?;
            // Add short alias pointing to the same index
            if let Some(&idx) = self.lookup.get(full_name.as_str()) {
                self.lookup.insert(member_name.clone(), idx);
            }
        }
        // Remove short aliases to avoid polluting the global namespace
        for name in &short_names {
            // Only remove if it still points to a prefixed entry
            if let Some(&idx) = self.lookup.get(name.as_str()) {
                let full = format!("{}.{}", prefix, name);
                if self.lookup.get(full.as_str()) == Some(&idx) {
                    self.lookup.remove(name.as_str());
                }
            }
        }
        Ok(())
    }

    fn eval_ty(&self, val: &A<semtree::Val>) -> Result<(u8, Ty)> {
        let Some(v) = val.inner() else {
            return Err("invalid type expression".to_string());
        };
        match v {
            semtree::Val::Path(path_a) => {
                let name = self.path_name(path_a);
                if name.as_deref() == Some("*") {
                    return Ok((0, Ty::Zero));
                }
                Err("expected `*` or arrow type".to_string())
            }
            semtree::Val::Op(l, op_a, _, r) => {
                if let Some(semtree::Op::Arrow(semtree::ArrowTy::To)) = op_a.inner() {
                    let mut s = self.eval_val_inner(l)?;
                    let mut t = self.eval_val_inner(r)?;
                    let sd = s.pure.dim().in_space;
                    let td = t.pure.dim().in_space;
                    let max = sd.max(td);
                    s = lift_dim(s, max);
                    t = lift_dim(t, max);
                    check_prim(&s.pure, &t.pure)?;
                    let d = max + 1;
                    Ok((d, Ty::Succ(s, t)))
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
                let name = self
                    .path_name(path_a)
                    .ok_or_else(|| "invalid path".to_string())?;
                let index = self
                    .lookup
                    .get(&name)
                    .ok_or_else(|| format!("unknown variable: {}", name))?;
                Ok(self.elements[*index].cell.clone())
            }
            semtree::Val::Op(l, op_a, _, r) => {
                let Some(op) = op_a.inner() else {
                    return Err("invalid operator".to_string());
                };
                match op {
                    semtree::Op::Comp(n) => {
                        let mut lc = self.eval_val_inner(l)?;
                        let mut rc = self.eval_val_inner(r)?;
                        let max = lc.pure.dim().in_space.max(rc.pure.dim().in_space);
                        lc = lift_dim(lc, max);
                        rc = lift_dim(rc, max);
                        Ok(FreeCell::comp(*n as u8, vec![lc, rc])?)
                    }
                    semtree::Op::Arrow(semtree::ArrowTy::To) => {
                        // Parenthesized arrow in value position — treat as comp
                        // e.g. (a → b) used as a value means the prim cell for a→b
                        // Actually this shouldn't normally appear; error
                        Err("arrow in value position".to_string())
                    }
                    _ => Err("unsupported operator in value".to_string()),
                }
            }
            semtree::Val::Lit(_) => Err("literal in value position".to_string()),
        }
    }

    fn path_name(&self, path_a: &A<semtree::Path>) -> Option<String> {
        let path = path_a.inner()?;
        let mut parts = Vec::new();
        for seg_a in &path.0 {
            let seg = seg_a.inner()?;
            let name = seg.0.inner()?;
            parts.push(name.0.clone());
        }
        if parts.is_empty() {
            return None;
        }
        Some(parts.join("."))
    }

    fn extract_color(&self, decos: &[Rc<A<semtree::Val>>]) -> Option<(u8, u8, u8)> {
        for deco in decos {
            let Some(val) = deco.inner() else {
                continue;
            };
            let semtree::Val::Path(path_a) = val else {
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
                "gray" => {
                    if args.len() == 1 {
                        if let Some(n) = self.extract_number(&args[0]) {
                            let g = n as u8;
                            return Some((g, g, g));
                        }
                    }
                }
                "rgb" => {
                    if args.len() == 3 {
                        if let (Some(r), Some(g), Some(b)) = (
                            self.extract_number(&args[0]),
                            self.extract_number(&args[1]),
                            self.extract_number(&args[2]),
                        ) {
                            return Some((r as u8, g as u8, b as u8));
                        }
                    }
                }
                "hsv" => {
                    if !args.is_empty() {
                        let h = self.extract_rational(&args[0]);
                        let s = if args.len() >= 2 {
                            self.extract_rational(&args[1])
                        } else {
                            Some(1.0)
                        };
                        let v = if args.len() >= 3 {
                            self.extract_rational(&args[2])
                        } else {
                            Some(1.0)
                        };
                        if let (Some(h), Some(s), Some(v)) = (h, s, v) {
                            return Some(hsv2rgb(h, s, v));
                        }
                    }
                }
                _ => {}
            }
        }
        None
    }

    fn extract_number(&self, param_a: &A<semtree::ParamVal>) -> Option<f64> {
        let param = param_a.inner()?;
        let val = param.val.inner()?;
        match val {
            semtree::Val::Lit(lit_a) => match lit_a.inner()? {
                semtree::Lit::Number(s) => s.parse::<f64>().ok(),
                _ => None,
            },
            _ => None,
        }
    }

    fn extract_rational(&self, param_a: &A<semtree::ParamVal>) -> Option<f64> {
        let param = param_a.inner()?;
        let val = param.val.inner()?;
        match val {
            semtree::Val::Lit(lit_a) => match lit_a.inner()? {
                semtree::Lit::Number(s) => s.parse::<f64>().ok(),
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
            // Handle dotted number paths like 0.6 → Path([Seg("0"), Seg("6")])
            semtree::Val::Path(path_a) => {
                let path = path_a.inner()?;
                if path.1.is_some() {
                    return None;
                }
                let mut parts = Vec::new();
                for seg_a in &path.0 {
                    let seg = seg_a.inner()?;
                    if !seg.1 .0.is_empty() {
                        return None;
                    }
                    let name = seg.0.inner()?;
                    if !crate::convert::is_number_str(&name.0) {
                        return None;
                    }
                    parts.push(name.0.as_str());
                }
                parts.join(".").parse::<f64>().ok()
            }
            _ => None,
        }
    }
}

fn lift_dim(mut cell: FreeCell, target: u8) -> FreeCell {
    while cell.pure.dim().in_space < target {
        cell = FreeCell::id(cell);
    }
    cell
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
    let (module, _check_errors) = crate::check::check(sem_prog, &tokens);
    // Note: check errors (e.g. `*` undefined) are non-fatal; the module is still built.
    let mut table = Table::new();
    table.load_module(&module)?;
    Ok(table)
}
