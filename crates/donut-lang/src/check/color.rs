use super::*;

// --- Color spec ---

#[derive(Clone, Copy)]
pub(super) enum ColorSpec {
    Absolute(u8, u8, u8),
    Lighten(f64),
    Darken(f64),
}

// --- Checker methods ---

impl<'a> Checker<'a> {
    // --- Meta reduction ---

    pub(super) fn reduce_meta(&self, arg: &PrimArg) -> PrimArg {
        match arg {
            PrimArg::Nat(_) | PrimArg::Rat(_) | PrimArg::Cell(_) => arg.clone(),
            PrimArg::App(id, args) => {
                let reduced: Vec<PrimArg> = args.iter().map(|a| self.reduce_meta(a)).collect();
                if let Some(result) = self.builtin_reduce(*id, &reduced) {
                    return result;
                }
                PrimArg::App(*id, reduced)
            }
        }
    }

    fn builtin_reduce(&self, id: PrimId, args: &[PrimArg]) -> Option<PrimArg> {
        let rgb_id = self.meta_id("rgb")?;
        if self.meta_id("hsv") == Some(id) {
            let h = args.get(0)?.as_rat()?;
            let s = args.get(1).and_then(|a| a.as_rat()).unwrap_or(1.0);
            let v = args.get(2).and_then(|a| a.as_rat()).unwrap_or(1.0);
            let (r, g, b) = hsv_to_rgb(h, s, v);
            return Some(PrimArg::App(rgb_id, vec![
                PrimArg::Nat(r as u64), PrimArg::Nat(g as u64), PrimArg::Nat(b as u64),
            ]));
        }
        if self.meta_id("lerp") == Some(id) {
            let a = args.get(0)?;
            let b = args.get(1)?;
            let x = args.get(2)?.as_rat()?;
            if let (PrimArg::App(a_id, a_args), PrimArg::App(b_id, b_args)) = (a, b) {
                if *a_id == rgb_id && *b_id == rgb_id {
                    let interp = |i: usize| -> Option<PrimArg> {
                        let av = match a_args.get(i)? { PrimArg::Nat(n) => *n as f64, _ => return None };
                        let bv = match b_args.get(i)? { PrimArg::Nat(n) => *n as f64, _ => return None };
                        Some(PrimArg::Nat((av + (bv - av) * x).round() as u64))
                    };
                    return Some(PrimArg::App(rgb_id, vec![interp(0)?, interp(1)?, interp(2)?]));
                }
            }
        }
        None
    }

    // --- Decorator evaluation ---

    pub(super) fn eval_decorators(&mut self, decos: &[ValId]) -> Vec<PrimArg> {
        let decorator_type = self.meta_id("decorator")
            .map(|id| MetaType(id, vec![]));
        let mut result = Vec::new();
        for &deco_id in decos {
            let deco = self.program.val(deco_id);
            let span = deco.1.clone();
            let is_decorator = self.check_meta_type(&deco.0)
                .filter(|ty| decorator_type.as_ref() == Some(ty))
                .is_some();
            if is_decorator {
                match self.eval_meta_val(&deco.0) {
                    Ok(prim_arg) => result.push(self.reduce_meta(&prim_arg)),
                    Err(msg) => self.error_at(&span, msg),
                }
            } else {
                self.error_at(&span, "decorator must have type `base.decorator`");
            }
        }
        result
    }

    fn style_meta_id(&self, name: &str) -> Option<PrimId> {
        let full = format!("style.{}", name);
        self.lookup.get(&full).and_then(|&idx| {
            match &self.entries[idx].body {
                EntryBody::Meta(prim) => Some(prim.id),
                _ => None,
            }
        })
    }

    pub(super) fn extract_color(&self, decos: &[PrimArg]) -> Option<ColorSpec> {
        let rgb_id = self.meta_id("rgb")?;
        let color_id = self.style_meta_id("color");
        let lighten_id = self.style_meta_id("lighten");
        let darken_id = self.style_meta_id("darken");
        for deco in decos {
            let PrimArg::App(id, args) = deco else { continue };
            if color_id == Some(*id) {
                let Some(PrimArg::App(cid, color_args)) = args.first() else { continue };
                if *cid != rgb_id { continue; }
                let r = match color_args.get(0)? { PrimArg::Nat(n) => *n as u8, _ => continue };
                let g = match color_args.get(1)? { PrimArg::Nat(n) => *n as u8, _ => continue };
                let b = match color_args.get(2)? { PrimArg::Nat(n) => *n as u8, _ => continue };
                return Some(ColorSpec::Absolute(r, g, b));
            }
            if lighten_id == Some(*id) {
                let amount = args.first().and_then(|a| a.as_rat()).unwrap_or(0.3);
                return Some(ColorSpec::Lighten(amount));
            }
            if darken_id == Some(*id) {
                let amount = args.first().and_then(|a| a.as_rat()).unwrap_or(0.3);
                return Some(ColorSpec::Darken(amount));
            }
        }
        None
    }

    // --- Auto color ---

    fn boundary_avg_hue(&self, pure: &PureCell) -> Option<f64> {
        let s = pure.s();
        let t = pure.t();
        let mut prim_ids = collect_prim_ids(&s);
        prim_ids.extend(collect_prim_ids(&t));
        prim_ids.sort();
        prim_ids.dedup();

        let pi2 = 2.0 * std::f64::consts::PI;
        let mut sin_sum = 0.0;
        let mut cos_sum = 0.0;
        let mut count = 0;

        for id in &prim_ids {
            if let Some(decl) = self.prim_decls.get(id) {
                let (hue, sat, _) = rgb_to_hsv(decl.color);
                if sat < 0.05 {
                    continue;
                }
                let angle = hue * pi2;
                sin_sum += angle.sin();
                cos_sum += angle.cos();
                count += 1;
            }
        }

        if count == 0 {
            return None;
        }
        let avg = sin_sum.atan2(cos_sum);
        Some(((avg / pi2) % 1.0 + 1.0) % 1.0)
    }

    pub(super) fn auto_color(&self, body: &EntryBody) -> (u8, u8, u8) {
        let index = self.entries.len();
        match body {
            EntryBody::Cell(cell) => {
                let dim = cell.pure.dim().in_space;
                match dim {
                    0 => {
                        let hue = golden_angle_hue(index);
                        hsv_to_rgb(hue, 0.3, 0.35)
                    }
                    1 => {
                        let base_hue = self.boundary_avg_hue(&cell.pure)
                            .unwrap_or_else(|| golden_angle_hue(index));
                        let hue = base_hue + golden_angle_hue(index) * 0.4;
                        hsv_to_rgb(hue, 0.55, 0.65)
                    }
                    _ => {
                        let base_hue = self.boundary_avg_hue(&cell.pure)
                            .unwrap_or_else(|| golden_angle_hue(index));
                        let hue = base_hue + golden_angle_hue(index) * 0.25;
                        hsv_to_rgb(hue, 0.35, 0.88)
                    }
                }
            }
            _ => (128, 128, 128),
        }
    }

    pub(super) fn resolve_color(&self, spec: Option<ColorSpec>, body: &EntryBody) -> (u8, u8, u8) {
        match spec {
            Some(ColorSpec::Absolute(r, g, b)) => (r, g, b),
            Some(ColorSpec::Lighten(amount)) => {
                let base = self.auto_color(body);
                lighten_color(base, amount)
            }
            Some(ColorSpec::Darken(amount)) => {
                let base = self.auto_color(body);
                darken_color(base, amount)
            }
            None => self.auto_color(body),
        }
    }
}

// --- Free functions ---

fn golden_angle_hue(index: usize) -> f64 {
    ((index as f64 * 137.508) % 360.0) / 360.0
}

pub(super) fn hsv_to_rgb(h: f64, s: f64, v: f64) -> (u8, u8, u8) {
    let h = ((h % 1.0) + 1.0) % 1.0;
    let c = v * s;
    let h6 = h * 6.0;
    let x = c * (1.0 - ((h6 % 2.0) - 1.0).abs());
    let m = v - c;
    let (r, g, b) = match h6 as u32 {
        0 => (c, x, 0.0),
        1 => (x, c, 0.0),
        2 => (0.0, c, x),
        3 => (0.0, x, c),
        4 => (x, 0.0, c),
        _ => (c, 0.0, x),
    };
    (
        ((r + m) * 255.0) as u8,
        ((g + m) * 255.0) as u8,
        ((b + m) * 255.0) as u8,
    )
}

fn rgb_to_hsv(color: (u8, u8, u8)) -> (f64, f64, f64) {
    let r = color.0 as f64 / 255.0;
    let g = color.1 as f64 / 255.0;
    let b = color.2 as f64 / 255.0;
    let max = r.max(g).max(b);
    let min = r.min(g).min(b);
    let d = max - min;
    if d < 1e-6 {
        return (0.0, 0.0, max);
    }
    let h = if (max - r).abs() < 1e-6 {
        (((g - b) / d) % 6.0 + 6.0) % 6.0
    } else if (max - g).abs() < 1e-6 {
        (b - r) / d + 2.0
    } else {
        (r - g) / d + 4.0
    };
    (h / 6.0, if max > 0.0 { d / max } else { 0.0 }, max)
}

fn lighten_color(base: (u8, u8, u8), amount: f64) -> (u8, u8, u8) {
    let (h, s, v) = rgb_to_hsv(base);
    let v = v + (1.0 - v) * amount;
    let s = s * (1.0 - amount * 0.3);
    hsv_to_rgb(h, s, v)
}

fn darken_color(base: (u8, u8, u8), amount: f64) -> (u8, u8, u8) {
    let (h, s, v) = rgb_to_hsv(base);
    let v = v * (1.0 - amount);
    hsv_to_rgb(h, s, v)
}

fn collect_prim_ids(pure: &PureCell) -> Vec<PrimId> {
    match pure {
        PureCell::Prim(prim, _, dim) if dim.effective == dim.in_space => vec![prim.id],
        PureCell::Prim(_, _, _) => vec![],
        PureCell::Comp(_, children, _) => {
            children.iter().flat_map(collect_prim_ids).collect()
        }
    }
}
