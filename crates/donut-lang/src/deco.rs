use std::collections::HashMap;

use crate::types::env::{self, as_meta, Color, Decorator, Meta, Style};
use donut_core::cell::Globular;
use donut_core::common::{PrimId, PureVal};
use donut_core::pure_cell::PureCell;

/// Evaluate decorator expressions and assign auto colors.
///
/// 1. Reduce all decorator PureVal::App trees to concrete Meta values
/// 2. For each def with a cell value, resolve final color (explicit or auto)
pub fn decorate(env: &mut env::Env) {
    // Pass 1: reduce decorator expressions
    for def in &mut env.defs {
        let evaluated: Vec<PureVal> = def
            .decos
            .iter()
            .map(|deco| reduce(deco, &env.items))
            .collect();
        def.decos = evaluated;
    }

    // Pass 2: resolve colors → write to def.style.color
    let mut prim_colors: HashMap<PrimId, Color> = HashMap::new();

    for i in 0..env.defs.len() {
        let spec = extract_color_spec(&env.defs[i].decos);
        let color = resolve_color(spec, &env.defs[i], &prim_colors);

        if let Some(item_id) = env.defs[i].item {
            if let Some(prim_id) = env.items[item_id.0].prim_id {
                prim_colors.insert(prim_id, color);
            }
        }

        env.defs[i].style.color = color;
    }
}

// --- Color spec extraction ---

enum ColorSpec {
    Absolute(Color),
    Lighten(f64),
    Darken(f64),
}

fn extract_color_spec(decos: &[PureVal]) -> Option<ColorSpec> {
    for deco in decos {
        if let Some(Meta::Deco(Decorator::Style(style))) = as_meta(deco) {
            match style {
                Style::Color(c) => return Some(ColorSpec::Absolute(*c)),
                Style::Lighten(a) => return Some(ColorSpec::Lighten(*a)),
                Style::Darken(a) => return Some(ColorSpec::Darken(*a)),
            }
        }
    }
    None
}

fn resolve_color(
    spec: Option<ColorSpec>,
    def: &env::Def,
    prim_colors: &HashMap<PrimId, Color>,
) -> Color {
    match spec {
        Some(ColorSpec::Absolute(c)) => c,
        Some(ColorSpec::Lighten(amount)) => {
            let base = auto_color(def, prim_colors);
            lighten_color(base, amount)
        }
        Some(ColorSpec::Darken(amount)) => {
            let base = auto_color(def, prim_colors);
            darken_color(base, amount)
        }
        None => auto_color(def, prim_colors),
    }
}

// --- Auto color ---

fn auto_color(def: &env::Def, prim_colors: &HashMap<PrimId, Color>) -> Color {
    let cell = match &def.val {
        PureVal::Cell(pc) => pc,
        _ => return Color::gray(),
    };
    let dim = cell.dim().in_space;
    let index = def.item.map(|id| id.0).unwrap_or(0);
    match dim {
        0 => {
            let hue = golden_angle_hue(index);
            hsv_to_rgb(hue, 0.3, 0.35)
        }
        1 => {
            let base_hue = boundary_avg_hue(cell, prim_colors)
                .unwrap_or_else(|| golden_angle_hue(index));
            let hue = base_hue + golden_angle_hue(index) * 0.4;
            hsv_to_rgb(hue, 0.55, 0.65)
        }
        _ => {
            let base_hue = boundary_avg_hue(cell, prim_colors)
                .unwrap_or_else(|| golden_angle_hue(index));
            let hue = base_hue + golden_angle_hue(index) * 0.25;
            hsv_to_rgb(hue, 0.35, 0.88)
        }
    }
}

fn boundary_avg_hue(cell: &PureCell, prim_colors: &HashMap<PrimId, Color>) -> Option<f64> {
    let s = cell.s();
    let t = cell.t();
    let mut prim_ids = collect_prim_ids(&s);
    prim_ids.extend(collect_prim_ids(&t));
    prim_ids.sort();
    prim_ids.dedup();

    let pi2 = 2.0 * std::f64::consts::PI;
    let mut sin_sum = 0.0;
    let mut cos_sum = 0.0;
    let mut count = 0;

    for id in &prim_ids {
        if let Some(color) = prim_colors.get(id) {
            let (hue, sat, _) = rgb_to_hsv(*color);
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

fn collect_prim_ids(cell: &PureCell) -> Vec<PrimId> {
    match cell {
        PureCell::Prim(prim, _, dim) if dim.effective == dim.in_space => vec![prim.id],
        PureCell::Prim(_, _, _) => vec![],
        PureCell::Comp(_, children, _) => children.iter().flat_map(collect_prim_ids).collect(),
    }
}

fn golden_angle_hue(index: usize) -> f64 {
    ((index as f64 * 137.508) % 360.0) / 360.0
}

// --- Reduction ---

fn reduce(pv: &PureVal, items: &[env::Item]) -> PureVal {
    match pv {
        PureVal::App(ext_id, args) => {
            let reduced: Vec<PureVal> = args.iter().map(|a| reduce(a, items)).collect();
            let cname = &items[ext_id.0 as usize].cname;
            if let Some(result) = eval_builtin(cname, &reduced) {
                result
            } else {
                PureVal::App(*ext_id, reduced)
            }
        }
        _ => pv.clone(),
    }
}

fn eval_builtin(cname: &str, args: &[PureVal]) -> Option<PureVal> {
    match cname {
        "ui::rgb" => eval_rgb(args),
        "ui::hsv" => eval_hsv(args),
        "ui::hue" => eval_hue(args),
        "ui::gray" => eval_gray(args),
        "ui::lerp" => eval_lerp(args),
        "ui::style.color" => eval_style_color(args),
        "ui::style.lighten" => eval_style_lighten(args),
        "ui::style.darken" => eval_style_darken(args),
        "ui::style" => eval_style(args),
        _ => None,
    }
}

// --- Builtin evaluators ---

fn eval_rgb(args: &[PureVal]) -> Option<PureVal> {
    let r = extract_nat(args.get(0)?)? as u8;
    let g = extract_nat(args.get(1)?)? as u8;
    let b = extract_nat(args.get(2)?)? as u8;
    Some(Meta::Color(Color::new(r, g, b)).into())
}

fn eval_hsv(args: &[PureVal]) -> Option<PureVal> {
    let h = extract_rat(args.get(0)?)?;
    let s = extract_rat_or(args.get(1), 1.0);
    let v = extract_rat_or(args.get(2), 1.0);
    Some(Meta::Color(hsv_to_rgb(h, s, v)).into())
}

fn eval_hue(args: &[PureVal]) -> Option<PureVal> {
    let h = extract_rat(args.get(0)?)?;
    Some(Meta::Color(hsv_to_rgb(h, 0.7, 0.8)).into())
}

fn eval_gray(args: &[PureVal]) -> Option<PureVal> {
    let v = extract_rat_or(args.get(0), 0.5);
    let c = (v * 255.0).round() as u8;
    Some(Meta::Color(Color::new(c, c, c)).into())
}

fn eval_lerp(args: &[PureVal]) -> Option<PureVal> {
    let a = extract_color(args.get(0)?)?;
    let b = extract_color(args.get(1)?)?;
    let t = extract_rat(args.get(2)?)?;
    let r = (a.r() as f64 + (b.r() as f64 - a.r() as f64) * t).round() as u8;
    let g = (a.g() as f64 + (b.g() as f64 - a.g() as f64) * t).round() as u8;
    let b_val = (a.b() as f64 + (b.b() as f64 - a.b() as f64) * t).round() as u8;
    Some(Meta::Color(Color::new(r, g, b_val)).into())
}

fn eval_style_color(args: &[PureVal]) -> Option<PureVal> {
    let c = extract_color(args.get(0)?)?;
    Some(Meta::Deco(Decorator::Style(Style::Color(c))).into())
}

fn eval_style_lighten(args: &[PureVal]) -> Option<PureVal> {
    let amount = extract_rat_or(args.get(0), 0.3);
    Some(Meta::Deco(Decorator::Style(Style::Lighten(amount))).into())
}

fn eval_style_darken(args: &[PureVal]) -> Option<PureVal> {
    let amount = extract_rat_or(args.get(0), 0.3);
    Some(Meta::Deco(Decorator::Style(Style::Darken(amount))).into())
}

fn eval_style(args: &[PureVal]) -> Option<PureVal> {
    let c = extract_color(args.get(0)?)?;
    Some(Meta::Deco(Decorator::Style(Style::Color(c))).into())
}

// --- Value extraction ---

fn extract_nat(pv: &PureVal) -> Option<u64> {
    match as_meta(pv)? {
        Meta::Nat(n) => Some(*n),
        _ => None,
    }
}

fn extract_rat(pv: &PureVal) -> Option<f64> {
    match as_meta(pv)? {
        Meta::Rat(r) => Some(*r),
        Meta::Nat(n) => Some(*n as f64),
        _ => None,
    }
}

fn extract_rat_or(pv: Option<&PureVal>, default: f64) -> f64 {
    pv.and_then(extract_rat).unwrap_or(default)
}

fn extract_color(pv: &PureVal) -> Option<Color> {
    match as_meta(pv)? {
        Meta::Color(c) => Some(*c),
        _ => None,
    }
}

// --- Color math ---

fn hsv_to_rgb(h: f64, s: f64, v: f64) -> Color {
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
    Color::new(
        ((r + m) * 255.0) as u8,
        ((g + m) * 255.0) as u8,
        ((b + m) * 255.0) as u8,
    )
}

fn rgb_to_hsv(color: Color) -> (f64, f64, f64) {
    let r = color.r() as f64 / 255.0;
    let g = color.g() as f64 / 255.0;
    let b = color.b() as f64 / 255.0;
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

fn lighten_color(base: Color, amount: f64) -> Color {
    let (h, s, v) = rgb_to_hsv(base);
    let v = v + (1.0 - v) * amount;
    let s = s * (1.0 - amount * 0.3);
    hsv_to_rgb(h, s, v)
}

fn darken_color(base: Color, amount: f64) -> Color {
    let (h, s, v) = rgb_to_hsv(base);
    let v = v * (1.0 - amount);
    hsv_to_rgb(h, s, v)
}
