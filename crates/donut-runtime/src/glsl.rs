use donut_core::cell::Globular;
use donut_core::common::{PrimId, PureVal};
use donut_core::free_cell::{Cell, CellF, FreeCell};
use donut_core::pure_cell::PureCell;
use donut_lang::types::env::{self, Meta};
use std::collections::HashMap;

use crate::source_width;

#[derive(Debug, Clone, PartialEq)]
pub enum GlslTy {
    Float,
    Vec2,
    Vec3,
    Int,
    Bool,
}

impl GlslTy {
    fn decl(&self) -> &'static str {
        match self {
            GlslTy::Float => "float",
            GlslTy::Vec2 => "vec2",
            GlslTy::Vec3 => "vec3",
            GlslTy::Int => "int",
            GlslTy::Bool => "bool",
        }
    }
}

#[derive(Debug, Clone)]
struct Var {
    name: String,
    ty: GlslTy,
}

struct GlslCompiler {
    counter: usize,
    lines: Vec<String>,
    prim_names: HashMap<PrimId, String>,
}

impl GlslCompiler {
    fn new(prim_names: HashMap<PrimId, String>) -> Self {
        GlslCompiler {
            counter: 0,
            lines: Vec::new(),
            prim_names,
        }
    }

    fn fresh(&mut self, ty: GlslTy) -> Var {
        let name = format!("v{}", self.counter);
        self.counter += 1;
        Var { name, ty }
    }

    fn emit(&mut self, line: String) {
        self.lines.push(line);
    }

    fn compile_cell(&mut self, cell: &Cell, inputs: &[Var]) -> Result<Vec<Var>, String> {
        match cell.0.as_ref() {
            CellF::Prim(prim, _, _) => self.compile_prim(prim, inputs),
            CellF::Id(_) => Ok(inputs.to_vec()),
            CellF::Comp(1, children) => {
                let mut current = inputs.to_vec();
                for child in children {
                    current = self.compile_cell(child, &current)?;
                }
                Ok(current)
            }
            CellF::Comp(0, children) => {
                let mut offset = 0;
                let mut result = Vec::new();
                for child in children {
                    let w = source_width(child);
                    let child_inputs = &inputs[offset..offset + w];
                    let child_outputs = self.compile_cell(child, child_inputs)?;
                    result.extend(child_outputs);
                    offset += w;
                }
                Ok(result)
            }
            CellF::Zero(_) => Err("cannot compile a 0-cell to GLSL".into()),
            CellF::Comp(axis, _) => Err(format!("unsupported composition axis {} for GLSL", axis)),
        }
    }

    fn compile_prim(
        &mut self,
        prim: &donut_core::common::Prim,
        inputs: &[Var],
    ) -> Result<Vec<Var>, String> {
        let name = self
            .prim_names
            .get(&prim.id)
            .ok_or_else(|| format!("unknown prim {}", prim.id))?
            .clone();
        match name.as_str() {
            // --- f32 ---
            "sys::f32.lit" => {
                let r = extract_rat(&prim.args, 0)?;
                let v = self.fresh(GlslTy::Float);
                self.emit(format!("{} {} = {};", v.ty.decl(), v.name, format_float(r)));
                Ok(vec![v])
            }
            "sys::f32.add" => Ok(self.binary(GlslTy::Float, "+", &inputs[0], &inputs[1])),
            "sys::f32.sub" => Ok(self.binary(GlslTy::Float, "-", &inputs[0], &inputs[1])),
            "sys::f32.mul" => Ok(self.binary(GlslTy::Float, "*", &inputs[0], &inputs[1])),
            "sys::f32.div" => Ok(self.binary(GlslTy::Float, "/", &inputs[0], &inputs[1])),
            "sys::f32.neg" => Ok(self.unary(GlslTy::Float, "-", &inputs[0])),
            "sys::f32.floor" => {
                let v = self.fresh(GlslTy::Int);
                self.emit(format!("{} {} = int(floor({}));", v.ty.decl(), v.name, inputs[0].name));
                Ok(vec![v])
            }
            "sys::f32.ceil" => {
                let v = self.fresh(GlslTy::Int);
                self.emit(format!("{} {} = int(ceil({}));", v.ty.decl(), v.name, inputs[0].name));
                Ok(vec![v])
            }
            "sys::f32.eq" => Ok(self.compare(GlslTy::Float, "==", &inputs[0], &inputs[1])),
            "sys::f32.ne" => Ok(self.compare(GlslTy::Float, "!=", &inputs[0], &inputs[1])),
            "sys::f32.lt" => Ok(self.compare(GlslTy::Float, "<", &inputs[0], &inputs[1])),
            "sys::f32.le" => Ok(self.compare(GlslTy::Float, "<=", &inputs[0], &inputs[1])),
            "sys::f32.gt" => Ok(self.compare(GlslTy::Float, ">", &inputs[0], &inputs[1])),
            "sys::f32.ge" => Ok(self.compare(GlslTy::Float, ">=", &inputs[0], &inputs[1])),

            // --- u32 ---
            "sys::u32.lit" => {
                let n = extract_nat(&prim.args, 0)?;
                let v = self.fresh(GlslTy::Int);
                self.emit(format!("{} {} = {};", v.ty.decl(), v.name, n));
                Ok(vec![v])
            }
            "sys::u32.add" => Ok(self.binary(GlslTy::Int, "+", &inputs[0], &inputs[1])),
            "sys::u32.sub" => Ok(self.binary(GlslTy::Int, "-", &inputs[0], &inputs[1])),
            "sys::u32.mul" => Ok(self.binary(GlslTy::Int, "*", &inputs[0], &inputs[1])),
            "sys::u32.div" => Ok(self.binary(GlslTy::Int, "/", &inputs[0], &inputs[1])),
            "sys::u32.mod" => {
                let v = self.fresh(GlslTy::Int);
                self.emit(format!(
                    "{} {} = {} - {} * ({} / {});",
                    v.ty.decl(),
                    v.name,
                    inputs[0].name,
                    inputs[1].name,
                    inputs[0].name,
                    inputs[1].name
                ));
                Ok(vec![v])
            }
            "sys::u32.neg" => Ok(self.unary(GlslTy::Int, "-", &inputs[0])),
            "sys::u32.eq" => Ok(self.compare(GlslTy::Int, "==", &inputs[0], &inputs[1])),
            "sys::u32.ne" => Ok(self.compare(GlslTy::Int, "!=", &inputs[0], &inputs[1])),
            "sys::u32.lt" => Ok(self.compare(GlslTy::Int, "<", &inputs[0], &inputs[1])),
            "sys::u32.le" => Ok(self.compare(GlslTy::Int, "<=", &inputs[0], &inputs[1])),
            "sys::u32.gt" => Ok(self.compare(GlslTy::Int, ">", &inputs[0], &inputs[1])),
            "sys::u32.ge" => Ok(self.compare(GlslTy::Int, ">=", &inputs[0], &inputs[1])),
            "sys::u32.to_f32" => Ok(self.func_call(GlslTy::Float, "float", &inputs[0])),
            "sys::u32.to_i32" => Ok(self.func_call(GlslTy::Int, "int", &inputs[0])),

            // --- i32 ---
            "sys::i32.lit" => {
                let n = extract_nat(&prim.args, 0)?;
                let v = self.fresh(GlslTy::Int);
                self.emit(format!("{} {} = {};", v.ty.decl(), v.name, n as i32));
                Ok(vec![v])
            }
            "sys::i32.add" => Ok(self.binary(GlslTy::Int, "+", &inputs[0], &inputs[1])),
            "sys::i32.sub" => Ok(self.binary(GlslTy::Int, "-", &inputs[0], &inputs[1])),
            "sys::i32.mul" => Ok(self.binary(GlslTy::Int, "*", &inputs[0], &inputs[1])),
            "sys::i32.div" => Ok(self.binary(GlslTy::Int, "/", &inputs[0], &inputs[1])),
            "sys::i32.mod" => {
                let v = self.fresh(GlslTy::Int);
                self.emit(format!(
                    "{} {} = {} - {} * ({} / {});",
                    v.ty.decl(),
                    v.name,
                    inputs[0].name,
                    inputs[1].name,
                    inputs[0].name,
                    inputs[1].name
                ));
                Ok(vec![v])
            }
            "sys::i32.neg" => Ok(self.unary(GlslTy::Int, "-", &inputs[0])),
            "sys::i32.eq" => Ok(self.compare(GlslTy::Int, "==", &inputs[0], &inputs[1])),
            "sys::i32.ne" => Ok(self.compare(GlslTy::Int, "!=", &inputs[0], &inputs[1])),
            "sys::i32.lt" => Ok(self.compare(GlslTy::Int, "<", &inputs[0], &inputs[1])),
            "sys::i32.le" => Ok(self.compare(GlslTy::Int, "<=", &inputs[0], &inputs[1])),
            "sys::i32.gt" => Ok(self.compare(GlslTy::Int, ">", &inputs[0], &inputs[1])),
            "sys::i32.ge" => Ok(self.compare(GlslTy::Int, ">=", &inputs[0], &inputs[1])),
            "sys::i32.to_f32" => Ok(self.func_call(GlslTy::Float, "float", &inputs[0])),

            // --- bool ---
            "sys::bool.lit" => {
                let n = extract_nat(&prim.args, 0)?;
                let v = self.fresh(GlslTy::Bool);
                self.emit(format!("{} {} = {};", v.ty.decl(), v.name, n != 0));
                Ok(vec![v])
            }
            "sys::bool.true" => {
                let v = self.fresh(GlslTy::Bool);
                self.emit(format!("{} {} = true;", v.ty.decl(), v.name));
                Ok(vec![v])
            }
            "sys::bool.false" => {
                let v = self.fresh(GlslTy::Bool);
                self.emit(format!("{} {} = false;", v.ty.decl(), v.name));
                Ok(vec![v])
            }
            "sys::bool.not" => {
                let v = self.fresh(GlslTy::Bool);
                self.emit(format!("{} {} = !{};", v.ty.decl(), v.name, inputs[0].name));
                Ok(vec![v])
            }
            "sys::bool.and" => {
                let v = self.fresh(GlslTy::Bool);
                self.emit(format!(
                    "{} {} = {} && {};",
                    v.ty.decl(),
                    v.name,
                    inputs[0].name,
                    inputs[1].name
                ));
                Ok(vec![v])
            }
            "sys::bool.or" => {
                let v = self.fresh(GlslTy::Bool);
                self.emit(format!(
                    "{} {} = {} || {};",
                    v.ty.decl(),
                    v.name,
                    inputs[0].name,
                    inputs[1].name
                ));
                Ok(vec![v])
            }
            "sys::bool.ind" => {
                let n = inputs.len();
                let half = (n - 1) / 2;
                let cond = &inputs[n - 1];
                let mut result = Vec::new();
                for i in 0..half {
                    let v = self.fresh(inputs[i].ty.clone());
                    self.emit(format!(
                        "{} {} = {} ? {} : {};",
                        v.ty.decl(), v.name, cond.name, inputs[i].name, inputs[i + half].name
                    ));
                    result.push(v);
                }
                Ok(result)
            }

            // --- f32x2 ---
            "sys::f32x2.lit" => {
                let x = extract_rat(&prim.args, 0)?;
                let y = extract_rat(&prim.args, 1)?;
                let v = self.fresh(GlslTy::Vec2);
                self.emit(format!(
                    "{} {} = vec2({}, {});",
                    v.ty.decl(),
                    v.name,
                    format_float(x),
                    format_float(y)
                ));
                Ok(vec![v])
            }
            "sys::f32x2.pack" => {
                let v = self.fresh(GlslTy::Vec2);
                self.emit(format!(
                    "{} {} = vec2({}, {});",
                    v.ty.decl(),
                    v.name,
                    inputs[0].name,
                    inputs[1].name
                ));
                Ok(vec![v])
            }
            "sys::f32x2.unpack" => {
                let vx = self.fresh(GlslTy::Float);
                let vy = self.fresh(GlslTy::Float);
                self.emit(format!(
                    "{} {} = {}.x;",
                    vx.ty.decl(),
                    vx.name,
                    inputs[0].name
                ));
                self.emit(format!(
                    "{} {} = {}.y;",
                    vy.ty.decl(),
                    vy.name,
                    inputs[0].name
                ));
                Ok(vec![vx, vy])
            }
            "sys::f32x2.add" => Ok(self.binary(GlslTy::Vec2, "+", &inputs[0], &inputs[1])),
            "sys::f32x2.sub" => Ok(self.binary(GlslTy::Vec2, "-", &inputs[0], &inputs[1])),
            "sys::f32x2.mul" => Ok(self.binary(GlslTy::Vec2, "*", &inputs[0], &inputs[1])),
            "sys::f32x2.div" => Ok(self.binary(GlslTy::Vec2, "/", &inputs[0], &inputs[1])),
            "sys::f32x2.neg" => Ok(self.unary(GlslTy::Vec2, "-", &inputs[0])),
            "sys::f32x2.scale" => {
                let v = self.fresh(GlslTy::Vec2);
                self.emit(format!(
                    "{} {} = {} * {};",
                    v.ty.decl(),
                    v.name,
                    inputs[0].name,
                    inputs[1].name
                ));
                Ok(vec![v])
            }

            // --- f32x3 ---
            "sys::f32x3.lit" => {
                let x = extract_rat(&prim.args, 0)?;
                let y = extract_rat(&prim.args, 1)?;
                let z = extract_rat(&prim.args, 2)?;
                let v = self.fresh(GlslTy::Vec3);
                self.emit(format!(
                    "{} {} = vec3({}, {}, {});",
                    v.ty.decl(),
                    v.name,
                    format_float(x),
                    format_float(y),
                    format_float(z)
                ));
                Ok(vec![v])
            }
            "sys::f32x3.pack" => {
                let v = self.fresh(GlslTy::Vec3);
                self.emit(format!(
                    "{} {} = vec3({}, {}, {});",
                    v.ty.decl(),
                    v.name,
                    inputs[0].name,
                    inputs[1].name,
                    inputs[2].name
                ));
                Ok(vec![v])
            }
            "sys::f32x3.unpack" => {
                let vx = self.fresh(GlslTy::Float);
                let vy = self.fresh(GlslTy::Float);
                let vz = self.fresh(GlslTy::Float);
                self.emit(format!(
                    "{} {} = {}.x;",
                    vx.ty.decl(),
                    vx.name,
                    inputs[0].name
                ));
                self.emit(format!(
                    "{} {} = {}.y;",
                    vy.ty.decl(),
                    vy.name,
                    inputs[0].name
                ));
                self.emit(format!(
                    "{} {} = {}.z;",
                    vz.ty.decl(),
                    vz.name,
                    inputs[0].name
                ));
                Ok(vec![vx, vy, vz])
            }
            "sys::f32x3.add" => Ok(self.binary(GlslTy::Vec3, "+", &inputs[0], &inputs[1])),
            "sys::f32x3.sub" => Ok(self.binary(GlslTy::Vec3, "-", &inputs[0], &inputs[1])),
            "sys::f32x3.mul" => Ok(self.binary(GlslTy::Vec3, "*", &inputs[0], &inputs[1])),
            "sys::f32x3.div" => Ok(self.binary(GlslTy::Vec3, "/", &inputs[0], &inputs[1])),
            "sys::f32x3.neg" => Ok(self.unary(GlslTy::Vec3, "-", &inputs[0])),
            "sys::f32x3.scale" => {
                let v = self.fresh(GlslTy::Vec3);
                self.emit(format!(
                    "{} {} = {} * {};",
                    v.ty.decl(),
                    v.name,
                    inputs[0].name,
                    inputs[1].name
                ));
                Ok(vec![v])
            }

            // val (polymorphic)
            "sys::val.dup" => {
                let mut r = inputs.to_vec();
                r.extend_from_slice(inputs);
                Ok(r)
            }
            "sys::val.drop" => Ok(vec![]),
            "sys::val.swap" => {
                let wx = crate::extract_cell_width(&prim.args, 0)?;
                let mut r = inputs[wx..].to_vec();
                r.extend_from_slice(&inputs[..wx]);
                Ok(r)
            }

            _ => Err(format!("unsupported prim for GLSL: {}", name)),
        }
    }

    // Helpers for common patterns

    fn binary(&mut self, ty: GlslTy, op: &str, a: &Var, b: &Var) -> Vec<Var> {
        let v = self.fresh(ty);
        self.emit(format!(
            "{} {} = {} {} {};",
            v.ty.decl(),
            v.name,
            a.name,
            op,
            b.name
        ));
        vec![v]
    }

    fn unary(&mut self, ty: GlslTy, op: &str, a: &Var) -> Vec<Var> {
        let v = self.fresh(ty);
        self.emit(format!("{} {} = {}{};", v.ty.decl(), v.name, op, a.name));
        vec![v]
    }

    fn func_call(&mut self, ty: GlslTy, func: &str, a: &Var) -> Vec<Var> {
        let v = self.fresh(ty);
        self.emit(format!("{} {} = {}({});", v.ty.decl(), v.name, func, a.name));
        vec![v]
    }

    fn compare(&mut self, _input_ty: GlslTy, op: &str, a: &Var, b: &Var) -> Vec<Var> {
        let v = self.fresh(GlslTy::Bool);
        self.emit(format!(
            "{} {} = {} {} {};",
            v.ty.decl(),
            v.name,
            a.name,
            op,
            b.name
        ));
        vec![v]
    }
}

// --- Helpers ---

fn extract_nat(args: &[PureVal], index: usize) -> Result<u64, String> {
    match args.get(index).and_then(env::as_meta) {
        Some(Meta::Nat(n)) => Ok(*n),
        _ => Err(format!("missing nat parameter at index {}", index)),
    }
}

fn extract_rat(args: &[PureVal], index: usize) -> Result<f64, String> {
    match args.get(index).and_then(env::as_meta) {
        Some(Meta::Rat(r)) => Ok(*r),
        Some(Meta::Nat(n)) => Ok(*n as f64),
        _ => Err(format!("missing rat parameter at index {}", index)),
    }
}

fn sanitize_glsl_name(name: &str) -> String {
    let mut result = String::with_capacity(name.len());
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            result.push(ch);
        } else {
            result.push('_');
        }
    }
    if result.is_empty() || result.as_bytes()[0].is_ascii_digit() {
        result.insert(0, '_');
    }
    if is_glsl_reserved(&result) {
        result.insert(0, '_');
    }
    result
}

fn is_glsl_reserved(name: &str) -> bool {
    matches!(
        name,
        "float"
            | "int"
            | "bool"
            | "void"
            | "vec2"
            | "vec3"
            | "vec4"
            | "mat2"
            | "mat3"
            | "mat4"
            | "sampler2D"
            | "samplerCube"
            | "main"
            | "if"
            | "else"
            | "for"
            | "while"
            | "do"
            | "return"
            | "break"
            | "continue"
            | "discard"
            | "in"
            | "out"
            | "inout"
            | "uniform"
            | "varying"
            | "precision"
            | "true"
            | "false"
            | "texture"
            | "texture2D"
            | "gl_FragCoord"
            | "gl_FragColor"
    )
}

fn format_float(v: f64) -> String {
    if v.fract() == 0.0 {
        format!("{:.1}", v)
    } else {
        format!("{}", v)
    }
}

fn float_width(ty: &GlslTy) -> usize {
    match ty {
        GlslTy::Float => 1,
        GlslTy::Vec2 => 2,
        GlslTy::Vec3 => 3,
        GlslTy::Int | GlslTy::Bool => 0,
    }
}

fn total_float_width(types: &[GlslTy]) -> Option<usize> {
    let mut total = 0;
    for ty in types {
        let w = float_width(ty);
        if w == 0 {
            return None;
        }
        total += w;
    }
    Some(total)
}

/// Expand output variable names into individual float component expressions.
/// e.g. ["o0", "o1"] with types [Vec2, Float] → ["o0.x", "o0.y", "o1"]
fn expand_to_floats(names: &[String], types: &[GlslTy]) -> Vec<String> {
    let mut comps = Vec::new();
    for (name, ty) in names.iter().zip(types.iter()) {
        match ty {
            GlslTy::Float => comps.push(name.clone()),
            GlslTy::Vec2 => {
                comps.push(format!("{}.x", name));
                comps.push(format!("{}.y", name));
            }
            GlslTy::Vec3 => {
                comps.push(format!("{}.x", name));
                comps.push(format!("{}.y", name));
                comps.push(format!("{}.z", name));
            }
            _ => {}
        }
    }
    comps
}

/// Build UV component expressions from the cell's input float width.
///
/// Padding rules (ad-hoc, easy to change):
/// - 2 components → [uv.x, uv.y]        (full UV)
/// - 1 component  → [uv.x]              (x only, y=0 implicit)
fn build_uv_components(input_width: usize) -> Result<Vec<&'static str>, String> {
    match input_width {
        2 => Ok(vec!["uv.x", "uv.y"]),
        1 => Ok(vec!["uv.x"]),
        n => Err(format!(
            "source must have 1..=2 float components, got {}",
            n
        )),
    }
}

/// Build the RGB vec3 expression from output float components.
///
/// Padding rules (ad-hoc, easy to change):
/// - 3 components → vec3(r, g, b)          (direct RGB)
/// - 2 components → vec3(r, g, 0.0)        (RG, B=0)
/// - 1 component  → vec3(x, x, x)          (grayscale)
fn build_rgb_expr(float_comps: &[String]) -> Result<String, String> {
    match float_comps.len() {
        3 => Ok(format!("vec3({}, {}, {})", float_comps[0], float_comps[1], float_comps[2])),
        2 => Ok(format!("vec3({}, {}, 0.0)", float_comps[0], float_comps[1])),
        1 => Ok(format!("vec3({})", float_comps[0])),
        n => Err(format!("target must have 1..=3 float components, got {}", n)),
    }
}

fn prim_to_glsl_ty(
    prim_id: PrimId,
    prim_names: &HashMap<PrimId, String>,
) -> Result<GlslTy, String> {
    let name = prim_names
        .get(&prim_id)
        .ok_or_else(|| format!("unknown type prim {}", prim_id))?;
    match name.as_str() {
        "sys::f32" => Ok(GlslTy::Float),
        "sys::f32x2" => Ok(GlslTy::Vec2),
        "sys::f32x3" => Ok(GlslTy::Vec3),
        "sys::u32" => Ok(GlslTy::Int),
        "sys::i32" => Ok(GlslTy::Int),
        "sys::bool" => Ok(GlslTy::Bool),
        _ => Err(format!("unsupported type for GLSL: {}", name)),
    }
}

fn extract_1cell_types(
    pc: &PureCell,
    prim_names: &HashMap<PrimId, String>,
) -> Result<Vec<GlslTy>, String> {
    match pc {
        PureCell::Prim(prim, _, dim) => {
            if dim.effective == 0 {
                // 0-cell (base object) → no value slots
                Ok(vec![])
            } else {
                Ok(vec![prim_to_glsl_ty(prim.id, prim_names)?])
            }
        }
        PureCell::Comp(_, children, _) => {
            let mut types = Vec::new();
            for child in children {
                types.extend(extract_1cell_types(child, prim_names)?);
            }
            Ok(types)
        }
    }
}

// --- Public API ---

/// Result of compiling a cell to GLSL.
#[derive(Debug)]
pub struct GlslFunction {
    /// The GLSL function body (declarations and assignments).
    pub body: String,
    /// Input parameter types (in order).
    pub inputs: Vec<GlslTy>,
    /// Output types (in order).
    pub outputs: Vec<GlslTy>,
}

impl GlslFunction {
    /// Format as a GLSL function definition.
    /// The function takes inputs as parameters and returns outputs via out parameters.
    pub fn to_function(&self, name: &str) -> String {
        let safe_name = sanitize_glsl_name(name);
        let mut s = String::new();

        // Build parameter list
        let mut params = Vec::new();
        for (i, ty) in self.inputs.iter().enumerate() {
            params.push(format!("in {} i{}", ty.decl(), i));
        }
        for (i, ty) in self.outputs.iter().enumerate() {
            params.push(format!("out {} o{}", ty.decl(), i));
        }

        s.push_str(&format!("void {}({}) {{\n", safe_name, params.join(", ")));
        for line in self.body.lines() {
            s.push_str("    ");
            s.push_str(line);
            s.push('\n');
        }
        s.push_str("}\n");
        s
    }

    /// Build a `vec3 <name>(vec2 uv)` wrapper function that calls the cell
    /// function and returns an RGB color.
    ///
    /// The cell function itself should be emitted separately via `to_function()`.
    ///
    /// Input types must total 1..=2 float components.
    /// Output types must total 1..=3 float components.
    pub fn to_color_wrapper(&self, name: &str) -> Result<String, String> {
        let input_width = total_float_width(&self.inputs)
            .ok_or("input types must be float/vec2/vec3 for fragment shader")?;
        let _ = total_float_width(&self.outputs)
            .ok_or("output types must be float/vec2/vec3 for fragment shader")?;
        let uv_comps = build_uv_components(input_width)?;

        // Build input arguments from UV components
        let mut input_args = Vec::new();
        let mut offset = 0;
        for ty in &self.inputs {
            let w = float_width(ty);
            if *ty == GlslTy::Vec2 && offset == 0 && w == 2 {
                input_args.push("uv".to_string());
            } else {
                for j in 0..w {
                    input_args.push(uv_comps[offset + j].to_string());
                }
            }
            offset += w;
        }

        // Output variable declarations and call arguments
        let mut out_decls = Vec::new();
        let mut out_names = Vec::new();
        for (i, ty) in self.outputs.iter().enumerate() {
            let oname = format!("o{}", i);
            out_decls.push(format!("    {} {};\n", ty.decl(), oname));
            out_names.push(oname);
        }

        // Function call: cell(input_args..., out_names...)
        let call_args: Vec<&str> = input_args
            .iter()
            .chain(out_names.iter())
            .map(|s| s.as_str())
            .collect();
        let call_str = format!("    cell({});\n", call_args.join(", "));

        let float_comps = expand_to_floats(&out_names, &self.outputs);
        let rgb = build_rgb_expr(&float_comps)?;

        let mut s = format!("vec3 {}(vec2 uv) {{\n", name);
        for decl in &out_decls {
            s.push_str(decl);
        }
        s.push_str(&call_str);
        s.push_str(&format!("    return {};\n", rgb));
        s.push_str("}\n");

        Ok(s)
    }
}

/// Compile a 2-cell to a GLSL function.
///
/// The cell must be a 2-cell. All primitives must have GLSL implementations.
/// No restriction on source/target types — that is checked by the caller.
pub fn compile_to_glsl(
    cell: &FreeCell,
    prim_names: &HashMap<PrimId, String>,
) -> Result<GlslFunction, String> {
    let dim = cell.pure.dim().in_space;
    if dim != 2 {
        return Err(format!("expected 2-cell, got {}-cell", dim));
    }

    let source = cell.pure.s();
    let target = cell.pure.t();
    let input_types = extract_1cell_types(&source, prim_names)?;
    let output_types = extract_1cell_types(&target, prim_names)?;

    // Create input variables
    let mut compiler = GlslCompiler::new(prim_names.clone());
    let inputs: Vec<Var> = input_types
        .iter()
        .enumerate()
        .map(|(i, ty)| Var {
            name: format!("i{}", i),
            ty: ty.clone(),
        })
        .collect();

    let outputs = compiler.compile_cell(&cell.cell, &inputs)?;

    // Assign outputs to out parameters
    let mut body = String::new();
    for line in &compiler.lines {
        body.push_str(line);
        body.push('\n');
    }
    for (i, var) in outputs.iter().enumerate() {
        body.push_str(&format!("o{} = {};\n", i, var.name));
    }

    Ok(GlslFunction {
        body,
        inputs: input_types,
        outputs: output_types,
    })
}
