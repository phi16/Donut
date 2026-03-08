use donut_core::cell::Globular;
use donut_core::common::{PrimArg, PrimId};
use donut_core::free_cell::{Cell, CellF, FreeCell};
use donut_core::pure_cell::PureCell;
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
            CellF::Comp(axis, _) => {
                Err(format!("unsupported composition axis {} for GLSL", axis))
            }
        }
    }

    fn compile_prim(&mut self, prim: &donut_core::common::Prim, inputs: &[Var]) -> Result<Vec<Var>, String> {
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
            "sys::f32.dup" => Ok(vec![inputs[0].clone(), inputs[0].clone()]),
            "sys::f32.drop" => Ok(vec![]),
            "sys::f32.swap" => Ok(vec![inputs[1].clone(), inputs[0].clone()]),

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
                    v.ty.decl(), v.name, inputs[0].name, inputs[1].name, inputs[0].name, inputs[1].name
                ));
                Ok(vec![v])
            }
            "sys::u32.neg" => Ok(self.unary(GlslTy::Int, "-", &inputs[0])),
            "sys::u32.eq" => Ok(self.compare(GlslTy::Int, "==", &inputs[0], &inputs[1])),
            "sys::u32.lt" => Ok(self.compare(GlslTy::Int, "<", &inputs[0], &inputs[1])),
            "sys::u32.le" => Ok(self.compare(GlslTy::Int, "<=", &inputs[0], &inputs[1])),
            "sys::u32.to_f32" => {
                let v = self.fresh(GlslTy::Float);
                self.emit(format!("{} {} = float({});", v.ty.decl(), v.name, inputs[0].name));
                Ok(vec![v])
            }
            "sys::u32.dup" => Ok(vec![inputs[0].clone(), inputs[0].clone()]),
            "sys::u32.drop" => Ok(vec![]),
            "sys::u32.swap" => Ok(vec![inputs[1].clone(), inputs[0].clone()]),

            // --- bool ---
            "sys::bool.lit" => {
                let n = extract_nat(&prim.args, 0)?;
                let v = self.fresh(GlslTy::Bool);
                self.emit(format!("{} {} = {};", v.ty.decl(), v.name, n != 0));
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
                    v.ty.decl(), v.name, inputs[0].name, inputs[1].name
                ));
                Ok(vec![v])
            }
            "sys::bool.or" => {
                let v = self.fresh(GlslTy::Bool);
                self.emit(format!(
                    "{} {} = {} || {};",
                    v.ty.decl(), v.name, inputs[0].name, inputs[1].name
                ));
                Ok(vec![v])
            }
            "sys::bool.dup" => Ok(vec![inputs[0].clone(), inputs[0].clone()]),
            "sys::bool.drop" => Ok(vec![]),
            "sys::bool.swap" => Ok(vec![inputs[1].clone(), inputs[0].clone()]),

            // --- f32x2 ---
            "sys::f32x2.lit" => {
                let x = extract_rat(&prim.args, 0)?;
                let y = extract_rat(&prim.args, 1)?;
                let v = self.fresh(GlslTy::Vec2);
                self.emit(format!(
                    "{} {} = vec2({}, {});",
                    v.ty.decl(), v.name, format_float(x), format_float(y)
                ));
                Ok(vec![v])
            }
            "sys::f32x2.pack" => {
                let v = self.fresh(GlslTy::Vec2);
                self.emit(format!(
                    "{} {} = vec2({}, {});",
                    v.ty.decl(), v.name, inputs[0].name, inputs[1].name
                ));
                Ok(vec![v])
            }
            "sys::f32x2.unpack" => {
                let vx = self.fresh(GlslTy::Float);
                let vy = self.fresh(GlslTy::Float);
                self.emit(format!("{} {} = {}.x;", vx.ty.decl(), vx.name, inputs[0].name));
                self.emit(format!("{} {} = {}.y;", vy.ty.decl(), vy.name, inputs[0].name));
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
                    v.ty.decl(), v.name, inputs[0].name, inputs[1].name
                ));
                Ok(vec![v])
            }
            "sys::f32x2.dup" => Ok(vec![inputs[0].clone(), inputs[0].clone()]),
            "sys::f32x2.drop" => Ok(vec![]),
            "sys::f32x2.swap" => Ok(vec![inputs[1].clone(), inputs[0].clone()]),

            // --- f32x3 ---
            "sys::f32x3.lit" => {
                let x = extract_rat(&prim.args, 0)?;
                let y = extract_rat(&prim.args, 1)?;
                let z = extract_rat(&prim.args, 2)?;
                let v = self.fresh(GlslTy::Vec3);
                self.emit(format!(
                    "{} {} = vec3({}, {}, {});",
                    v.ty.decl(), v.name, format_float(x), format_float(y), format_float(z)
                ));
                Ok(vec![v])
            }
            "sys::f32x3.pack" => {
                let v = self.fresh(GlslTy::Vec3);
                self.emit(format!(
                    "{} {} = vec3({}, {}, {});",
                    v.ty.decl(), v.name, inputs[0].name, inputs[1].name, inputs[2].name
                ));
                Ok(vec![v])
            }
            "sys::f32x3.unpack" => {
                let vx = self.fresh(GlslTy::Float);
                let vy = self.fresh(GlslTy::Float);
                let vz = self.fresh(GlslTy::Float);
                self.emit(format!("{} {} = {}.x;", vx.ty.decl(), vx.name, inputs[0].name));
                self.emit(format!("{} {} = {}.y;", vy.ty.decl(), vy.name, inputs[0].name));
                self.emit(format!("{} {} = {}.z;", vz.ty.decl(), vz.name, inputs[0].name));
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
                    v.ty.decl(), v.name, inputs[0].name, inputs[1].name
                ));
                Ok(vec![v])
            }
            "sys::f32x3.dup" => Ok(vec![inputs[0].clone(), inputs[0].clone()]),
            "sys::f32x3.drop" => Ok(vec![]),
            "sys::f32x3.swap" => Ok(vec![inputs[1].clone(), inputs[0].clone()]),

            _ => Err(format!("unsupported prim for GLSL: {}", name)),
        }
    }

    // Helpers for common patterns

    fn binary(&mut self, ty: GlslTy, op: &str, a: &Var, b: &Var) -> Vec<Var> {
        let v = self.fresh(ty);
        self.emit(format!(
            "{} {} = {} {} {};",
            v.ty.decl(), v.name, a.name, op, b.name
        ));
        vec![v]
    }

    fn unary(&mut self, ty: GlslTy, op: &str, a: &Var) -> Vec<Var> {
        let v = self.fresh(ty);
        self.emit(format!("{} {} = {}{};", v.ty.decl(), v.name, op, a.name));
        vec![v]
    }

    fn compare(&mut self, _input_ty: GlslTy, op: &str, a: &Var, b: &Var) -> Vec<Var> {
        let v = self.fresh(GlslTy::Bool);
        self.emit(format!(
            "{} {} = {} {} {};",
            v.ty.decl(), v.name, a.name, op, b.name
        ));
        vec![v]
    }
}

// --- Helpers ---

fn extract_nat(args: &[PrimArg], index: usize) -> Result<u64, String> {
    match args.get(index) {
        Some(PrimArg::Nat(n)) => Ok(*n),
        _ => Err(format!("missing nat parameter at index {}", index)),
    }
}

fn extract_rat(args: &[PrimArg], index: usize) -> Result<f64, String> {
    match args.get(index) {
        Some(PrimArg::Rat(r)) => Ok(*r),
        Some(PrimArg::Nat(n)) => Ok(*n as f64),
        _ => Err(format!("missing rat parameter at index {}", index)),
    }
}

fn format_float(v: f64) -> String {
    if v.fract() == 0.0 {
        format!("{:.1}", v)
    } else {
        format!("{}", v)
    }
}

fn prim_to_glsl_ty(prim_id: PrimId, prim_names: &HashMap<PrimId, String>) -> Result<GlslTy, String> {
    let name = prim_names
        .get(&prim_id)
        .ok_or_else(|| format!("unknown type prim {}", prim_id))?;
    match name.as_str() {
        "sys::f32" => Ok(GlslTy::Float),
        "sys::f32x2" => Ok(GlslTy::Vec2),
        "sys::f32x3" => Ok(GlslTy::Vec3),
        "sys::u32" => Ok(GlslTy::Int),
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
        let mut s = String::new();

        // Build parameter list
        let mut params = Vec::new();
        for (i, ty) in self.inputs.iter().enumerate() {
            params.push(format!("in {} i{}", ty.decl(), i));
        }
        for (i, ty) in self.outputs.iter().enumerate() {
            params.push(format!("out {} o{}", ty.decl(), i));
        }

        s.push_str(&format!("void {}({}) {{\n", name, params.join(", ")));
        for line in self.body.lines() {
            s.push_str("    ");
            s.push_str(line);
            s.push('\n');
        }
        s.push_str("}\n");
        s
    }

    /// Build a complete fragment shader for f32x2 → f32x3 (UV → RGB).
    pub fn to_fragment_shader(&self) -> Result<String, String> {
        if self.inputs != [GlslTy::Vec2] {
            return Err(format!("source must be f32x2, got {:?}", self.inputs));
        }
        if self.outputs != [GlslTy::Vec3] {
            return Err(format!("target must be f32x3, got {:?}", self.outputs));
        }

        let mut shader = String::new();
        shader.push_str("precision mediump float;\n");
        shader.push_str("uniform vec2 u_resolution;\n\n");
        shader.push_str(&self.to_function("cell"));
        shader.push_str("\nvoid main() {\n");
        shader.push_str("    vec2 uv = gl_FragCoord.xy / u_resolution;\n");
        shader.push_str("    vec3 color;\n");
        shader.push_str("    cell(uv, color);\n");
        shader.push_str("    gl_FragColor = vec4(color, 1.0);\n");
        shader.push_str("}\n");

        Ok(shader)
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
