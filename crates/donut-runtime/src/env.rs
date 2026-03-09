use crate::{extract_cell_width, Runtime, Value};
use donut_core::common::{PrimArg, PrimId};
use std::collections::HashMap;

fn extract_nat(args: &[PrimArg]) -> Result<u64, String> {
    match args.first() {
        Some(PrimArg::Nat(n)) => Ok(*n),
        _ => Err("missing nat parameter".to_string()),
    }
}

fn extract_rat(args: &[PrimArg]) -> Result<f64, String> {
    match args.first() {
        Some(PrimArg::Rat(r)) => Ok(*r),
        Some(PrimArg::Nat(n)) => Ok(*n as f64),
        _ => Err("missing rat parameter".to_string()),
    }
}

fn extract_rat_at(args: &[PrimArg], index: usize) -> Result<f64, String> {
    match args.get(index) {
        Some(PrimArg::Rat(r)) => Ok(*r),
        Some(PrimArg::Nat(n)) => Ok(*n as f64),
        _ => Err(format!("missing rat parameter at index {}", index)),
    }
}

struct Op {
    name: &'static str,
    f: fn(&[PrimArg], &[Value]) -> Result<Vec<Value>, String>,
}

fn type_error() -> Result<Vec<Value>, String> {
    Err("type error".to_string())
}

fn ops() -> Vec<Op> {
    vec![
        // u32
        Op { name: "sys::u32.lit", f: |args, _| Ok(vec![Value::U32(extract_nat(args)? as u32)]) },
        Op { name: "sys::u32.add", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::U32(a.wrapping_add(*b))]),
            _ => type_error(),
        }},
        Op { name: "sys::u32.sub", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::U32(a.wrapping_sub(*b))]),
            _ => type_error(),
        }},
        Op { name: "sys::u32.mul", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::U32(a.wrapping_mul(*b))]),
            _ => type_error(),
        }},
        Op { name: "sys::u32.div", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => {
                if *b == 0 { Ok(vec![Value::U32(0)]) }
                else { Ok(vec![Value::U32(a / b)]) }
            }
            _ => type_error(),
        }},
        Op { name: "sys::u32.mod", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => {
                if *b == 0 { Ok(vec![Value::U32(0)]) }
                else { Ok(vec![Value::U32(a % b)]) }
            }
            _ => type_error(),
        }},
        Op { name: "sys::u32.neg", f: |_, v| match &v[0] {
            Value::U32(a) => Ok(vec![Value::U32(a.wrapping_neg())]),
            _ => type_error(),
        }},
        Op { name: "sys::u32.eq", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::Bool(a == b)]),
            _ => type_error(),
        }},
        Op { name: "sys::u32.ne", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::Bool(a != b)]),
            _ => type_error(),
        }},
        Op { name: "sys::u32.lt", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::Bool(a < b)]),
            _ => type_error(),
        }},
        Op { name: "sys::u32.le", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::Bool(a <= b)]),
            _ => type_error(),
        }},
        Op { name: "sys::u32.gt", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::Bool(a > b)]),
            _ => type_error(),
        }},
        Op { name: "sys::u32.ge", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::Bool(a >= b)]),
            _ => type_error(),
        }},
        Op { name: "sys::u32.to_f32", f: |_, v| match &v[0] {
            Value::U32(a) => Ok(vec![Value::F32(*a as f64)]),
            _ => type_error(),
        }},
        // f32
        Op { name: "sys::f32.lit", f: |args, _| Ok(vec![Value::F32(extract_rat(args)?)]) },
        Op { name: "sys::f32.add", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => Ok(vec![Value::F32(a + b)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32.sub", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => Ok(vec![Value::F32(a - b)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32.mul", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => Ok(vec![Value::F32(a * b)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32.div", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => Ok(vec![Value::F32(a / b)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32.neg", f: |_, v| match &v[0] {
            Value::F32(a) => Ok(vec![Value::F32(-a)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32.eq", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => Ok(vec![Value::Bool(a == b)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32.ne", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => Ok(vec![Value::Bool(a != b)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32.lt", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => Ok(vec![Value::Bool(a < b)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32.le", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => Ok(vec![Value::Bool(a <= b)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32.gt", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => Ok(vec![Value::Bool(a > b)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32.ge", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => Ok(vec![Value::Bool(a >= b)]),
            _ => type_error(),
        }},
        // bool
        Op { name: "sys::bool.lit", f: |args, _| Ok(vec![Value::Bool(extract_nat(args)? != 0)]) },
        Op { name: "sys::bool.true", f: |_, _| Ok(vec![Value::Bool(true)]) },
        Op { name: "sys::bool.false", f: |_, _| Ok(vec![Value::Bool(false)]) },
        Op { name: "sys::bool.not", f: |_, v| match &v[0] {
            Value::Bool(a) => Ok(vec![Value::Bool(!a)]),
            _ => type_error(),
        }},
        Op { name: "sys::bool.and", f: |_, v| match (&v[0], &v[1]) {
            (Value::Bool(a), Value::Bool(b)) => Ok(vec![Value::Bool(*a && *b)]),
            _ => type_error(),
        }},
        Op { name: "sys::bool.or", f: |_, v| match (&v[0], &v[1]) {
            (Value::Bool(a), Value::Bool(b)) => Ok(vec![Value::Bool(*a || *b)]),
            _ => type_error(),
        }},
        Op { name: "sys::bool.ind", f: |_, v| {
            let n = v.len();
            let half = (n - 1) / 2;
            let cond = match &v[n - 1] {
                Value::Bool(b) => *b,
                _ => return type_error(),
            };
            if cond {
                Ok(v[..half].to_vec())
            } else {
                Ok(v[half..n - 1].to_vec())
            }
        }},
        // f32x2
        Op { name: "sys::f32x2.lit", f: |args, _| Ok(vec![Value::F32x2(extract_rat_at(args, 0)?, extract_rat_at(args, 1)?)]) },
        Op { name: "sys::f32x2.pack", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => Ok(vec![Value::F32x2(*a, *b)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32x2.unpack", f: |_, v| match &v[0] {
            Value::F32x2(a, b) => Ok(vec![Value::F32(*a), Value::F32(*b)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32x2.add", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32x2(a0, a1), Value::F32x2(b0, b1)) => Ok(vec![Value::F32x2(a0 + b0, a1 + b1)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32x2.sub", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32x2(a0, a1), Value::F32x2(b0, b1)) => Ok(vec![Value::F32x2(a0 - b0, a1 - b1)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32x2.mul", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32x2(a0, a1), Value::F32x2(b0, b1)) => Ok(vec![Value::F32x2(a0 * b0, a1 * b1)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32x2.div", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32x2(a0, a1), Value::F32x2(b0, b1)) => Ok(vec![Value::F32x2(a0 / b0, a1 / b1)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32x2.neg", f: |_, v| match &v[0] {
            Value::F32x2(a, b) => Ok(vec![Value::F32x2(-a, -b)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32x2.scale", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32(s), Value::F32x2(a, b)) => Ok(vec![Value::F32x2(s * a, s * b)]),
            _ => type_error(),
        }},
        // f32x3
        Op { name: "sys::f32x3.lit", f: |args, _| Ok(vec![Value::F32x3(extract_rat_at(args, 0)?, extract_rat_at(args, 1)?, extract_rat_at(args, 2)?)]) },
        Op { name: "sys::f32x3.pack", f: |_, v| match (&v[0], &v[1], &v[2]) {
            (Value::F32(a), Value::F32(b), Value::F32(c)) => Ok(vec![Value::F32x3(*a, *b, *c)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32x3.unpack", f: |_, v| match &v[0] {
            Value::F32x3(a, b, c) => Ok(vec![Value::F32(*a), Value::F32(*b), Value::F32(*c)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32x3.add", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32x3(a0, a1, a2), Value::F32x3(b0, b1, b2)) => Ok(vec![Value::F32x3(a0 + b0, a1 + b1, a2 + b2)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32x3.sub", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32x3(a0, a1, a2), Value::F32x3(b0, b1, b2)) => Ok(vec![Value::F32x3(a0 - b0, a1 - b1, a2 - b2)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32x3.mul", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32x3(a0, a1, a2), Value::F32x3(b0, b1, b2)) => Ok(vec![Value::F32x3(a0 * b0, a1 * b1, a2 * b2)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32x3.div", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32x3(a0, a1, a2), Value::F32x3(b0, b1, b2)) => Ok(vec![Value::F32x3(a0 / b0, a1 / b1, a2 / b2)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32x3.neg", f: |_, v| match &v[0] {
            Value::F32x3(a, b, c) => Ok(vec![Value::F32x3(-a, -b, -c)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32x3.scale", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32(s), Value::F32x3(a, b, c)) => Ok(vec![Value::F32x3(s * a, s * b, s * c)]),
            _ => type_error(),
        }},
        // i32
        Op { name: "sys::i32.lit", f: |args, _| Ok(vec![Value::I32(extract_nat(args)? as i32)]) },
        Op { name: "sys::i32.add", f: |_, v| match (&v[0], &v[1]) {
            (Value::I32(a), Value::I32(b)) => Ok(vec![Value::I32(a.wrapping_add(*b))]),
            _ => type_error(),
        }},
        Op { name: "sys::i32.sub", f: |_, v| match (&v[0], &v[1]) {
            (Value::I32(a), Value::I32(b)) => Ok(vec![Value::I32(a.wrapping_sub(*b))]),
            _ => type_error(),
        }},
        Op { name: "sys::i32.mul", f: |_, v| match (&v[0], &v[1]) {
            (Value::I32(a), Value::I32(b)) => Ok(vec![Value::I32(a.wrapping_mul(*b))]),
            _ => type_error(),
        }},
        Op { name: "sys::i32.div", f: |_, v| match (&v[0], &v[1]) {
            (Value::I32(a), Value::I32(b)) => {
                if *b == 0 { Ok(vec![Value::I32(0)]) }
                else { Ok(vec![Value::I32(a.wrapping_div(*b))]) }
            }
            _ => type_error(),
        }},
        Op { name: "sys::i32.mod", f: |_, v| match (&v[0], &v[1]) {
            (Value::I32(a), Value::I32(b)) => {
                if *b == 0 { Ok(vec![Value::I32(0)]) }
                else { Ok(vec![Value::I32(a.wrapping_rem(*b))]) }
            }
            _ => type_error(),
        }},
        Op { name: "sys::i32.neg", f: |_, v| match &v[0] {
            Value::I32(a) => Ok(vec![Value::I32(a.wrapping_neg())]),
            _ => type_error(),
        }},
        Op { name: "sys::i32.eq", f: |_, v| match (&v[0], &v[1]) {
            (Value::I32(a), Value::I32(b)) => Ok(vec![Value::Bool(a == b)]),
            _ => type_error(),
        }},
        Op { name: "sys::i32.ne", f: |_, v| match (&v[0], &v[1]) {
            (Value::I32(a), Value::I32(b)) => Ok(vec![Value::Bool(a != b)]),
            _ => type_error(),
        }},
        Op { name: "sys::i32.lt", f: |_, v| match (&v[0], &v[1]) {
            (Value::I32(a), Value::I32(b)) => Ok(vec![Value::Bool(a < b)]),
            _ => type_error(),
        }},
        Op { name: "sys::i32.le", f: |_, v| match (&v[0], &v[1]) {
            (Value::I32(a), Value::I32(b)) => Ok(vec![Value::Bool(a <= b)]),
            _ => type_error(),
        }},
        Op { name: "sys::i32.gt", f: |_, v| match (&v[0], &v[1]) {
            (Value::I32(a), Value::I32(b)) => Ok(vec![Value::Bool(a > b)]),
            _ => type_error(),
        }},
        Op { name: "sys::i32.ge", f: |_, v| match (&v[0], &v[1]) {
            (Value::I32(a), Value::I32(b)) => Ok(vec![Value::Bool(a >= b)]),
            _ => type_error(),
        }},
        Op { name: "sys::i32.to_f32", f: |_, v| match &v[0] {
            Value::I32(a) => Ok(vec![Value::F32(*a as f64)]),
            _ => type_error(),
        }},
        // val (polymorphic)
        Op { name: "sys::val.dup", f: |_, v| {
            let n = v.len();
            let mut r = v.to_vec();
            r.extend_from_slice(&v[..n]);
            Ok(r)
        }},
        Op { name: "sys::val.drop", f: |_, _| Ok(vec![]) },
        Op { name: "sys::val.swap", f: |args, v| {
            let wx = extract_cell_width(args, 0)?;
            let mut r = v[wx..].to_vec();
            r.extend_from_slice(&v[..wx]);
            Ok(r)
        }},
    ]
}

/// Build a Runtime from a lookup table (entry name → PrimId).
pub fn register_sys(rt: &mut Runtime, lookup: &HashMap<String, PrimId>) {
    if let Some(&base_id) = lookup.get("sys::C") {
        rt.set_base(base_id);
    }
    for op in ops() {
        if let Some(&id) = lookup.get(op.name) {
            rt.register(id, op.f);
        }
    }
}
