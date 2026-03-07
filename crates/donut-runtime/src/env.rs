use crate::{Runtime, Value};
use donut_core::common::{PrimArg, PrimId};
use std::collections::HashMap;

fn extract_nat(args: &[PrimArg]) -> Result<u64, String> {
    match args.first() {
        Some(PrimArg::Nat(n)) => Ok(*n),
        _ => Err("missing nat parameter".to_string()),
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
        Op { name: "sys.u32.lit", f: |args, _| Ok(vec![Value::U32(extract_nat(args)? as u32)]) },
        Op { name: "sys.u32.add", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::U32(a.wrapping_add(*b))]),
            _ => type_error(),
        }},
        Op { name: "sys.u32.sub", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::U32(a.wrapping_sub(*b))]),
            _ => type_error(),
        }},
        Op { name: "sys.u32.mul", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::U32(a.wrapping_mul(*b))]),
            _ => type_error(),
        }},
        Op { name: "sys.u32.div", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => {
                if *b == 0 { Ok(vec![Value::U32(0)]) }
                else { Ok(vec![Value::U32(a / b)]) }
            }
            _ => type_error(),
        }},
        Op { name: "sys.u32.mod", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => {
                if *b == 0 { Ok(vec![Value::U32(0)]) }
                else { Ok(vec![Value::U32(a % b)]) }
            }
            _ => type_error(),
        }},
        Op { name: "sys.u32.neg", f: |_, v| match &v[0] {
            Value::U32(a) => Ok(vec![Value::U32(a.wrapping_neg())]),
            _ => type_error(),
        }},
        Op { name: "sys.u32.eq", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::Bool(a == b)]),
            _ => type_error(),
        }},
        Op { name: "sys.u32.lt", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::Bool(a < b)]),
            _ => type_error(),
        }},
        Op { name: "sys.u32.le", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::Bool(a <= b)]),
            _ => type_error(),
        }},
        Op { name: "sys.u32.to_f32", f: |_, v| match &v[0] {
            Value::U32(a) => Ok(vec![Value::F32(*a as f64)]),
            _ => type_error(),
        }},
        Op { name: "sys.u32.dup", f: |_, v| match &v[0] {
            Value::U32(a) => Ok(vec![Value::U32(*a), Value::U32(*a)]),
            _ => type_error(),
        }},
        // f32
        Op { name: "sys.f32.lit", f: |args, _| Ok(vec![Value::F32(extract_nat(args)? as f64)]) },
        Op { name: "sys.f32.add", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => Ok(vec![Value::F32(a + b)]),
            _ => type_error(),
        }},
        Op { name: "sys.f32.sub", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => Ok(vec![Value::F32(a - b)]),
            _ => type_error(),
        }},
        Op { name: "sys.f32.mul", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => Ok(vec![Value::F32(a * b)]),
            _ => type_error(),
        }},
        Op { name: "sys.f32.div", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => Ok(vec![Value::F32(a / b)]),
            _ => type_error(),
        }},
        Op { name: "sys.f32.neg", f: |_, v| match &v[0] {
            Value::F32(a) => Ok(vec![Value::F32(-a)]),
            _ => type_error(),
        }},
        Op { name: "sys.f32.dup", f: |_, v| match &v[0] {
            Value::F32(a) => Ok(vec![Value::F32(*a), Value::F32(*a)]),
            _ => type_error(),
        }},
        // bool
        Op { name: "sys.bool.lit", f: |args, _| Ok(vec![Value::Bool(extract_nat(args)? != 0)]) },
        Op { name: "sys.bool.not", f: |_, v| match &v[0] {
            Value::Bool(a) => Ok(vec![Value::Bool(!a)]),
            _ => type_error(),
        }},
        Op { name: "sys.bool.and", f: |_, v| match (&v[0], &v[1]) {
            (Value::Bool(a), Value::Bool(b)) => Ok(vec![Value::Bool(*a && *b)]),
            _ => type_error(),
        }},
        Op { name: "sys.bool.or", f: |_, v| match (&v[0], &v[1]) {
            (Value::Bool(a), Value::Bool(b)) => Ok(vec![Value::Bool(*a || *b)]),
            _ => type_error(),
        }},
        Op { name: "sys.bool.dup", f: |_, v| match &v[0] {
            Value::Bool(a) => Ok(vec![Value::Bool(*a), Value::Bool(*a)]),
            _ => type_error(),
        }},
    ]
}

/// Build a Runtime from a lookup table (entry name → PrimId).
pub fn register_sys(rt: &mut Runtime, lookup: &HashMap<String, PrimId>) {
    for op in ops() {
        if let Some(&id) = lookup.get(op.name) {
            rt.register(id, op.f);
        }
    }
}
