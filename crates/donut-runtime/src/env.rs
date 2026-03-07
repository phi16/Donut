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
        Op { name: "sys::u32.lt", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::Bool(a < b)]),
            _ => type_error(),
        }},
        Op { name: "sys::u32.le", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::Bool(a <= b)]),
            _ => type_error(),
        }},
        Op { name: "sys::u32.to_f32", f: |_, v| match &v[0] {
            Value::U32(a) => Ok(vec![Value::F32(*a as f64)]),
            _ => type_error(),
        }},
        Op { name: "sys::u32.dup", f: |_, v| match &v[0] {
            Value::U32(a) => Ok(vec![Value::U32(*a), Value::U32(*a)]),
            _ => type_error(),
        }},
        Op { name: "sys::u32.drop", f: |_, v| match &v[0] {
            Value::U32(_) => Ok(vec![]),
            _ => type_error(),
        }},
        Op { name: "sys::u32.swap", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::U32(*b), Value::U32(*a)]),
            _ => type_error(),
        }},
        // f32
        Op { name: "sys::f32.lit", f: |args, _| Ok(vec![Value::F32(extract_nat(args)? as f64)]) },
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
        Op { name: "sys::f32.dup", f: |_, v| match &v[0] {
            Value::F32(a) => Ok(vec![Value::F32(*a), Value::F32(*a)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32.drop", f: |_, v| match &v[0] {
            Value::F32(_) => Ok(vec![]),
            _ => type_error(),
        }},
        Op { name: "sys::f32.swap", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => Ok(vec![Value::F32(*b), Value::F32(*a)]),
            _ => type_error(),
        }},
        // bool
        Op { name: "sys::bool.lit", f: |args, _| Ok(vec![Value::Bool(extract_nat(args)? != 0)]) },
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
        Op { name: "sys::bool.dup", f: |_, v| match &v[0] {
            Value::Bool(a) => Ok(vec![Value::Bool(*a), Value::Bool(*a)]),
            _ => type_error(),
        }},
        Op { name: "sys::bool.drop", f: |_, v| match &v[0] {
            Value::Bool(_) => Ok(vec![]),
            _ => type_error(),
        }},
        Op { name: "sys::bool.swap", f: |_, v| match (&v[0], &v[1]) {
            (Value::Bool(a), Value::Bool(b)) => Ok(vec![Value::Bool(*b), Value::Bool(*a)]),
            _ => type_error(),
        }},
        // f32x2
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
        Op { name: "sys::f32x2.dup", f: |_, v| match &v[0] {
            Value::F32x2(a, b) => Ok(vec![Value::F32x2(*a, *b), Value::F32x2(*a, *b)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32x2.drop", f: |_, v| match &v[0] {
            Value::F32x2(_, _) => Ok(vec![]),
            _ => type_error(),
        }},
        Op { name: "sys::f32x2.swap", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32x2(a0, a1), Value::F32x2(b0, b1)) => Ok(vec![Value::F32x2(*b0, *b1), Value::F32x2(*a0, *a1)]),
            _ => type_error(),
        }},
        // f32x3
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
        Op { name: "sys::f32x3.dup", f: |_, v| match &v[0] {
            Value::F32x3(a, b, c) => Ok(vec![Value::F32x3(*a, *b, *c), Value::F32x3(*a, *b, *c)]),
            _ => type_error(),
        }},
        Op { name: "sys::f32x3.drop", f: |_, v| match &v[0] {
            Value::F32x3(_, _, _) => Ok(vec![]),
            _ => type_error(),
        }},
        Op { name: "sys::f32x3.swap", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32x3(a0, a1, a2), Value::F32x3(b0, b1, b2)) => Ok(vec![Value::F32x3(*b0, *b1, *b2), Value::F32x3(*a0, *a1, *a2)]),
            _ => type_error(),
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
