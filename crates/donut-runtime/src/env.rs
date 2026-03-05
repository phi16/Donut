use crate::{Runtime, Value};
use donut_core::common::{PrimArg, PrimId};
use std::collections::HashMap;

pub const ENV_SOURCE: &str = "\
env = {
    [gray[60]]
    C: *
    nat: *

    [hsv[0.6]]
    u32: C → C

    [hsv[0.1]]
    f32: C → C

    [hsv[0.3]]
    bool: C → C

    // u32 constants
    u32_lit[n: nat]: C → u32

    // u32 arithmetic
    u32_add: u32 u32 → u32
    u32_sub: u32 u32 → u32
    u32_mul: u32 u32 → u32
    u32_div: u32 u32 → u32
    u32_mod: u32 u32 → u32
    u32_neg: u32 → u32

    // f32 constants
    f32_lit[n: nat]: C → f32

    // f32 arithmetic
    f32_add: f32 f32 → f32
    f32_sub: f32 f32 → f32
    f32_mul: f32 f32 → f32
    f32_div: f32 f32 → f32
    f32_neg: f32 → f32

    // bool constants
    bool_lit[n: nat]: C → bool

    // bool operations
    bool_not: bool → bool
    bool_and: bool bool → bool
    bool_or: bool bool → bool

    // comparison
    u32_eq: u32 u32 → bool
    u32_lt: u32 u32 → bool
    u32_le: u32 u32 → bool

    // conversion
    u32_to_f32: u32 → f32

    // structural
    u32_dup: u32 → u32 u32
    f32_dup: f32 → f32 f32
    bool_dup: bool → bool bool
}

";

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
        // u32 constants
        Op { name: "env.u32_lit", f: |args, _| Ok(vec![Value::U32(extract_nat(args)? as u32)]) },
        // u32 arithmetic
        Op { name: "env.u32_add", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::U32(a.wrapping_add(*b))]),
            _ => type_error(),
        }},
        Op { name: "env.u32_sub", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::U32(a.wrapping_sub(*b))]),
            _ => type_error(),
        }},
        Op { name: "env.u32_mul", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::U32(a.wrapping_mul(*b))]),
            _ => type_error(),
        }},
        Op { name: "env.u32_div", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => {
                if *b == 0 { Ok(vec![Value::U32(0)]) }
                else { Ok(vec![Value::U32(a / b)]) }
            }
            _ => type_error(),
        }},
        Op { name: "env.u32_mod", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => {
                if *b == 0 { Ok(vec![Value::U32(0)]) }
                else { Ok(vec![Value::U32(a % b)]) }
            }
            _ => type_error(),
        }},
        Op { name: "env.u32_neg", f: |_, v| match &v[0] {
            Value::U32(a) => Ok(vec![Value::U32(a.wrapping_neg())]),
            _ => type_error(),
        }},
        // f32 constants
        Op { name: "env.f32_lit", f: |args, _| Ok(vec![Value::F32(extract_nat(args)? as f64)]) },
        // f32 arithmetic
        Op { name: "env.f32_add", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => Ok(vec![Value::F32(a + b)]),
            _ => type_error(),
        }},
        Op { name: "env.f32_sub", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => Ok(vec![Value::F32(a - b)]),
            _ => type_error(),
        }},
        Op { name: "env.f32_mul", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => Ok(vec![Value::F32(a * b)]),
            _ => type_error(),
        }},
        Op { name: "env.f32_div", f: |_, v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => Ok(vec![Value::F32(a / b)]),
            _ => type_error(),
        }},
        Op { name: "env.f32_neg", f: |_, v| match &v[0] {
            Value::F32(a) => Ok(vec![Value::F32(-a)]),
            _ => type_error(),
        }},
        // bool constants
        Op { name: "env.bool_lit", f: |args, _| Ok(vec![Value::Bool(extract_nat(args)? != 0)]) },
        // bool operations
        Op { name: "env.bool_not", f: |_, v| match &v[0] {
            Value::Bool(a) => Ok(vec![Value::Bool(!a)]),
            _ => type_error(),
        }},
        Op { name: "env.bool_and", f: |_, v| match (&v[0], &v[1]) {
            (Value::Bool(a), Value::Bool(b)) => Ok(vec![Value::Bool(*a && *b)]),
            _ => type_error(),
        }},
        Op { name: "env.bool_or", f: |_, v| match (&v[0], &v[1]) {
            (Value::Bool(a), Value::Bool(b)) => Ok(vec![Value::Bool(*a || *b)]),
            _ => type_error(),
        }},
        // comparison
        Op { name: "env.u32_eq", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::Bool(a == b)]),
            _ => type_error(),
        }},
        Op { name: "env.u32_lt", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::Bool(a < b)]),
            _ => type_error(),
        }},
        Op { name: "env.u32_le", f: |_, v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => Ok(vec![Value::Bool(a <= b)]),
            _ => type_error(),
        }},
        // conversion
        Op { name: "env.u32_to_f32", f: |_, v| match &v[0] {
            Value::U32(a) => Ok(vec![Value::F32(*a as f64)]),
            _ => type_error(),
        }},
        // structural
        Op { name: "env.u32_dup", f: |_, v| match &v[0] {
            Value::U32(a) => Ok(vec![Value::U32(*a), Value::U32(*a)]),
            _ => type_error(),
        }},
        Op { name: "env.f32_dup", f: |_, v| match &v[0] {
            Value::F32(a) => Ok(vec![Value::F32(*a), Value::F32(*a)]),
            _ => type_error(),
        }},
        Op { name: "env.bool_dup", f: |_, v| match &v[0] {
            Value::Bool(a) => Ok(vec![Value::Bool(*a), Value::Bool(*a)]),
            _ => type_error(),
        }},
    ]
}

/// Build a Runtime from a lookup table (entry name → PrimId).
/// Call this after loading ENV_SOURCE through donut-lang.
pub fn register_env(rt: &mut Runtime, lookup: &HashMap<String, PrimId>) {
    for op in ops() {
        if let Some(&id) = lookup.get(op.name) {
            rt.register(id, op.f);
        }
    }
}
