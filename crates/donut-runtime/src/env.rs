use crate::{Runtime, Value};
use donut_core::common::PrimId;
use std::collections::HashMap;

pub const ENV_SOURCE: &str = "\
env = {
    [gray[60]]
    C: *

    [hsv[0.6]]
    u32: C → C

    [hsv[0.1]]
    f32: C → C

    [hsv[0.3]]
    bool: C → C

    // u32 constants
    u32_0: C → u32
    u32_1: C → u32

    // u32 arithmetic
    u32_add: u32 u32 → u32
    u32_sub: u32 u32 → u32
    u32_mul: u32 u32 → u32
    u32_div: u32 u32 → u32
    u32_mod: u32 u32 → u32
    u32_neg: u32 → u32

    // f32 constants
    f32_0: C → f32
    f32_1: C → f32

    // f32 arithmetic
    f32_add: f32 f32 → f32
    f32_sub: f32 f32 → f32
    f32_mul: f32 f32 → f32
    f32_div: f32 f32 → f32
    f32_neg: f32 → f32

    // bool constants
    bool_true: C → bool
    bool_false: C → bool

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

struct Op {
    name: &'static str,
    f: fn(&[Value]) -> Vec<Value>,
}

fn ops() -> Vec<Op> {
    vec![
        // u32 constants
        Op { name: "env.u32_0", f: |_| vec![Value::U32(0)] },
        Op { name: "env.u32_1", f: |_| vec![Value::U32(1)] },
        // u32 arithmetic
        Op { name: "env.u32_add", f: |v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => vec![Value::U32(a.wrapping_add(*b))],
            _ => panic!("type error"),
        }},
        Op { name: "env.u32_sub", f: |v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => vec![Value::U32(a.wrapping_sub(*b))],
            _ => panic!("type error"),
        }},
        Op { name: "env.u32_mul", f: |v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => vec![Value::U32(a.wrapping_mul(*b))],
            _ => panic!("type error"),
        }},
        Op { name: "env.u32_div", f: |v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => {
                if *b == 0 { vec![Value::U32(0)] }
                else { vec![Value::U32(a / b)] }
            }
            _ => panic!("type error"),
        }},
        Op { name: "env.u32_mod", f: |v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => {
                if *b == 0 { vec![Value::U32(0)] }
                else { vec![Value::U32(a % b)] }
            }
            _ => panic!("type error"),
        }},
        Op { name: "env.u32_neg", f: |v| match &v[0] {
            Value::U32(a) => vec![Value::U32(a.wrapping_neg())],
            _ => panic!("type error"),
        }},
        // f32 constants
        Op { name: "env.f32_0", f: |_| vec![Value::F32(0.0)] },
        Op { name: "env.f32_1", f: |_| vec![Value::F32(1.0)] },
        // f32 arithmetic
        Op { name: "env.f32_add", f: |v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => vec![Value::F32(a + b)],
            _ => panic!("type error"),
        }},
        Op { name: "env.f32_sub", f: |v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => vec![Value::F32(a - b)],
            _ => panic!("type error"),
        }},
        Op { name: "env.f32_mul", f: |v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => vec![Value::F32(a * b)],
            _ => panic!("type error"),
        }},
        Op { name: "env.f32_div", f: |v| match (&v[0], &v[1]) {
            (Value::F32(a), Value::F32(b)) => vec![Value::F32(a / b)],
            _ => panic!("type error"),
        }},
        Op { name: "env.f32_neg", f: |v| match &v[0] {
            Value::F32(a) => vec![Value::F32(-a)],
            _ => panic!("type error"),
        }},
        // bool constants
        Op { name: "env.bool_true", f: |_| vec![Value::Bool(true)] },
        Op { name: "env.bool_false", f: |_| vec![Value::Bool(false)] },
        // bool operations
        Op { name: "env.bool_not", f: |v| match &v[0] {
            Value::Bool(a) => vec![Value::Bool(!a)],
            _ => panic!("type error"),
        }},
        Op { name: "env.bool_and", f: |v| match (&v[0], &v[1]) {
            (Value::Bool(a), Value::Bool(b)) => vec![Value::Bool(*a && *b)],
            _ => panic!("type error"),
        }},
        Op { name: "env.bool_or", f: |v| match (&v[0], &v[1]) {
            (Value::Bool(a), Value::Bool(b)) => vec![Value::Bool(*a || *b)],
            _ => panic!("type error"),
        }},
        // comparison
        Op { name: "env.u32_eq", f: |v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => vec![Value::Bool(a == b)],
            _ => panic!("type error"),
        }},
        Op { name: "env.u32_lt", f: |v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => vec![Value::Bool(a < b)],
            _ => panic!("type error"),
        }},
        Op { name: "env.u32_le", f: |v| match (&v[0], &v[1]) {
            (Value::U32(a), Value::U32(b)) => vec![Value::Bool(a <= b)],
            _ => panic!("type error"),
        }},
        // conversion
        Op { name: "env.u32_to_f32", f: |v| match &v[0] {
            Value::U32(a) => vec![Value::F32(*a as f64)],
            _ => panic!("type error"),
        }},
        // structural
        Op { name: "env.u32_dup", f: |v| match &v[0] {
            Value::U32(a) => vec![Value::U32(*a), Value::U32(*a)],
            _ => panic!("type error"),
        }},
        Op { name: "env.f32_dup", f: |v| match &v[0] {
            Value::F32(a) => vec![Value::F32(*a), Value::F32(*a)],
            _ => panic!("type error"),
        }},
        Op { name: "env.bool_dup", f: |v| match &v[0] {
            Value::Bool(a) => vec![Value::Bool(*a), Value::Bool(*a)],
            _ => panic!("type error"),
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
