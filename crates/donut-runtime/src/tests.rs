use crate::env::{register_env, ENV_SOURCE};
use crate::{extract_prim_id, Runtime, Value};
use donut_core::common::PrimId;
use std::collections::HashMap;

fn setup(user_code: &str) -> (Runtime, donut_lang::check::Env) {
    let code = format!("{}\n{}", ENV_SOURCE, user_code);
    let (env, errors) = donut_lang::load::load(&code);
    for (_, msg) in &errors {
        eprintln!("  warning: {}", msg);
    }

    // Build name → PrimId lookup from env entries
    let mut lookup: HashMap<String, PrimId> = HashMap::new();
    for (name, &idx) in &env.lookup {
        let entry = &env.entries[idx];
        if let Some(id) = extract_prim_id(&entry.cell.pure) {
            lookup.insert(name.clone(), id);
        }
    }

    let mut rt = Runtime::new();
    register_env(&mut rt, &lookup);
    (rt, env)
}

fn eval_entry(rt: &Runtime, env: &donut_lang::check::Env, name: &str) -> Vec<Value> {
    let idx = env.lookup.get(name).unwrap_or_else(|| panic!("entry '{}' not found", name));
    let cell = &env.entries[*idx].cell;
    rt.eval(cell, &[]).unwrap()
}

#[test]
fn test_constant() {
    let (rt, env) = setup("x = env.u32_0");
    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::U32(0)]);
}

#[test]
fn test_successor() {
    let (rt, env) = setup("\
one = env.u32_1
two = env.u32_1 env.u32_1; env.u32_add
");
    assert_eq!(eval_entry(&rt, &env, "one"), vec![Value::U32(1)]);
    assert_eq!(eval_entry(&rt, &env, "two"), vec![Value::U32(2)]);
}

#[test]
fn test_add() {
    let (rt, env) = setup("\
sum = env.u32_1 env.u32_1; env.u32_add
");
    assert_eq!(eval_entry(&rt, &env, "sum"), vec![Value::U32(2)]);
}

#[test]
fn test_mul() {
    let (rt, env) = setup("\
two = env.u32_1 env.u32_1; env.u32_add
three = two env.u32_1; env.u32_add
nine = three; env.u32_dup; env.u32_mul
");
    assert_eq!(eval_entry(&rt, &env, "three"), vec![Value::U32(3)]);
    assert_eq!(eval_entry(&rt, &env, "nine"), vec![Value::U32(9)]);
}

#[test]
fn test_parallel_and_sequential() {
    // (1 + 1) * (1 + 1 + 1) = 2 * 3 = 6
    let (rt, env) = setup("\
two = env.u32_1 env.u32_1; env.u32_add
three = two env.u32_1; env.u32_add
result = two three; env.u32_mul
");
    assert_eq!(eval_entry(&rt, &env, "result"), vec![Value::U32(6)]);
}

#[test]
fn test_bool() {
    let (rt, env) = setup("\
t = env.bool_true
f = env.bool_false
notf = env.bool_false; env.bool_not
and_tf = env.bool_true env.bool_false; env.bool_and
or_tf = env.bool_true env.bool_false; env.bool_or
");
    assert_eq!(eval_entry(&rt, &env, "t"), vec![Value::Bool(true)]);
    assert_eq!(eval_entry(&rt, &env, "f"), vec![Value::Bool(false)]);
    assert_eq!(eval_entry(&rt, &env, "notf"), vec![Value::Bool(true)]);
    assert_eq!(eval_entry(&rt, &env, "and_tf"), vec![Value::Bool(false)]);
    assert_eq!(eval_entry(&rt, &env, "or_tf"), vec![Value::Bool(true)]);
}

#[test]
fn test_comparison() {
    let (rt, env) = setup("\
one = env.u32_1
two = env.u32_1 env.u32_1; env.u32_add
eq_11 = one; env.u32_dup; env.u32_eq
lt_12 = one two; env.u32_lt
lt_21 = two one; env.u32_lt
");
    assert_eq!(eval_entry(&rt, &env, "eq_11"), vec![Value::Bool(true)]);
    assert_eq!(eval_entry(&rt, &env, "lt_12"), vec![Value::Bool(true)]);
    assert_eq!(eval_entry(&rt, &env, "lt_21"), vec![Value::Bool(false)]);
}

#[test]
fn test_f32() {
    let (rt, env) = setup("\
x = env.f32_1 env.f32_1; env.f32_add
");
    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::F32(2.0)]);
}

#[test]
fn test_conversion() {
    let (rt, env) = setup("\
x = env.u32_1 env.u32_1; env.u32_add; env.u32_to_f32
");
    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::F32(2.0)]);
}

#[test]
fn test_dup() {
    let (rt, env) = setup("\
x = env.u32_1; env.u32_dup; env.u32_add
");
    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::U32(2)]);
}
