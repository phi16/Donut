use crate::env::register_sys;
use crate::{Runtime, Value};
use donut_core::cell::Globular;
use donut_core::common::PrimId;
use std::collections::HashMap;

fn setup(user_code: &str) -> (Runtime, donut_lang::check::Env) {
    let prelude = "import \"base\"\nimport \"ui\"\nsys = import \"sys\"\n";
    let code = format!("{}{}", prelude, user_code);
    let (env, errors) = donut_lang::load::load(&code);
    for (_, msg) in &errors {
        eprintln!("  warning: {}", msg);
    }

    // Build name → PrimId lookup from env entries
    let mut lookup: HashMap<String, PrimId> = HashMap::new();
    for (name, &idx) in &env.lookup {
        let entry = &env.entries[idx];
        if let Some(cell) = entry.as_cell() {
            if let Some(id) = cell.pure.extract_prim_id() {
                lookup.insert(name.clone(), id);
            }
        }
    }

    let mut rt = Runtime::new();
    register_sys(&mut rt, &lookup);
    (rt, env)
}

fn eval_entry(rt: &Runtime, env: &donut_lang::check::Env, name: &str) -> Vec<Value> {
    let idx = env.lookup.get(name).unwrap_or_else(|| panic!("entry '{}' not found", name));
    let cell = env.entries[*idx].as_cell().unwrap();
    rt.eval(cell, &[]).unwrap()
}

#[test]
fn test_constant() {
    let (rt, env) = setup("x = sys.u32_lit[0]");
    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::U32(0)]);
}

#[test]
fn test_constant_42() {
    let (rt, env) = setup("x = sys.u32_lit[42]");
    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::U32(42)]);
}

#[test]
fn test_successor() {
    let (rt, env) = setup("\
one = sys.u32_lit[1]
two = sys.u32_lit[1] sys.u32_lit[1]; sys.u32_add
");
    assert_eq!(eval_entry(&rt, &env, "one"), vec![Value::U32(1)]);
    assert_eq!(eval_entry(&rt, &env, "two"), vec![Value::U32(2)]);
}

#[test]
fn test_add() {
    let (rt, env) = setup("\
sum = sys.u32_lit[1] sys.u32_lit[1]; sys.u32_add
");
    assert_eq!(eval_entry(&rt, &env, "sum"), vec![Value::U32(2)]);
}

#[test]
fn test_mul() {
    let (rt, env) = setup("\
two = sys.u32_lit[1] sys.u32_lit[1]; sys.u32_add
three = two sys.u32_lit[1]; sys.u32_add
nine = three; sys.u32_dup; sys.u32_mul
");
    assert_eq!(eval_entry(&rt, &env, "three"), vec![Value::U32(3)]);
    assert_eq!(eval_entry(&rt, &env, "nine"), vec![Value::U32(9)]);
}

#[test]
fn test_parallel_and_sequential() {
    // (1 + 1) * (1 + 1 + 1) = 2 * 3 = 6
    let (rt, env) = setup("\
two = sys.u32_lit[1] sys.u32_lit[1]; sys.u32_add
three = two sys.u32_lit[1]; sys.u32_add
result = two three; sys.u32_mul
");
    assert_eq!(eval_entry(&rt, &env, "result"), vec![Value::U32(6)]);
}

#[test]
fn test_bool() {
    let (rt, env) = setup("\
t = sys.bool_lit[1]
f = sys.bool_lit[0]
notf = sys.bool_lit[0]; sys.bool_not
and_tf = sys.bool_lit[1] sys.bool_lit[0]; sys.bool_and
or_tf = sys.bool_lit[1] sys.bool_lit[0]; sys.bool_or
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
one = sys.u32_lit[1]
two = sys.u32_lit[1] sys.u32_lit[1]; sys.u32_add
eq_11 = one; sys.u32_dup; sys.u32_eq
lt_12 = one two; sys.u32_lt
lt_21 = two one; sys.u32_lt
");
    assert_eq!(eval_entry(&rt, &env, "eq_11"), vec![Value::Bool(true)]);
    assert_eq!(eval_entry(&rt, &env, "lt_12"), vec![Value::Bool(true)]);
    assert_eq!(eval_entry(&rt, &env, "lt_21"), vec![Value::Bool(false)]);
}

#[test]
fn test_f32() {
    let (rt, env) = setup("\
x = sys.f32_lit[1] sys.f32_lit[1]; sys.f32_add
");
    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::F32(2.0)]);
}

#[test]
fn test_conversion() {
    let (rt, env) = setup("\
x = sys.u32_lit[1] sys.u32_lit[1]; sys.u32_add; sys.u32_to_f32
");
    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::F32(2.0)]);
}

#[test]
fn test_dup() {
    let (rt, env) = setup("\
x = sys.u32_lit[1]; sys.u32_dup; sys.u32_add
");
    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::U32(2)]);
}

#[test]
fn test_user_scenario() {
    let code = "\
C: *
x: C → C
one: C → x
add: x x → x

F: C ~> sys.C
F(x) = sys.u32
F(one) = sys.u32_lit[1]
F(add) = sys.u32_add

two = sys.u32_lit[1] sys.u32_lit[1]; sys.u32_add
three = two sys.u32_lit[1]; sys.u32_add
result = two three; sys.u32_mul

result2 = F(add)
";
    let (rt, env) = setup(code);

    assert!(rt.is_evaluable(&env.entries[*env.lookup.get("result").unwrap()].as_cell().unwrap()), "result should be evaluable");
    assert_eq!(eval_entry(&rt, &env, "result"), vec![Value::U32(6)]);

    let result2_cell = &env.entries[*env.lookup.get("result2").unwrap()].as_cell().unwrap();
    assert!(!rt.is_evaluable(result2_cell), "result2 needs 2 inputs, should not be evaluable with no input");
}

#[test]
fn test_functor_application() {
    let (rt, env) = setup("\
mycat = {
    C: *
    [hsv[0.6]]
    nat: C → C
    zero: C → nat
    succ: nat → nat
    add: nat nat → nat
    dup: nat → nat nat
}
F: mycat.C ~> sys.C
F(mycat.nat) = sys.u32
F(mycat.zero) = sys.u32_lit[0]
F(mycat.succ) = sys.u32_lit[1] sys.u32; sys.u32_add
F(mycat.add) = sys.u32_add
F(mycat.dup) = sys.u32_dup
one = F(mycat.zero; mycat.succ)
two = F(mycat.zero; mycat.succ; mycat.succ)
sum = F(mycat.zero; mycat.succ) F(mycat.zero; mycat.succ); F(mycat.add)
");
    assert_eq!(eval_entry(&rt, &env, "one"), vec![Value::U32(1)]);
    assert_eq!(eval_entry(&rt, &env, "two"), vec![Value::U32(2)]);
    assert_eq!(eval_entry(&rt, &env, "sum"), vec![Value::U32(2)]);
}

#[test]
fn test_parametric_functor_mapping() {
    let (rt, env) = setup("\
C: *
K: C → C
x[n: base.nat]: C → K

F: C ~> sys.C
F(K) = sys.u32
[n: base.nat] F(x[n]) = sys.u32_lit[n]

result = F(x[32])
");
    assert!(rt.is_evaluable(&env.entries[*env.lookup.get("result").unwrap()].as_cell().unwrap()), "result should be evaluable");
    assert_eq!(eval_entry(&rt, &env, "result"), vec![Value::U32(32)]);
}

#[test]
fn test_functor_2cell() {
    let (_rt, env) = setup("\
C: *
x: C → C
th: x → x

F: C ~> sys.C
F(x) = sys.u32
F(th) = sys.u32

result = F(th)
");
    let result_idx = *env.lookup.get("result").expect("result not found");
    let result_cell = &env.entries[result_idx].as_cell().unwrap();
    assert_eq!(result_cell.pure.dim().in_space, 2);
}
