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

    // Build name → PrimId lookup from prim_decls (canonical names)
    let lookup: HashMap<String, PrimId> = env.prim_decls.iter()
        .map(|(&id, decl)| (decl.name.clone(), id))
        .collect();

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
    let (rt, env) = setup("x = sys.u32.lit[0]");
    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::U32(0)]);
}

#[test]
fn test_constant_42() {
    let (rt, env) = setup("x = sys.u32.lit[42]");
    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::U32(42)]);
}

#[test]
fn test_successor() {
    let (rt, env) = setup("\
one = sys.u32.lit[1]
two = sys.u32.lit[1] sys.u32.lit[1]; sys.u32.add
");
    assert_eq!(eval_entry(&rt, &env, "one"), vec![Value::U32(1)]);
    assert_eq!(eval_entry(&rt, &env, "two"), vec![Value::U32(2)]);
}

#[test]
fn test_add() {
    let (rt, env) = setup("\
sum = sys.u32.lit[1] sys.u32.lit[1]; sys.u32.add
");
    assert_eq!(eval_entry(&rt, &env, "sum"), vec![Value::U32(2)]);
}

#[test]
fn test_mul() {
    let (rt, env) = setup("\
two = sys.u32.lit[1] sys.u32.lit[1]; sys.u32.add
three = two sys.u32.lit[1]; sys.u32.add
nine = three; sys.u32.dup; sys.u32.mul
");
    assert_eq!(eval_entry(&rt, &env, "three"), vec![Value::U32(3)]);
    assert_eq!(eval_entry(&rt, &env, "nine"), vec![Value::U32(9)]);
}

#[test]
fn test_parallel_and_sequential() {
    // (1 + 1) * (1 + 1 + 1) = 2 * 3 = 6
    let (rt, env) = setup("\
two = sys.u32.lit[1] sys.u32.lit[1]; sys.u32.add
three = two sys.u32.lit[1]; sys.u32.add
result = two three; sys.u32.mul
");
    assert_eq!(eval_entry(&rt, &env, "result"), vec![Value::U32(6)]);
}

#[test]
fn test_bool() {
    let (rt, env) = setup("\
t = sys.bool.lit[1]
f = sys.bool.lit[0]
notf = sys.bool.lit[0]; sys.bool.not
and_tf = sys.bool.lit[1] sys.bool.lit[0]; sys.bool.and
or_tf = sys.bool.lit[1] sys.bool.lit[0]; sys.bool.or
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
one = sys.u32.lit[1]
two = sys.u32.lit[1] sys.u32.lit[1]; sys.u32.add
eq_11 = one; sys.u32.dup; sys.u32.eq
lt_12 = one two; sys.u32.lt
lt_21 = two one; sys.u32.lt
");
    assert_eq!(eval_entry(&rt, &env, "eq_11"), vec![Value::Bool(true)]);
    assert_eq!(eval_entry(&rt, &env, "lt_12"), vec![Value::Bool(true)]);
    assert_eq!(eval_entry(&rt, &env, "lt_21"), vec![Value::Bool(false)]);
}

#[test]
fn test_f32() {
    let (rt, env) = setup("\
x = sys.f32.lit[1] sys.f32.lit[1]; sys.f32.add
");
    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::F32(2.0)]);
}

#[test]
fn test_conversion() {
    let (rt, env) = setup("\
x = sys.u32.lit[1] sys.u32.lit[1]; sys.u32.add; sys.u32.to_f32
");
    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::F32(2.0)]);
}

#[test]
fn test_dup() {
    let (rt, env) = setup("\
x = sys.u32.lit[1]; sys.u32.dup; sys.u32.add
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
F(one) = sys.u32.lit[1]
F(add) = sys.u32.add

two = sys.u32.lit[1] sys.u32.lit[1]; sys.u32.add
three = two sys.u32.lit[1]; sys.u32.add
result = two three; sys.u32.mul

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
F(mycat.zero) = sys.u32.lit[0]
F(mycat.succ) = sys.u32.lit[1] sys.u32; sys.u32.add
F(mycat.add) = sys.u32.add
F(mycat.dup) = sys.u32.dup
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
x[n: nat]: C → K

F: C ~> sys.C
F(K) = sys.u32
[n: nat] F(x[n]) = sys.u32.lit[n]

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

// --- import "sys" (without named binding) ---

fn setup_bare(user_code: &str) -> (Runtime, donut_lang::check::Env) {
    let prelude = "import \"base\"\nimport \"ui\"\nimport \"sys\"\n";
    let code = format!("{}{}", prelude, user_code);
    let (env, errors) = donut_lang::load::load(&code);
    for (_, msg) in &errors {
        eprintln!("  warning: {}", msg);
    }

    let lookup: HashMap<String, PrimId> = env.prim_decls.iter()
        .map(|(&id, decl)| (decl.name.clone(), id))
        .collect();

    let mut rt = Runtime::new();
    register_sys(&mut rt, &lookup);
    (rt, env)
}

#[test]
fn test_bare_import() {
    let (rt, env) = setup_bare("x = u32.lit[42]");
    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::U32(42)]);
}

#[test]
fn test_bare_add() {
    let (rt, env) = setup_bare("\
sum = u32.lit[1] u32.lit[1]; u32.add
");
    assert_eq!(eval_entry(&rt, &env, "sum"), vec![Value::U32(2)]);
}

#[test]
fn test_bare_bool() {
    let (rt, env) = setup_bare("\
t = bool.lit[1]
notf = bool.lit[0]; bool.not
");
    assert_eq!(eval_entry(&rt, &env, "t"), vec![Value::Bool(true)]);
    assert_eq!(eval_entry(&rt, &env, "notf"), vec![Value::Bool(true)]);
}

#[test]
fn test_bare_f32() {
    let (rt, env) = setup_bare("\
x = f32.lit[1] f32.lit[1]; f32.add
");
    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::F32(2.0)]);
}

#[test]
fn test_bare_functor() {
    let (rt, env) = setup_bare("\
mycat = {
    K: *
    [hsv[0.6]]
    x: K → K
    zero: K → x
    succ: x → x
    add: x x → x
}
F: mycat.K ~> C
F(mycat.x) = u32
F(mycat.zero) = u32.lit[0]
F(mycat.succ) = u32.lit[1] u32; u32.add
F(mycat.add) = u32.add
one = F(mycat.zero; mycat.succ)
sum = F(mycat.zero; mycat.succ) F(mycat.zero; mycat.succ); F(mycat.add)
");
    assert_eq!(eval_entry(&rt, &env, "one"), vec![Value::U32(1)]);
    assert_eq!(eval_entry(&rt, &env, "sum"), vec![Value::U32(2)]);
}

// --- canonical name consistency ---

#[test]
fn test_canonical_names_match() {
    // Both import styles should produce identical canonical names in prim_decls
    let named_code = "import \"base\"\nimport \"ui\"\nsys = import \"sys\"\n";
    let bare_code = "import \"base\"\nimport \"ui\"\nimport \"sys\"\n";

    let (named_env, _) = donut_lang::load::load(named_code);
    let (bare_env, _) = donut_lang::load::load(bare_code);

    let mut named_names: Vec<String> = named_env.prim_decls.values()
        .map(|d| d.name.clone()).collect();
    let mut bare_names: Vec<String> = bare_env.prim_decls.values()
        .map(|d| d.name.clone()).collect();
    named_names.sort();
    bare_names.sort();

    assert_eq!(named_names, bare_names,
        "canonical names should be identical regardless of import style");
}

// --- cross-import (module that imports another module) ---

fn setup_with_sources(user_code: &str, sources: HashMap<String, String>) -> (Runtime, donut_lang::check::Env) {
    let (env, errors) = donut_lang::load::load_with_sources(user_code, sources);
    for (_, msg) in &errors {
        eprintln!("  warning: {}", msg);
    }

    let lookup: HashMap<String, PrimId> = env.prim_decls.iter()
        .map(|(&id, decl)| (decl.name.clone(), id))
        .collect();

    let mut rt = Runtime::new();
    register_sys(&mut rt, &lookup);
    (rt, env)
}

#[test]
fn test_cross_import_named() {
    // "mylib" imports "sys" and re-exports operations
    let mut sources = HashMap::new();
    sources.insert("mylib".to_string(), "\
import \"base\"
import \"ui\"
import \"sys\"
inc: u32 → u32
inc = u32.lit[1] u32; u32.add
".to_string());

    let code = "import \"base\"\nimport \"ui\"\nmylib = import \"mylib\"\nx = mylib.u32.lit[0]; mylib.inc; mylib.inc\n";
    let (rt, env) = setup_with_sources(code, sources);
    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::U32(2)]);
}

#[test]
fn test_cross_import_bare() {
    let mut sources = HashMap::new();
    sources.insert("mylib".to_string(), "\
import \"base\"
import \"ui\"
import \"sys\"
inc: u32 → u32
inc = u32.lit[1] u32; u32.add
".to_string());

    let code = "import \"base\"\nimport \"ui\"\nimport \"mylib\"\nx = u32.lit[0]; inc; inc; inc\n";
    let (rt, env) = setup_with_sources(code, sources);
    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::U32(3)]);
}

#[test]
fn test_cross_import_canonical_names() {
    // sys items accessed through mylib should still have sys:: canonical names
    let mut sources = HashMap::new();
    sources.insert("mylib".to_string(), "\
import \"base\"
import \"ui\"
import \"sys\"
".to_string());

    let named_code = "import \"base\"\nimport \"ui\"\nmylib = import \"mylib\"\n";
    let bare_code = "import \"base\"\nimport \"ui\"\nimport \"mylib\"\n";

    let (named_env, _) = donut_lang::load::load_with_sources(named_code, sources.clone());
    let (bare_env, _) = donut_lang::load::load_with_sources(bare_code, sources);

    let mut named_names: Vec<String> = named_env.prim_decls.values()
        .map(|d| d.name.clone()).collect();
    let mut bare_names: Vec<String> = bare_env.prim_decls.values()
        .map(|d| d.name.clone()).collect();
    named_names.sort();
    bare_names.sort();

    assert_eq!(named_names, bare_names,
        "canonical names should match across import styles for cross-imports");

    // sys items accessed through mylib should retain sys:: prefix
    assert!(bare_names.iter().any(|n| n == "sys::u32.lit"),
        "expected sys::u32.lit, got: {:?}", bare_names);
    assert!(bare_names.iter().any(|n| n == "sys::C"),
        "expected sys::C, got: {:?}", bare_names);
}

// --- mixed import styles ---

#[test]
fn test_mixed_import() {
    // Use both bare and named import for sys
    let prelude = "import \"base\"\nimport \"ui\"\nimport \"sys\"\nsys = import \"sys\"\n";
    let code = format!("{}x = sys.u32.lit[1] u32.lit[2]; sys.u32.add\n", prelude);
    let (env, errors) = donut_lang::load::load(&code);
    for (_, msg) in &errors {
        eprintln!("  warning: {}", msg);
    }

    let lookup: HashMap<String, PrimId> = env.prim_decls.iter()
        .map(|(&id, decl)| (decl.name.clone(), id))
        .collect();
    let mut rt = Runtime::new();
    register_sys(&mut rt, &lookup);

    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::U32(3)]);
}
