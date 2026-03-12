use crate::env::register_sys;
use crate::{Runtime, Value};
use donut_core::cell::Globular;
use donut_core::common::{PrimId, PureVal};
use donut_core::free_cell::FreeCell;
use donut_lang::types::env::Env;
use std::collections::HashMap;

fn prim_lookup(env: &Env) -> HashMap<String, PrimId> {
    env.prim_item
        .iter()
        .map(|(&prim_id, &item_id)| (env.items[item_id.0].cname.clone(), prim_id))
        .collect()
}

fn prim_names(env: &Env) -> HashMap<PrimId, String> {
    env.prim_item
        .iter()
        .map(|(&prim_id, &item_id)| (prim_id, env.items[item_id.0].cname.clone()))
        .collect()
}

fn find_def_cell(env: &Env, name: &str) -> FreeCell {
    let module = env.root.lookup.get(name).unwrap_or_else(|| panic!("def '{}' not found", name));
    let def_id = module.this.unwrap_or_else(|| panic!("'{}' has no def", name));
    match &env.defs[def_id.0].val {
        PureVal::Cell(pc) => FreeCell::from_pure(pc),
        other => panic!("expected Cell for '{}', got {:?}", name, other),
    }
}

fn setup(user_code: &str) -> (Runtime, Env) {
    let prelude = "import \"base\"\nimport \"ui\"\nsys = import \"sys\"\n";
    let code = format!("{}{}", prelude, user_code);
    let (env, errors) = donut_lang::load::load(&code);
    for (_, msg) in &errors {
        eprintln!("  warning: {}", msg);
    }

    let lookup = prim_lookup(&env);
    let mut rt = Runtime::new();
    register_sys(&mut rt, &lookup);
    rt.set_def_subs(env.def_subs.clone());
    (rt, env)
}

fn eval_entry(rt: &Runtime, env: &Env, name: &str) -> Vec<Value> {
    let cell = find_def_cell(env, name);
    let pn = prim_names(env);
    rt.eval(&cell, &[], &pn).unwrap()
}

/// Evaluate with DeclDef expansion applied.
fn eval_entry_expanded(rt: &Runtime, env: &Env, name: &str) -> Vec<Value> {
    let module = env.root.lookup.get(name).unwrap_or_else(|| panic!("def '{}' not found", name));
    let def_id = module.this.unwrap();
    let pc = match &env.defs[def_id.0].val {
        PureVal::Cell(pc) => pc,
        other => panic!("expected Cell for '{}', got {:?}", name, other),
    };
    let expanded = rt.expand(pc);
    let pn = prim_names(env);
    rt.eval(&expanded, &[], &pn).unwrap()
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
    let (rt, env) = setup(
        "\
one = sys.u32.lit[1]
two = sys.u32.lit[1] sys.u32.lit[1]; sys.u32.add
",
    );
    assert_eq!(eval_entry(&rt, &env, "one"), vec![Value::U32(1)]);
    assert_eq!(eval_entry(&rt, &env, "two"), vec![Value::U32(2)]);
}

#[test]
fn test_add() {
    let (rt, env) = setup(
        "\
sum = sys.u32.lit[1] sys.u32.lit[1]; sys.u32.add
",
    );
    assert_eq!(eval_entry(&rt, &env, "sum"), vec![Value::U32(2)]);
}

#[test]
fn test_mul() {
    let (rt, env) = setup(
        "\
two = sys.u32.lit[1] sys.u32.lit[1]; sys.u32.add
three = two sys.u32.lit[1]; sys.u32.add
nine = three; sys.val.dup[sys.u32]; sys.u32.mul
",
    );
    assert_eq!(eval_entry(&rt, &env, "three"), vec![Value::U32(3)]);
    assert_eq!(eval_entry(&rt, &env, "nine"), vec![Value::U32(9)]);
}

#[test]
fn test_parallel_and_sequential() {
    let (rt, env) = setup(
        "\
two = sys.u32.lit[1] sys.u32.lit[1]; sys.u32.add
three = two sys.u32.lit[1]; sys.u32.add
result = two three; sys.u32.mul
",
    );
    assert_eq!(eval_entry(&rt, &env, "result"), vec![Value::U32(6)]);
}

#[test]
fn test_bool() {
    let (rt, env) = setup(
        "\
t = sys.bool.lit[1]
f = sys.bool.lit[0]
notf = sys.bool.lit[0]; sys.bool.not
and_tf = sys.bool.lit[1] sys.bool.lit[0]; sys.bool.and
or_tf = sys.bool.lit[1] sys.bool.lit[0]; sys.bool.or
",
    );
    assert_eq!(eval_entry(&rt, &env, "t"), vec![Value::Bool(true)]);
    assert_eq!(eval_entry(&rt, &env, "f"), vec![Value::Bool(false)]);
    assert_eq!(eval_entry(&rt, &env, "notf"), vec![Value::Bool(true)]);
    assert_eq!(eval_entry(&rt, &env, "and_tf"), vec![Value::Bool(false)]);
    assert_eq!(eval_entry(&rt, &env, "or_tf"), vec![Value::Bool(true)]);
}

#[test]
fn test_comparison() {
    let (rt, env) = setup(
        "\
one = sys.u32.lit[1]
two = sys.u32.lit[1] sys.u32.lit[1]; sys.u32.add
eq_11 = one; sys.val.dup[sys.u32]; sys.u32.eq
lt_12 = one two; sys.u32.lt
lt_21 = two one; sys.u32.lt
",
    );
    assert_eq!(eval_entry(&rt, &env, "eq_11"), vec![Value::Bool(true)]);
    assert_eq!(eval_entry(&rt, &env, "lt_12"), vec![Value::Bool(true)]);
    assert_eq!(eval_entry(&rt, &env, "lt_21"), vec![Value::Bool(false)]);
}

#[test]
fn test_f32() {
    let (rt, env) = setup(
        "\
x = sys.f32.lit[1] sys.f32.lit[1]; sys.f32.add
",
    );
    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::F32(2.0)]);
}

#[test]
fn test_conversion() {
    let (rt, env) = setup(
        "\
x = sys.u32.lit[1] sys.u32.lit[1]; sys.u32.add; sys.u32.to_f32
",
    );
    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::F32(2.0)]);
}

#[test]
fn test_dup() {
    let (rt, env) = setup(
        "\
x = sys.u32.lit[1]; sys.val.dup[sys.u32]; sys.u32.add
",
    );
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

    assert!(
        rt.is_evaluable(&find_def_cell(&env, "result")),
        "result should be evaluable"
    );
    assert_eq!(eval_entry(&rt, &env, "result"), vec![Value::U32(6)]);

    let result2_cell = find_def_cell(&env, "result2");
    assert!(
        !rt.is_evaluable(&result2_cell),
        "result2 needs 2 inputs, should not be evaluable with no input"
    );
}

#[test]
fn test_functor_application() {
    let (rt, env) = setup(
        "\
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
F(mycat.dup) = sys.val.dup[sys.u32]
one = F(mycat.zero; mycat.succ)
two = F(mycat.zero; mycat.succ; mycat.succ)
sum = F(mycat.zero; mycat.succ) F(mycat.zero; mycat.succ); F(mycat.add)
",
    );
    assert_eq!(eval_entry(&rt, &env, "one"), vec![Value::U32(1)]);
    assert_eq!(eval_entry(&rt, &env, "two"), vec![Value::U32(2)]);
    assert_eq!(eval_entry(&rt, &env, "sum"), vec![Value::U32(2)]);
}

#[test]
fn test_parametric_functor_mapping() {
    let (rt, env) = setup(
        "\
C: *
K: C → C
x[n: nat]: C → K

F: C ~> sys.C
F(K) = sys.u32
[n: nat] F(x[n]) = sys.u32.lit[n]

result = F(x[32])
",
    );
    assert!(
        rt.is_evaluable(&find_def_cell(&env, "result")),
        "result should be evaluable"
    );
    assert_eq!(eval_entry(&rt, &env, "result"), vec![Value::U32(32)]);
}

#[test]
fn test_functor_2cell() {
    let (_rt, env) = setup(
        "\
C: *
x: C → C
th: x → x

F: C ~> sys.C
F(x) = sys.u32
F(th) = sys.u32

result = F(th)
",
    );
    let result_cell = find_def_cell(&env, "result");
    assert_eq!(result_cell.pure.dim().in_space, 2);
}

// --- import "sys" (without named binding) ---

fn setup_bare(user_code: &str) -> (Runtime, Env) {
    let prelude = "import \"base\"\nimport \"ui\"\nimport \"sys\"\n";
    let code = format!("{}{}", prelude, user_code);
    let (env, errors) = donut_lang::load::load(&code);
    for (_, msg) in &errors {
        eprintln!("  warning: {}", msg);
    }

    let lookup = prim_lookup(&env);
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
    let (rt, env) = setup_bare(
        "\
sum = u32.lit[1] u32.lit[1]; u32.add
",
    );
    assert_eq!(eval_entry(&rt, &env, "sum"), vec![Value::U32(2)]);
}

#[test]
fn test_bare_bool() {
    let (rt, env) = setup_bare(
        "\
t = bool.lit[1]
notf = bool.lit[0]; bool.not
",
    );
    assert_eq!(eval_entry(&rt, &env, "t"), vec![Value::Bool(true)]);
    assert_eq!(eval_entry(&rt, &env, "notf"), vec![Value::Bool(true)]);
}

#[test]
fn test_bare_f32() {
    let (rt, env) = setup_bare(
        "\
x = f32.lit[1] f32.lit[1]; f32.add
",
    );
    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::F32(2.0)]);
}

#[test]
fn test_bare_functor() {
    let (rt, env) = setup_bare(
        "\
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
",
    );
    assert_eq!(eval_entry(&rt, &env, "one"), vec![Value::U32(1)]);
    assert_eq!(eval_entry(&rt, &env, "sum"), vec![Value::U32(2)]);
}

// --- canonical name consistency ---

#[test]
fn test_canonical_names_match() {
    let named_code = "import \"base\"\nimport \"ui\"\nsys = import \"sys\"\n";
    let bare_code = "import \"base\"\nimport \"ui\"\nimport \"sys\"\n";

    let (named_env, _) = donut_lang::load::load(named_code);
    let (bare_env, _) = donut_lang::load::load(bare_code);

    let mut named_names: Vec<String> = named_env
        .prim_item
        .iter()
        .map(|(&_, &item_id)| named_env.items[item_id.0].cname.clone())
        .collect();
    let mut bare_names: Vec<String> = bare_env
        .prim_item
        .iter()
        .map(|(&_, &item_id)| bare_env.items[item_id.0].cname.clone())
        .collect();
    named_names.sort();
    bare_names.sort();

    assert_eq!(
        named_names, bare_names,
        "canonical names should be identical regardless of import style"
    );
}

// --- cross-import (module that imports another module) ---

fn setup_with_sources(
    user_code: &str,
    sources: HashMap<String, String>,
) -> (Runtime, Env) {
    let (env, errors) = donut_lang::load::load_with_sources(user_code, sources);
    for (_, msg) in &errors {
        eprintln!("  warning: {}", msg);
    }

    let lookup = prim_lookup(&env);
    let mut rt = Runtime::new();
    register_sys(&mut rt, &lookup);
    (rt, env)
}

#[test]
fn test_cross_import_named() {
    let mut sources = HashMap::new();
    sources.insert(
        "mylib".to_string(),
        "\
import \"base\"
import \"ui\"
import \"sys\"
inc: u32 → u32
inc = u32.lit[1] u32; u32.add
"
        .to_string(),
    );

    let code = "import \"base\"\nimport \"ui\"\nmylib = import \"mylib\"\nx = mylib.u32.lit[0]; mylib.inc; mylib.inc\n";
    let (rt, env) = setup_with_sources(code, sources);
    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::U32(2)]);
}

#[test]
fn test_cross_import_bare() {
    let mut sources = HashMap::new();
    sources.insert(
        "mylib".to_string(),
        "\
import \"base\"
import \"ui\"
import \"sys\"
inc: u32 → u32
inc = u32.lit[1] u32; u32.add
"
        .to_string(),
    );

    let code = "import \"base\"\nimport \"ui\"\nimport \"mylib\"\nx = u32.lit[0]; inc; inc; inc\n";
    let (rt, env) = setup_with_sources(code, sources);
    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::U32(3)]);
}

#[test]
fn test_cross_import_canonical_names() {
    let mut sources = HashMap::new();
    sources.insert(
        "mylib".to_string(),
        "\
import \"base\"
import \"ui\"
import \"sys\"
"
        .to_string(),
    );

    let named_code = "import \"base\"\nimport \"ui\"\nmylib = import \"mylib\"\n";
    let bare_code = "import \"base\"\nimport \"ui\"\nimport \"mylib\"\n";

    let (named_env, _) = donut_lang::load::load_with_sources(named_code, sources.clone());
    let (bare_env, _) = donut_lang::load::load_with_sources(bare_code, sources);

    let mut named_names: Vec<String> = named_env
        .prim_item
        .iter()
        .map(|(&_, &item_id)| named_env.items[item_id.0].cname.clone())
        .collect();
    let mut bare_names: Vec<String> = bare_env
        .prim_item
        .iter()
        .map(|(&_, &item_id)| bare_env.items[item_id.0].cname.clone())
        .collect();
    named_names.sort();
    bare_names.sort();

    assert_eq!(
        named_names, bare_names,
        "canonical names should match across import styles for cross-imports"
    );

    assert!(
        bare_names.iter().any(|n| n == "sys::u32.lit"),
        "expected sys::u32.lit, got: {:?}",
        bare_names
    );
    assert!(
        bare_names.iter().any(|n| n == "sys::C"),
        "expected sys::C, got: {:?}",
        bare_names
    );
}

// --- mixed import styles ---

#[test]
fn test_mixed_import() {
    let prelude = "import \"base\"\nimport \"ui\"\nimport \"sys\"\nsys = import \"sys\"\n";
    let code = format!("{}x = sys.u32.lit[1] u32.lit[2]; sys.u32.add\n", prelude);
    let (env, errors) = donut_lang::load::load(&code);
    for (_, msg) in &errors {
        eprintln!("  warning: {}", msg);
    }

    let lookup = prim_lookup(&env);
    let mut rt = Runtime::new();
    register_sys(&mut rt, &lookup);

    assert_eq!(eval_entry(&rt, &env, "x"), vec![Value::U32(3)]);
}

// --- GLSL compilation ---

fn compile_entry(
    env: &Env,
    name: &str,
) -> Result<crate::glsl::GlslFunction, String> {
    let cell = find_def_cell(env, name);
    crate::glsl::compile_to_glsl(&cell, &prim_names(env))
}

fn compile_shader(env: &Env, name: &str) -> Result<String, String> {
    let func = compile_entry(env, name)?;
    let cell_fn = func.to_function("cell");
    let wrapper = func.to_color_wrapper("cell_color")?;
    Ok(format!("{}\n{}", cell_fn, wrapper))
}

#[test]
fn test_glsl_constant_color() {
    let (_, env) = setup("my_color = sys.f32.lit[1] sys.f32.lit[0] sys.f32.lit[0]; sys.f32x3.pack");
    let func = compile_entry(&env, "my_color").unwrap();
    assert!(func.inputs.is_empty());
    assert_eq!(func.outputs, vec![crate::glsl::GlslTy::Vec3]);
    assert!(func.to_color_wrapper("cell_color").is_err());
}

#[test]
fn test_glsl_unpack_repack() {
    let (_, env) = setup(
        "\
shader = sys.f32x2.unpack sys.f32.lit[0]; sys.f32x3.pack
",
    );
    let glsl = compile_shader(&env, "shader").unwrap();

    assert!(glsl.contains("void cell("));
    assert!(glsl.contains("vec3 cell_color(vec2 uv)"));
}

#[test]
fn test_glsl_identity_style() {
    let (_, env) = setup(
        "\
shader = sys.f32x2.unpack sys.f32.lit[0]; sys.f32x3.pack
",
    );
    let glsl = compile_shader(&env, "shader").unwrap();
    assert!(glsl.contains(".x;"));
    assert!(glsl.contains(".y;"));
    assert!(glsl.contains("0.0"));
    assert!(glsl.contains("return vec3(o0.x, o0.y, o0.z);"));
}

#[test]
fn test_glsl_arithmetic() {
    let (_, env) = setup("\
shader = sys.f32x2.unpack sys.f32.lit[0.5] sys.f32.lit[0.5]; sys.f32.mul sys.f32.mul sys.f32.lit[0]; sys.f32x3.pack
");
    let glsl = compile_shader(&env, "shader").unwrap();

    assert!(glsl.contains("*"));
    assert!(glsl.contains("0.5"));
}

#[test]
fn test_glsl_dup() {
    let (_, env) = setup(
        "\
shader = sys.f32x2.unpack; sys.val.dup[sys.f32] sys.val.drop[sys.f32] sys.f32.lit[0]; sys.f32x3.pack
",
    );
    let glsl = compile_shader(&env, "shader").unwrap();

    assert!(glsl.contains("vec3 cell_color(vec2 uv)"));
}

#[test]
fn test_glsl_function_output() {
    let (_, env) = setup(
        "\
shader = sys.f32x2.unpack sys.f32.lit[0]; sys.f32x3.pack
",
    );
    let func = compile_entry(&env, "shader").unwrap();
    let fn_str = func.to_function("my_shader");
    assert!(fn_str.contains("void my_shader(in vec2 i0, out vec3 o0)"));
}

#[test]
fn test_glsl_wrong_dimension() {
    let (_, env) = setup("t: sys.C → sys.C");
    let result = compile_entry(&env, "t");
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("1-cell"));
}

#[test]
fn test_glsl_wrong_source_for_shader() {
    let (_, env) = setup(
        "\
my_cell = sys.f32.lit[1] sys.f32.lit[0] sys.f32.lit[0]; sys.f32x3.pack
",
    );
    let func = compile_entry(&env, "my_cell").unwrap();
    assert!(func.to_color_wrapper("cell_color").is_err());
}

// --- DeclDef expansion ---

#[test]
fn test_decldef_0cell() {
    // u32 := sys.u32 means u32 expands to sys.u32 at runtime
    // result uses sys.u32.lit directly (parametric DeclDef not yet supported)
    let (rt, env) = setup(
        "u32 := sys.u32\nresult = sys.u32.lit[42]",
    );
    let expanded = rt.expand(match &env.defs[env.root.lookup.get("result").unwrap().this.unwrap().0].val {
        PureVal::Cell(pc) => pc,
        _ => panic!(),
    });
    // After expansion, the cell's 0-cell boundaries should use sys.u32's PrimId
    assert!(rt.is_evaluable(&expanded));
    let pn = prim_names(&env);
    let result = rt.eval(&expanded, &[], &pn).unwrap();
    assert_eq!(result, vec![Value::U32(42)]);
}

#[test]
fn test_decldef_op() {
    // DeclDef for operations: add := sys.u32.add
    let (rt, env) = setup(
        "C := sys.C\nu32 := sys.u32\nadd := sys.u32.add\nresult: u32 u32 → u32 = sys.u32.lit[3] sys.u32.lit[4]; add",
    );
    let result = eval_entry_expanded(&rt, &env, "result");
    assert_eq!(result, vec![Value::U32(7)]);
}

#[test]
fn test_decldef_transitive() {
    // x := sys.u32, y := x — transitive expansion
    let (rt, env) = setup(
        "C := sys.C\nx := sys.u32\ny := x\nresult: y → y = sys.u32.lit[10]",
    );
    let result = eval_entry_expanded(&rt, &env, "result");
    assert_eq!(result, vec![Value::U32(10)]);
}
