use crate::check::check;
use crate::convert::convert;
use crate::parse::parse;
use crate::resolve::resolve;
use crate::tokenize::tokenize;
use crate::types::env::{self, ArrowTy, Meta, Ty};
use crate::types::item::*;
use donut_core::common::PureVal;

fn run_check(code: &str) -> (env::Env, Vec<String>) {
    let (tokens, _, _) = tokenize(code.trim());
    let (program, _) = parse(&tokens);
    let (sem_prog, conv_errors) = convert(program, &tokens);
    assert!(
        conv_errors.is_empty(),
        "unexpected convert errors: {conv_errors:?}"
    );
    let (program, res_errors) = resolve(sem_prog, &tokens);
    assert!(
        res_errors.is_empty(),
        "unexpected resolve errors: {res_errors:?}"
    );
    let (env, errors) = check(&program, &tokens);
    let error_msgs: Vec<String> = errors.into_iter().map(|(_, msg)| msg).collect();
    (env, error_msgs)
}

fn check_ok(code: &str) -> env::Env {
    let (env, errors) = run_check(code);
    assert!(errors.is_empty(), "unexpected check errors: {errors:?}");
    env
}

fn get_def<'a>(env: &'a env::Env, name: &str) -> &'a env::Def {
    let module = env
        .root
        .lookup
        .get(name)
        .unwrap_or_else(|| panic!("def `{}` not found", name));
    let def_id = module.this.unwrap();
    &env.defs[def_id.0]
}

// --- Tests ---

#[test]
fn simple_decl() {
    let env = check_ok("x: *");
    let x = get_def(&env, "x");
    assert_eq!(x.ty, Ty::Star);
    assert!(x.item.is_some());
}

#[test]
fn simple_alias() {
    let env = check_ok("x = *");
    let x = get_def(&env, "x");
    assert!(x.item.is_none());
    assert_eq!(*env::as_meta(&x.val).unwrap(), Meta::Ty(Ty::Star));
}

#[test]
fn nat_literal() {
    let env = check_ok("x = 42");
    let x = get_def(&env, "x");
    assert_eq!(*env::as_meta(&x.val).unwrap(), Meta::Nat(42));
}

#[test]
fn rat_literal() {
    let env = check_ok("x = 0.5");
    let x = get_def(&env, "x");
    assert_eq!(*env::as_meta(&x.val).unwrap(), Meta::Rat(0.5));
}

#[test]
fn arrow_type_to() {
    let env = check_ok("a: *\nb: *\nf: a → b");
    let f = get_def(&env, "f");
    assert!(matches!(f.ty, Ty::Arrow(_, ArrowTy::To, _, _)));
}

#[test]
fn type_alias() {
    let env = check_ok("T = *\nx: T");
    let x = get_def(&env, "x");
    assert_eq!(x.ty, Ty::Star);
}

#[test]
fn meta_decl() {
    let env = check_ok("x: meta");
    let x = get_def(&env, "x");
    assert_eq!(x.ty, Ty::Meta);
}

#[test]
fn alias_chain() {
    let env = check_ok("A = *\nB = A\nx: B");
    let x = get_def(&env, "x");
    assert_eq!(x.ty, Ty::Star);
}

#[test]
fn decl_has_prim_id() {
    let env = check_ok("x: *");
    let x = get_def(&env, "x");
    let item = &env.items[x.item.unwrap().0];
    assert!(item.prim_id.is_some());
}

#[test]
fn alias_no_item() {
    let env = check_ok("x = 42");
    let x = get_def(&env, "x");
    assert!(x.item.is_none());
}

#[test]
fn multiple_decls_distinct_prim_ids() {
    let env = check_ok("a: *\nb: *");
    let a_item = &env.items[get_def(&env, "a").item.unwrap().0];
    let b_item = &env.items[get_def(&env, "b").item.unwrap().0];
    assert_ne!(a_item.prim_id, b_item.prim_id);
}

#[test]
fn arrow_value_as_type() {
    // a → b used as value should produce Meta::Ty(Arrow(...))
    let env = check_ok("a: *\nb: *\nT = a → b");
    let t = get_def(&env, "T");
    let meta = env::as_meta(&t.val).unwrap();
    assert!(matches!(meta, Meta::Ty(Ty::Arrow(_, ArrowTy::To, _, _))));
}

#[test]
fn root_module_has_all_defs() {
    let env = check_ok("a: *\nb = 1\nc: meta");
    assert!(env.root.lookup.contains_key("a"));
    assert!(env.root.lookup.contains_key("b"));
    assert!(env.root.lookup.contains_key("c"));
}

// --- Param tests ---

#[test]
fn param_decl_has_item() {
    let env = check_ok("A[x: *]: x → x");
    let a = get_def(&env, "A");
    assert_eq!(a.params.len(), 1);
    let param_item = &env.items[a.params[0].0];
    assert_eq!(param_item.lname, "x");
    assert_eq!(param_item.ty, Ty::Star);
    assert!(param_item.prim_id.is_some());
}

#[test]
fn param_type_is_checked() {
    let env = check_ok("T = *\nA[x: T]: *");
    let a = get_def(&env, "A");
    let param_item = &env.items[a.params[0].0];
    assert_eq!(param_item.ty, Ty::Star);
}

#[test]
fn multiple_params() {
    let env = check_ok("A[x: *, y: *]: x → y");
    let a = get_def(&env, "A");
    assert_eq!(a.params.len(), 2);
    let px = &env.items[a.params[0].0];
    let py = &env.items[a.params[1].0];
    assert_eq!(px.lname, "x");
    assert_eq!(py.lname, "y");
    assert_ne!(px.prim_id, py.prim_id);
}

// --- Scope / module tests ---

#[test]
fn scope_children_checked() {
    let env = check_ok("m = {\n  x: *\n  y: *\n}");
    let m = get_def(&env, "m");
    // m itself has no item (alias to module body)
    assert!(m.item.is_none());
    // children should be in defs
    assert!(env.defs.iter().any(|d| d.lname == "x" && d.item.is_some()));
    assert!(env.defs.iter().any(|d| d.lname == "y" && d.item.is_some()));
}

#[test]
fn scope_creates_module() {
    let env = check_ok("m = {\n  x: *\n}");
    let m_module = env.root.lookup.get("m").unwrap();
    let m_def = &env.defs[m_module.this.unwrap().0];
    assert_eq!(m_def.lname, "m");
    // Children are accessible through the module's lookup
    assert!(m_module.lookup.contains_key("x"));
}

#[test]
fn arrow_eq_type() {
    let env = check_ok("a: *\nb: *\nf: a ~ b");
    let f = get_def(&env, "f");
    assert!(matches!(f.ty, Ty::Arrow(_, ArrowTy::Eq, _, _)));
}

#[test]
fn nested_alias_to_meta() {
    let env = check_ok("A = meta\nx: A");
    let x = get_def(&env, "x");
    assert_eq!(x.ty, Ty::Meta);
}

#[test]
fn builtin_star_is_meta_typed() {
    let env = check_ok("x = *");
    let x = get_def(&env, "x");
    let meta = env::as_meta(&x.val).unwrap();
    assert_eq!(*meta, Meta::Ty(Ty::Star));
}

#[test]
fn builtin_meta_is_meta_typed() {
    let env = check_ok("x = meta");
    let x = get_def(&env, "x");
    let meta = env::as_meta(&x.val).unwrap();
    assert_eq!(*meta, Meta::Ty(Ty::Meta));
}

#[test]
fn def_references_earlier_def() {
    let env = check_ok("a: *\nb: a → a");
    let b = get_def(&env, "b");
    assert!(matches!(b.ty, Ty::Arrow(_, ArrowTy::To, _, _)));
}

#[test]
fn param_used_in_body_type() {
    let env = check_ok("F[x: *]: x → x");
    let f = get_def(&env, "F");
    assert!(matches!(f.ty, Ty::Arrow(_, ArrowTy::To, _, _)));
}

// --- Module structure tests ---

#[test]
fn module_children_in_lookup() {
    let env = check_ok("m = {\n  x: *\n  y: *\n}");
    let m = env.root.lookup.get("m").unwrap();
    assert!(m.lookup.contains_key("x"));
    assert!(m.lookup.contains_key("y"));
}

#[test]
fn nested_module() {
    let env = check_ok("a = {\n  b = {\n    c: *\n  }\n}");
    let a = env.root.lookup.get("a").unwrap();
    let b = a.lookup.get("b").unwrap();
    assert!(b.lookup.contains_key("c"));
}

#[test]
fn add_merges_modules() {
    let env = check_ok("a = {\n  x: *\n}\na += {\n  y: *\n}");
    let a = env.root.lookup.get("a").unwrap();
    assert!(a.lookup.contains_key("x"), "x should be in a");
    assert!(a.lookup.contains_key("y"), "y should be in a after +=");
}

#[test]
fn arrow_level_inferred() {
    // a, b: * (level 0) → a → b should be level 1
    let env = check_ok("a: *\nb: *\nf: a → b");
    let f = get_def(&env, "f");
    assert!(matches!(f.ty, Ty::Arrow(1, ArrowTy::To, _, _)));
}

#[test]
fn arrow_level_2() {
    // f, g: a → b (level 1) → f → g should be level 2
    let env = check_ok("a: *\nb: *\nf: a → b\ng: a → b\nh: f → g");
    let h = get_def(&env, "h");
    assert!(matches!(h.ty, Ty::Arrow(2, ArrowTy::To, _, _)));
}

#[test]
fn with_clause_members() {
    let env = check_ok("cat = {\n  x: *\n} with {\n  y: *\n}");
    let cat = env.root.lookup.get("cat").unwrap();
    assert!(cat.lookup.contains_key("x"));
    assert!(cat.lookup.contains_key("y"));
}
