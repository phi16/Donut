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
    let def_id = env.root.lookup.get(name).unwrap_or_else(|| panic!("def `{}` not found", name));
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
