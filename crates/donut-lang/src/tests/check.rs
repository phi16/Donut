use crate::check::check;
use crate::convert::convert;
use crate::parse::parse;
use crate::resolve::resolve;
use crate::tokenize::tokenize;
use crate::types::env::{self, ArrowTy, Meta, Ty};
use donut_core::cell::Globular;
use donut_core::common::PureVal;
use donut_core::pure_cell::PureCell;

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

// --- PureCell construction tests ---

#[test]
fn star_decl_produces_zero_cell() {
    let env = check_ok("x: *");
    let x = get_def(&env, "x");
    match &x.val {
        PureVal::Cell(pc) => {
            assert_eq!(pc.dim().in_space, 0);
        }
        _ => panic!("expected Cell, got {:?}", x.val),
    }
}

#[test]
fn arrow_decl_produces_prim_cell() {
    let env = check_ok("a: *\nb: *\nf: a → b");
    let f = get_def(&env, "f");
    match &f.val {
        PureVal::Cell(pc) => {
            assert_eq!(pc.dim().in_space, 1);
            assert_eq!(pc.s().dim().in_space, 0);
            assert_eq!(pc.t().dim().in_space, 0);
        }
        _ => panic!("expected Cell, got {:?}", f.val),
    }
}

#[test]
fn arrow_2_cell() {
    let env = check_ok("a: *\nb: *\nf: a → b\ng: a → b\nh: f → g");
    let h = get_def(&env, "h");
    match &h.val {
        PureVal::Cell(pc) => {
            assert_eq!(pc.dim().in_space, 2);
            assert_eq!(pc.s().dim().in_space, 1);
            assert_eq!(pc.t().dim().in_space, 1);
        }
        _ => panic!("expected Cell, got {:?}", h.val),
    }
}

#[test]
fn composition_of_cells() {
    let env = check_ok("a: *\nb: *\nc: *\nf: a → b\ng: b → c");
    let f = get_def(&env, "f");
    let g = get_def(&env, "g");
    // f and g should be composable (f.t == g.s in the category sense)
    match (&f.val, &g.val) {
        (PureVal::Cell(fc), PureVal::Cell(gc)) => {
            assert_eq!(fc.dim().in_space, 1);
            assert_eq!(gc.dim().in_space, 1);
            // f.t and g.s should be the same 0-cell (b)
            assert!(fc.t().is_convertible(&gc.s()));
        }
        _ => panic!("expected Cells"),
    }
}

#[test]
fn meta_decl_not_cell() {
    let env = check_ok("x: meta");
    let x = get_def(&env, "x");
    assert!(!matches!(x.val, PureVal::Cell(_)));
}

// --- Type consistency tests ---

#[test]
fn alias_with_matching_type_ok() {
    // x: meta = * should succeed (Meta::Ty is a meta value)
    let env = check_ok("x: meta = *");
    let x = get_def(&env, "x");
    assert_eq!(x.ty, Ty::Meta);
}

#[test]
fn alias_with_cell_type_mismatch() {
    // f: a → b = a (a is 0-cell, but type says 1-cell)
    let (_, errors) = run_check("a: *\nb: *\nf: a → b = a");
    assert!(!errors.is_empty(), "expected type mismatch error");
}

#[test]
fn alias_meta_value_matches() {
    // T: meta = * should succeed
    let env = check_ok("T: meta = *");
    let t = get_def(&env, "T");
    assert_eq!(t.ty, Ty::Meta);
    assert_eq!(*env::as_meta(&t.val).unwrap(), Meta::Ty(Ty::Star));
}

// --- Type inference tests ---

#[test]
fn infer_ty_nat() {
    let env = check_ok("x = 42");
    let x = get_def(&env, "x");
    assert_eq!(x.ty, Ty::Nat);
}

#[test]
fn infer_ty_rat() {
    let env = check_ok("x = 0.5");
    let x = get_def(&env, "x");
    assert_eq!(x.ty, Ty::Rat);
}

#[test]
fn infer_ty_meta() {
    let env = check_ok("x = *");
    let x = get_def(&env, "x");
    assert_eq!(x.ty, Ty::Meta);
}

#[test]
fn infer_ty_arrow_from_type_expr() {
    // x = a → b produces Meta::Ty(Arrow(...)), so x.ty should be Meta
    let env = check_ok("a: *\nb: *\nx = a → b");
    let x = get_def(&env, "x");
    assert_eq!(x.ty, Ty::Meta);
}

#[test]
fn alias_dim_lift() {
    // a is 0-cell, declared as a → a (level 1) → should be lifted to id(a)
    let env = check_ok("a: *\nb: *\nf: a → b\nh: a → a = a");
    let h = get_def(&env, "h");
    match &h.val {
        PureVal::Cell(pc) => {
            assert_eq!(pc.dim().in_space, 1);
            // id(a) has source = a and target = a
            assert!(pc.s().is_convertible(&pc.t()));
        }
        _ => panic!("expected Cell"),
    }
}

#[test]
fn infer_ty_parametric_app() {
    // F[x: *]: x → x, a: *, y = F[a] → y's type should be a → a
    let env = check_ok("F[x: *]: x → x\na: *\ny = F[a]");
    let y = get_def(&env, "y");
    assert!(matches!(y.ty, Ty::Arrow(1, ArrowTy::To, _, _)));
}

// --- Parametric substitution tests ---

#[test]
fn parametric_app_cell_subst() {
    // F[x: *]: x → x applied to a should produce a cell with s == a, t == a
    let env = check_ok("F[x: *]: x → x\na: *\nb: *\ny = F[a]\nz = F[b]");
    let a = get_def(&env, "a");
    let b = get_def(&env, "b");
    let y = get_def(&env, "y");
    let z = get_def(&env, "z");
    match (&y.val, &z.val) {
        (PureVal::Cell(yc), PureVal::Cell(zc)) => {
            // y = F[a]: source and target should be a
            if let PureVal::Cell(ac) = &a.val {
                assert!(yc.s().is_convertible(ac));
                assert!(yc.t().is_convertible(ac));
            }
            // z = F[b]: source and target should be b
            if let PureVal::Cell(bc) = &b.val {
                assert!(zc.s().is_convertible(bc));
                assert!(zc.t().is_convertible(bc));
            }
            // y and z should have different source/target
            assert!(!yc.s().is_convertible(&zc.s()));
        }
        _ => panic!("expected Cells"),
    }
}

#[test]
fn parametric_app_two_params() {
    // G[x: *, y: *]: x → y applied to a, b
    let env = check_ok("G[x: *, y: *]: x → y\na: *\nb: *\nw = G[a, b]");
    let a = get_def(&env, "a");
    let b = get_def(&env, "b");
    let w = get_def(&env, "w");
    match &w.val {
        PureVal::Cell(wc) => {
            if let (PureVal::Cell(ac), PureVal::Cell(bc)) = (&a.val, &b.val) {
                assert!(wc.s().is_convertible(ac));
                assert!(wc.t().is_convertible(bc));
            }
        }
        _ => panic!("expected Cell"),
    }
}

#[test]
fn parametric_type_alias_subst() {
    // T[x: *] = x → x, then T[a] should be Arrow type with a on both sides
    let env = check_ok("T[x: *] = x → x\na: *\nf: T[a]");
    let f = get_def(&env, "f");
    assert!(matches!(f.ty, Ty::Arrow(1, ArrowTy::To, _, _)));
    // f's cell should have s == t == a
    match &f.val {
        PureVal::Cell(fc) => {
            assert!(fc.s().is_convertible(&fc.t()));
        }
        _ => panic!("expected Cell"),
    }
}

// --- Composition tests ---
// Space = Comp(0) = 0次合成（1-cell の逐次合成: f g means f then g）
// ; = Comp(1) = 1次合成

#[test]
fn sequential_composition() {
    // f g (space) composes 1-cells sequentially: f.t must == g.s
    let env = check_ok("a: *\nb: *\nc: *\nf: a → b\ng: b → c\nh = f g");
    let h = get_def(&env, "h");
    let a = get_def(&env, "a");
    let c = get_def(&env, "c");
    match &h.val {
        PureVal::Cell(hc) => {
            assert_eq!(hc.dim().in_space, 1);
            if let (PureVal::Cell(ac), PureVal::Cell(cc)) = (&a.val, &c.val) {
                assert!(hc.s().is_convertible(ac));
                assert!(hc.t().is_convertible(cc));
            }
        }
        _ => panic!("expected Cell"),
    }
}

#[test]
fn composition_type_inferred() {
    let env = check_ok("a: *\nb: *\nc: *\nf: a → b\ng: b → c\nh = f g");
    let h = get_def(&env, "h");
    assert!(matches!(h.ty, Ty::Arrow(1, ArrowTy::To, _, _)));
}

#[test]
fn incompatible_composition_errors() {
    // f: a → b, g: c → d — 0次合成 should fail (b != c)
    let (_, errors) = run_check("a: *\nb: *\nc: *\nd: *\nf: a → b\ng: c → d\nh = f g");
    assert!(!errors.is_empty(), "expected composition error");
}

// --- Dim lift tests ---

#[test]
fn dim_lift_0_to_1() {
    // a: * declared as a → a = a — 0-cell lifted to identity 1-cell
    let env = check_ok("a: *\nh: a → a = a");
    let h = get_def(&env, "h");
    match &h.val {
        PureVal::Cell(pc) => {
            assert_eq!(pc.dim().in_space, 1);
            assert!(pc.s().is_convertible(&pc.t()));
        }
        _ => panic!("expected Cell"),
    }
}

#[test]
fn dim_lift_source_target_check() {
    // a: *, b: *, declared as a → b = a — lift a to id(a), but target should be a, not b → error
    let (_, errors) = run_check("a: *\nb: *\nh: a → b = a");
    assert!(!errors.is_empty(), "expected target mismatch error");
}

// --- Type consistency error tests ---

#[test]
fn alias_meta_type_mismatch() {
    // x: meta = 42 — nat is not a meta value (Meta::Nat is meta!)
    // Actually Meta::Nat IS a meta value. So this should succeed.
    let env = check_ok("x: meta = 42");
    let x = get_def(&env, "x");
    assert_eq!(x.ty, Ty::Meta);
}

#[test]
fn alias_cell_for_meta_errors() {
    // a: *, x: meta = a — cell is not a meta value
    let (_, errors) = run_check("a: *\nx: meta = a");
    assert!(!errors.is_empty(), "expected meta type error");
}

// --- Arrow source/target identity tests ---

#[test]
fn arrow_source_target_cells() {
    // f: a → b — f.val should have source = a.val and target = b.val
    let env = check_ok("a: *\nb: *\nf: a → b");
    let a = get_def(&env, "a");
    let b = get_def(&env, "b");
    let f = get_def(&env, "f");
    match (&f.val, &a.val, &b.val) {
        (PureVal::Cell(fc), PureVal::Cell(ac), PureVal::Cell(bc)) => {
            assert!(fc.s().is_convertible(ac));
            assert!(fc.t().is_convertible(bc));
        }
        _ => panic!("expected Cells"),
    }
}

#[test]
fn arrow_endo_source_eq_target() {
    let env = check_ok("a: *\nf: a → a");
    let f = get_def(&env, "f");
    match &f.val {
        PureVal::Cell(fc) => {
            assert!(fc.s().is_convertible(&fc.t()));
        }
        _ => panic!("expected Cell"),
    }
}

// --- 2-cell boundary tests ---

#[test]
fn two_cell_boundary_consistency() {
    // h: f → g should have h.s.s == h.s.t's source etc.
    let env = check_ok("a: *\nb: *\nf: a → b\ng: a → b\nh: f → g");
    let h = get_def(&env, "h");
    match &h.val {
        PureVal::Cell(hc) => {
            // globular: h.s.s == h.t.s, h.s.t == h.t.t
            assert!(hc.s().s().is_convertible(&hc.t().s()));
            assert!(hc.s().t().is_convertible(&hc.t().t()));
        }
        _ => panic!("expected Cell"),
    }
}

#[test]
fn two_cell_incompatible_boundary_errors() {
    // f: a → b, g: c → d, h: f → g — should fail (f.s != g.s or f.t != g.t)
    let (_, errors) = run_check("a: *\nb: *\nc: *\nd: *\nf: a → b\ng: c → d\nh: f → g");
    assert!(!errors.is_empty(), "expected boundary mismatch error");
}

// --- Misc edge cases ---

#[test]
fn type_alias_arrow() {
    // T = a → b, f: T — f should have Arrow type
    let env = check_ok("a: *\nb: *\nT = a → b\nf: T");
    let f = get_def(&env, "f");
    assert!(matches!(f.ty, Ty::Arrow(1, ArrowTy::To, _, _)));
}

#[test]
fn infer_ty_from_cell_alias() {
    // x: *, y = x — y should have type Star (inferred from 0-cell)
    let env = check_ok("x: *\ny = x");
    let y = get_def(&env, "y");
    assert_eq!(y.ty, Ty::Star);
}

#[test]
fn infer_ty_from_1cell_alias() {
    // f: a → b, g = f — g should have Arrow type
    let env = check_ok("a: *\nb: *\nf: a → b\ng = f");
    let g = get_def(&env, "g");
    assert!(matches!(g.ty, Ty::Arrow(1, ArrowTy::To, _, _)));
}
