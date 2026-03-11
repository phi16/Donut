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
    let error_msgs: Vec<String> = errors
        .into_iter()
        .map(|(pos, msg)| format!("{}:{}: {}", pos.line, pos.col, msg))
        .collect();
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
fn meta_decl_error() {
    let (_, errors) = run_check("x: meta");
    assert!(!errors.is_empty(), "user-defined meta type should error");
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
    let env = check_ok("a: *\nb = 1\nc = *");
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
    let (_, errors) = run_check("A = meta\nx: A");
    assert!(!errors.is_empty(), "user-defined meta type via alias should error");
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
    let (_, errors) = run_check("x: meta");
    assert!(!errors.is_empty(), "user-defined meta type should error");
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
    assert!(
        errors[0].contains("is not convertible to"),
        "error should mention convertibility: {}",
        errors[0]
    );
    // Error message should use named prim display, not raw PrimId
    assert!(
        !errors[0].contains("P"),
        "error should not contain raw PrimId: {}",
        errors[0]
    );
}

// --- Parameter count tests ---

#[test]
fn param_count_too_many() {
    let (_, errors) = run_check("import \"sys\"\nx = f32.lit[1, 2]");
    assert!(!errors.is_empty(), "expected param count error");
    assert!(errors[0].contains("expects 1 parameters, but 2 were given"), "wrong error: {}", errors[0]);
}

#[test]
fn param_count_too_few() {
    let (_, errors) = run_check("A[x: *, y: *] = { u: x → y }\nB: *\nc = A[B]");
    assert!(!errors.is_empty(), "expected param count error");
    assert!(errors[0].contains("expects 2 parameters, but 1 were given"), "wrong error: {}", errors[0]);
}

#[test]
fn param_count_zero_given_nonzero_expected() {
    let (_, errors) = run_check("A[x: *]: * = x\nB: *\nc = A");
    assert!(!errors.is_empty(), "expected param count error");
    assert!(errors[0].contains("expects 1 parameters, but 0 were given"), "wrong error: {}", errors[0]);
}

#[test]
fn param_count_correct() {
    check_ok("import \"sys\"\nx = f32.lit[1]");
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

// =============================================
// Tests migrated from old_check
// =============================================

// --- Basic (non-parametric) ---

#[test]
fn basic_cells_dim() {
    // u: 0-cell, x: 1-cell, m: 2-cell (x x → x uses composition in type)
    let env = check_ok("u: *\nx: u → u\nm: x x → x");
    let u = get_def(&env, "u");
    let x = get_def(&env, "x");
    let m = get_def(&env, "m");
    match (&u.val, &x.val, &m.val) {
        (PureVal::Cell(uc), PureVal::Cell(xc), PureVal::Cell(mc)) => {
            assert_eq!(uc.dim().in_space, 0);
            assert_eq!(xc.dim().in_space, 1);
            assert_eq!(mc.dim().in_space, 2);
            // m.s should be x x (composition)
            // m.t should be x
            assert!(mc.t().is_convertible(xc));
        }
        _ => panic!("expected Cells"),
    }
}

#[test]
fn basic_alias_assoc() {
    // Associator: a 3-cell from composed boundaries
    let env = check_ok("u: *\nx: u → u\nm: x x → x\nrm = x m; m\nlm = m x; m\nassoc: rm → lm");
    let assoc = get_def(&env, "assoc");
    match &assoc.val {
        PureVal::Cell(pc) => {
            assert_eq!(pc.dim().in_space, 3);
            // globular: assoc.s.s == assoc.t.s, assoc.s.t == assoc.t.t
            assert!(pc.s().s().is_convertible(&pc.t().s()));
            assert!(pc.s().t().is_convertible(&pc.t().t()));
        }
        _ => panic!("expected Cell"),
    }
}

#[test]
fn basic_module_path_access() {
    // cat.u used in external type
    let env = check_ok("cat = {\n  u: *\n  x: u → u\n}\nf: cat.u → cat.u");
    let f = get_def(&env, "f");
    match &f.val {
        PureVal::Cell(fc) => {
            assert_eq!(fc.dim().in_space, 1);
            assert!(fc.s().is_convertible(&fc.t()));
        }
        _ => panic!("expected Cell"),
    }
}

// --- Parametric declarations ---

#[test]
fn parametric_decl_is_cell() {
    // A[x: *]: x → x — A should be a 1-cell with distinct source/target per param
    let env = check_ok("A[x: *]: x → x");
    let a = get_def(&env, "A");
    match &a.val {
        PureVal::Cell(pc) => {
            assert_eq!(pc.dim().in_space, 1);
            // source == target (both are x)
            assert!(pc.s().is_convertible(&pc.t()));
        }
        _ => panic!("expected Cell"),
    }
}

#[test]
fn parametric_decl_one_cell_param() {
    // B[f: u → u]: f → f is a 2-cell
    let env = check_ok("u: *\nB[f: u → u]: f → f");
    let b = get_def(&env, "B");
    match &b.val {
        PureVal::Cell(pc) => {
            assert_eq!(pc.dim().in_space, 2);
        }
        _ => panic!("expected Cell"),
    }
}

// --- Instantiation ---

#[test]
fn instantiation_1cell_param() {
    // A[x: u → u]: x → x, then A[y] and A[z] with concrete params
    let env = check_ok("u: *\nA[x: u → u]: x → x\ny: u → u\nz: u → u\nay = A[y]\naz = A[z]");
    let y = get_def(&env, "y");
    let z = get_def(&env, "z");
    let ay = get_def(&env, "ay");
    let az = get_def(&env, "az");
    match (&y.val, &z.val, &ay.val, &az.val) {
        (PureVal::Cell(yc), PureVal::Cell(zc), PureVal::Cell(ayc), PureVal::Cell(azc)) => {
            // ay.s == y, ay.t == y
            assert!(ayc.s().is_convertible(yc));
            assert!(ayc.t().is_convertible(yc));
            // az.s == z, az.t == z
            assert!(azc.s().is_convertible(zc));
            assert!(azc.t().is_convertible(zc));
            // ay != az
            assert!(!ayc.is_convertible(azc));
        }
        _ => panic!("expected Cells"),
    }
}

// --- += forward reference ---

#[test]
fn add_forward_reference_source_target() {
    let env = check_ok("u: *\nf: *\nu += {\n  to_f: u → f\n}");
    let u_def = get_def(&env, "u");
    let f_def = get_def(&env, "f");
    // Find to_f in module
    let u_module = env.root.lookup.get("u").unwrap();
    let to_f_module = u_module.lookup.get("to_f").unwrap();
    let to_f = &env.defs[to_f_module.this.unwrap().0];
    match (&u_def.val, &f_def.val, &to_f.val) {
        (PureVal::Cell(uc), PureVal::Cell(fc), PureVal::Cell(tc)) => {
            assert!(tc.s().is_convertible(uc));
            assert!(tc.t().is_convertible(fc));
        }
        _ => panic!("expected Cells"),
    }
}

#[test]
fn add_chained_forward_references() {
    let env = check_ok(
        "C: *\nx: C → C with { a = x }\ny: C → x\nx += { b = y }\nz = x.b\nx += { c = z }",
    );
    // x.a = x → 1-cell
    let x_mod = env.root.lookup.get("x").unwrap();
    let a_mod = x_mod.lookup.get("a").unwrap();
    let a = &env.defs[a_mod.this.unwrap().0];
    match &a.val {
        PureVal::Cell(pc) => assert_eq!(pc.dim().in_space, 1),
        _ => panic!("expected Cell"),
    }
    // y: C → x, x is 1-cell → y is 2-cell
    let y = get_def(&env, "y");
    match &y.val {
        PureVal::Cell(pc) => assert_eq!(pc.dim().in_space, 2),
        _ => panic!("expected Cell"),
    }
    // z = x.b = y → 2-cell
    let z = get_def(&env, "z");
    match &z.val {
        PureVal::Cell(pc) => assert_eq!(pc.dim().in_space, 2),
        _ => panic!("expected Cell"),
    }
    // x.c = z → 2-cell
    let c_mod = x_mod.lookup.get("c").unwrap();
    let c = &env.defs[c_mod.this.unwrap().0];
    match &c.val {
        PureVal::Cell(pc) => assert_eq!(pc.dim().in_space, 2),
        _ => panic!("expected Cell"),
    }
}

// --- Type aliases ---

#[test]
fn type_alias_composition() {
    // T = x x → x should work as a type alias for composed boundary
    let env = check_ok("u: *\nx: u → u\nT = x x → x\nm: T");
    let m = get_def(&env, "m");
    match &m.val {
        PureVal::Cell(pc) => {
            assert_eq!(pc.dim().in_space, 2);
        }
        _ => panic!("expected Cell"),
    }
}

#[test]
fn type_alias_star_decl() {
    let env = check_ok("T = *\nu: T");
    let u = get_def(&env, "u");
    match &u.val {
        PureVal::Cell(pc) => {
            assert_eq!(pc.dim().in_space, 0);
        }
        _ => panic!("expected Cell"),
    }
}

#[test]
fn parametric_star_body() {
    // g[x: *] = * should be valid (g: meta)
    let env = check_ok("g[x: *] = *");
    let g = get_def(&env, "g");
    assert_eq!(g.ty, Ty::Meta);
}

#[test]
fn cell_param_as_body() {
    // h[x: *, y: x → x]: x → x = y
    let env = check_ok("h[x: *, y: x → x]: x → x = y");
    let h = get_def(&env, "h");
    assert!(matches!(h.ty, Ty::Arrow(1, ArrowTy::To, _, _)));
}

#[test]
fn cell_param_identity() {
    // id[C: *, x: C → C]: x → x = x
    // x is a 1-cell, x → x is level 2, so body x is lifted to id(x) = 2-cell
    let env = check_ok("id[C: *, x: C → C]: x → x = x");
    let id_def = get_def(&env, "id");
    match &id_def.val {
        PureVal::Cell(pc) => {
            assert_eq!(pc.dim().in_space, 2);
            assert!(pc.s().is_convertible(&pc.t()));
        }
        _ => panic!("expected Cell"),
    }
}

#[test]
fn type_alias_parametric_boundary() {
    // g[x: *] = x → x, m: g[u], n: g[v] — verify source/target
    let env = check_ok("g[x: *] = x → x\nu: *\nv: *\nm: g[u]\nn: g[v]");
    let u = get_def(&env, "u");
    let v = get_def(&env, "v");
    let m = get_def(&env, "m");
    let n = get_def(&env, "n");
    match (&m.val, &n.val, &u.val, &v.val) {
        (PureVal::Cell(mc), PureVal::Cell(nc), PureVal::Cell(uc), PureVal::Cell(vc)) => {
            assert!(mc.s().is_convertible(uc));
            assert!(mc.t().is_convertible(uc));
            assert!(nc.s().is_convertible(vc));
            assert!(nc.t().is_convertible(vc));
        }
        _ => panic!("expected Cells"),
    }
}

#[test]
fn type_alias_alias_chain() {
    // T = x x → x, S = T, m: S → m should be 2-cell
    let env = check_ok("u: *\nx: u → u\nT = x x → x\nS = T\nm: S");
    let m = get_def(&env, "m");
    match &m.val {
        PureVal::Cell(pc) => {
            assert_eq!(pc.dim().in_space, 2);
        }
        _ => panic!("expected Cell"),
    }
}

#[test]
fn type_alias_cross_usage() {
    let env = check_ok("u: *\nEndo[X: *] = X → X\nf: Endo[u]\ng: Endo[u]\nalpha: f → g");
    let u = get_def(&env, "u");
    let f = get_def(&env, "f");
    let g = get_def(&env, "g");
    let alpha = get_def(&env, "alpha");
    match (&f.val, &g.val, &u.val) {
        (PureVal::Cell(fc), PureVal::Cell(gc), PureVal::Cell(uc)) => {
            // f and g: u → u
            assert!(fc.s().is_convertible(uc));
            assert!(gc.s().is_convertible(uc));
        }
        _ => panic!("expected Cells"),
    }
    match &alpha.val {
        PureVal::Cell(pc) => {
            assert_eq!(pc.dim().in_space, 2);
        }
        _ => panic!("expected Cell"),
    }
}

// --- += cross reference ---

#[test]
fn add_assign_cross_ref() {
    let env = check_ok("C: *\nx: C → C\ny: C → C\nx += {\n  z: C → y\n}");
    let x_mod = env.root.lookup.get("x").unwrap();
    let z_mod = x_mod.lookup.get("z").unwrap();
    let z = &env.defs[z_mod.this.unwrap().0];
    match &z.val {
        PureVal::Cell(pc) => {
            assert_eq!(pc.dim().in_space, 2);
        }
        _ => panic!("expected Cell"),
    }
}

// --- Functor tests ---

#[test]
fn functor_basic() {
    check_ok(
        "src = {\n  C: *\n  X: C → C\n  m: X X → X\n}\ntgt = {\n  D: *\n  Y: D → D\n  n: Y Y → Y\n}\nF: src.C ~> tgt.D\nF(src.X) = tgt.Y\nF(src.m) = tgt.n",
    );
}

#[test]
fn functor_dimension_mismatch() {
    let (_, errors) = run_check(
        "src = {\n  C: *\n  X: C → C\n}\ntgt = {\n  D: *\n  Y: D → D\n  n: Y Y → Y\n}\nF: src.C ~> tgt.D\nF(src.X) = tgt.n",
    );
    assert!(!errors.is_empty(), "expected dimension mismatch error");
}

#[test]
fn functor_source_mismatch() {
    let (_, errors) = run_check(
        "src = {\n  C: *\n  X: C → C\n  Y: C → C\n  m: X X → Y\n}\ntgt = {\n  D: *\n  A: D → D\n  B: D → D\n  n: A A → A\n}\nF: src.C ~> tgt.D\nF(src.X) = tgt.A\nF(src.Y) = tgt.B\nF(src.m) = tgt.n",
    );
    assert!(!errors.is_empty(), "expected boundary mismatch error");
}

#[test]
fn functor_missing_mapping() {
    let (_, errors) = run_check(
        "src = {\n  C: *\n  X: C → C\n  Y: C → C\n  m: X → Y\n}\ntgt = {\n  D: *\n  A: D → D\n  B: D → D\n  n: A → B\n}\nF: src.C ~> tgt.D\nF(src.m) = tgt.n",
    );
    assert!(!errors.is_empty(), "expected no mapping error");
    // Error message should include functor name and use named prims
    assert!(
        errors[0].contains("functor F"),
        "error should mention functor name: {}",
        errors[0]
    );
    assert!(
        !errors[0].contains("PrimId"),
        "error should not contain raw PrimId: {}",
        errors[0]
    );
}

#[test]
fn functor_preserves_composition_boundary() {
    check_ok(
        "src = {\n  C: *\n  X: C → C\n  m: X X → X\n  u: C → X\n}\ntgt = {\n  D: *\n  Y: D → D\n  n: Y Y → Y\n  v: D → Y\n}\nF: src.C ~> tgt.D\nF(src.X) = tgt.Y\nF(src.m) = tgt.n\nF(src.u) = tgt.v",
    );
}

#[test]
fn functor_base_case_explicit() {
    // F(src.C) = tgt.D is redundant but allowed
    check_ok(
        "src = {\n  C: *\n  X: C → C\n}\ntgt = {\n  D: *\n  Y: D → D\n}\nF: src.C ~> tgt.D\nF(src.C) = tgt.D\nF(src.X) = tgt.Y",
    );
}

// --- Qname / module name tests ---

#[test]
fn qname_toplevel() {
    let env = check_ok("C: *\nx: C → C");
    let c = get_def(&env, "C");
    assert_eq!(c.qname, "C");
    let x = get_def(&env, "x");
    assert_eq!(x.qname, "x");
}

#[test]
fn qname_module() {
    let env = check_ok("cat = {\n  u: *\n  x: u → u\n}");
    // Children of cat should have "cat." prefix in qname
    let cat_mod = env.root.lookup.get("cat").unwrap();
    let u_mod = cat_mod.lookup.get("u").unwrap();
    let u = &env.defs[u_mod.this.unwrap().0];
    assert_eq!(u.qname, "cat.u");
    let x_mod = cat_mod.lookup.get("x").unwrap();
    let x = &env.defs[x_mod.this.unwrap().0];
    assert_eq!(x.qname, "cat.x");
}

#[test]
fn qname_nested_module() {
    let env = check_ok("a = {\n  b = {\n    u: *\n  }\n}");
    let a_mod = env.root.lookup.get("a").unwrap();
    let b_mod = a_mod.lookup.get("b").unwrap();
    let u_mod = b_mod.lookup.get("u").unwrap();
    let u = &env.defs[u_mod.this.unwrap().0];
    assert_eq!(u.qname, "a.b.u");
}

#[test]
fn qname_with_clause() {
    let env = check_ok("C: *\nx: C → C with {\n  m: x x → x\n}");
    let x_mod = env.root.lookup.get("x").unwrap();
    let m_mod = x_mod.lookup.get("m").unwrap();
    let m = &env.defs[m_mod.this.unwrap().0];
    assert_eq!(m.qname, "x.m");
}

#[test]
fn qname_add_clause() {
    let env = check_ok("C: *\nx: C → C\nx += {\n  m: x x → x\n}");
    let x_mod = env.root.lookup.get("x").unwrap();
    let m_mod = x_mod.lookup.get("m").unwrap();
    let m = &env.defs[m_mod.this.unwrap().0];
    assert_eq!(m.qname, "x.m");
}

// --- Module body with ref ---

#[test]
fn module_body_with_ref() {
    let env = check_ok("x = {\n  a: *\n}\ny = x with {\n  b: a → a\n}");
    let y_mod = env.root.lookup.get("y").unwrap();
    assert!(y_mod.lookup.contains_key("a"));
    assert!(y_mod.lookup.contains_key("b"));
}

// --- Parametric module instantiation ---

fn get_member_def<'a>(env: &'a env::Env, parent: &str, child: &str) -> &'a env::Def {
    let parent_mod = env.root.lookup.get(parent).unwrap_or_else(|| panic!("module `{}` not found", parent));
    let child_mod = parent_mod.lookup.get(child).unwrap_or_else(|| panic!("member `{}.{}` not found", parent, child));
    &env.defs[child_mod.this.unwrap().0]
}

#[test]
fn parametric_module() {
    let env = check_ok(
        "cat[C: *] = {\n  x: C → C\n  m: x x → x\n}\nu: *\nv: *\ncu = cat[u]\ncv = cat[v]",
    );
    let u = get_def(&env, "u");
    let v = get_def(&env, "v");

    // cu.x should have source/target = u
    let cu_x = get_member_def(&env, "cu", "x");
    match (&u.val, &cu_x.val) {
        (PureVal::Cell(uc), PureVal::Cell(cxc)) => {
            assert_eq!(cxc.s(), *uc);
            assert_eq!(cxc.t(), *uc);
        }
        _ => panic!("expected Cell values"),
    }

    // cv.x should have source/target = v
    let cv_x = get_member_def(&env, "cv", "x");
    match (&v.val, &cv_x.val) {
        (PureVal::Cell(vc), PureVal::Cell(cxc)) => {
            assert_eq!(cxc.s(), *vc);
            assert_eq!(cxc.t(), *vc);
        }
        _ => panic!("expected Cell values"),
    }

    // cu.x ≠ cv.x
    assert_ne!(cu_x.val, cv_x.val);

    // cu.m should be a 2-cell
    let cu_m = get_member_def(&env, "cu", "m");
    match &cu_m.val {
        PureVal::Cell(pc) => assert_eq!(pc.dim().in_space, 2),
        _ => panic!("expected Cell"),
    }
}

#[test]
fn parametric_module_composition() {
    let env = check_ok(
        "cat[C: *] = {\n  x: C → C\n  m: x x → x\n}\nu: *\nc = cat[u]\ndouble = c.x c.x; c.m",
    );
    let double = get_def(&env, "double");
    match &double.val {
        PureVal::Cell(pc) => assert_eq!(pc.dim().in_space, 2),
        _ => panic!("expected Cell"),
    }
}

#[test]
fn prim_args_distinguish_instances() {
    let env = check_ok(
        "cat[C: *] = {\n  T[x: C → C]: x → x\n}\nu: *\nc = cat[u]\na: u → u\nb: u → u\nta = c.T[a]\ntb = c.T[b]",
    );
    let ta = get_def(&env, "ta");
    let tb = get_def(&env, "tb");
    assert_ne!(ta.val, tb.val);
    match (&ta.val, &tb.val) {
        (PureVal::Cell(ac), PureVal::Cell(bc)) => {
            assert_eq!(ac.dim().in_space, 2);
            assert_eq!(bc.dim().in_space, 2);
            assert_ne!(ac.s(), bc.s());
        }
        _ => panic!("expected Cell values"),
    }
}

#[test]
fn nested_module_instantiation_two_levels() {
    let env = check_ok(
        "outer[C: *] = {\n  inner = {\n    x: C → C\n  }\n}\nu: *\nv: *\nou = outer[u]\nov = outer[v]",
    );
    let u = get_def(&env, "u");
    let v = get_def(&env, "v");

    let ou_mod = env.root.lookup.get("ou").unwrap();
    let inner_mod = ou_mod.lookup.get("inner").unwrap();
    let ou_x = &env.defs[inner_mod.lookup.get("x").unwrap().this.unwrap().0];

    let ov_mod = env.root.lookup.get("ov").unwrap();
    let ov_inner = ov_mod.lookup.get("inner").unwrap();
    let ov_x = &env.defs[ov_inner.lookup.get("x").unwrap().this.unwrap().0];

    match (&u.val, &ou_x.val) {
        (PureVal::Cell(uc), PureVal::Cell(xc)) => {
            assert_eq!(xc.s(), *uc);
            assert_eq!(xc.t(), *uc);
        }
        _ => panic!("expected Cell values"),
    }
    match (&v.val, &ov_x.val) {
        (PureVal::Cell(vc), PureVal::Cell(xc)) => {
            assert_eq!(xc.s(), *vc);
        }
        _ => panic!("expected Cell values"),
    }
    assert_ne!(ou_x.val, ov_x.val);
}

#[test]
fn nested_module_instantiation_three_levels() {
    let env = check_ok(
        "outer[C: *] = {\n  mid = {\n    inner = {\n      x: C → C\n    }\n  }\n}\nu: *\nv: *\nou = outer[u]\nov = outer[v]",
    );
    let u = get_def(&env, "u");
    let v = get_def(&env, "v");

    let ou_x = {
        let ou = env.root.lookup.get("ou").unwrap();
        let mid = ou.lookup.get("mid").unwrap();
        let inner = mid.lookup.get("inner").unwrap();
        &env.defs[inner.lookup.get("x").unwrap().this.unwrap().0]
    };
    let ov_x = {
        let ov = env.root.lookup.get("ov").unwrap();
        let mid = ov.lookup.get("mid").unwrap();
        let inner = mid.lookup.get("inner").unwrap();
        &env.defs[inner.lookup.get("x").unwrap().this.unwrap().0]
    };

    match (&u.val, &ou_x.val) {
        (PureVal::Cell(uc), PureVal::Cell(xc)) => assert_eq!(xc.s(), *uc),
        _ => panic!("expected Cell values"),
    }
    match (&v.val, &ov_x.val) {
        (PureVal::Cell(vc), PureVal::Cell(xc)) => assert_eq!(xc.s(), *vc),
        _ => panic!("expected Cell values"),
    }
    assert_ne!(ou_x.val, ov_x.val);
}

#[test]
fn nested_module_use_after_instantiation() {
    let env = check_ok(
        "cat[C: *] = {\n  arr = {\n    x: C → C\n    m: x x → x\n  }\n}\nu: *\nc = cat[u]\ndouble = c.arr.x c.arr.x; c.arr.m",
    );
    let double = get_def(&env, "double");
    match &double.val {
        PureVal::Cell(pc) => assert_eq!(pc.dim().in_space, 2),
        _ => panic!("expected Cell"),
    }
}

#[test]
fn type_alias_in_parametric_module() {
    let env = check_ok(
        "cat[C: *] = {\n  T = C → C\n  x: T\n}\nu: *\nv: *\ncu = cat[u]\ncv = cat[v]",
    );
    let u = get_def(&env, "u");
    let v = get_def(&env, "v");
    let cu_x = get_member_def(&env, "cu", "x");
    let cv_x = get_member_def(&env, "cv", "x");
    match (&u.val, &cu_x.val) {
        (PureVal::Cell(uc), PureVal::Cell(xc)) => assert_eq!(xc.s(), *uc),
        _ => panic!("expected Cell values"),
    }
    match (&v.val, &cv_x.val) {
        (PureVal::Cell(vc), PureVal::Cell(xc)) => assert_eq!(xc.s(), *vc),
        _ => panic!("expected Cell values"),
    }
}

// --- Import tests ---

#[test]
fn import_base_passes() {
    check_ok("import \"base\"");
}

#[test]
fn import_sys_passes() {
    check_ok("import \"sys\"");
}

#[test]
fn import_ui_passes() {
    check_ok("import \"ui\"");
}

#[test]
fn import_sys_named() {
    let env = check_ok("sys = import \"sys\"");
    let sys_mod = env.root.lookup.get("sys").unwrap();
    assert!(sys_mod.lookup.contains_key("f32"));
    assert!(sys_mod.lookup.contains_key("u32"));
    assert!(sys_mod.lookup.contains_key("bool"));
}

#[test]
fn import_sys_bare_members() {
    // bare import: sys members promoted to root scope
    let env = check_ok("import \"sys\"");
    assert!(env.root.lookup.contains_key("f32"));
    assert!(env.root.lookup.contains_key("u32"));
}

#[test]
fn import_sys_named_composition() {
    // named import: sys.u32.lit[1]; sys.u32.to_f32 = C → f32 (2-cell)
    let env = check_ok("sys = import \"sys\"\nx = sys.u32.lit[1]; sys.u32.to_f32");
    let x = get_def(&env, "x");
    match &x.val {
        PureVal::Cell(pc) => assert_eq!(pc.dim().in_space, 2),
        _ => panic!("expected Cell"),
    }
}

#[test]
fn import_sys_bare_composition() {
    // bare import: u32.lit[1]; u32.to_f32 = C → f32 (2-cell)
    let env = check_ok("import \"sys\"\nx = u32.lit[1]; u32.to_f32");
    let x = get_def(&env, "x");
    match &x.val {
        PureVal::Cell(pc) => assert_eq!(pc.dim().in_space, 2),
        _ => panic!("expected Cell"),
    }
}

#[test]
fn use_sys_passes() {
    check_ok("use \"sys\"");
}

#[test]
fn use_base_passes() {
    check_ok("use \"base\"");
}

#[test]
fn use_ui_passes() {
    check_ok("use \"ui\"");
}

#[test]
fn style_color_decorator() {
    check_ok("import \"ui\"\n[style.color[hue[0.5]]]\nu: *");
}

#[test]
fn style_color_in_module() {
    check_ok("import \"ui\"\ncat = {\n  [style.color[hue[0.5]]]\n  u: *\n}");
}

#[test]
fn nat_example_bisect() {
    check_ok("import \"ui\"\nnat = {\n  [style.color[hue[0.33]]]\n  C: *\n  [style.color[hue[0.55]]]\n  Nat: C → C\n  [style.color[hue[0.08]]]\n  Bool: C → C\n  zero: C → Nat\n  succ: Nat → Nat\n  add: Nat Nat → Nat\n  foo: Nat → Nat\n}");
}

#[test]
fn self_referential_param() {
    let (_, errors) = run_check("X[x: X] = x");
    eprintln!("errors: {:?}", errors);
}

#[test]
fn lit_prim_args() {
    let env = check_ok("import \"base\"\nimport \"ui\"\nsys = import \"sys\"\nx = sys.u32.lit[42]");
    let x = get_def(&env, "x");
    eprintln!("x.val = {:?}", x.val);
    if let PureVal::Cell(pc) = &x.val {
        match pc {
            donut_core::pure_cell::PureCell::Prim(prim, _, _) => {
                eprintln!("prim.args = {:?}", prim.args);
                assert!(!prim.args.is_empty(), "prim should have args");
            }
            _ => panic!("expected Prim"),
        }
    }
}

// --- DeclDef ---

#[test]
fn decldef_with_type() {
    let env = check_ok("a: *\nb: *\nf: a ~ b\nx: * := a");
    let x = get_def(&env, "x");
    assert_eq!(x.ty, Ty::Star);
    assert!(x.item.is_some());
    // x.def should exist as a member
    let x_module = env.root.lookup.get("x").unwrap();
    let def_module = x_module.lookup.get("def").unwrap();
    let def_id = def_module.this.unwrap();
    let def_def = &env.defs[def_id.0];
    assert!(matches!(def_def.ty, Ty::Arrow(_, ArrowTy::Eq, _, _)));
}

#[test]
fn decldef_type_inferred() {
    let env = check_ok("a: *\nx := a");
    let x = get_def(&env, "x");
    assert_eq!(x.ty, Ty::Star, "type should be inferred from body");
    assert!(x.item.is_some());
}

#[test]
fn decldef_arrow_inferred() {
    let env = check_ok("a: *\nb: *\nf: a → b\nx := f");
    let x = get_def(&env, "x");
    assert!(
        matches!(x.ty, Ty::Arrow(_, ArrowTy::To, _, _)),
        "type should be inferred as arrow from body"
    );
}

#[test]
fn decldef_member_type() {
    // x.def : x ~ a (equivalence cell)
    let env = check_ok("a: *\nx: * := a");
    let x_module = env.root.lookup.get("x").unwrap();
    let def_module = x_module.lookup.get("def").unwrap();
    let def_id = def_module.this.unwrap();
    let def_def = &env.defs[def_id.0];
    if let Ty::Arrow(level, arrow_ty, src, tgt) = &def_def.ty {
        assert_eq!(*level, 1);
        assert_eq!(*arrow_ty, ArrowTy::Eq);
        // src should be x (a 0-cell), tgt should be a (a 0-cell)
        assert!(matches!(src, PureVal::Cell(pc) if pc.dim().in_space == 0));
        assert!(matches!(tgt, PureVal::Cell(pc) if pc.dim().in_space == 0));
    } else {
        panic!("expected Arrow(Eq) type, got {:?}", def_def.ty);
    }
}
