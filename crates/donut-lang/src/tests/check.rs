use crate::check::Env;
use donut_core::cell::Globular;

fn check_source(code: &str) -> Result<Env, String> {
    let code = super::dedent(code.trim_matches('\n'));
    let (env, errors) = crate::load::load(&code);
    if errors.is_empty() {
        Ok(env)
    } else {
        Err(errors
            .iter()
            .map(|(_, msg)| msg.as_str())
            .collect::<Vec<_>>()
            .join("\n"))
    }
}

// --- Basic (non-parametric) ---

#[test]
fn basic_cells() {
    let env = check_source(
        r#"
        u: *
        x: u → u
        m: x x → x
        "#,
    )
    .unwrap();
    assert_eq!(env.entries.len(), 3);
    assert_eq!(env.entries[0].body.as_cell().unwrap().pure.dim().in_space, 0);
    assert_eq!(env.entries[1].body.as_cell().unwrap().pure.dim().in_space, 1);
    assert_eq!(env.entries[2].body.as_cell().unwrap().pure.dim().in_space, 2);
}

#[test]
fn basic_alias() {
    let env = check_source(
        r#"
        u: *
        x: u → u
        m: x x → x
        rm = x m; m
        lm = m x; m
        assoc: rm → lm
        "#,
    )
    .unwrap();
    assert_eq!(env.entries[5].name, "assoc");
    assert_eq!(env.entries[5].body.as_cell().unwrap().pure.dim().in_space, 3);
}

#[test]
fn basic_module() {
    let env = check_source(
        r#"
        cat = {
            u: *
            x: u → u
        }
        f: cat.u → cat.u
        "#,
    )
    .unwrap();
    assert_eq!(env.entries[0].name, "cat.u");
    assert_eq!(env.entries[1].name, "cat.x");
    assert_eq!(env.entries[2].name, "f");
}

#[test]
fn nat_example() {
    let input = include_str!("../../../../examples/nat.donut");
    let env = check_source(input).unwrap();

    assert!(env.lookup.contains_key("nat.C"));
    assert!(env.lookup.contains_key("nat.Nat"));
    assert!(env.lookup.contains_key("nat.add"));
    assert!(env.lookup.contains_key("u8.D"));
    assert!(env.lookup.contains_key("one"));
    assert!(env.lookup.contains_key("double"));
    assert!(env.lookup.contains_key("sum_12"));

    let add_assoc = &env.entries[env.lookup["add_assoc"]];
    assert_eq!(add_assoc.body.as_cell().unwrap().pure.dim().in_space, 3);
}

// --- Parametric declaration (cell params) ---

#[test]
fn parametric_decl_zero_cell() {
    // A[x: *] declares a family of 0-cells
    let env = check_source(
        r#"
        A[x: *]: x → x
        "#,
    )
    .unwrap();
    // x (fresh param) + A
    assert_eq!(env.entries.len(), 2);
    let a = &env.entries[1];
    assert_eq!(a.name, "A");
    assert_eq!(a.body.as_cell().unwrap().pure.dim().in_space, 1);
    // A's Prim should have args (the fresh param)
    match &a.body.as_cell().unwrap().pure {
        donut_core::pure_cell::PureCell::Prim(prim, _, _) => {
            assert_eq!(prim.args.len(), 1);
        }
        _ => panic!("expected Prim"),
    }
}

#[test]
fn parametric_decl_one_cell() {
    // B[f: x → x] declares a family parameterized by 1-cells
    let env = check_source(
        r#"
        u: *
        B[f: u → u]: f → f
        "#,
    )
    .unwrap();
    // u + f (fresh) + B
    assert_eq!(env.entries.len(), 3);
    let b = &env.entries[2];
    assert_eq!(b.name, "B");
    // f is a 1-cell, f → f creates a 2-cell
    assert_eq!(b.body.as_cell().unwrap().pure.dim().in_space, 2);
}

// --- Instantiation (simple) ---

#[test]
fn instantiation_simple() {
    let env = check_source(
        r#"
        u: *
        A[x: u → u]: x → x
        y: u → u
        z: u → u
        ay = A[y]
        az = A[z]
        "#,
    )
    .unwrap();

    let y = &env.entries[env.lookup["y"]];
    let ay = &env.entries[env.lookup["ay"]];
    let az = &env.entries[env.lookup["az"]];

    // ay's source/target should be y
    assert_eq!(ay.body.as_cell().unwrap().pure.s(), y.body.as_cell().unwrap().pure);
    assert_eq!(ay.body.as_cell().unwrap().pure.t(), y.body.as_cell().unwrap().pure);

    // az's source/target should be z
    let z = &env.entries[env.lookup["z"]];
    assert_eq!(az.body.as_cell().unwrap().pure.s(), z.body.as_cell().unwrap().pure);
    assert_eq!(az.body.as_cell().unwrap().pure.t(), z.body.as_cell().unwrap().pure);

    // ay ≠ az (different args)
    assert_ne!(ay.body.as_cell().unwrap().pure, az.body.as_cell().unwrap().pure);
}

// --- Parametric module ---

#[test]
fn parametric_module() {
    let env = check_source(
        r#"
        cat[C: *] = {
            x: C → C
            m: x x → x
        }
        u: *
        v: *
        cu = cat[u]
        cv = cat[v]
        "#,
    )
    .unwrap();

    let u = &env.entries[env.lookup["u"]];
    let v = &env.entries[env.lookup["v"]];

    // cu.x should have source/target = u
    let cu_x = &env.entries[env.lookup["cu.x"]];
    assert_eq!(cu_x.body.as_cell().unwrap().pure.s(), u.body.as_cell().unwrap().pure);
    assert_eq!(cu_x.body.as_cell().unwrap().pure.t(), u.body.as_cell().unwrap().pure);

    // cv.x should have source/target = v
    let cv_x = &env.entries[env.lookup["cv.x"]];
    assert_eq!(cv_x.body.as_cell().unwrap().pure.s(), v.body.as_cell().unwrap().pure);
    assert_eq!(cv_x.body.as_cell().unwrap().pure.t(), v.body.as_cell().unwrap().pure);

    // cu.x ≠ cv.x
    assert_ne!(cu_x.body.as_cell().unwrap().pure, cv_x.body.as_cell().unwrap().pure);

    // cu.m should exist and be a 2-cell
    let cu_m = &env.entries[env.lookup["cu.m"]];
    assert_eq!(cu_m.body.as_cell().unwrap().pure.dim().in_space, 2);
}

#[test]
fn parametric_module_composition() {
    // Verify that instantiated members compose correctly
    let env = check_source(
        r#"
        cat[C: *] = {
            x: C → C
            m: x x → x
        }
        u: *
        c = cat[u]
        double = c.x c.x; c.m
        "#,
    )
    .unwrap();

    // double should be a valid 2-cell
    let double = &env.entries[env.lookup["double"]];
    assert_eq!(double.body.as_cell().unwrap().pure.dim().in_space, 2);
}

// --- Prim args equality ---

#[test]
fn prim_args_distinguish_instances() {
    let env = check_source(
        r#"
        cat[C: *] = {
            T[x: C → C]: x → x
        }
        u: *
        c = cat[u]
        a: u → u
        b: u → u
        ta = c.T[a]
        tb = c.T[b]
        "#,
    )
    .unwrap();

    let ta = &env.entries[env.lookup["ta"]];
    let tb = &env.entries[env.lookup["tb"]];

    // ta ≠ tb because different Prim args
    assert_ne!(ta.body.as_cell().unwrap().pure, tb.body.as_cell().unwrap().pure);

    // Both should be 2-cells
    assert_eq!(ta.body.as_cell().unwrap().pure.dim().in_space, 2);
    assert_eq!(tb.body.as_cell().unwrap().pure.dim().in_space, 2);

    // ta and tb have different source: ta.s() = a, tb.s() = b
    assert_ne!(ta.body.as_cell().unwrap().pure.s(), tb.body.as_cell().unwrap().pure.s());
}

// --- Functor ---

#[test]
fn functor_basic() {
    // Well-typed functor: all mappings have correct boundaries
    check_source(
        r#"
        src = {
            C: *
            X: C → C
            m: X X → X
        }
        tgt = {
            D: *
            Y: D → D
            n: Y Y → Y
        }
        F: src.C ~> tgt.D
        F(src.X) = tgt.Y
        F(src.m) = tgt.n
        "#,
    )
    .unwrap();
}

#[test]
fn functor_dimension_mismatch() {
    // Mapping a 1-cell to a 2-cell should fail
    let err = check_source(
        r#"
        src = {
            C: *
            X: C → C
        }
        tgt = {
            D: *
            Y: D → D
            n: Y Y → Y
        }
        F: src.C ~> tgt.D
        F(src.X) = tgt.n
        "#,
    )
    .unwrap_err();
    assert!(err.contains("dimension mismatch"), "got: {}", err);
}

#[test]
fn functor_source_mismatch() {
    // Mapping source boundary doesn't match
    let err = check_source(
        r#"
        src = {
            C: *
            X: C → C
            Y: C → C
            m: X X → Y
        }
        tgt = {
            D: *
            A: D → D
            B: D → D
            n: A A → A
        }
        F: src.C ~> tgt.D
        F(src.X) = tgt.A
        F(src.Y) = tgt.B
        F(src.m) = tgt.n
        "#,
    )
    .unwrap_err();
    // F(m): F(X) F(X) → F(Y) = A A → B, but n: A A → A, target mismatch
    assert!(err.contains("mismatch"), "got: {}", err);
}

#[test]
fn functor_missing_mapping() {
    // Applying functor to boundary with unmapped prim should error
    let err = check_source(
        r#"
        src = {
            C: *
            X: C → C
            Y: C → C
            m: X → Y
        }
        tgt = {
            D: *
            A: D → D
            B: D → D
            n: A → B
        }
        F: src.C ~> tgt.D
        F(src.m) = tgt.n
        "#,
    )
    .unwrap_err();
    // F(m): F(X) → F(Y), but X and Y have no mapping
    assert!(err.contains("no mapping"), "got: {}", err);
}

#[test]
fn functor_preserves_composition_boundary() {
    // 2-cell with composite source: m: X X → X
    // F should map the composed boundary correctly
    check_source(
        r#"
        src = {
            C: *
            X: C → C
            m: X X → X
            u: C → X
        }
        tgt = {
            D: *
            Y: D → D
            n: Y Y → Y
            v: D → Y
        }
        F: src.C ~> tgt.D
        F(src.X) = tgt.Y
        F(src.m) = tgt.n
        F(src.u) = tgt.v
        "#,
    )
    .unwrap();
}

#[test]
fn functor_base_case_explicit() {
    // Explicitly writing F(src.C) = tgt.D is redundant but allowed
    check_source(
        r#"
        src = {
            C: *
            X: C → C
        }
        tgt = {
            D: *
            Y: D → D
        }
        F: src.C ~> tgt.D
        F(src.C) = tgt.D
        F(src.X) = tgt.Y
        "#,
    )
    .unwrap();
}

#[test]
fn functor_reject_unrelated_zero_cell() {
    // Mapping an unrelated 0-cell should be rejected
    let err = check_source(
        r#"
        src = {
            C: *
        }
        tgt = {
            D: *
        }
        other: *
        F: src.C ~> tgt.D
        F(other) = tgt.D
        "#,
    )
    .unwrap_err();
    assert!(err.contains("already implicitly defined"), "got: {}", err);
}

// --- Nested module instantiation (deep) ---

#[test]
fn nested_module_instantiation_two_levels() {
    // inner module inside a parametric module should be instantiated correctly
    let env = check_source(
        r#"
        outer[C: *] = {
            inner = {
                x: C → C
            }
        }
        u: *
        v: *
        ou = outer[u]
        ov = outer[v]
        "#,
    )
    .unwrap();

    let u = &env.entries[env.lookup["u"]];
    let v = &env.entries[env.lookup["v"]];

    // ou.inner.x should have source/target = u
    let ou_x = &env.entries[env.lookup["ou.inner.x"]];
    assert_eq!(ou_x.body.as_cell().unwrap().pure.s(), u.body.as_cell().unwrap().pure);
    assert_eq!(ou_x.body.as_cell().unwrap().pure.t(), u.body.as_cell().unwrap().pure);

    // ov.inner.x should have source/target = v
    let ov_x = &env.entries[env.lookup["ov.inner.x"]];
    assert_eq!(ov_x.body.as_cell().unwrap().pure.s(), v.body.as_cell().unwrap().pure);
    assert_eq!(ov_x.body.as_cell().unwrap().pure.t(), v.body.as_cell().unwrap().pure);

    // Different instances
    assert_ne!(ou_x.body.as_cell().unwrap().pure, ov_x.body.as_cell().unwrap().pure);
}

#[test]
fn nested_module_instantiation_three_levels() {
    // 3-level deep nesting: outer > mid > inner
    let env = check_source(
        r#"
        outer[C: *] = {
            mid = {
                inner = {
                    x: C → C
                }
            }
        }
        u: *
        v: *
        ou = outer[u]
        ov = outer[v]
        "#,
    )
    .unwrap();

    let u = &env.entries[env.lookup["u"]];
    let v = &env.entries[env.lookup["v"]];

    let ou_x = &env.entries[env.lookup["ou.mid.inner.x"]];
    assert_eq!(ou_x.body.as_cell().unwrap().pure.s(), u.body.as_cell().unwrap().pure);

    let ov_x = &env.entries[env.lookup["ov.mid.inner.x"]];
    assert_eq!(ov_x.body.as_cell().unwrap().pure.s(), v.body.as_cell().unwrap().pure);

    assert_ne!(ou_x.body.as_cell().unwrap().pure, ov_x.body.as_cell().unwrap().pure);
}

#[test]
fn nested_module_use_after_instantiation() {
    // Use deeply nested members in expressions after instantiation
    let env = check_source(
        r#"
        cat[C: *] = {
            arr = {
                x: C → C
                m: x x → x
            }
        }
        u: *
        c = cat[u]
        double = c.arr.x c.arr.x; c.arr.m
        "#,
    )
    .unwrap();

    let double = &env.entries[env.lookup["double"]];
    assert_eq!(double.body.as_cell().unwrap().pure.dim().in_space, 2);
}
