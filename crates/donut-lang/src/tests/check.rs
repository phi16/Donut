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
    assert_eq!(env.entries[0].as_cell().unwrap().pure.dim().in_space, 0);
    assert_eq!(env.entries[1].as_cell().unwrap().pure.dim().in_space, 1);
    assert_eq!(env.entries[2].as_cell().unwrap().pure.dim().in_space, 2);
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
    assert_eq!(env.entries[5].as_cell().unwrap().pure.dim().in_space, 3);
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
    assert_eq!(add_assoc.as_cell().unwrap().pure.dim().in_space, 3);
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
    assert_eq!(a.as_cell().unwrap().pure.dim().in_space, 1);
    // A's Prim should have args (the fresh param)
    match &a.as_cell().unwrap().pure {
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
    assert_eq!(b.as_cell().unwrap().pure.dim().in_space, 2);
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
    assert_eq!(ay.as_cell().unwrap().pure.s(), y.as_cell().unwrap().pure);
    assert_eq!(ay.as_cell().unwrap().pure.t(), y.as_cell().unwrap().pure);

    // az's source/target should be z
    let z = &env.entries[env.lookup["z"]];
    assert_eq!(az.as_cell().unwrap().pure.s(), z.as_cell().unwrap().pure);
    assert_eq!(az.as_cell().unwrap().pure.t(), z.as_cell().unwrap().pure);

    // ay ≠ az (different args)
    assert_ne!(ay.as_cell().unwrap().pure, az.as_cell().unwrap().pure);
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
    assert_eq!(cu_x.as_cell().unwrap().pure.s(), u.as_cell().unwrap().pure);
    assert_eq!(cu_x.as_cell().unwrap().pure.t(), u.as_cell().unwrap().pure);

    // cv.x should have source/target = v
    let cv_x = &env.entries[env.lookup["cv.x"]];
    assert_eq!(cv_x.as_cell().unwrap().pure.s(), v.as_cell().unwrap().pure);
    assert_eq!(cv_x.as_cell().unwrap().pure.t(), v.as_cell().unwrap().pure);

    // cu.x ≠ cv.x
    assert_ne!(cu_x.as_cell().unwrap().pure, cv_x.as_cell().unwrap().pure);

    // cu.m should exist and be a 2-cell
    let cu_m = &env.entries[env.lookup["cu.m"]];
    assert_eq!(cu_m.as_cell().unwrap().pure.dim().in_space, 2);
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
    assert_eq!(double.as_cell().unwrap().pure.dim().in_space, 2);
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
    assert_ne!(ta.as_cell().unwrap().pure, tb.as_cell().unwrap().pure);

    // Both should be 2-cells
    assert_eq!(ta.as_cell().unwrap().pure.dim().in_space, 2);
    assert_eq!(tb.as_cell().unwrap().pure.dim().in_space, 2);

    // ta and tb have different source: ta.s() = a, tb.s() = b
    assert_ne!(ta.as_cell().unwrap().pure.s(), tb.as_cell().unwrap().pure.s());
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

#[test]
fn functor_parametric_mapping() {
    // Functor mapping with decorator params should type-check
    check_source(
        r#"
        import "base"
        sys = import "sys"
        C: *
        u: C → C
        x[n: base.nat]: C → u
        F: C ~> sys.C
        F(u) = sys.u32
        [n: base.nat] F(x[n]) = sys.u32_lit[n]
        "#,
    )
    .unwrap();
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
    assert_eq!(ou_x.as_cell().unwrap().pure.s(), u.as_cell().unwrap().pure);
    assert_eq!(ou_x.as_cell().unwrap().pure.t(), u.as_cell().unwrap().pure);

    // ov.inner.x should have source/target = v
    let ov_x = &env.entries[env.lookup["ov.inner.x"]];
    assert_eq!(ov_x.as_cell().unwrap().pure.s(), v.as_cell().unwrap().pure);
    assert_eq!(ov_x.as_cell().unwrap().pure.t(), v.as_cell().unwrap().pure);

    // Different instances
    assert_ne!(ou_x.as_cell().unwrap().pure, ov_x.as_cell().unwrap().pure);
}

// --- Meta evaluation ---

#[test]
fn meta_variable_reference() {
    // x = 32 as a meta value, referenced in decorator
    let env = check_source(
        r#"
        import "base"
        import "ui"
        g = 80
        [style[gray[g]]]
        u: *
        "#,
    )
    .unwrap();
    let u = &env.entries[env.lookup["u"]];
    assert_eq!(u.color, (80, 80, 80));
}

#[test]
fn meta_typed_body() {
    // Explicit meta type with body
    let env = check_source(
        r#"
        import "base"
        import "ui"
        g: base.nat = 80
        [style[gray[g]]]
        u: *
        "#,
    )
    .unwrap();
    let u = &env.entries[env.lookup["u"]];
    assert_eq!(u.color, (80, 80, 80));
}

#[test]
fn meta_typed_body_mismatch() {
    // g: meta = 80 should be a type error (80 is nat, not meta)
    let result = check_source(
        r#"
        import "base"
        g: base.meta = 80
        "#,
    );
    assert!(result.is_err());
}

#[test]
fn meta_parametric_function() {
    // f[x: nat]: nat = x, used in decorator
    let env = check_source(
        r#"
        import "base"
        import "ui"
        f[x: base.nat]: base.nat = x
        [style[gray[f[120]]]]
        u: *
        "#,
    )
    .unwrap();
    let u = &env.entries[env.lookup["u"]];
    assert_eq!(u.color, (120, 120, 120));
}

#[test]
fn meta_parametric_color_function() {
    // Custom color function that wraps rgb
    let env = check_source(
        r#"
        import "base"
        import "ui"
        mycolor[r g b: base.nat]: base.color = rgb[r, g, b]
        [style[mycolor[10, 20, 30]]]
        u: *
        "#,
    )
    .unwrap();
    let u = &env.entries[env.lookup["u"]];
    assert_eq!(u.color, (10, 20, 30));
}

#[test]
fn meta_hsv_reduces_to_rgb() {
    let env = check_source(
        r#"
        import "base"
        import "ui"
        [style[hsv[0.0, 1, 1]]]
        u: *
        "#,
    )
    .unwrap();
    let u = &env.entries[env.lookup["u"]];
    // hsv(0, 1, 1) = red
    assert_eq!(u.color, (255, 0, 0));
}

#[test]
fn meta_hsv_stored_variable() {
    let env = check_source(
        r#"
        import "base"
        import "ui"
        c: base.color = hsv[0.0, 1, 1]
        [style[c]]
        u: *
        "#,
    )
    .unwrap();
    let u = &env.entries[env.lookup["u"]];
    assert_eq!(u.color, (255, 0, 0));
}

#[test]
fn meta_lerp_colors() {
    let env = check_source(
        r#"
        import "base"
        import "ui"
        [style[lerp[rgb[0, 0, 0], rgb[100, 200, 50], 0.5]]]
        u: *
        "#,
    )
    .unwrap();
    let u = &env.entries[env.lookup["u"]];
    assert_eq!(u.color, (50, 100, 25));
}

#[test]
fn meta_lerp_with_gray() {
    let env = check_source(
        r#"
        import "base"
        import "ui"
        [style[lerp[gray[0], gray[200], 0.5]]]
        u: *
        "#,
    )
    .unwrap();
    let u = &env.entries[env.lookup["u"]];
    assert_eq!(u.color, (100, 100, 100));
}

#[test]
fn meta_lerp_with_hsv_and_rgb() {
    let env = check_source(
        r#"
        import "base"
        import "ui"
        [style[lerp[hsv[0.0, 1, 1], rgb[0, 0, 255], 0.5]]]
        u: *
        "#,
    )
    .unwrap();
    let u = &env.entries[env.lookup["u"]];
    // hsv(0,1,1)=rgb(255,0,0), lerp with rgb(0,0,255) at 0.5 = (128, 0, 128)
    assert_eq!(u.color, (128, 0, 128));
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
    assert_eq!(ou_x.as_cell().unwrap().pure.s(), u.as_cell().unwrap().pure);

    let ov_x = &env.entries[env.lookup["ov.mid.inner.x"]];
    assert_eq!(ov_x.as_cell().unwrap().pure.s(), v.as_cell().unwrap().pure);

    assert_ne!(ou_x.as_cell().unwrap().pure, ov_x.as_cell().unwrap().pure);
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
    assert_eq!(double.as_cell().unwrap().pure.dim().in_space, 2);
}

// --- Type alias ---

#[test]
fn type_alias_arrow() {
    // T = x x → x should define a type alias, then m: T should work
    let env = check_source(
        r#"
        u: *
        x: u → u
        T = x x → x
        m: T
        "#,
    )
    .unwrap();
    assert_eq!(env.entries[env.lookup["m"]].as_cell().unwrap().pure.dim().in_space, 2);
}

#[test]
fn type_alias_star() {
    // T = * should define a type alias for 0-cell type
    let env = check_source(
        r#"
        T = *
        u: T
        "#,
    )
    .unwrap();
    assert_eq!(env.entries[env.lookup["u"]].as_cell().unwrap().pure.dim().in_space, 0);
}

// --- Parametric type-level entries (issue.donut) ---

#[test]
fn parametric_star_body() {
    // g[x: *] = * should be valid (g: meta)
    check_source(
        r#"
        g[x: *] = *
        "#,
    )
    .unwrap();
}

// --- Cell param as body (issue.donut) ---

#[test]
fn cell_param_as_body() {
    // h[x: *, y: x → x]: x → x = y
    let env = check_source(
        r#"
        h[x: *, y: x → x]: x → x = y
        "#,
    )
    .unwrap();
    assert!(env.lookup.contains_key("h"));
}

#[test]
fn type_alias_in_parametric_module() {
    // Type alias inside a parametric module should be instantiated correctly
    let env = check_source(
        r#"
        cat[C: *] = {
            T = C → C
            x: T
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
    let cu_x = &env.entries[env.lookup["cu.x"]];
    let cv_x = &env.entries[env.lookup["cv.x"]];
    // cu.x should be u → u
    assert_eq!(cu_x.as_cell().unwrap().pure.s(), u.as_cell().unwrap().pure);
    // cv.x should be v → v
    assert_eq!(cv_x.as_cell().unwrap().pure.s(), v.as_cell().unwrap().pure);
}

#[test]
fn type_alias_parametric() {
    // g[x: *] = x → x, then m: g[u] should be u → u
    let env = check_source(
        r#"
        g[x: *] = x → x
        u: *
        v: *
        m: g[u]
        n: g[v]
        "#,
    )
    .unwrap();
    let u = &env.entries[env.lookup["u"]];
    let v = &env.entries[env.lookup["v"]];
    let m = &env.entries[env.lookup["m"]];
    let n = &env.entries[env.lookup["n"]];
    // m: g[u] = u → u
    assert_eq!(m.as_cell().unwrap().pure.s(), u.as_cell().unwrap().pure);
    assert_eq!(m.as_cell().unwrap().pure.t(), u.as_cell().unwrap().pure);
    // n: g[v] = v → v
    assert_eq!(n.as_cell().unwrap().pure.s(), v.as_cell().unwrap().pure);
}

#[test]
fn module_meta_value_instantiation() {
    // Meta values inside a parametric module should be accessible after instantiation
    let env = check_source(
        r#"
        import "ui"
        config[C: *] = {
            my_gray: base.nat = 120
        }
        u: *
        c = config[u]
        [style[gray[c.my_gray]]]
        x: u → u
        "#,
    )
    .unwrap();
    let x = &env.entries[env.lookup["x"]];
    assert_eq!(x.color, (120, 120, 120));
}

#[test]
fn default_donut_example() {
    let input = include_str!("../../../../donut-app/src/default.donut");
    check_source(input).unwrap();
}

#[test]
fn type_alias_alias() {
    // f = T where T is a type alias should make f also a type alias
    let env = check_source(
        r#"
        u: *
        x: u → u
        T = x x → x
        S = T
        m: S
        "#,
    )
    .unwrap();
    assert_eq!(env.entries[env.lookup["m"]].as_cell().unwrap().pure.dim().in_space, 2);
}

// --- Module member meta reference ---

#[test]
fn module_member_meta_reference() {
    // config.my_hue used in decorator
    let env = check_source(
        r#"
        import "ui"
        config = {
            my_hue: base.rat = 0.6
        }
        u: *
        [style[hue[config.my_hue]]]
        x: u → u
        "#,
    )
    .unwrap();
    let x = &env.entries[env.lookup["x"]];
    let u = &env.entries[env.lookup["u"]];
    assert_ne!(x.color, u.color);
}

#[test]
fn type_alias_cross_usage() {
    // Multiple type aliases used together
    let env = check_source(
        r#"
        u: *
        Endo[X: *] = X → X
        Hom[X: *, Y: *] = X → Y
        f: Endo[u]
        g: Endo[u]
        alpha: f → g
        "#,
    )
    .unwrap();
    let f = &env.entries[env.lookup["f"]];
    let g = &env.entries[env.lookup["g"]];
    let u = &env.entries[env.lookup["u"]];
    // f and g have same source/target (u)
    assert_eq!(f.as_cell().unwrap().pure.s(), u.as_cell().unwrap().pure);
    assert_eq!(g.as_cell().unwrap().pure.s(), u.as_cell().unwrap().pure);
    // alpha is a 2-cell
    assert_eq!(env.entries[env.lookup["alpha"]].as_cell().unwrap().pure.dim().in_space, 2);
}

#[test]
fn typed_donut_example() {
    let input = include_str!("../../../../examples/typed.donut");
    let env = check_source(input).unwrap();

    // Type alias Endo[u] = u → u: f should be 1-cell
    assert_eq!(env.entries[env.lookup["f"]].as_cell().unwrap().pure.dim().in_space, 1);
    // Hom[u, v] = u → v: h should be 1-cell
    assert_eq!(env.entries[env.lookup["h"]].as_cell().unwrap().pure.dim().in_space, 1);

    // alpha: f → g should be 2-cell (same source/target)
    assert_eq!(env.entries[env.lookup["alpha"]].as_cell().unwrap().pure.dim().in_space, 2);

    // End (chained alias) works: k is 1-cell
    assert_eq!(env.entries[env.lookup["k"]].as_cell().unwrap().pure.dim().in_space, 1);

    // Parametric module with type alias inside
    assert_eq!(env.entries[env.lookup["cu.x"]].as_cell().unwrap().pure.dim().in_space, 1);
    assert_eq!(env.entries[env.lookup["cu.m"]].as_cell().unwrap().pure.dim().in_space, 2);
    assert_eq!(env.entries[env.lookup["cu.a"]].as_cell().unwrap().pure.dim().in_space, 3);

    // id_cell[u, f] = f: witness should equal f
    assert_eq!(
        env.entries[env.lookup["witness"]].as_cell().unwrap().pure,
        env.entries[env.lookup["f"]].as_cell().unwrap().pure,
    );

    // obj[X] = *: p and q are 0-cells (different objects)
    assert_eq!(env.entries[env.lookup["p"]].as_cell().unwrap().pure.dim().in_space, 0);
    assert_eq!(env.entries[env.lookup["q"]].as_cell().unwrap().pure.dim().in_space, 0);
    assert_ne!(
        env.entries[env.lookup["p"]].as_cell().unwrap().pure,
        env.entries[env.lookup["q"]].as_cell().unwrap().pure,
    );

    // Meta config: colored has custom color
    let colored = &env.entries[env.lookup["colored"]];
    let dimmed = &env.entries[env.lookup["dimmed"]];
    assert_ne!(colored.color, dimmed.color);
}
