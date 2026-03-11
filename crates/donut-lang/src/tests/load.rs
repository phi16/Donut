use crate::types::env::Env;
use crate::types::item::DefId;
use donut_core::cell::Globular;
use donut_core::common::PureVal;
use donut_core::pure_cell::PureCell;

fn load(code: &str) -> Env {
    let code = super::dedent(code.trim_matches('\n'));
    let (env, errors) = crate::load::load(&code);
    assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    env
}

/// Navigate a dotted path (e.g. "cat.u") through the nested Module structure
fn find_def(env: &Env, path: &str) -> DefId {
    let parts: Vec<&str> = path.split('.').collect();
    let mut module = &env.root;
    for (i, part) in parts.iter().enumerate() {
        let child = module.lookup.get(*part).unwrap_or_else(|| {
            panic!("module lookup failed for '{}' at part '{}'", path, part)
        });
        if i == parts.len() - 1 {
            return child.this.unwrap_or_else(|| {
                panic!("module '{}' has no def", path)
            });
        }
        module = child;
    }
    unreachable!()
}

fn has_def(env: &Env, path: &str) -> bool {
    let parts: Vec<&str> = path.split('.').collect();
    let mut module = &env.root;
    for (i, part) in parts.iter().enumerate() {
        match module.lookup.get(*part) {
            Some(child) => {
                if i == parts.len() - 1 {
                    return child.this.is_some();
                }
                module = child;
            }
            None => return false,
        }
    }
    false
}

fn def_cell(env: &Env, path: &str) -> PureCell {
    let def_id = find_def(env, path);
    match &env.defs[def_id.0].val {
        PureVal::Cell(pc) => pc.clone(),
        other => panic!("expected Cell for '{}', got {:?}", path, other),
    }
}

#[test]
fn test_load() {
    let input = r#"
        u: *
        x: u → u
        m: x x → x
        assoc: x m; m → m x; m
        rm = x m; m
        lm = m x; m
        assoc2: rm → lm
    "#;
    let env = load(input);

    // All names exist
    for name in &["u", "x", "m", "assoc", "rm", "lm", "assoc2"] {
        assert!(has_def(&env, name), "missing def: {}", name);
    }

    // u is 0-dim
    assert_eq!(def_cell(&env, "u").dim().in_space, 0);
    // x is 1-dim
    assert_eq!(def_cell(&env, "x").dim().in_space, 1);
    // m is 2-dim (x x → x)
    assert_eq!(def_cell(&env, "m").dim().in_space, 2);
    // assoc is 3-dim
    assert_eq!(def_cell(&env, "assoc").dim().in_space, 3);
}

#[test]
fn test_load_pentagon() {
    let input = r#"
        u: *
        x: u → u
        m: x x → x
        a: m x; m → x m; m
        chl: (x m; m) x → x m x; m x
        chr: x m x; x m → x (m x; m)
        aaa =
            a x; m ;;
            chl; m ;;
            x m x; a ;;
            chr; m ;;
            x a; m
        ch0: m x x; x m → m m
        ch1: m m → x x m; m x

        kl: (m x; m) x → m x x; m x
        kr: x x m; x m → x (x m; m)
        oao =
            kl; m ;;
            m x x; a ;;
            (ch0 ;; ch1); m ;;
            x x m; a ;;
            kr; m

        pentagon: aaa → oao
    "#;
    let env = load(input);
    assert!(has_def(&env, "pentagon"));
}

#[test]
fn test_load_colors() {
    let input = r#"
        import "base"
        import "ui"
        [style.color[gray[80]]]
        u: *
        [style.color[hsv[0.6, 1, 1]]]
        x: u → u
        [style.color[rgb[255, 0, 128]]]
        m: x x → x
    "#;
    let env = load(input);
    // Check that decorators are stored
    let u_def = &env.defs[find_def(&env, "u").0];
    assert_eq!(u_def.decos.len(), 1, "u should have 1 decorator");
    let x_def = &env.defs[find_def(&env, "x").0];
    assert_eq!(x_def.decos.len(), 1, "x should have 1 decorator");
    let m_def = &env.defs[find_def(&env, "m").0];
    assert_eq!(m_def.decos.len(), 1, "m should have 1 decorator");
}

#[test]
fn test_load_members() {
    let input = r#"
        cat = {
            u: *
            x: u → u
        }
        f: cat.u → cat.u
    "#;
    let env = load(input);
    assert!(has_def(&env, "cat.u"));
    assert!(has_def(&env, "cat.x"));
    assert!(has_def(&env, "f"));
}

#[test]
fn test_load_nested_members() {
    let input = r#"
        a = {
            b = {
                u: *
                x: u → u
            }
            y: b.u → b.u
        }
        f: a.b.x a.b.x → a.b.x
    "#;
    let env = load(input);
    assert!(has_def(&env, "a.b.u"));
    assert!(has_def(&env, "a.b.x"));
    assert!(has_def(&env, "a.y"));
    assert!(has_def(&env, "f"));
    assert_eq!(def_cell(&env, "f").dim().in_space, 2);
}

#[test]
fn test_load_nat_example() {
    let input = include_str!("../../../../examples/nat.donut");
    let env = load(input);

    // Modules: nat and u8
    assert!(has_def(&env, "nat.C"));
    assert!(has_def(&env, "nat.Nat"));
    assert!(has_def(&env, "nat.add"));
    assert!(has_def(&env, "u8.D"));
    assert!(has_def(&env, "u8.U8"));
    assert!(has_def(&env, "u8.add"));

    // Derived constants
    assert!(has_def(&env, "one"));
    assert!(has_def(&env, "two"));
    assert!(has_def(&env, "three"));

    // Derived operations
    assert!(has_def(&env, "double"));
    assert!(has_def(&env, "square"));

    // Composite computations
    assert!(has_def(&env, "sum_12"));
    assert!(has_def(&env, "sum_123"));

    // Properties (3-cells)
    assert_eq!(def_cell(&env, "add_assoc").dim().in_space, 3);
    assert_eq!(def_cell(&env, "add_comm").dim().in_space, 3);

    // Equivalence (~ arrow)
    assert_eq!(def_cell(&env, "succ_add").dim().in_space, 3);

}

#[test]
fn test_load_parametric_example() {
    let input = include_str!("../../../../examples/parametric.donut");
    let env = load(input);

    // Template members exist
    assert!(has_def(&env, "cat.x"));
    assert!(has_def(&env, "cat.m"));
    assert!(has_def(&env, "cat.a"));

    // Instantiated members exist
    assert!(has_def(&env, "c.x"));
    assert!(has_def(&env, "c.m"));
    assert!(has_def(&env, "c.a"));

    // c.a is a 3-cell
    assert_eq!(def_cell(&env, "c.a").dim().in_space, 3);

    // c.x is a 1-cell
    assert_eq!(def_cell(&env, "c.x").dim().in_space, 1);

    // result exists
    assert!(has_def(&env, "result"));
}

#[test]
fn test_display_params() {
    let input = r#"
        C: *
        rep2[x: C → C, f: x → x] = f; f
    "#;
    let env = load(input);
    let def_id = find_def(&env, "rep2");
    let def = &env.defs[def_id.0];
    assert_eq!(env.display_params(def), "[x: C → C, f: x → x]");
}

#[test]
fn test_display_def_signature() {
    let input = r#"
        C: *
        rep2[x: C → C, f: x → x] = f; f
    "#;
    let env = load(input);
    let def_id = find_def(&env, "rep2");
    let def = &env.defs[def_id.0];
    assert_eq!(env.display_def_signature(def), "rep2[x: C → C, f: x → x]: x → x");
}

#[test]
fn test_load_default_donut() {
    let input = include_str!("../../../../donut-app/src/default.donut");
    load(input);
}
