use crate::check::Env;
use donut_core::cell::Globular;
use donut_core::common::PrimArg;
use donut_core::pure_cell::PureCell;

fn load(code: &str) -> Env {
    let code = super::dedent(code.trim_matches('\n'));
    let (env, errors) = crate::load::load(&code);
    assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    env
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
    let table = load(input);
    assert_eq!(table.entries.len(), 7);
    assert_eq!(table.entries[0].name, "u");
    assert_eq!(table.entries[1].name, "x");
    assert_eq!(table.entries[2].name, "m");
    assert_eq!(table.entries[3].name, "assoc");
    assert_eq!(table.entries[4].name, "rm");
    assert_eq!(table.entries[5].name, "lm");
    assert_eq!(table.entries[6].name, "assoc2");

    // u is 0-dim
    assert_eq!(table.entries[0].as_cell().unwrap().pure.dim().in_space, 0);
    // x is 1-dim
    assert_eq!(table.entries[1].as_cell().unwrap().pure.dim().in_space, 1);
    // m is 2-dim (x x → x)
    assert_eq!(table.entries[2].as_cell().unwrap().pure.dim().in_space, 2);
    // assoc is 3-dim
    assert_eq!(table.entries[3].as_cell().unwrap().pure.dim().in_space, 3);
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
    let table = load(input);
    assert!(table.entries.len() > 0);
    let last = table.entries.last().unwrap();
    assert_eq!(last.name, "pentagon");
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
    let table = load(input);
    let u = table.lookup["u"];
    let x = table.lookup["x"];
    let m = table.lookup["m"];
    assert_eq!(table.entries[u].color, (80, 80, 80));
    // hsv(0.6, 1, 1) should produce a blue-ish color
    let c = table.entries[x].color;
    assert!(c.2 > c.0 && c.2 > c.1, "hsv(0.6) should be blue-ish: {:?}", c);
    assert_eq!(table.entries[m].color, (255, 0, 128));
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
    let table = load(input);
    assert_eq!(table.entries.len(), 3);
    assert_eq!(table.entries[0].name, "cat.u");
    assert_eq!(table.entries[1].name, "cat.x");
    assert_eq!(table.entries[2].name, "f");
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
    let table = load(input);
    assert_eq!(table.entries[0].name, "a.b.u");
    assert_eq!(table.entries[1].name, "a.b.x");
    assert_eq!(table.entries[2].name, "a.y");
    assert_eq!(table.entries[3].name, "f");
    assert_eq!(table.entries[3].as_cell().unwrap().pure.dim().in_space, 2);
}

#[test]
fn test_load_auto_color() {
    let input = r#"
        u: *
        x: u → u
    "#;
    let table = load(input);
    // Without explicit color, auto_color is used
    let c0 = table.entries[0].color;
    let c1 = table.entries[1].color;
    // Colors should be different
    assert_ne!(c0, c1);
}

#[test]
fn test_load_nat_example() {
    let input = include_str!("../../../../examples/nat.donut");
    let table = load(input);

    // Modules: nat and u8
    assert!(table.lookup.contains_key("nat.C"));
    assert!(table.lookup.contains_key("nat.Nat"));
    assert!(table.lookup.contains_key("nat.add"));
    assert!(table.lookup.contains_key("u8.D"));
    assert!(table.lookup.contains_key("u8.U8"));
    assert!(table.lookup.contains_key("u8.add"));

    // Derived constants
    assert!(table.lookup.contains_key("one"));
    assert!(table.lookup.contains_key("two"));
    assert!(table.lookup.contains_key("three"));

    // Derived operations
    assert!(table.lookup.contains_key("double"));
    assert!(table.lookup.contains_key("square"));

    // Composite computations
    assert!(table.lookup.contains_key("sum_12"));
    assert!(table.lookup.contains_key("sum_123"));

    // Properties (3-cells)
    let add_assoc = &table.entries[table.lookup["add_assoc"]];
    assert_eq!(add_assoc.as_cell().unwrap().pure.dim().in_space, 3);

    let add_comm = &table.entries[table.lookup["add_comm"]];
    assert_eq!(add_comm.as_cell().unwrap().pure.dim().in_space, 3);

    // Equivalence (~ arrow)
    let succ_add = &table.entries[table.lookup["succ_add"]];
    assert_eq!(succ_add.as_cell().unwrap().pure.dim().in_space, 3);

    // Functor type declaration is not yet loaded as a cell
    assert!(!table.lookup.contains_key("compile"));
}

#[test]
fn test_load_parametric_example() {
    let input = include_str!("../../../../examples/parametric.donut");
    let env = load(input);

    // Template param and members exist
    assert!(env.lookup.contains_key("C"));
    assert!(env.lookup.contains_key("cat.x"));
    assert!(env.lookup.contains_key("cat.m"));
    assert!(env.lookup.contains_key("cat.a"));

    // Instantiated members exist
    assert!(env.lookup.contains_key("c.x"));
    assert!(env.lookup.contains_key("c.m"));
    assert!(env.lookup.contains_key("c.a"));

    // c.a is a 3-cell
    let ca = &env.entries[env.lookup["c.a"]];
    assert_eq!(ca.as_cell().unwrap().pure.dim().in_space, 3);

    // c.x's prim should have args (the substituted u)
    let cx = &env.entries[env.lookup["c.x"]];
    match &cx.as_cell().unwrap().pure {
        PureCell::Prim(prim, _, _) => {
            assert_eq!(prim.args.len(), 1, "c.x should have 1 arg (u)");
            // The arg should be a Cell containing u's PureCell
            let u_entry = &env.entries[env.lookup["u"]];
            match &prim.args[0] {
                PrimArg::Cell(cell) => {
                    assert_eq!(cell, &u_entry.as_cell().unwrap().pure, "c.x's arg should be u");
                }
                _ => panic!("expected Cell arg"),
            }
        }
        _ => panic!("expected Prim"),
    }

    // result is the last entry
    let last = env.entries.last().unwrap();
    assert_eq!(last.name, "result");
}

#[test]
fn test_load_default_donut() {
    let input = include_str!("../../../../donut-app/src/default.donut");
    let env = load(input);

    // Should load without errors
    assert!(env.lookup.contains_key("u"));
    assert!(env.lookup.contains_key("pentagon"));
}
