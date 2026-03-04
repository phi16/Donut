use crate::load::load;
use donut_core::cell::Globular;

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
    let table = load(input).unwrap();
    assert_eq!(table.elements.len(), 7);
    assert_eq!(table.elements[0].name, "u");
    assert_eq!(table.elements[1].name, "x");
    assert_eq!(table.elements[2].name, "m");
    assert_eq!(table.elements[3].name, "assoc");
    assert_eq!(table.elements[4].name, "rm");
    assert_eq!(table.elements[5].name, "lm");
    assert_eq!(table.elements[6].name, "assoc2");

    // u is 0-dim
    assert_eq!(table.elements[0].cell.pure.dim().in_space, 0);
    // x is 1-dim
    assert_eq!(table.elements[1].cell.pure.dim().in_space, 1);
    // m is 2-dim (x x → x)
    assert_eq!(table.elements[2].cell.pure.dim().in_space, 2);
    // assoc is 3-dim
    assert_eq!(table.elements[3].cell.pure.dim().in_space, 3);
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
    let table = load(input).unwrap();
    assert!(table.elements.len() > 0);
    let last = table.elements.last().unwrap();
    assert_eq!(last.name, "pentagon");
}

#[test]
fn test_load_colors() {
    let input = r#"
        gray: *
        hsv: *
        rgb: *
        [gray[80]]
        u: *
        [hsv[0.6, 1, 1]]
        x: u → u
        [rgb[255, 0, 128]]
        m: x x → x
    "#;
    let table = load(input).unwrap();
    let u = table.lookup["u"];
    let x = table.lookup["x"];
    let m = table.lookup["m"];
    assert_eq!(table.elements[u].color, (80, 80, 80));
    // hsv(0.6, 1, 1) should produce a blue-ish color
    let c = table.elements[x].color;
    assert!(c.2 > c.0 && c.2 > c.1, "hsv(0.6) should be blue-ish: {:?}", c);
    assert_eq!(table.elements[m].color, (255, 0, 128));
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
    let table = load(input).unwrap();
    assert_eq!(table.elements.len(), 3);
    assert_eq!(table.elements[0].name, "cat.u");
    assert_eq!(table.elements[1].name, "cat.x");
    assert_eq!(table.elements[2].name, "f");
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
    let table = load(input).unwrap();
    assert_eq!(table.elements[0].name, "a.b.u");
    assert_eq!(table.elements[1].name, "a.b.x");
    assert_eq!(table.elements[2].name, "a.y");
    assert_eq!(table.elements[3].name, "f");
    assert_eq!(table.elements[3].cell.pure.dim().in_space, 2);
}

#[test]
fn test_load_auto_color() {
    let input = r#"
        u: *
        x: u → u
    "#;
    let table = load(input).unwrap();
    // Without explicit color, auto_color is used
    let c0 = table.elements[0].color;
    let c1 = table.elements[1].color;
    // Colors should be different
    assert_ne!(c0, c1);
}

#[test]
fn test_load_nat_example() {
    let input = include_str!("../../../../examples/nat.donut");
    let table = load(input).unwrap();

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
    let add_assoc = &table.elements[table.lookup["add_assoc"]];
    assert_eq!(add_assoc.cell.pure.dim().in_space, 3);

    let add_comm = &table.elements[table.lookup["add_comm"]];
    assert_eq!(add_comm.cell.pure.dim().in_space, 3);

    // Equivalence (~ arrow)
    let succ_add = &table.elements[table.lookup["succ_add"]];
    assert_eq!(succ_add.cell.pure.dim().in_space, 3);

    // Functor type declaration is skipped, but mappings are loaded
    assert!(table.lookup.contains_key("compile"));
}
