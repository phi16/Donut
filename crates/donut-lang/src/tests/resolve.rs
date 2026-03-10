use crate::convert::convert;
use crate::parse::parse;
use crate::tokenize::tokenize;
use crate::types::item::*;

fn check_errs(code: &str) -> Vec<String> {
    let (tokens, _, _) = tokenize(code.trim());
    let (program, _) = parse(&tokens);
    let (sem_prog, conv_errors) = convert(program, &tokens);
    assert!(
        conv_errors.is_empty(),
        "unexpected convert errors: {conv_errors:?}"
    );
    let (_program, errors) = crate::check::resolve(sem_prog, &tokens);
    errors.into_iter().map(|(_, msg)| msg).collect()
}

fn check_ok(code: &str) {
    let errs = check_errs(code);
    assert!(errs.is_empty(), "unexpected errors: {errs:?}");
}

fn check_module(code: &str) -> Program {
    let (tokens, _, _) = tokenize(code.trim());
    let (program, _) = parse(&tokens);
    let (sem_prog, conv_errors) = convert(program, &tokens);
    assert!(
        conv_errors.is_empty(),
        "unexpected convert errors: {conv_errors:?}"
    );
    let (program, errors) = crate::check::resolve(sem_prog, &tokens);
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    program
}

fn get_item<'a>(p: &'a Program, name: &str) -> &'a Item {
    p.item(p.root.get(name).unwrap())
}

/// Get the lnames of a module's entries.
fn names<'a>(p: &'a Program, m: &Module) -> Vec<&'a str> {
    m.entries
        .iter()
        .map(|&id| p.item(id).lname.as_str())
        .collect()
}

/// Extract a single-segment path name from a Val.
fn val_as_path_name<'a>(p: &'a Program, val_id: ValId) -> Option<&'a str> {
    match p.val(val_id) {
        Val::Path(path) if path.segments.len() == 1 => Some(&path.segments[0].name),
        _ => None,
    }
}

/// Extract the string literal value from a Val.
fn val_as_string<'a>(p: &'a Program, val_id: ValId) -> Option<&'a str> {
    match p.val(val_id) {
        Val::Lit(Lit::String(s)) => Some(s),
        _ => None,
    }
}

// ============================================================
// Basic resolution
// ============================================================

#[test]
fn basic_resolution() {
    check_ok("x = \"a\"\ny = x");
}

#[test]
fn undefined_name() {
    let errs = check_errs("x = y");
    assert!(
        errs.iter()
            .any(|e| e.contains("undefined") && e.contains("y")),
        "{errs:?}"
    );
}

#[test]
fn duplicate_definition() {
    let errs = check_errs("x = \"a\"\nx = \"b\"");
    assert!(
        errs.iter().any(|e| e.contains("duplicate definition")),
        "{errs:?}"
    );
}

#[test]
fn duplicate_definition_in_module() {
    let errs = check_errs("m = {\n  x = \"a\"\n  x = \"b\"\n}");
    assert!(
        errs.iter().any(|e| e.contains("duplicate definition")),
        "{errs:?}"
    );
}

#[test]
fn scope_ordering() {
    let errs = check_errs("x = y\ny = \"a\"");
    assert!(
        errs.iter()
            .any(|e| e.contains("undefined") && e.contains("y")),
        "{errs:?}"
    );
}

#[test]
fn string_literal_no_resolution() {
    check_ok("x = \"hello\"");
}

#[test]
fn number_literal_no_resolution() {
    check_ok("x = 42");
    check_ok("x = -1");
    check_ok("x = 0");
}

// ============================================================
// Params
// ============================================================

#[test]
fn param_in_scope() {
    check_ok("T = \"t\"\nf[x: T] = x");
}

#[test]
fn param_not_visible_outside() {
    let errs = check_errs("T = \"t\"\nf[x: T] = \"a\"\ny = x");
    assert!(
        errs.iter()
            .any(|e| e.contains("undefined") && e.contains("x")),
        "{errs:?}"
    );
}

#[test]
fn decorator_param_in_scope() {
    let errs = check_errs("T = \"t\"\n[p: T] f: T = p");
    assert!(
        errs.iter()
            .any(|e| e.contains("decorator parameter") && e.contains("p")),
        "{errs:?}"
    );
}

// ============================================================
// Where
// ============================================================

#[test]
fn where_scope() {
    check_ok("x = g\n  where {\n    g = \"a\"\n  }");
}

#[test]
fn where_closed() {
    let errs = check_errs("x = \"a\"\n  where {\n    g = \"b\"\n  }\ny = g");
    assert!(
        errs.iter()
            .any(|e| e.contains("undefined") && e.contains("g")),
        "{errs:?}"
    );
}

#[test]
fn where_ordering() {
    let errs = check_errs("x = g\n  where {\n    g = h\n    h = \"a\"\n  }");
    assert!(
        errs.iter()
            .any(|e| e.contains("undefined") && e.contains("h")),
        "{errs:?}"
    );
}

#[test]
fn where_reverse_order() {
    check_ok(
        r#"
x = "val"
  where {
    a = b
  }
  where {
    b = "inner"
  }
"#,
    );
}

// ============================================================
// Module
// ============================================================

#[test]
fn module_scope() {
    check_ok("m = {\n    x = \"a\"\n    y = x\n}");
}

#[test]
fn module_ref_outer() {
    check_ok("a = \"x\"\nm = {\n    x = a\n}");
}

#[test]
fn module_inner_not_visible() {
    let errs = check_errs("m = {\n    x = \"a\"\n}\ny = x");
    assert!(
        errs.iter()
            .any(|e| e.contains("undefined") && e.contains("x")),
        "{errs:?}"
    );
}

// ============================================================
// Module member resolution
// ============================================================

#[test]
fn dotted_ref_member() {
    check_ok("m = {\n    x = \"a\"\n}\ny = m.x");
}

#[test]
fn dotted_ref_undefined_member() {
    let errs = check_errs("m = {\n    x = \"a\"\n}\ny = m.z");
    assert!(
        errs.iter()
            .any(|e| e.contains("undefined member") && e.contains("z")),
        "{errs:?}"
    );
}

#[test]
fn dotted_ref_deep() {
    check_ok("a = {\n    b = {\n        c = \"x\"\n    }\n}\ny = a.b.c");
}

#[test]
fn dotted_ref_deep_undefined() {
    let errs = check_errs("a = {\n    b = \"x\"\n}\ny = a.b.c");
    assert!(
        errs.iter()
            .any(|e| e.contains("undefined member") && e.contains("c")),
        "{errs:?}"
    );
}

// ============================================================
// Dotted declarations
// ============================================================

#[test]
fn dotted_decl_basic() {
    check_ok("m = \"a\"\nm.x = \"b\"");
}

#[test]
fn dotted_decl_module() {
    check_ok("m = {\n    dummy = \"a\"\n}\nm.x = \"b\"");
}

#[test]
fn dotted_decl_undefined_prefix() {
    let errs = check_errs("m.x = \"a\"");
    assert!(
        errs.iter()
            .any(|e| e.contains("undefined") && e.contains("m")),
        "{errs:?}"
    );
}

#[test]
fn dotted_decl_ref_after() {
    check_ok("m = \"a\"\nm.x = \"b\"\ny = m.x");
}

// ============================================================
// With clauses
// ============================================================

#[test]
fn with_basic() {
    check_ok("x = \"a\"\n  with {\n    y = \"b\"\n  }\nz = x.y");
}

#[test]
fn with_self_ref() {
    check_ok("x = \"a\"\n  with {\n    y = x\n  }");
}

#[test]
fn with_duplicate_key() {
    let errs = check_errs("x = \"a\"\n  with {\n    y = \"b\"\n  }\n  with {\n    y = \"c\"\n  }");
    assert!(
        errs.iter()
            .any(|e| e.contains("duplicate member") && e.contains("y")),
        "{errs:?}"
    );
}

#[test]
fn with_body_and_with_conflict() {
    let errs = check_errs("x = {\n    a = \"1\"\n}\n  with {\n    a = \"2\"\n  }");
    assert!(
        errs.iter()
            .any(|e| e.contains("duplicate member") && e.contains("a")),
        "{errs:?}"
    );
}

// ============================================================
// +=
// ============================================================

#[test]
fn add_basic() {
    check_ok("x = {\n    a = \"1\"\n}\nx += {\n    b = \"2\"\n}\ny = x.b");
}

#[test]
fn add_undefined_lhs() {
    let errs = check_errs("x += {\n    a = \"1\"\n}");
    assert!(
        errs.iter()
            .any(|e| e.contains("must be declared before `+=`")),
        "{errs:?}"
    );
}

#[test]
fn add_duplicate_key() {
    let errs = check_errs("x = {\n    a = \"1\"\n}\nx += {\n    a = \"2\"\n}");
    assert!(
        errs.iter()
            .any(|e| e.contains("duplicate member") && e.contains("a")),
        "{errs:?}"
    );
}

#[test]
fn add_from_ref() {
    check_ok("m = {\n    a = \"1\"\n}\nx = {\n    b = \"2\"\n}\nx += m\ny = x.a");
}

#[test]
fn add_literal_error() {
    let errs = check_errs("x = \"a\"\nx += \"b\"");
    assert!(errs.iter().any(|e| e.contains("`+=` requires")), "{errs:?}");
}

#[test]
fn add_comp_error() {
    let errs = check_errs("a = \"x\"\nb = \"y\"\nx = \"z\"\nx += a ; b");
    assert!(errs.iter().any(|e| e.contains("`+=` requires")), "{errs:?}");
}

#[test]
fn add_arrow_error() {
    let errs = check_errs("a = \"x\"\nb = \"y\"\nx = \"z\"\nx += a → b");
    assert!(errs.iter().any(|e| e.contains("`+=` requires")), "{errs:?}");
}

#[test]
fn add_no_members() {
    check_ok("m = \"value\"\nx = \"z\"\nx += m");
}

#[test]
fn add_dotted_path_ref() {
    check_ok(
        r#"
a = {
    b = {
        c = "1"
    }
}
x = "z"
x += a.b
y = x.c
"#,
    );
}

#[test]
fn add_multiple() {
    check_ok(
        r#"
x = {
    a = "1"
}
x += {
    b = "2"
}
x += {
    c = "3"
}
r1 = x.a
r2 = x.b
r3 = x.c
"#,
    );
}

#[test]
fn add_number_error() {
    let errs = check_errs("x = \"a\"\nx += 42");
    assert!(errs.iter().any(|e| e.contains("`+=` requires")), "{errs:?}");
}

// ============================================================
// Value and members coexistence
// ============================================================

#[test]
fn value_and_members() {
    check_ok("x = \"a\"\nx += {\n    y = \"b\"\n}\nz = x\nw = x.y");
}

// ============================================================
// Operators
// ============================================================

#[test]
fn op_both_sides_checked() {
    let errs = check_errs("x = a ; b");
    assert!(
        errs.len() >= 2,
        "expected errors for both a and b: {errs:?}"
    );
}

#[test]
fn op_with_defined_names() {
    check_ok("a = \"x\"\nb = \"y\"\nx = a ; b");
}

// ============================================================
// Compound tests
// ============================================================

#[test]
fn compound_nested_module_with_params() {
    check_ok(
        r#"
T = "t"
m[x: T] = {
    inner = x
}
"#,
    );
}

#[test]
fn compound_deep_dotted_ref_through_modules() {
    check_ok(
        r#"
a = {
    b = {
        c = {
            val = "leaf"
        }
    }
}
r = a.b.c.val
"#,
    );
}

#[test]
fn compound_dotted_decl_with_params() {
    check_ok(
        r#"
T = "t"
m = "base"
m.f[x: T] = x
"#,
    );
}

#[test]
fn compound_dotted_decl_then_ref() {
    check_ok(
        r#"
m = "root"
m.a = "1"
m.b = "2"
x = m.a
y = m.b
"#,
    );
}

#[test]
fn compound_with_where_combined() {
    check_ok(
        r#"
x = g
  with {
    a = "member"
  }
  where {
    g = "val"
  }
r = x.a
"#,
    );
}

#[test]
fn compound_where_module_in_body() {
    check_ok(
        r#"
x = helper.val
  where {
    helper = {
        val = "x"
    }
  }
"#,
    );
}

#[test]
fn compound_with_nested_with() {
    check_ok(
        r#"
x = "val"
  with {
    a = "inner"
      with {
        b = "nested"
      }
  }
r1 = x.a
r2 = x.a.b
"#,
    );
}

#[test]
fn compound_add_with_and_dotted_combined() {
    check_ok(
        r#"
m = {
    a = "1"
}
  with {
    b = "2"
  }
m.c = "3"
m += {
    d = "4"
}
r1 = m.a
r2 = m.b
r3 = m.c
r4 = m.d
"#,
    );
}

#[test]
fn compound_add_transfers_deep_members() {
    check_ok(
        r#"
src = {
    inner = {
        val = "deep"
    }
}
dst = {
    other = "x"
}
dst += src
r = dst.inner.val
"#,
    );
}

#[test]
fn compound_decorator_param_with_module() {
    let errs = check_errs(
        r#"
T = "t"
[p: T] m = {
    x = p
}
"#,
    );
    assert!(
        errs.iter()
            .any(|e| e.contains("decorator parameter") && e.contains("p")),
        "{errs:?}"
    );
}

#[test]
fn compound_param_visible_in_with() {
    check_ok(
        r#"
T = "t"
f[x: T] = "a"
  with {
    y = x
  }
"#,
    );
}

#[test]
fn compound_name_visible_in_where() {
    check_ok(
        r#"
f = g
  where {
    g = f
  }
"#,
    );
}

#[test]
fn compound_dotted_decl_duplicate_member() {
    let errs = check_errs(
        r#"
m = {
    x = "1"
}
m.x = "2"
"#,
    );
    assert!(
        errs.iter()
            .any(|e| e.contains("duplicate member") && e.contains("x")),
        "{errs:?}"
    );
}

#[test]
fn compound_with_on_both_decls() {
    check_ok(
        r#"
a = "v1"
  with {
    x = "m1"
  }
b = "v2"
  with {
    y = a.x
  }
r1 = a.x
r2 = b.y
"#,
    );
}

#[test]
fn compound_multi_name_decl_only() {
    check_ok(
        r#"
T = "t"
a b: T
r1 = a
r2 = b
"#,
    );
}

#[test]
fn compound_kitchen_sink() {
    check_ok(
        r#"
T = "type"
U = "type2"

base = {
    core = {
        v = "x"
    }
    util = "u"
}

ext[q: U] = base.core.v
  with {
    sub = "s"
  }
  where {
    helper = base.util
  }

ext.extra = "e"

r1 = ext.sub
r2 = ext.extra
r3 = base.core.v
"#,
    );
}

#[test]
fn compound_error_deep_undefined_member() {
    let errs = check_errs(
        r#"
a = {
    b = {
        c = "x"
    }
}
r = a.b.missing.c
"#,
    );
    assert!(
        errs.iter()
            .any(|e| e.contains("undefined member") && e.contains("missing")),
        "{errs:?}"
    );
}

#[test]
fn compound_error_add_then_duplicate_via_dotted() {
    let errs = check_errs(
        r#"
m = "base"
m += {
    x = "1"
}
m.x = "2"
"#,
    );
    assert!(
        errs.iter()
            .any(|e| e.contains("duplicate member") && e.contains("x")),
        "{errs:?}"
    );
}

// ============================================================
// Anonymous block
// ============================================================

#[test]
fn anon_block_names_visible_outside() {
    check_ok(
        r#"
{
    x = "a"
    y = "b"
}
r1 = x
r2 = y
"#,
    );
}

#[test]
fn anon_block_with_decorator() {
    check_ok(
        r#"
T = "t"
[T] {
    x = "a"
    y = "b"
}
r = x
"#,
    );
}

#[test]
fn anon_block_where() {
    check_ok(
        r#"
{
    x = g
}
  where {
    g = "val"
  }
r = x
"#,
    );
}

#[test]
fn anon_block_deco_param_visible() {
    let errs = check_errs(
        r#"
T = "t"
[p: T] {
    x = p
}
r = x
"#,
    );
    assert!(
        errs.iter()
            .any(|e| e.contains("decorator parameter") && e.contains("p")),
        "{errs:?}"
    );
}

#[test]
fn anon_block_deco_param_not_leaking() {
    let errs = check_errs(
        r#"
T = "t"
[p: T] {
    x = p
}
r = p
"#,
    );
    assert!(
        errs.iter()
            .any(|e| e.contains("undefined") && e.contains("p")),
        "{errs:?}"
    );
}

#[test]
fn anon_block_where_not_leaking() {
    let errs = check_errs(
        r#"
{
    x = g
}
  where {
    g = "val"
  }
r = g
"#,
    );
    assert!(
        errs.iter()
            .any(|e| e.contains("undefined") && e.contains("g")),
        "{errs:?}"
    );
}

#[test]
fn anon_block_with() {
    check_ok(
        r#"
{
    x = "a"
}
  with {
    y = "b"
  }
r1 = x
r2 = y
"#,
    );
}

#[test]
fn anon_block_ordering() {
    let errs = check_errs(
        r#"
{
    x = y
    y = "a"
}
"#,
    );
    assert!(
        errs.iter()
            .any(|e| e.contains("undefined") && e.contains("y")),
        "{errs:?}"
    );
}

// ============================================================
// Module output tests
// ============================================================

#[test]
fn module_output_declaration_order() {
    let p = check_module(
        r#"
a = "1"
b = "2"
c = "3"
"#,
    );
    assert_eq!(names(&p, &p.root), vec!["a", "b", "c"]);
}

#[test]
fn module_output_val_content() {
    let p = check_module(
        r#"
x = "hello"
y = "world"
"#,
    );
    assert_eq!(
        val_as_string(&p, get_item(&p, "x").val().unwrap()),
        Some("\"hello\"")
    );
    assert_eq!(
        val_as_string(&p, get_item(&p, "y").val().unwrap()),
        Some("\"world\"")
    );
}

#[test]
fn module_output_val_path_content() {
    let p = check_module(
        r#"
a = "v"
b = a
"#,
    );
    assert_eq!(
        val_as_path_name(&p, get_item(&p, "b").val().unwrap()),
        Some("a")
    );
}

#[test]
fn module_output_ty_content() {
    let p = check_module(
        r#"
T = "t"
U = "u"
x: T
y: U = "v"
"#,
    );
    assert_eq!(
        val_as_path_name(&p, get_item(&p, "x").ty.unwrap()),
        Some("T")
    );
    assert_eq!(
        val_as_path_name(&p, get_item(&p, "y").ty.unwrap()),
        Some("U")
    );
    assert!(get_item(&p, "T").ty.is_none());
}

#[test]
fn module_output_param_ty_content() {
    let p = check_module(
        r#"
T = "t"
U = "u"
f[x: T, y: U] = "v"
"#,
    );
    let f = get_item(&p, "f");
    assert_eq!(f.params.len(), 2);
    assert_eq!(f.params[0].name, "x");
    assert_eq!(val_as_path_name(&p, f.params[0].ty), Some("T"));
    assert_eq!(f.params[1].name, "y");
    assert_eq!(val_as_path_name(&p, f.params[1].ty), Some("U"));
}

#[test]
fn module_output_nested_val_content() {
    let p = check_module(
        r#"
m = {
    x = "inner"
}
"#,
    );
    let m_item = get_item(&p, "m");
    let x_id = m_item.members().unwrap().get("x").unwrap();
    let x_item = p.item(x_id);
    assert_eq!(val_as_string(&p, x_item.val().unwrap()), Some("\"inner\""));
}

#[test]
fn module_output_where_not_in_members() {
    let p = check_module(
        r#"
x = g
  where {
    g = "val"
  }
"#,
    );
    let x = get_item(&p, "x");
    assert!(
        x.members().unwrap().get("g").is_none(),
        "where binding should not be a member"
    );
    assert_eq!(val_as_path_name(&p, x.val().unwrap()), Some("g"));
}

#[test]
fn module_output_with_val_and_members() {
    let p = check_module(
        r#"
x = "base"
  with {
    a = "m1"
    b = "m2"
  }
"#,
    );
    let x = get_item(&p, "x");
    assert_eq!(val_as_string(&p, x.val().unwrap()), Some("\"base\""));
    let a = p.item(x.members().unwrap().get("a").unwrap());
    assert_eq!(val_as_string(&p, a.val().unwrap()), Some("\"m1\""));
    let b = p.item(x.members().unwrap().get("b").unwrap());
    assert_eq!(val_as_string(&p, b.val().unwrap()), Some("\"m2\""));
}

#[test]
fn module_output_deco_param_not_in_module() {
    let errs = check_errs(
        r#"
T = "t"
[p: T] f = "v"
"#,
    );
    assert!(
        errs.iter()
            .any(|e| e.contains("decorator parameters require a functor application")),
        "{errs:?}"
    );
}

#[test]
fn module_output_module_with_params() {
    let p = check_module(
        r#"
T = "t"
m[x: T] = {
    inner = x
}
"#,
    );
    let item = get_item(&p, "m");
    assert_eq!(item.params.len(), 1);
    assert_eq!(item.params[0].name, "x");
    assert_eq!(val_as_path_name(&p, item.params[0].ty), Some("T"));
    assert!(item.val().is_none(), "module body should not produce val");
    assert_eq!(names(&p, item.members().unwrap()), vec!["inner"]);
}

#[test]
fn module_output_nested_members_order() {
    let p = check_module(
        r#"
m = {
    x = "1"
    y = "2"
    z = "3"
}
"#,
    );
    let members = get_item(&p, "m").members().unwrap();
    assert_eq!(names(&p, members), vec!["x", "y", "z"]);
}

#[test]
fn module_output_with_members_order() {
    let p = check_module(
        r#"
x = "v"
  with {
    a = "1"
    b = "2"
  }
"#,
    );
    let members = get_item(&p, "x").members().unwrap();
    assert_eq!(names(&p, members), vec!["a", "b"]);
}

#[test]
fn module_output_dotted_decl_preserves_order() {
    let p = check_module(
        r#"
m = {
    a = "1"
}
m.b = "2"
m.c = "3"
"#,
    );
    let members = get_item(&p, "m").members().unwrap();
    assert_eq!(names(&p, members), vec!["a", "b", "c"]);
}

#[test]
fn module_output_add_preserves_order() {
    let p = check_module(
        r#"
x = {
    a = "1"
}
x += {
    b = "2"
    c = "3"
}
"#,
    );
    let members = get_item(&p, "x").members().unwrap();
    assert_eq!(names(&p, members), vec!["a", "b", "c"]);
}

#[test]
fn module_output_anon_block_order() {
    let p = check_module(
        r#"
a = "1"
{
    b = "2"
    c = "3"
}
d = "4"
"#,
    );
    assert_eq!(names(&p, &p.root), vec!["a", "b", "c", "d"]);
}

#[test]
fn module_output_deep_nesting() {
    let p = check_module(
        r#"
a = {
    b = {
        c = "leaf"
        d = "leaf2"
    }
}
"#,
    );
    let a = get_item(&p, "a");
    let b_id = a.members().unwrap().get("b").unwrap();
    let b = p.item(b_id);
    assert_eq!(names(&p, b.members().unwrap()), vec!["c", "d"]);
}

// ============================================================
// Functor application constraints
// ============================================================

#[test]
fn functor_app_alias_ok() {
    check_ok("A = \"a\"\nB = \"b\"\nf: A ~> B\na = \"y\"\nf(a) = \"z\"");
}

#[test]
fn functor_app_decl_error() {
    let errs = check_errs("A = \"a\"\nB = \"b\"\nf: A ~> B\na = \"y\"\nf(a): \"z\"");
    assert!(
        errs.iter().any(|e| e.contains("functor application")),
        "{errs:?}"
    );
}

#[test]
fn functor_app_module_error() {
    let errs = check_errs("A = \"a\"\nB = \"b\"\nf: A ~> B\na = \"y\"\nf(a) = {\n    x = \"v\"\n}");
    assert!(
        errs.iter().any(|e| e.contains("functor application")),
        "{errs:?}"
    );
}

#[test]
fn functor_app_with_error() {
    let errs = check_errs(
        "A = \"a\"\nB = \"b\"\nf: A ~> B\na = \"y\"\nf(a) = \"z\" with {\n    w = \"v\"\n}",
    );
    assert!(
        errs.iter().any(|e| e.contains("functor application")),
        "{errs:?}"
    );
}

#[test]
fn functor_app_not_a_functor() {
    let errs = check_errs("f = \"x\"\na = \"y\"\nf(a) = \"z\"");
    assert!(
        errs.iter().any(|e| e.contains("is not a functor")),
        "{errs:?}"
    );
}

#[test]
fn functor_app_mappings_stored() {
    let p = check_module(
        "A = \"a\"\nB = \"b\"\na = \"x\"\nb = \"y\"\nf: A ~> B\nf(a) = \"x\"\nf(b) = \"y\"",
    );
    let f = get_item(&p, "f");
    match &f.body {
        ItemBody::Functor { mappings } => assert_eq!(mappings.len(), 2),
        _ => panic!("expected Functor body"),
    }
}

#[test]
fn functor_app_undefined_functor() {
    let errs = check_errs("a = \"a\"\ng(a) = \"x\"");
    assert!(
        errs.iter().any(|e| e.contains("undefined functor")),
        "{errs:?}"
    );
}

#[test]
fn functor_app_deco_param_stored() {
    let p = check_module(
        r#"
A = "a"
B = "b"
T = "t"
a[n: T] = "x"
f: A ~> B
[n: T] f(a[n]) = "y"
"#,
    );
    let f = get_item(&p, "f");
    match &f.body {
        ItemBody::Functor { mappings } => {
            assert_eq!(mappings.len(), 1);
            assert_eq!(mappings[0].params.len(), 1);
            assert_eq!(mappings[0].params[0].name, "n");
        }
        _ => panic!("expected Functor body"),
    }
}

#[test]
fn functor_app_deco_param_must_appear_in_applicand() {
    let errs = check_errs(
        r#"
A = "a"
B = "b"
T = "t"
a = "x"
f: A ~> B
[n: T] f(a) = "y"
"#,
    );
    assert!(
        errs.iter()
            .any(|e| e.contains("decorator parameter must appear in functor applicand")),
        "{errs:?}"
    );
}

#[test]
fn functor_app_deco_param_usable_in_body() {
    check_ok(
        r#"
A = "a"
B = "b"
T = "t"
a[n: T] = "x"
b[n: T] = "y"
f: A ~> B
[n: T] f(a[n]) = b[n]
"#,
    );
}

// ============================================================
// Import
// ============================================================

#[test]
fn import_base() {
    let p = check_module("import \"base\"\nC: *\nx: C → C");
    assert!(p.root.get("C").is_some());
    assert!(p.root.get("nat").is_some());
    assert!(p.root.get("x").is_some());
}

#[test]
fn import_named() {
    let p = check_module("b = import \"base\"\nC: *\nx: C → C");
    assert!(p.root.get("b").is_some());
    assert!(p.root.get("x").is_some());
}

#[test]
fn import_sys_after_base() {
    let p = check_module(
        "\
import \"base\"
sys = import \"sys\"
x = sys.u32.lit[42]
",
    );
    assert!(p.root.get("sys").is_some());
    assert!(p.root.get("x").is_some());
}

#[test]
fn import_unknown() {
    let errs = check_errs("import \"unknown\"");
    assert!(
        errs.iter().any(|e| e.contains("unknown import")),
        "{errs:?}"
    );
}

// ============================================================
// Use
// ============================================================

#[test]
fn use_base_not_exported() {
    let p = check_module(
        "\
use \"base\"
x = \"hello\"
",
    );
    assert!(p.root.get("x").is_some());
    assert!(
        p.root.get("nat").is_none(),
        "used entries should not be exported"
    );
}

#[test]
fn use_base_available_for_resolution() {
    check_ok(
        "\
use \"base\"
f[n: nat] = \"x\"
",
    );
}

#[test]
fn use_unknown() {
    let errs = check_errs("use \"unknown\"");
    assert!(
        errs.iter().any(|e| e.contains("unknown import")),
        "{errs:?}"
    );
}

#[test]
fn use_in_block_not_exported() {
    let p = check_module(
        "\
m = {
    use \"base\"
    f[n: nat] = \"x\"
}
",
    );
    assert!(p.root.get("m").is_some());
    assert!(p.root.get("nat").is_none());
}

#[test]
fn import_does_not_reexport_used() {
    let p = check_module(
        "\
import \"ui\"
C: *
x: C → C
",
    );
    assert!(p.root.get("C").is_some());
    assert!(p.root.get("style").is_some());
    assert!(p.root.get("nat").is_none());
    assert!(p.root.get("decorator").is_none());
}

// ============================================================
// Gen/Item structure tests (NEW)
// ============================================================

#[test]
fn gen_created_for_declaration() {
    let p = check_module("T = \"t\"\nx: T");
    // x: T should create a Gen (Decl)
    let decl_gens: Vec<_> = p
        .gens
        .iter()
        .filter(|g| matches!(g.kind, GenKind::Decl))
        .collect();
    assert!(
        decl_gens.iter().any(|g| g.lname == "x"),
        "expected Gen(Decl) for x, got: {:?}",
        decl_gens.iter().map(|g| &g.lname).collect::<Vec<_>>()
    );
}

#[test]
fn gen_not_created_for_alias() {
    let p = check_module("a = \"v\"\nb = a");
    // b = a is an alias, no Gen should be created for b
    let b_gens: Vec<_> = p.gens.iter().filter(|g| g.lname == "b").collect();
    assert!(
        b_gens.is_empty(),
        "alias should not create a Gen, got: {:?}",
        b_gens
    );
}

#[test]
fn gen_created_for_def() {
    let p = check_module("T = \"t\"\ng: T := \"v\"");
    let def_gens: Vec<_> = p
        .gens
        .iter()
        .filter(|g| matches!(g.kind, GenKind::Def) && g.lname == "g")
        .collect();
    assert_eq!(def_gens.len(), 1, "expected Gen(Def) for g");
}

#[test]
fn gen_created_for_param() {
    let p = check_module("T = \"t\"\nf[x: T] = \"v\"");
    let param_gens: Vec<_> = p
        .gens
        .iter()
        .filter(|g| matches!(g.kind, GenKind::Param))
        .collect();
    assert!(
        param_gens.iter().any(|g| g.lname == "x"),
        "expected Gen(Param) for x, got: {:?}",
        param_gens.iter().map(|g| &g.lname).collect::<Vec<_>>()
    );
}

#[test]
fn gen_cname_basic() {
    let p = check_module("T = \"t\"\na: T");
    let g = p.gens.iter().find(|g| g.lname == "a").unwrap();
    assert_eq!(g.cname, "a");
}

#[test]
fn gen_cname_in_module() {
    let p = check_module(
        r#"
T = "t"
m = {
    a: T
}
"#,
    );
    let g = p.gens.iter().find(|g| g.lname == "a").unwrap();
    assert_eq!(g.cname, "m.a");
}

#[test]
fn gen_cname_with_origin() {
    let p = check_module("import \"base\"");
    // base.donut defines "nat: meta" etc.
    let nat_gen = p.gens.iter().find(|g| g.lname == "nat");
    assert!(nat_gen.is_some(), "expected Gen for nat from base import");
    assert!(
        nat_gen.unwrap().cname.starts_with("base::"),
        "expected cname with base:: prefix, got: {}",
        nat_gen.unwrap().cname
    );
}

#[test]
fn gen_param_cname() {
    let p = check_module("T = \"t\"\nf[x: T] = \"v\"");
    let param_gen = p
        .gens
        .iter()
        .find(|g| g.lname == "x" && matches!(g.kind, GenKind::Param))
        .unwrap();
    assert_eq!(param_gen.cname, "f#x");
}

#[test]
fn item_qname_basic() {
    let p = check_module("a = \"v\"");
    let item = get_item(&p, "a");
    assert_eq!(item.qname, "a");
    assert_eq!(item.lname, "a");
}

#[test]
fn item_qname_in_module() {
    let p = check_module(
        r#"
m = {
    a = "v"
}
"#,
    );
    let m = get_item(&p, "m");
    let a_id = m.members().unwrap().get("a").unwrap();
    let a = p.item(a_id);
    assert_eq!(a.qname, "m.a");
    assert_eq!(a.lname, "a");
}

#[test]
fn item_qname_nested_module() {
    let p = check_module(
        r#"
a = {
    b = {
        c = "v"
    }
}
"#,
    );
    let a = get_item(&p, "a");
    let b_id = a.members().unwrap().get("b").unwrap();
    let b = p.item(b_id);
    assert_eq!(b.qname, "a.b");
    let c_id = b.members().unwrap().get("c").unwrap();
    let c = p.item(c_id);
    assert_eq!(c.qname, "a.b.c");
}

#[test]
fn gen_dedup_across_imports() {
    // Importing the same module twice should reuse Gens
    let p = check_module(
        "\
import \"base\"
b = import \"base\"
",
    );
    let nat_gens: Vec<_> = p.gens.iter().filter(|g| g.cname == "base::nat").collect();
    assert_eq!(
        nat_gens.len(),
        1,
        "expected exactly one Gen for base::nat (dedup), got {}",
        nat_gens.len()
    );
}

#[test]
fn gen_decl_has_type() {
    let p = check_module("T = \"t\"\na: T");
    let g = p.gens.iter().find(|g| g.lname == "a").unwrap();
    // Gen.ty should point to a Val::Path with name "T"
    assert_eq!(val_as_path_name(&p, g.ty), Some("T"));
}

#[test]
fn gen_decl_has_params() {
    let p = check_module("T = \"t\"\nU = \"u\"\nf[x: T, y: U]: T");
    let g = p.gens.iter().find(|g| g.lname == "f").unwrap();
    assert_eq!(g.params.len(), 2);
    assert_eq!(g.params[0].name, "x");
    assert_eq!(g.params[1].name, "y");
}

#[test]
fn multiple_decls_multiple_gens() {
    let p = check_module("T = \"t\"\na: T\nb: T\nc: T");
    let decl_gens: Vec<_> = p
        .gens
        .iter()
        .filter(|g| matches!(g.kind, GenKind::Decl))
        .map(|g| g.lname.as_str())
        .collect();
    assert!(decl_gens.contains(&"a"));
    assert!(decl_gens.contains(&"b"));
    assert!(decl_gens.contains(&"c"));
}

#[test]
fn gen_not_created_for_composition() {
    let p = check_module("a = \"x\"\nb = \"y\"\nf = a ; b");
    let f_gens: Vec<_> = p.gens.iter().filter(|g| g.lname == "f").collect();
    assert!(
        f_gens.is_empty(),
        "composition should not create a Gen: {:?}",
        f_gens
    );
}

#[test]
fn item_origin_set_for_import() {
    let p = check_module("import \"base\"");
    // Items from import should have origin set
    let nat_item = get_item(&p, "nat");
    assert_eq!(nat_item.origin.as_deref(), Some("base"));
}
