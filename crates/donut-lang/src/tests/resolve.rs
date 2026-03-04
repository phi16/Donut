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
    let (_module, errors) = crate::resolve::resolve(sem_prog, &tokens);
    errors.into_iter().map(|(_, msg)| msg).collect()
}

fn check_ok(code: &str) {
    let errs = check_errs(code);
    assert!(errs.is_empty(), "unexpected check errors: {errs:?}");
}

fn check_module(code: &str) -> Module {
    let (tokens, _, _) = tokenize(code.trim());
    let (program, _) = parse(&tokens);
    let (sem_prog, conv_errors) = convert(program, &tokens);
    assert!(
        conv_errors.is_empty(),
        "unexpected convert errors: {conv_errors:?}"
    );
    let (module, errors) = crate::resolve::resolve(sem_prog, &tokens);
    assert!(errors.is_empty(), "unexpected check errors: {errors:?}");
    module
}

// --- Basic resolution ---
// Note: in Donut, numbers (1, 42, ...) are tokenized as Names, not literals.
// Use string literals ("...") for leaf values that don't require name resolution.

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
fn scope_ordering() {
    // y is not yet defined when x is checked
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
    // Numbers are converted to literals, no name resolution needed
    check_ok("x = 42");
    check_ok("x = -1");
    check_ok("x = 0");
}

// --- Params ---

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
    check_ok("T = \"t\"\n[p: T] f: T = p");
}

// --- Where ---

#[test]
fn where_scope() {
    check_ok("x = g\n  where {\n    g = \"a\"\n  }");
}

#[test]
fn where_closed() {
    // g is defined in where, not visible outside
    let errs = check_errs("x = \"a\"\n  where {\n    g = \"b\"\n  }\ny = g");
    assert!(
        errs.iter()
            .any(|e| e.contains("undefined") && e.contains("g")),
        "{errs:?}"
    );
}

#[test]
fn where_ordering() {
    // within where, top-to-bottom
    let errs = check_errs("x = g\n  where {\n    g = h\n    h = \"a\"\n  }");
    assert!(
        errs.iter()
            .any(|e| e.contains("undefined") && e.contains("h")),
        "{errs:?}"
    );
}

#[test]
fn where_reverse_order() {
    // With reverse processing, the second where's binding is registered first,
    // making it visible in the first where clause
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

// --- Module ---

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
    // x is defined inside m, not visible outside
    let errs = check_errs("m = {\n    x = \"a\"\n}\ny = x");
    assert!(
        errs.iter()
            .any(|e| e.contains("undefined") && e.contains("x")),
        "{errs:?}"
    );
}

// --- Module member resolution ---

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

// --- Dotted declarations ---

#[test]
fn dotted_decl_basic() {
    // Any declared name can have members added (namespace separation)
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

// --- With clauses ---

#[test]
fn with_basic() {
    check_ok("x = \"a\"\n  with {\n    y = \"b\"\n  }\nz = x.y");
}

#[test]
fn with_self_ref() {
    // x is visible inside its own with block
    check_ok("x = \"a\"\n  with {\n    y = x\n  }");
}

#[test]
fn with_duplicate_key() {
    let errs =
        check_errs("x = \"a\"\n  with {\n    y = \"b\"\n  }\n  with {\n    y = \"c\"\n  }");
    assert!(
        errs.iter()
            .any(|e| e.contains("duplicate member") && e.contains("y")),
        "{errs:?}"
    );
}

#[test]
fn with_body_and_with_conflict() {
    // Module body member conflicts with with clause member
    let errs = check_errs("x = {\n    a = \"1\"\n}\n  with {\n    a = \"2\"\n  }");
    assert!(
        errs.iter()
            .any(|e| e.contains("duplicate member") && e.contains("a")),
        "{errs:?}"
    );
}

// --- += ---

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

// --- Value and members coexistence ---

#[test]
fn value_and_members() {
    // x has both a value and members
    check_ok("x = \"a\"\nx += {\n    y = \"b\"\n}\nz = x\nw = x.y");
}

// --- Operators ---

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

// --- Compound tests ---

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
    // with must come before where in Donut syntax
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
    // Module body + with + dotted decl + += all contributing members
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
    check_ok(
        r#"
T = "t"
[p: T] m = {
    x = p
}
"#,
    );
}

#[test]
fn compound_param_visible_in_with() {
    // Params are visible in with clauses (with is part of the RHS)
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
    // Declared name is visible in its own where clause
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
    // Multiple names allowed in declaration-only form
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

[p: T] ext[q: U] = base.core.v
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

// --- Anonymous block tests ---

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
    check_ok(
        r#"
T = "t"
[p: T] {
    x = p
}
r = x
"#,
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
    // Top-to-bottom within the block
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

// --- Module output tests ---

fn names(m: &Module) -> Vec<&str> {
    m.entries.iter().map(|(n, _)| n.as_str()).collect()
}

#[test]
fn module_output_declaration_order() {
    let m = check_module(
        r#"
a = "1"
b = "2"
c = "3"
"#,
    );
    assert_eq!(names(&m), vec!["a", "b", "c"]);
}

#[test]
fn module_output_item_kind() {
    let m = check_module(
        r#"
T = "t"
x: T
y = "v"
z := "w"
"#,
    );
    assert!(matches!(m.get("T").unwrap().kind, Some(ItemKind::Alias)));
    assert!(matches!(m.get("x").unwrap().kind, Some(ItemKind::Decl)));
    assert!(matches!(m.get("y").unwrap().kind, Some(ItemKind::Alias)));
    assert!(matches!(m.get("z").unwrap().kind, Some(ItemKind::Def)));
}

/// Extract the single path name from a Val (e.g. Path with one segment "T" → "T")
fn val_as_path_name(val: &S<Val>) -> Option<&str> {
    match &val.0 {
        Val::Path(path) => {
            if path.segments.len() == 1 {
                Some(&path.segments[0].0.name)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Extract the string literal value from a Val
fn val_as_string(val: &S<Val>) -> Option<&str> {
    match &val.0 {
        Val::Lit(Lit::String(s)) => Some(s),
        _ => None,
    }
}

#[test]
fn module_output_val_content() {
    let m = check_module(
        r#"
x = "hello"
y = "world"
"#,
    );
    assert_eq!(val_as_string(m.get("x").unwrap().val().unwrap()), Some("\"hello\""));
    assert_eq!(val_as_string(m.get("y").unwrap().val().unwrap()), Some("\"world\""));
}

#[test]
fn module_output_val_path_content() {
    let m = check_module(
        r#"
a = "v"
b = a
"#,
    );
    assert_eq!(val_as_path_name(m.get("b").unwrap().val().unwrap()), Some("a"));
}

#[test]
fn module_output_ty_content() {
    let m = check_module(
        r#"
T = "t"
U = "u"
x: T
y: U = "v"
"#,
    );
    assert_eq!(val_as_path_name(m.get("x").unwrap().ty.as_ref().unwrap()), Some("T"));
    assert_eq!(val_as_path_name(m.get("y").unwrap().ty.as_ref().unwrap()), Some("U"));
    assert!(m.get("T").unwrap().ty.is_none());
}

#[test]
fn module_output_param_ty_content() {
    let m = check_module(
        r#"
T = "t"
U = "u"
f[x: T, y: U] = "v"
"#,
    );
    let f = m.get("f").unwrap();
    assert_eq!(f.params.len(), 2);
    assert_eq!(f.params[0].name, "x");
    assert_eq!(val_as_path_name_from_ty(&f.params[0].ty), Some("T"));
    assert_eq!(f.params[1].name, "y");
    assert_eq!(val_as_path_name_from_ty(&f.params[1].ty), Some("U"));
}

fn val_as_path_name_from_ty(val: &S<Val>) -> Option<&str> {
    val_as_path_name(val)
}

#[test]
fn module_output_nested_val_content() {
    let m = check_module(
        r#"
m = {
    x = "inner"
}
"#,
    );
    let x = m.get("m").unwrap().members().unwrap().get("x").unwrap();
    assert_eq!(val_as_string(x.val().unwrap()), Some("\"inner\""));
}

#[test]
fn module_output_where_not_in_members() {
    // where clause bindings should NOT appear in the item's members
    let m = check_module(
        r#"
x = g
  where {
    g = "val"
  }
"#,
    );
    let x = m.get("x").unwrap();
    assert!(x.members().unwrap().get("g").is_none(), "where binding should not be a member");
    assert_eq!(val_as_path_name(x.val().unwrap()), Some("g"));
}

#[test]
fn module_output_with_val_and_members() {
    let m = check_module(
        r#"
x = "base"
  with {
    a = "m1"
    b = "m2"
  }
"#,
    );
    let x = m.get("x").unwrap();
    assert_eq!(val_as_string(x.val().unwrap()), Some("\"base\""));
    assert_eq!(val_as_string(x.members().unwrap().get("a").unwrap().val().unwrap()), Some("\"m1\""));
    assert_eq!(val_as_string(x.members().unwrap().get("b").unwrap().val().unwrap()), Some("\"m2\""));
}

#[test]
fn module_output_deco_param_not_in_module() {
    // Decorator params should not appear in the module output
    let m = check_module(
        r#"
T = "t"
[p: T] f = "v"
"#,
    );
    assert!(m.get("p").is_none(), "deco param should not be in module");
    assert!(m.get("f").is_some());
}

#[test]
fn module_output_module_with_params() {
    let m = check_module(
        r#"
T = "t"
m[x: T] = {
    inner = x
}
"#,
    );
    let item = m.get("m").unwrap();
    assert_eq!(item.params.len(), 1);
    assert_eq!(item.params[0].name, "x");
    assert_eq!(val_as_path_name_from_ty(&item.params[0].ty), Some("T"));
    assert!(item.val().is_none(), "module body should not produce val");
    assert_eq!(names(item.members().unwrap()), vec!["inner"]);
}

#[test]
fn module_output_nested_members_order() {
    let m = check_module(
        r#"
m = {
    x = "1"
    y = "2"
    z = "3"
}
"#,
    );
    let members = m.get("m").unwrap().members().unwrap();
    assert_eq!(names(members), vec!["x", "y", "z"]);
}

#[test]
fn module_output_with_members_order() {
    let m = check_module(
        r#"
x = "v"
  with {
    a = "1"
    b = "2"
  }
"#,
    );
    let members = m.get("x").unwrap().members().unwrap();
    assert_eq!(names(members), vec!["a", "b"]);
}

#[test]
fn module_output_dotted_decl_preserves_order() {
    let m = check_module(
        r#"
m = {
    a = "1"
}
m.b = "2"
m.c = "3"
"#,
    );
    let members = m.get("m").unwrap().members().unwrap();
    assert_eq!(names(members), vec!["a", "b", "c"]);
}

#[test]
fn module_output_add_preserves_order() {
    let m = check_module(
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
    let members = m.get("x").unwrap().members().unwrap();
    assert_eq!(names(members), vec!["a", "b", "c"]);
}

#[test]
fn module_output_anon_block_order() {
    let m = check_module(
        r#"
a = "1"
{
    b = "2"
    c = "3"
}
d = "4"
"#,
    );
    assert_eq!(names(&m), vec!["a", "b", "c", "d"]);
}

#[test]
fn module_output_deep_nesting() {
    let m = check_module(
        r#"
a = {
    b = {
        c = "leaf"
        d = "leaf2"
    }
}
"#,
    );
    let b = m.get("a").unwrap().members().unwrap().get("b").unwrap();
    assert_eq!(names(b.members().unwrap()), vec!["c", "d"]);
}

// --- Functor application constraints ---

#[test]
fn functor_app_alias_ok() {
    // f must be declared with ~> type to be a functor
    check_ok("A = \"a\"\nB = \"b\"\nf: A ~> B\na = \"y\"\nf(a) = \"z\"");
}

#[test]
fn functor_app_decl_error() {
    let errs = check_errs("A = \"a\"\nB = \"b\"\nf: A ~> B\na = \"y\"\nf(a): \"z\"");
    assert!(
        errs.iter().any(|e| e.contains("functor application")),
        "expected functor application error: {errs:?}"
    );
}

#[test]
fn functor_app_module_error() {
    let errs = check_errs("A = \"a\"\nB = \"b\"\nf: A ~> B\na = \"y\"\nf(a) = {\n    x = \"v\"\n}");
    assert!(
        errs.iter().any(|e| e.contains("functor application")),
        "expected functor application error: {errs:?}"
    );
}

#[test]
fn functor_app_with_error() {
    let errs = check_errs("A = \"a\"\nB = \"b\"\nf: A ~> B\na = \"y\"\nf(a) = \"z\" with {\n    w = \"v\"\n}");
    assert!(
        errs.iter().any(|e| e.contains("functor application")),
        "expected functor application error: {errs:?}"
    );
}

#[test]
fn functor_app_not_a_functor() {
    let errs = check_errs("f = \"x\"\na = \"y\"\nf(a) = \"z\"");
    assert!(
        errs.iter().any(|e| e.contains("is not a functor")),
        "expected 'not a functor' error: {errs:?}"
    );
}

#[test]
fn functor_app_mappings_stored() {
    let m = check_module("A = \"a\"\nB = \"b\"\na = \"x\"\nb = \"y\"\nf: A ~> B\nf(a) = \"x\"\nf(b) = \"y\"");
    let f = m.get("f").unwrap();
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
        "expected undefined functor error: {errs:?}"
    );
}
