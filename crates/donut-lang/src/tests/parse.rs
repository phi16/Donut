use crate::parse::parse;
use crate::pretty::pretty;
use crate::tokenize::tokenize;

fn p(code: &str) -> String {
    let (tokens, _, errors) = tokenize(code.trim());
    assert!(errors.is_empty(), "tokenize errors: {errors:?}");
    let (result, errors) = parse(&tokens);
    assert!(errors.is_empty(), "parse errors: {errors:?}");
    pretty(&result)
}

fn parse_result(code: &str) -> (String, Vec<String>) {
    let (tokens, _, _) = tokenize(code.trim());
    let (result, errors) = parse(&tokens);
    (
        pretty(&result),
        errors.into_iter().map(|(_, msg)| msg).collect(),
    )
}

fn errs(code: &str) -> Vec<String> {
    let (tokens, _, _) = tokenize(code.trim());
    let (_, errors) = parse(&tokens);
    errors.into_iter().map(|(_, msg)| msg).collect()
}

#[test]
fn decl_dots() {
    assert_eq!(p("..."), "...\n");
    assert_eq!(p("x = {\n    ...\n}"), "x = {\n    ...\n}\n");
    assert_eq!(p("[A] ..."), "[A] ...\n");
    assert_eq!(p("...\nx = y"), "...\nx = y\n");
}

#[test]
fn decl_forms() {
    assert_eq!(p("x"), "x\n");
    assert_eq!(p("x: A"), "x: A\n");
    assert_eq!(p("x = y"), "x = y\n");
    assert_eq!(p("x: A = y"), "x: A = y\n");
    assert_eq!(p("x: A := y"), "x: A := y\n");
    assert_eq!(p("x += y"), "x += y\n");
    assert_eq!(p("x += { a = b }"), "x += {\n    a = b\n}\n");
    assert_eq!(p("x y z: A = b"), "x y z: A = b\n");
    assert_eq!(p("a b c: T"), "a b c: T\n");
    assert_eq!(p("x.y = z"), "x.y = z\n");
}

#[test]
fn value_forms() {
    assert_eq!(p("x = a b c"), "x = a b c\n");
    assert_eq!(p("x = a ; b"), "x = a; b\n");
    assert_eq!(p("x = a ;; b"), "x = a;; b\n");
    assert_eq!(p("x = a ;;; b"), "x = a;;; b\n");
    assert_eq!(p("x = a ;* b"), "x = a;* b\n");
    assert_eq!(p("x = a ;2 b"), "x = a;2 b\n");
    assert_eq!(p("x = a ;0 b"), "x = a;0 b\n");
    assert_eq!(p("x = a ;10 b"), "x = a;10 b\n");
    assert_eq!(p("x: A → B"), "x: A → B\n");
    assert_eq!(p("x: A -> B"), "x: A → B\n"); // ASCII -> is the same as Unicode →
    assert_eq!(p("x: A → B → C"), "x: A → B → C\n");
    assert_eq!(p("x: A ~ B"), "x: A ~ B\n");
    assert_eq!(p("x: A ~> B"), "x: A ~> B\n");
    assert_eq!(p("x = (a ; b)"), "x = (a; b)\n");
    assert_eq!(p("x = ..."), "x = ...\n");
}

#[test]
fn path_and_params() {
    assert_eq!(p("x = a.b.c"), "x = a.b.c\n");
    assert_eq!(p("x = a.b.c.d"), "x = a.b.c.d\n");
    assert_eq!(p("x = f[a]"), "x = f[a]\n");
    assert_eq!(p("x = f[a, b: c, d = e]"), "x = f[a, b: c, d = e]\n");
    assert_eq!(p("x = f[a].g[b]"), "x = f[a].g[b]\n");
    assert_eq!(p("x = f(a)"), "x = f(a)\n"); // connected → functor application
    assert_eq!(p("x = f (a)"), "x = f (a)\n"); // space → juxtaposition
    assert_eq!(p("x = a.b(c)"), "x = a.b(c)\n");
}

#[test]
fn literals() {
    assert_eq!(p(r#"x = "hello""#), "x = \"hello\"\n");
    assert_eq!(p("x = 42"), "x = 42\n");
    assert_eq!(p("x = [a, b]"), "x = [a, b]\n");
    assert_eq!(p("x = {a: b}"), "x = {\n    a: b\n}\n");
}

#[test]
fn module_forms() {
    assert_eq!(p("x = {}"), "x = {\n}\n");
    assert_eq!(p("x = { a = b }"), "x = {\n    a = b\n}\n");
    assert_eq!(
        p("x = {\n    a = b\n    c = d\n}"),
        "x = {\n    a = b\n    c = d\n}\n"
    );
    // anonymous module inside a block is parsed as its own decl
    assert_eq!(
        p("x = {\n    {}\n    a = b\n}"),
        "x = {\n    {\n    }\n    a = b\n}\n"
    );
    assert_eq!(p(r#"import "foo""#), "import \"foo\"\n");
    assert_eq!(p(r#"import "foo/bar/baz""#), "import \"foo/bar/baz\"\n");
}

#[test]
fn clauses() {
    // inline (all at indent=0): works
    assert_eq!(p("x = y with { a = b }"), "x = y with {\n    a = b\n}\n");
    assert_eq!(p("x = y where { a = b }"), "x = y where {\n    a = b\n}\n");
    // multi-line with: see indent_with_clause_* tests below
}

#[test]
fn decorators() {
    assert_eq!(p("[A] x = y"), "[A] x = y\n");
    assert_eq!(p("[A, B] x = y"), "[A, B] x = y\n");
    assert_eq!(p("[style[...]] y: *"), "[style[...]] y: *\n");
    assert_eq!(p("[f[a, b]] x = y"), "[f[a, b]] x = y\n");
    assert_eq!(p("[a: T, b = v] x: U"), "[a: T, b = v] x: U\n");
    // decorator alone on its own line → decorator-only decl; next line is independent
    assert_eq!(p("[A]\nx = y"), "[A]\nx = y\n");
    assert_eq!(p("[A]\n[B]\n[C]"), "[A]\n[B]\n[C]\n");
}

#[test]
fn multiple_decls() {
    assert_eq!(p("x = y\nz = w"), "x = y\nz = w\n");
    assert_eq!(p("x = y\n[A]\nz = w"), "x = y\n[A]\nz = w\n");
}

// ================================================================
// カンマ区切り宣言のエッジケース
// ================================================================

#[test]
fn comma_trailing_eoi() {
    // 末尾カンマで EOI → エラーなしで x だけ
    assert_eq!(p("x,"), "x\n");
}

#[test]
fn comma_then_anon_module() {
    // カンマの後に匿名モジュール
    assert_eq!(p("x, { a = b }"), "x\n{\n    a = b\n}\n");
}

#[test]
fn comma_after_module_rhs() {
    // モジュール代入の後にカンマ
    assert_eq!(p("x = { a = b }, y"), "x = {\n    a = b\n}\ny\n");
}

#[test]
fn comma_after_with_clause() {
    // with/where 節の後にカンマ
    assert_eq!(
        p("x = y with { a = b }, z"),
        "x = y with {\n    a = b\n}\nz\n"
    );
    assert_eq!(
        p("x = y where { a = b }, z"),
        "x = y where {\n    a = b\n}\nz\n"
    );
}

#[test]
fn comma_trailing_in_block() {
    // ブロック内末尾カンマ
    assert_eq!(p("x = { a, }"), "x = {\n    a\n}\n");
}

#[test]
fn comma_decorated_decls() {
    // デコレータ付き宣言のカンマ区切り
    assert_eq!(p("[A] x, [B] y"), "[A] x\n[B] y\n");
}

#[test]
fn comma_block_multiline_same_indent() {
    // ブロック内カンマ区切り、続きが同じ indent の次行
    assert_eq!(
        p("x = {\n  a = 1,\n  b = 2\n}"),
        "x = {\n    a = 1\n    b = 2\n}\n"
    );
}

#[test]
fn comma_block_shallower_indent_after_comma() {
    // ブロック内でカンマの後、b が a より浅い indent になっている
    // a (indent=4) → カンマ → b (indent=2): self.indent が 4 のまま残り
    // b の next_indent=2 < last_indent=4 となってブロック内に入れなくなる
    let (result, errors) = parse_result("x = {\n    a,\n  b\n}");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert_eq!(result, "x = {\n    a\n    b\n}\n");
}

#[test]
fn comma_separated_decls() {
    // 1行に複数の宣言をカンマで区切って並べられる
    assert_eq!(p("x, y"), "x\ny\n");
    assert_eq!(p("x: A, y: B"), "x: A\ny: B\n");
    assert_eq!(p("x, y, z"), "x\ny\nz\n");
    // カンマの後に改行しても続く
    assert_eq!(p("x,\ny"), "x\ny\n");
    // ブロック内でもカンマ区切りが使える
    assert_eq!(p("x = { a, b }"), "x = {\n    a\n    b\n}\n");
    // カンマ区切りと通常の改行区切りの混在
    assert_eq!(p("a, b\nc, d"), "a\nb\nc\nd\n");
}

#[test]
fn error_unexpected_at_eoi() {
    // `.` is consumed by consume_indent now; "unexpected" is more informative
    assert_eq!(errs("."), vec!["unexpected '.'"]);
}

#[test]
fn error_unclosed_bracket() {
    assert!(errs("[x").iter().any(|e| e.contains("']'")));
}

// ================================================================
// Indent behavior tests
// Principles:
//   1. Errors are isolated per decl — next decl is not affected
//   2. A decl sees only tokens at strictly deeper indent (col > indent)
//      OR closing brackets at same-or-deeper indent (col >= indent)
// ================================================================

/// Values can span indented continuation lines.
#[test]
fn indent_value_continues_deeper() {
    assert_eq!(p("x = y\n  z"), "x = y z\n"); // z at col=2 > indent=0 → juxtaposition
    assert_eq!(p("x = y\nz"), "x = y\nz\n"); // z at col=0 = indent=0 → new decl
}

/// Closing brackets are visible at same indent via eat_close (col >= indent).
#[test]
fn indent_close_bracket_same_level() {
    assert_eq!(p("[a\n]"), "[a]\n"); // ] at col=0, same as [ at col=0
    assert_eq!(p("[a,\n  b\n]"), "[a, b]\n"); // b at col=2, ] at col=0
    assert_eq!(p("x = {\n  a = b\n}"), "x = {\n    a = b\n}\n"); // } at col=0
}

/// Nested blocks at proper indentation levels.
#[test]
fn indent_nested_blocks() {
    assert_eq!(
        p("x = {\n    a = b\n    c = d\n}"),
        "x = {\n    a = b\n    c = d\n}\n"
    );
    assert_eq!(
        p("x = {\n    a = {\n        b = c\n    }\n}"),
        "x = {\n    a = {\n        b = c\n    }\n}\n"
    );
}

#[test]
fn indent_with_clause_inline() {
    assert_eq!(p("x = y with { a = b }"), "x = y with {\n    a = b\n}\n");
    // `with` at deeper indent — consume_indent now stops before closing brackets
    assert_eq!(p("x = y\n  with { a = b }"), "x = y with {\n    a = b\n}\n");
}

#[test]
fn indent_with_clause_multiline_body() {
    // } on its own line — consume_indent now stops before closing brackets
    assert_eq!(
        p("x = y\n  with {\n    a = b\n  }"),
        "x = y with {\n    a = b\n}\n"
    );
}

#[test]
fn indent_with_clause_same_indent() {
    // `with` at same indent as decl is NOT a clause — only deeper tokens continue a decl
    // (closing brackets are an exception: they can appear at the same indent)
    let (result, errors) = parse_result("x = y\nwith { a = b }");
    assert!(result.contains("x = y"));
    assert!(!errors.is_empty()); // `with` not recognized as clause at col=0
}

/// Unrecognized token inside a block is consumed, and subsequent decls still parse.
/// This works because consume_indent fires (self.indent=4 != last_indent=0) and
/// peek() sees `.` via decl_head=true (nothing called next() yet), so it's consumed.
#[test]
fn error_recovery_garbage_in_block() {
    let (result, errors) = parse_result("x = {\n    .\n    a = b\n}");
    assert_eq!(errors, vec!["unexpected '.'"]);
    assert!(result.contains("a = b")); // a = b IS parsed — recovery works here
}

#[test]
fn error_recovery_garbage_at_indent0() {
    // consume_indent now fires even at indent=0 (peek() sees non-closing token)
    let (result, errors) = parse_result(".\nx = y");
    assert_eq!(errors, vec!["unexpected '.'"]);
    assert!(result.contains("x = y")); // x = y IS parsed after recovery
}

#[test]
fn error_recovery_value_error_at_indent0() {
    // leftover `.` after failed val is consumed by trailing consume_indent
    let (result, errors) = parse_result("x = .\ny = z");
    assert!(!errors.is_empty());
    assert!(result.contains("y = z")); // y = z IS parsed after recovery
}

/// Integration test: a program using many grammar features in an n-category context.
#[test]
fn integration_n_category() {
    // --- main program (no errors) ---
    let code = r#"import "prelude"

*: *
Nat Bool: *
1: *

zero: 1 → Nat
succ: Nat → Nat
add: Nat Nat → Nat

[n: Nat → Nat] fixed: n → n

k[c: *, f g: c → c, a: f → g]: f → g = id[a]

F: Nat ~> Nat

bool: 1 → Bool
  with {
    true: 1 → bool
    false: 1 → bool
  }

bool += {
  not: bool → bool
}

comp: Nat → Nat = succ succ
  where {
    succ: Nat → Nat
  }"#;

    assert_eq!(
        p(code),
        r#"import "prelude"
*: *
Nat Bool: *
1: *
zero: 1 → Nat
succ: Nat → Nat
add: Nat Nat → Nat
[n: Nat → Nat] fixed: n → n
k[c: *, f g: c → c, a: f → g]: f → g = id[a]
F: Nat ~> Nat
bool: 1 → Bool with {
    true: 1 → bool
    false: 1 → bool
}
bool += {
    not: bool → bool
}
comp: Nat → Nat = succ succ where {
    succ: Nat → Nat
}
"#
    );

    // --- error recovery: garbage token between valid declarations ---
    let (result, errors) = parse_result("succ: Nat → Nat\n.\nzero: 1 → Nat");
    assert_eq!(errors, vec!["unexpected '.'"]);
    assert!(result.contains("succ: Nat → Nat"));
    assert!(result.contains("zero: 1 → Nat"));
}

#[test]
fn error_recovery_nested_bracket_fail_at_indent0() {
    // `[(.]`: separator recovery in separated() consumes `.` and closes `]`
    // both lines are parsed as decorator-only decls
    let (result, errors) = parse_result("[(.]\n[(.]");
    assert_eq!(result.matches("[(?)]").count(), 2); // both lines parsed
    assert!(!errors.iter().any(|e| e.contains("end of input")));
    assert!(!errors.is_empty()); // paren/bracket errors still present
}

#[test]
fn error_recovery_module_literal_after_name() {
    // `a {}`: `a` completes as a decl, then `{}` is consumed as garbage by
    // consume_indent (depth==0 so close brackets don't stop the loop).
    // `[.]` on the second line is reached.
    let (result, errors) = parse_result("a {}\n[.]");
    assert_eq!(result, "a\n[]\n");
    assert!(
        errors.iter().any(|e| e == "unexpected '{'"),
        "errors: {errors:?}"
    );
    assert!(
        !errors.iter().any(|e| e.contains("end of input")),
        "got eoi: {errors:?}"
    );
    assert!(result.contains("[]")); // second line IS reached
}

#[test]
fn error_recovery_unclosed_module() {
    // `a {` with no closing `}` — consume_indent consumes `{` and hits EOI.
    let (_, errors) = parse_result("a {\nb = c");
    assert!(!errors.is_empty());
    assert!(
        !errors.iter().any(|e| e.contains("end of input")),
        "got eoi: {errors:?}"
    );
}

#[test]
fn error_recovery_unclosed_bracket() {
    // `[x` with no closing `]`
    let (_, errors) = parse_result("[x\ny = z");
    assert!(!errors.is_empty());
    assert!(
        !errors.iter().any(|e| e.contains("end of input")),
        "got eoi: {errors:?}"
    );
}
