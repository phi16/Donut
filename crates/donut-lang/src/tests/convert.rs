use crate::convert::convert;
use crate::parse::parse;
use crate::tokenize::tokenize;
use crate::types::common::S;
use crate::types::semtree;

// --- Minimal pretty printer for semtree ---

struct PP(String);
impl PP {
    fn s(&mut self, s: &str) { self.0.push_str(s); }
}

trait Fmt { fn fmt(&self, p: &mut PP); }

impl<T: Fmt> Fmt for S<T> {
    fn fmt(&self, p: &mut PP) {
        self.0.fmt(p);
    }
}
impl Fmt for semtree::Name {
    fn fmt(&self, p: &mut PP) { p.s(&self.0); }
}
impl Fmt for String {
    fn fmt(&self, p: &mut PP) { p.s(self); }
}
impl Fmt for semtree::Op {
    fn fmt(&self, p: &mut PP) {
        match self {
            semtree::Op::Comp(0) => p.s(" "),
            semtree::Op::Comp(n) => { p.s(" "); p.s(&";".repeat(*n as usize)); p.s(" "); }
            semtree::Op::CompStar => p.s(" ;* "),
            semtree::Op::Arrow(semtree::ArrowTy::To) => p.s(" → "),
            semtree::Op::Arrow(semtree::ArrowTy::Eq) => p.s(" ~ "),
            semtree::Op::Arrow(semtree::ArrowTy::Functor) => p.s(" ~> "),
        }
    }
}
impl Fmt for semtree::ParamDecl {
    fn fmt(&self, p: &mut PP) { self.name.fmt(p); p.s(": "); self.ty.fmt(p); }
}
impl Fmt for semtree::ParamVal {
    fn fmt(&self, p: &mut PP) {
        if let Some(n) = &self.name { n.fmt(p); p.s(" = "); }
        self.val.fmt(p);
    }
}
impl<T: Fmt> Fmt for semtree::Params<T> {
    fn fmt(&self, p: &mut PP) {
        if self.0.is_empty() { return; }
        p.s("[");
        for (i, x) in self.0.iter().enumerate() { if i > 0 { p.s(", "); } x.fmt(p); }
        p.s("]");
    }
}
impl<P: Fmt> Fmt for semtree::Segment<P> {
    fn fmt(&self, p: &mut PP) { self.0.fmt(p); self.1.fmt(p); }
}
impl<P> Fmt for semtree::Path<P> where semtree::Segment<P>: Fmt {
    fn fmt(&self, p: &mut PP) {
        for (i, s) in self.0.iter().enumerate() { if i > 0 { p.s("."); } s.fmt(p); }
        if let Some(v) = &self.1 { p.s("("); v.fmt(p); p.s(")"); }
    }
}
impl Fmt for semtree::Key {
    fn fmt(&self, p: &mut PP) {
        match self { semtree::Key::Name(n) => n.fmt(p), semtree::Key::String(s) => s.fmt(p) }
    }
}
impl Fmt for semtree::Lit {
    fn fmt(&self, p: &mut PP) {
        match self {
            semtree::Lit::Number(n) => p.s(n),
            semtree::Lit::String(s) => p.s(s),
            semtree::Lit::Array(vs) => {
                p.s("[");
                for (i, v) in vs.iter().enumerate() { if i > 0 { p.s(", "); } v.fmt(p); }
                p.s("]");
            }
            semtree::Lit::Object(kvs) => {
                p.s("{");
                for (i, (k, v)) in kvs.iter().enumerate() {
                    if i > 0 { p.s(", "); }
                    k.fmt(p); p.s(": "); v.fmt(p);
                }
                p.s("}");
            }
        }
    }
}
impl Fmt for semtree::Val {
    fn fmt(&self, p: &mut PP) {
        match self {
            semtree::Val::Path(path) => path.fmt(p),
            semtree::Val::Lit(lit) => lit.fmt(p),
            semtree::Val::Op(l, op, params, r) => {
                p.s("(");
                l.0.fmt(p);
                op.fmt(p);
                if let Some(pv) = params { pv.fmt(p); }
                r.0.fmt(p);
                p.s(")");
            }
            semtree::Val::Any => p.s("?"),
        }
    }
}
impl Fmt for semtree::AssignOp {
    fn fmt(&self, p: &mut PP) {
        match self {
            semtree::AssignOp::Decl => p.s(": "),
            semtree::AssignOp::Alias => p.s(" = "),
            semtree::AssignOp::Def => p.s(" := "),
            semtree::AssignOp::Add => p.s(" += "),
        }
    }
}
impl Fmt for semtree::Module {
    fn fmt(&self, p: &mut PP) {
        match self {
            semtree::Module::Block(ds) => {
                p.s("{ ");
                for (i, d) in ds.iter().enumerate() { if i > 0 { p.s("; "); } d.fmt(p); }
                p.s(" }");
            }
            semtree::Module::Import(l) => { p.s("import "); l.fmt(p); }
        }
    }
}
impl Fmt for semtree::Decorator {
    fn fmt(&self, p: &mut PP) {
        match self {
            semtree::Decorator::Param(n, v) => {
                p.s("["); n.fmt(p); p.s(": "); v.fmt(p); p.s("]");
            }
            semtree::Decorator::Deco(v) => {
                p.s("["); v.fmt(p); p.s("]");
            }
        }
    }
}
impl Fmt for semtree::DeclUnit {
    fn fmt(&self, p: &mut PP) {
        for (i, n) in self.names.iter().enumerate() { if i > 0 { p.s(" "); } n.fmt(p); }
        if let Some(ty) = &self.ty { p.s(": "); ty.fmt(p); }
        if let Some(body) = &self.body {
            self.op.fmt(p);
            match body {
                semtree::ValMod::Val(v) => v.fmt(p),
                semtree::ValMod::Mod(m) => m.fmt(p),
            }
        }
    }
}
impl Fmt for semtree::Decl {
    fn fmt(&self, p: &mut PP) {
        for d in &self.decos { d.fmt(p); p.s(" "); }
        match &self.main {
            semtree::DeclMain::Unit(u) => u.fmt(p),
            semtree::DeclMain::Mod(m) => m.fmt(p),
        }
        for w in &self.with_clauses { p.s(" with "); w.fmt(p); }
        for w in &self.where_clauses { p.s(" where "); w.fmt(p); }
    }
}
impl Fmt for semtree::Program {
    fn fmt(&self, p: &mut PP) {
        for d in &self.0 { d.fmt(p); p.s("\n"); }
    }
}

fn pretty_sem(prog: &semtree::Program) -> String {
    let mut p = PP(String::new());
    prog.fmt(&mut p);
    p.0
}

// --- Test helpers ---

fn c(code: &str) -> String {
    let (tokens, _, tok_errors) = tokenize(code.trim());
    assert!(tok_errors.is_empty(), "tokenize errors: {tok_errors:?}");
    let (program, parse_errors) = parse(&tokens);
    assert!(parse_errors.is_empty(), "parse errors: {parse_errors:?}");
    let (result, conv_errors) = convert(program, &tokens);
    assert!(conv_errors.is_empty(), "convert errors: {conv_errors:?}");
    pretty_sem(&result)
}

fn conv_errs(code: &str) -> Vec<String> {
    let (tokens, _, _) = tokenize(code.trim());
    let (program, _) = parse(&tokens);
    let (_, errors) = convert(program, &tokens);
    errors.into_iter().map(|(_, msg)| msg).collect()
}

fn conv_ok(code: &str) {
    let errs = conv_errs(code);
    assert!(errs.is_empty(), "unexpected convert errors: {errs:?}");
}

// --- Tests ---

#[test]
fn basic_decl() {
    assert_eq!(c("x = a"), "x = a\n");
    assert_eq!(c("x: T = a"), "x: T = a\n");
    assert_eq!(c("x := a"), "x := a\n");
    assert_eq!(c("x += a"), "x += a\n");
}

#[test]
fn decl_only() {
    assert_eq!(c("x: T"), "x: T\n");
    assert_eq!(c("f[a: A]: T"), "f[a: A]: T\n");
}

#[test]
fn val_single_op() {
    assert_eq!(c("x = a ; b"), "x = (a ; b)\n");
    assert_eq!(c("x = a b"), "x = (a b)\n");
}

#[test]
fn val_precedence() {
    // Comp(1) is stronger than Comp(2)
    assert_eq!(c("x = a ; b ;; c"), "x = ((a ; b) ;; c)\n");
    // Comp(0) is strongest
    assert_eq!(c("x = a b ;; c"), "x = ((a b) ;; c)\n");
    // Stronger op on right side
    assert_eq!(c("x = a ;; b ; c"), "x = (a ;; (b ; c))\n");
}

#[test]
fn val_comp_lit() {
    // ;N is CompLit(N), same precedence as CompRep of same level
    assert_eq!(c("x = a ;2 b"), "x = (a ;; b)\n");
    assert_eq!(c("x = a ;0 b"), "x = (a b)\n");
    // CompLit interacts with other ops
    assert_eq!(c("x = a ;2 b ; c"), "x = (a ;; (b ; c))\n");
    assert_eq!(c("x = a ; b ;2 c"), "x = ((a ; b) ;; c)\n");
}

#[test]
fn val_left_assoc() {
    assert_eq!(c("x = a ; b ; c"), "x = ((a ; b) ; c)\n");
    assert_eq!(c("x = a b c"), "x = ((a b) c)\n");
    assert_eq!(c("x = a ;; b ;; c"), "x = ((a ;; b) ;; c)\n");
}

#[test]
fn val_arrow() {
    assert_eq!(c("x = a → b"), "x = (a → b)\n");
    assert_eq!(c("x = a ~ b"), "x = (a ~ b)\n");
    assert_eq!(c("x = a ~> b"), "x = (a ~> b)\n");
    // Arrow is weakest
    assert_eq!(c("x = a ; b → c"), "x = ((a ; b) → c)\n");
    assert_eq!(c("x = a → b ; c"), "x = (a → (b ; c))\n");
}

#[test]
fn val_comp_star() {
    assert_eq!(c("x = a ;* b"), "x = (a ;* b)\n");
    assert_eq!(c("x = a ; b ;* c"), "x = ((a ; b) ;* c)\n");
}

#[test]
fn val_paren() {
    // Parens override precedence
    assert_eq!(c("x = (a ;; b) ; c"), "x = ((a ;; b) ; c)\n");
    assert_eq!(c("x = a ; (b ;; c)"), "x = (a ; (b ;; c))\n");
}

#[test]
fn val_literals() {
    assert_eq!(c("x = 42"), "x = 42\n");
    assert_eq!(c("x = \"hello\""), "x = \"hello\"\n");
    assert_eq!(c("x = [1, 2, 3]"), "x = [1, 2, 3]\n");
    // {a: 1} is parsed as a module block, not an object literal
}

#[test]
fn number_literal_detection() {
    // Standalone digits → number literal
    assert_eq!(c("x = 0"), "x = 0\n");
    assert_eq!(c("x = 123"), "x = 123\n");
    // Negative number
    assert_eq!(c("x = -42"), "x = -42\n");
    // Not a number: digit + non-digit → stays as path
    assert_eq!(c("x = 2D"), "x = 2D\n"); // path, not literal
    // Dotted number path: 12.34 → path (like Rust's x.0)
    assert_eq!(c("a.b = 12.34"), "a.b = 12.34\n");
    // Number with params → stays as path
    assert_eq!(c("x = 42[a]"), "x = 42[a]\n");
}

#[test]
fn path_with_val_params() {
    assert_eq!(c("x = f[a]"), "x = f[a]\n");
    assert_eq!(c("x = f[a, b]"), "x = f[a, b]\n");
}

#[test]
fn path_with_named_params() {
    assert_eq!(c("x = f[n = a]"), "x = f[n = a]\n");
}

#[test]
fn decl_params() {
    assert_eq!(c("f[x: T] = a"), "f[x: T] = a\n");
    assert_eq!(c("f[x: T, y: U] = a"), "f[x: T, y: U] = a\n");
}

#[test]
fn decl_params_fan_out() {
    // Multiple names with same type get fanned out
    assert_eq!(c("f[x y: T] = a"), "f[x: T, y: T] = a\n");
}

#[test]
fn dotted_path() {
    assert_eq!(c("x = a.b.c"), "x = a.b.c\n");
    assert_eq!(c("a.b = c"), "a.b = c\n");
}

#[test]
fn decorators() {
    assert_eq!(c("[v] x = a"), "[v] x = a\n");
    assert_eq!(c("[n: T] x = a"), "[n: T] x = a\n");
}

#[test]
fn module_block() {
    assert_eq!(c("x = {\n    a = b\n}"), "x = { a = b }\n");
}

#[test]
fn clauses() {
    assert_eq!(
        c("x = a\n  with {\n    y = b\n  }"),
        "x = a with { y = b }\n"
    );
    assert_eq!(
        c("x = a\n  where {\n    y = b\n  }"),
        "x = a where { y = b }\n"
    );
    assert_eq!(
        c("x = a\n  with {\n    y = b\n  }\n  where {\n    z = c\n  }"),
        "x = a with { y = b } where { z = c }\n"
    );
}

// --- Composite tests ---

#[test]
fn kitchen_sink() {
    let code = "\
[p: T]
[deco] f[x y: A, z: B]: R = (a ; b ;; c) -> d e
  with {
    g = h ;* i
  }
  where {
    j = k[n = 1]
  }
a.b.c += [2, \"hello\"]
s := 42";
    assert_eq!(
        c(code),
        "\
[p: T] [deco] f[x: A, y: A, z: B]: R = (((a ; b) ;; c) → (d e)) with { g = (h ;* i) } where { j = k[n = 1] }
a.b.c += [2, \"hello\"]
s := 42
"
    );
}

#[test]
fn nested_module() {
    let code = "\
m = {
  x := y ; z
  [q: U] w = v
}";
    assert_eq!(c(code), "m = { x := (y ; z); [q: U] w = v }\n");
}

#[test]
fn compound_errors() {
    let code = "\
[n = v]
x = a -> b -> c
  where {
    y = 1
  }
  with {
    z = 2
  }";
    let errs = conv_errs(code);
    assert!(errs.iter().any(|e| e.contains("named")), "expected named param error: {errs:?}");
    assert!(errs.iter().any(|e| e.contains("chain")), "expected chained arrow error: {errs:?}");
    assert!(errs.iter().any(|e| e.contains("with")), "expected with/where order error: {errs:?}");
}

// --- Error cases ---

#[test]
fn error_chained_arrows() {
    let errs = conv_errs("x = a → b → c");
    assert_eq!(errs, vec!["cannot chain `*` or arrow operators"]);
}

#[test]
fn error_chained_star_arrow() {
    let errs = conv_errs("x = a ;* b → c");
    assert_eq!(errs, vec!["cannot chain `*` or arrow operators"]);
}

#[test]
fn error_named_in_decorator() {
    // `=` in params produces ParamTy::Named, which is invalid in decorator context
    let errs = conv_errs("[n = v] x = a");
    assert!(errs.iter().any(|e| e.contains("named")), "expected named param error, got: {errs:?}");
}

#[test]
fn error_where_after_with() {
    let errs = conv_errs(
        "x = a\n  where {\n    y = b\n  }\n  with {\n    z = c\n  }"
    );
    assert_eq!(errs, vec!["`with` clause must come before `where` clause"]);
}

#[test]
fn error_where_non_alias() {
    let errs = conv_errs("x = 1\n  where {\n    y := 2\n  }");
    assert!(errs.iter().any(|e| e.contains("alias") || e.contains("`=`")), "expected alias-only error: {errs:?}");
}

#[test]
fn error_where_add() {
    let errs = conv_errs("x = 1\n  where {\n    y += 2\n  }");
    assert!(errs.iter().any(|e| e.contains("alias") || e.contains("`=`")), "expected alias-only error: {errs:?}");
}

#[test]
fn error_where_decl_only() {
    // Pure declaration (x: T) is not allowed in where clause
    let errs = conv_errs("T = 1\nx = 1\n  where {\n    y: T\n  }");
    assert!(errs.iter().any(|e| e.contains("alias") || e.contains("`=`")), "expected alias-only error: {errs:?}");
}

#[test]
fn error_where_nested_module_def() {
    // := inside a module inside where should also be rejected
    let errs = conv_errs(r#"
x = 1
  where {
    m = {
        y := 2
    }
  }
"#);
    assert!(errs.iter().any(|e| e.contains("alias") || e.contains("`=`")), "expected alias-only error in nested module: {errs:?}");
}

#[test]
fn where_alias_ok() {
    conv_ok("x = g\n  where {\n    g = 1\n  }");
}

#[test]
fn error_multi_name_with_assign() {
    let errs = conv_errs("a b = 1");
    assert!(
        errs.iter().any(|e| e.contains("multiple names") && e.contains("declaration-only")),
        "{errs:?}"
    );
}

#[test]
fn multi_name_decl_only_ok() {
    conv_ok("a b: T");
}
