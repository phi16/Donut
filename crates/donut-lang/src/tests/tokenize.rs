use crate::tokenize::tokenize;
use crate::types::token::{Token, TokenTy};
use TokenTy::*;

fn tok(input: &str) -> Vec<Token<'_>> {
    let (tokens, _, errors) = tokenize(input);
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    tokens
}
fn tys(ts: &[Token]) -> Vec<TokenTy> {
    ts.iter().map(|t| t.ty.clone()).collect()
}
fn strs<'a>(ts: &[Token<'a>]) -> Vec<&'a str> {
    ts.iter().map(|t| t.str).collect()
}
fn connected(ts: &[Token]) -> Vec<bool> {
    ts.iter().map(|t| t.connected).collect()
}
fn indents(ts: &[Token]) -> Vec<usize> {
    ts.iter().map(|t| t.indent).collect()
}

#[test]
fn operator_detection() {
    // : is a separator so := fuses into Operator; x+=y stays one Name since = is not
    assert_eq!(tys(&tok("x:=y")), [Name, Operator, Name]);
    assert_eq!(tys(&tok("x+=y")), [Name]);
    assert_eq!(tys(&tok("x += y")), [Name, Operator, Name]);
    assert_eq!(tys(&tok("+= ==")), [Operator, Name]);
    assert_eq!(tys(&tok(":==")), [Operator, Operator]);
}

#[test]
fn dot_sequences() {
    let ts = tok(". .* .. ...");
    assert_eq!(tys(&ts), [Operator, Operator, Keyword, Keyword]);
    assert_eq!(strs(&ts), [".", ".*", "..", "..."]);
    // count > 1 disables *, so ..* → Keyword(..) + Name(*)
    assert_eq!(tys(&tok("..*")), [Keyword, Name]);
}

#[test]
fn semicolon_sequences() {
    let ts = tok("; ;* ;; ;;;");
    assert_eq!(tys(&ts), [Operator, Operator, Operator, Operator]);
    assert_eq!(strs(&ts), [";", ";*", ";;", ";;;"]);
    // count > 1 disables *, so ;;* → Operator(;;) + Name(*)
    assert_eq!(tys(&tok(";;*")), [Operator, Name]);
}

#[test]
fn colon_sequences() {
    // := fuses into Operator; lone : and :: are separate Symbols
    assert_eq!(tys(&tok(":= : ::")), [Operator, Symbol, Symbol, Symbol]);
}

#[test]
fn connected_flag() {
    // consecutive without space → connected
    assert_eq!(connected(&tok("f(x)")), [false, true, true, true]);
    // space before ( → not connected
    assert_eq!(connected(&tok("f (x)")), [false, false, true, true]);
    // across line breaks → not connected
    assert_eq!(connected(&tok("x\ny")), [false, false]);
}

#[test]
fn indent_tracking() {
    assert_eq!(indents(&tok("x\n  y\n  z\nw")), [0, 2, 2, 0]);
    // all same-line tokens share the indent of the first on that line
    assert_eq!(indents(&tok("  a b c")), [2, 2, 2]);
}

#[test]
fn string_literal() {
    assert_eq!(tys(&tok(r#""abc""#)), [String]);
    assert_eq!(strs(&tok(r#""""#)), [r#""""#]);
    assert_eq!(strs(&tok(r#""a\"b""#)), [r#""a\"b""#]);
}

#[test]
fn string_unterminated() {
    let (_, _, errors) = tokenize(r#""abc"#);
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].1, "unterminated string literal");
}

#[test]
fn whitespace_error() {
    let (_, _, errors) = tokenize("x\ty");
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].1, "only space character is allowed as whitespace");
}
