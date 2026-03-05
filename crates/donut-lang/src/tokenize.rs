use std::iter::Peekable;

use crate::types::common::*;
use crate::types::token::*;

fn is_keyword(s: &str) -> bool {
    match s {
        "with" | "where" | "import" => true,
        _ => false,
    }
}

fn is_operator(s: &str) -> bool {
    match s {
        "=" | ":=" | "+=" | "->" | "→" | "~" | "~>" => true,
        _ => false,
    }
}

fn is_separator(c: char) -> bool {
    match c {
        '.' | ',' | ':' | ';' | '(' | ')' | '{' | '}' | '[' | ']' => true,
        _ => false,
    }
}

#[derive(Debug, Clone)]
struct Loc<'a> {
    str: &'a str,
    ln: usize,
    col_chars: usize,
    col_bytes: usize,
}

type LocChar<'a> = (Loc<'a>, char);

struct Tokenizer<'a, I: Iterator<Item = LocChar<'a>>> {
    iter: Peekable<I>,
    connected: bool,
    last_line: usize,
    errors: Vec<Error>,
}

impl<'a, I: Iterator<Item = LocChar<'a>>> Tokenizer<'a, I> {
    fn new(iter: I) -> Self {
        Tokenizer {
            iter: iter.peekable(),
            connected: false,
            last_line: 0,
            errors: vec![],
        }
    }

    fn add_error(&mut self, pos: TokenPos, msg: &str) {
        self.errors.push((pos, msg.to_string()));
    }

    fn make_token_internal(&mut self, l0: &Loc<'a>, ty: TokenTy, err: Option<&str>) -> Token<'a> {
        let str = match self.iter.peek() {
            Some((l, _)) if l0.ln == l.ln => &l0.str[l0.col_bytes..l.col_bytes],
            _ => &l0.str[l0.col_bytes..],
        };
        let ty = if ty != TokenTy::Name {
            ty
        } else if is_keyword(str) {
            TokenTy::Keyword
        } else if is_operator(str) {
            TokenTy::Operator
        } else {
            TokenTy::Name
        };
        let pos = TokenPos {
            line: l0.ln,
            col: l0.col_chars,
            len: str.chars().count(),
        };
        if let Some(err) = err {
            self.add_error(pos.clone(), err);
        }
        let connected = self.last_line == l0.ln && self.connected;
        self.connected = ty != TokenTy::Whitespace;
        self.last_line = l0.ln;
        Token {
            str,
            ty,
            pos,
            indent: 0,
            connected,
        }
    }

    fn make_token(&mut self, l0: &Loc<'a>, ty: TokenTy) -> Token<'a> {
        self.make_token_internal(l0, ty, None)
    }

    fn make_token_with_error(&mut self, l0: &Loc<'a>, ty: TokenTy, err: &str) -> Token<'a> {
        self.make_token_internal(l0, ty, Some(err))
    }

    fn string_literal(&mut self) -> Token<'a> {
        let (l0, c0) = self.iter.next().unwrap();
        assert_eq!(c0, '"');
        let ty = TokenTy::String;
        let err = "unterminated string literal";
        while let Some((l, c)) = self.iter.peek() {
            if l0.ln != l.ln {
                return self.make_token_with_error(&l0, ty, err);
            }
            if c0 == *c {
                // end quote
                self.iter.next();
                return self.make_token(&l0, ty);
            }
            if *c == '\\' {
                // escape sequence
                self.iter.next();
                if let Some((l, _)) = self.iter.peek() {
                    if l0.ln != l.ln {
                        continue;
                    }
                    // we don't need to care about the escape-sequence length,
                    // because we are just waiting for the closing quote
                    self.iter.next();
                } else {
                    continue;
                }
            } else {
                self.iter.next();
            }
        }
        self.make_token_with_error(&l0, ty, err)
    }
}

impl<'a, I: Iterator<Item = LocChar<'a>>> Iterator for Tokenizer<'a, I> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let (l0, c0) = self.iter.peek()?.clone();
        if c0 == '\r' {
            // skip carriage return (CRLF support)
            self.iter.next();
            return self.next();
        } else if c0.is_whitespace() {
            // whitespace
            while let Some((l, c)) = self.iter.peek() {
                if l0.ln == l.ln && c.is_whitespace() {
                    if *c == '\r' {
                        // skip
                    } else if *c != ' ' {
                        let pos = TokenPos {
                            line: l.ln,
                            col: l.col_chars,
                            len: 1,
                        };
                        self.add_error(pos, "only space character is allowed as whitespace");
                    }
                    self.iter.next();
                } else {
                    break;
                }
            }
            Some(self.make_token(&l0, TokenTy::Whitespace))
        } else if is_separator(c0) {
            // separator symbol
            self.iter.next();
            if c0 == ':' {
                if let Some((l, '=')) = self.iter.peek() {
                    if l0.ln == l.ln {
                        self.iter.next();
                        return Some(self.make_token(&l0, TokenTy::Operator));
                    }
                }
            } else if c0 == '.' || c0 == ';' {
                // ....., ;...;
                let mut count = 1;
                loop {
                    if let Some((l, c)) = self.iter.peek() {
                        if l0.ln == l.ln && c0 == *c {
                            self.iter.next();
                            count += 1;
                            continue;
                        }
                    }
                    break;
                }
                let ty = if c0 == '.' && count >= 2 {
                    TokenTy::Keyword
                } else {
                    if count == 1 {
                        if let Some((l, c)) = self.iter.peek() {
                            if l0.ln == l.ln && *c == '*' {
                                self.iter.next();
                            }
                        }
                    }
                    TokenTy::Operator
                };
                return Some(self.make_token(&l0, ty));
            }
            Some(self.make_token(&l0, TokenTy::Symbol))
        } else if c0 == '"' {
            // char or string
            Some(self.string_literal())
        } else {
            // name
            while let Some((l, c)) = self.iter.peek() {
                if l0.ln != l.ln || c.is_whitespace() || is_separator(*c) {
                    break;
                }
                self.iter.next();
            }
            Some(self.make_token(&l0, TokenTy::Name))
        }
    }
}

pub fn tokenize<'a>(code: &'a str) -> (Vec<Token<'a>>, Vec<TokenPos>, Vec<Error>) {
    let (lines, comments): (Vec<_>, Vec<_>) = code
        .lines()
        .enumerate()
        .map(|(ln, line)| {
            let (line, comment) = match line.find("//") {
                Some(ix) => {
                    let l = &line[..ix];
                    let c = &line[ix..];
                    let pos = TokenPos {
                        line: ln,
                        col: l.chars().count(),
                        len: c.chars().count(),
                    };
                    (l, Some(pos))
                }
                None => (line, None),
            };
            let line_iter =
                line.char_indices()
                    .enumerate()
                    .map(move |(col_chars, (col_bytes, c))| {
                        let loc = Loc {
                            str: &line,
                            ln,
                            col_chars,
                            col_bytes,
                        };
                        (loc, c)
                    });
            (line_iter, comment)
        })
        .unzip();
    let comments = comments.into_iter().filter_map(|c| c).collect();
    let iter = lines.into_iter().flatten();
    let mut tokenizer = Tokenizer::new(iter);
    let mut tokens = vec![];
    let mut indent = None;
    while let Some(token) = tokenizer.next() {
        if token.ty != TokenTy::Whitespace {
            let indent_lc = match indent {
                Some((l, c)) if l == token.pos.line => (l, c),
                _ => (token.pos.line, token.pos.col),
            };
            indent = Some(indent_lc);
            let mut token = token;
            token.indent = indent_lc.1;
            tokens.push(token);
        }
    }
    (tokens, comments, tokenizer.errors)
}
