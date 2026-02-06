use donut_lang::types::{self, token::TokenIx};
use std::cell::RefCell;

#[derive(Debug, Clone)]
pub enum TokenType {
    Keyword,
    Unknown,
    Number,
    String,
    Symbol,
    Comment,
}

pub struct Diagnostic {
    pub begin_line: u32,
    pub begin_column: u32,
    pub end_line: u32,
    pub end_column: u32,
    pub message: String,
}

pub struct TokenData {
    pub line: u32,
    pub column: u32,
    pub length: u32,
    pub token_type: TokenType,
}

struct Context {
    tokens: Vec<TokenData>,
}

impl Context {
    fn new(tokens: Vec<TokenData>) -> Self {
        Self { tokens }
    }

    fn into_inner(self) -> Vec<TokenData> {
        self.tokens
    }

    fn pos(&self, ix: usize) -> (u32, u32, u32) {
        let t = self.tokens.get(ix).unwrap();
        (t.line, t.column, t.length)
    }

    fn mark_as(&mut self, ix: &TokenIx, offset: i32, t: TokenType) {
        let index = ix.clone().into_inner() as i32 + offset;
        let d = match self.tokens.get_mut(index as usize) {
            Some(token) => token,
            None => return,
        };
        d.token_type = t;
    }
}

pub fn tokenize_example(code: &str) -> (Vec<TokenData>, Vec<Diagnostic>) {
    let lines = code.lines().collect::<Vec<_>>();

    let (tokens, comments, errors) = donut_lang::tokenize::tokenize(code);
    let x = RefCell::new(Context::new(
        tokens
            .iter()
            .map(|t| {
                let line = t.pos.line;
                let column = t.pos.col;
                let length = t.pos.len;
                let mut l = lines.get(line).unwrap().chars();
                let utf16_column = l
                    .by_ref()
                    .take(column)
                    .map(|c| c.len_utf16())
                    .sum::<usize>() as u32;
                let utf16_length = l.take(length).map(|c| c.len_utf16()).sum::<usize>() as u32;
                TokenData {
                    line: line as u32,
                    column: utf16_column,
                    length: utf16_length,
                    token_type: TokenType::Unknown,
                }
            })
            .collect::<Vec<_>>(),
    ));

    let mut diags = Vec::new();
    let mut add_diag = |pos: &types::token::TokenPos, msg: &str| {
        let begin_line = pos.line as u32;
        let begin_column = pos.col as u32;
        let len = pos.len as u32;
        let end_line = begin_line;
        let end_column = begin_column + len;
        diags.push(Diagnostic {
            begin_line,
            begin_column,
            end_line,
            end_column,
            message: msg.to_string(),
        });
    };

    {
        let mut x = x.borrow_mut();
        tokens.iter().enumerate().for_each(|(ix, t)| {
            x.mark_as(
                &TokenIx::new(ix),
                0,
                match t.ty {
                    types::token::TokenTy::Ident => match t.str {
                        "with" | "where" => TokenType::Keyword,
                        _ => TokenType::Unknown,
                    },
                    types::token::TokenTy::Number => TokenType::Number,
                    types::token::TokenTy::Char => TokenType::String,
                    types::token::TokenTy::String => TokenType::String,
                    types::token::TokenTy::Reserved => TokenType::Symbol,
                    types::token::TokenTy::Comment => TokenType::Comment,
                    types::token::TokenTy::Whitespace => TokenType::Unknown,
                },
            );
        });
    }
    for (pos, e) in &errors {
        add_diag(pos, e);
    }
    let comments_iter = comments.into_iter().map(|pos| {
        let mut l = lines.get(pos.line).unwrap().chars().map(|c| c.len_utf16());
        let utf16_column = l.by_ref().take(pos.col).sum::<usize>() as u32;
        let utf16_length = l.sum::<usize>() as u32;
        TokenData {
            line: pos.line as u32,
            column: utf16_column,
            length: utf16_length,
            token_type: TokenType::Comment,
        }
    });
    let mut tokens_iter = x.into_inner().into_inner().into_iter().peekable();
    let mut comments_iter = comments_iter.peekable();
    let mut res = Vec::new();
    loop {
        match (tokens_iter.peek(), comments_iter.peek()) {
            (Some(t), Some(c)) => {
                let token_first = if t.line < c.line {
                    true
                } else if t.line > c.line {
                    false
                } else {
                    t.column < c.column
                };
                if token_first {
                    res.push(tokens_iter.next().unwrap());
                } else {
                    res.push(comments_iter.next().unwrap());
                }
            }
            (Some(_), None) => {
                res.extend(tokens_iter);
                break;
            }
            (None, Some(_)) => {
                res.extend(comments_iter);
                break;
            }
            (None, None) => {
                break;
            }
        }
    }
    (res, diags)
}
