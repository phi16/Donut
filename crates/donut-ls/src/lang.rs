use donut_lang::types;
use donut_lang::types::tree;
use std::cell::RefCell;

#[derive(Debug, Clone)]
pub enum TokenType {
    Unknown,
    Keyword,
    Operator,
    Symbol,
    Number,
    String,
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

    fn mark_as(&mut self, ix: &usize, offset: i32, t: TokenType) {
        let index = *ix as i32 + offset;
        let d = match self.tokens.get_mut(index as usize) {
            Some(token) => token,
            None => return,
        };
        d.token_type = t;
    }
}

/* trait Marking {
    fn mark(&self, x: &mut Context);
}

impl<T: Marking> Marking for types::tree::A<T> {
    fn mark(&self, x: &mut Context) {
        if let tree::A::Accepted(t, _) = self {
            t.mark(x);
        }
    }
}
impl<T: Marking> Marking for Vec<T> {
    fn mark(&self, x: &mut Context) {
        for t in self {
            t.mark(x);
        }
    }
}
impl<T: Marking> Marking for Option<T> {
    fn mark(&self, x: &mut Context) {
        if let Some(t) = self {
            t.mark(x);
        }
    }
}
impl Marking for tree::Clause {
    fn mark(&self, x: &mut Context) {
        self.0.mark(x); // ClauseTy
        self.1.mark(x);
    }
}
impl Marking for tree::DeclMain {
    fn mark(&self, x: &mut Context) {
        match self {
            tree::DeclMain::Unit(d) => d.mark(x),
            tree::DeclMain::Mod(m) => m.mark(x),
        }
    }
}
impl Marking for tree::Decl {
    fn mark(&self, x: &mut Context) {
        self.decos.mark(x);
        self.main.mark(x);
        self.clauses.mark(x);
    }
}
impl Marking for tree::Program {
    fn mark(&self, x: &mut Context) {
        self.0.mark(x);
    }
} */

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
                &ix,
                0,
                match t.ty {
                    types::token::TokenTy::Name => TokenType::Unknown,
                    types::token::TokenTy::Keyword => TokenType::Keyword,
                    types::token::TokenTy::Operator => TokenType::Operator,
                    types::token::TokenTy::Symbol => TokenType::Symbol,
                    types::token::TokenTy::Number => TokenType::Number,
                    types::token::TokenTy::String => TokenType::String,
                    types::token::TokenTy::Whitespace => TokenType::Unknown,
                },
            );
        });
    }
    for (pos, e) in &errors {
        add_diag(pos, e);
    }
    eprintln!("tokens: {:?}", tokens);
    eprintln!("errors: {:?}", errors);
    if errors.is_empty() {
        let (program, errors) = donut_lang::parse::parse(&tokens);
        eprintln!("program: {:?}", program);
        eprintln!("errors: {:?}", errors);
        // program.mark(&mut x.borrow_mut());
        for (pos, e) in &errors {
            add_diag(pos, e);
        }
        // TODO
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
