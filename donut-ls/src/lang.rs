use donut_lang::types;
use donut_lang::types::common;
use donut_lang::types::syntree;

#[derive(Debug, Clone)]
pub enum TokenType {
    Unknown,
    Keyword,
    Operator,
    Symbol,
    Number,
    String,
    Comment,
    Parameter, // [x: T] の x などパラメータ名
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

    fn mark_as(&mut self, index: usize, t: TokenType) {
        let d = match self.tokens.get_mut(index) {
            Some(token) => token,
            None => return,
        };
        d.token_type = t;
    }
    fn mark_elem_as<T>(&mut self, e: &common::A<T>, t: TokenType) {
        if let common::A::Accepted(_, span) = e {
            // TODO: end?
            self.mark_as(span.start, t);
        }
    }
}

trait Marking {
    fn mark(&self, x: &mut Context);
}

impl<T: Marking> Marking for common::A<T> {
    fn mark(&self, x: &mut Context) {
        if let common::A::Accepted(t, _) = self {
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
impl<T: Marking, U: Marking> Marking for (T, U) {
    fn mark(&self, x: &mut Context) {
        self.0.mark(x);
        self.1.mark(x);
    }
}

impl Marking for syntree::Symbol {
    fn mark(&self, _: &mut Context) {}
}
impl Marking for syntree::Name {
    fn mark(&self, _: &mut Context) {}
}
impl Marking for syntree::ParamTy {
    fn mark(&self, _: &mut Context) {}
}
impl Marking for syntree::Param {
    fn mark(&self, x: &mut Context) {
        for name in &self.names {
            x.mark_elem_as(name, TokenType::Parameter);
        }
        self.ty.mark(x);
        self.val.mark(x);
    }
}
impl Marking for syntree::Params {
    fn mark(&self, x: &mut Context) {
        self.1.mark(x);
    }
}
impl Marking for syntree::Decorator {
    fn mark(&self, x: &mut Context) {
        x.mark_elem_as(&self.0, TokenType::Keyword);
        self.1.mark(x);
        x.mark_elem_as(&self.2, TokenType::Keyword);
    }
}
impl Marking for syntree::Segment {
    fn mark(&self, x: &mut Context) {
        self.0.mark(x);
        self.1.mark(x);
    }
}
impl Marking for syntree::Path {
    fn mark(&self, x: &mut Context) {
        self.0.mark(x);
        self.1.mark(x);
    }
}
impl Marking for syntree::Key {
    fn mark(&self, x: &mut Context) {
        match self {
            syntree::Key::Name(n) => n.mark(x),
            syntree::Key::String(_) => {}
        }
    }
}
impl Marking for syntree::Lit {
    fn mark(&self, x: &mut Context) {
        match self {
            syntree::Lit::Number(_) => {}
            syntree::Lit::String(_) => {}
            syntree::Lit::Array(vs) => vs.mark(x),
            syntree::Lit::Object(kvs) => kvs.mark(x),
        }
    }
}
impl Marking for syntree::Op {
    fn mark(&self, _: &mut Context) {}
}
impl Marking for syntree::Val0 {
    fn mark(&self, x: &mut Context) {
        match self {
            syntree::Val0::Path(p) => p.mark(x),
            syntree::Val0::Lit(l) => l.mark(x),
            syntree::Val0::Dots => {}
            syntree::Val0::Paren(v) => v.mark(x),
        }
    }
}
impl Marking for syntree::Val {
    fn mark(&self, x: &mut Context) {
        self.vs.mark(x);
        self.ops.mark(x);
    }
}
impl Marking for syntree::Module {
    fn mark(&self, x: &mut Context) {
        match self {
            syntree::Module::Block(ds) => ds.mark(x),
            syntree::Module::Import(l) => l.mark(x),
        }
    }
}
impl Marking for syntree::AssignOp {
    fn mark(&self, _: &mut Context) {}
}
impl Marking for syntree::ValMod {
    fn mark(&self, x: &mut Context) {
        match self {
            syntree::ValMod::Val(v) => v.mark(x),
            syntree::ValMod::Mod(m) => m.mark(x),
        }
    }
}
impl Marking for syntree::DeclUnit {
    fn mark(&self, x: &mut Context) {
        self.names.mark(x);
        self.ty.mark(x);
        self.assign.mark(x);
    }
}
impl Marking for syntree::ClauseTy {
    fn mark(&self, _: &mut Context) {}
}
impl Marking for syntree::Clause {
    fn mark(&self, x: &mut Context) {
        self.0.mark(x);
        self.1.mark(x);
    }
}
impl Marking for syntree::DeclMain {
    fn mark(&self, x: &mut Context) {
        match self {
            syntree::DeclMain::Unit(d) => d.mark(x),
            syntree::DeclMain::Mod(m) => m.mark(x),
            syntree::DeclMain::Dots => {}
        }
    }
}
impl Marking for syntree::Decl {
    fn mark(&self, x: &mut Context) {
        self.decos.mark(x);
        self.main.mark(x);
        self.clauses.mark(x);
    }
}
impl Marking for syntree::Program {
    fn mark(&self, x: &mut Context) {
        self.0.mark(x);
    }
}

fn to_diag(pos: &types::common::TokenPos, msg: &str) -> Diagnostic {
    Diagnostic {
        begin_line: pos.line as u32,
        begin_column: pos.col as u32,
        end_line: pos.line as u32,
        end_column: (pos.col + pos.len) as u32,
        message: msg.to_string(),
    }
}

fn to_utf16(lines: &[&str], line: usize, col: usize, len: usize) -> (u32, u32) {
    let mut chars = lines.get(line).unwrap().chars();
    let utf16_col = chars
        .by_ref()
        .take(col)
        .map(|c| c.len_utf16())
        .sum::<usize>() as u32;
    let utf16_len = chars.take(len).map(|c| c.len_utf16()).sum::<usize>() as u32;
    (utf16_col, utf16_len)
}

pub fn tokenize_example(code: &str) -> (Vec<TokenData>, Vec<Diagnostic>) {
    let lines = code.lines().collect::<Vec<_>>();

    let (tokens, comments, tokenize_errors) = donut_lang::tokenize::tokenize(code);

    // tokenizer の型付けを初期値として TokenData を構築
    let token_data = tokens
        .iter()
        .map(|t| {
            let (utf16_col, utf16_len) = to_utf16(&lines, t.pos.line, t.pos.col, t.pos.len);
            TokenData {
                line: t.pos.line as u32,
                column: utf16_col,
                length: utf16_len,
                token_type: match t.ty {
                    types::token::TokenTy::Name => TokenType::Unknown,
                    types::token::TokenTy::Keyword => TokenType::Keyword,
                    types::token::TokenTy::Operator => TokenType::Operator,
                    types::token::TokenTy::Symbol => TokenType::Symbol,
                    types::token::TokenTy::Number => TokenType::Number,
                    types::token::TokenTy::String => TokenType::String,
                    types::token::TokenTy::Whitespace => TokenType::Unknown,
                },
            }
        })
        .collect::<Vec<_>>();

    // tokenize エラーを診断として収集
    let mut diags: Vec<Diagnostic> = tokenize_errors
        .iter()
        .map(|(pos, msg)| to_diag(pos, msg))
        .collect();

    // parse は常に実行（エラー回復があるので tokenize エラーがあっても走らせる）
    let mut ctx = Context::new(token_data);
    let (program, parse_errors) = donut_lang::parse::parse(&tokens);
    program.mark(&mut ctx);
    for (pos, msg) in &parse_errors {
        diags.push(to_diag(pos, msg));
    }

    // コメントトークンを構築
    let comments_iter = comments.into_iter().map(|pos| {
        let (utf16_col, utf16_len) = to_utf16(&lines, pos.line, pos.col, pos.len);
        TokenData {
            line: pos.line as u32,
            column: utf16_col,
            length: utf16_len,
            token_type: TokenType::Comment,
        }
    });

    // トークンとコメントを行・列順にマージ
    let mut tokens_iter = ctx.into_inner().into_iter().peekable();
    let mut comments_iter = comments_iter.peekable();
    let mut res = Vec::new();
    loop {
        match (tokens_iter.peek(), comments_iter.peek()) {
            (Some(t), Some(c)) => {
                if (t.line, t.column) <= (c.line, c.column) {
                    res.push(tokens_iter.next().unwrap());
                } else {
                    res.push(comments_iter.next().unwrap());
                }
            }
            (Some(_), None) => {
                res.extend(tokens_iter);
                break;
            }
            (None, _) => {
                res.extend(comments_iter);
                break;
            }
        }
    }
    (res, diags)
}
