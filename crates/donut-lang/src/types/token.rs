#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenTy {
    Name,
    Keyword,  // with, where, import, ...
    Operator, // =, :=, +=, →, ~, ~>, ;, ;;, ;...;
    Symbol,   // .,:(){}[]
    Number,
    String,
    Whitespace,
}

#[derive(Debug, Clone)]
pub struct TokenPos {
    pub line: usize,
    pub col: usize,
    pub len: usize,
}

#[derive(Debug)]
pub struct Token<'a> {
    pub str: &'a str,
    pub ty: TokenTy,
    pub pos: TokenPos,
    pub indent: usize,
    pub connected: bool,
}

pub type Error = (TokenPos, String);

#[derive(Debug, Clone)]
pub struct TokenSpan {
    pub start: usize,
    pub end: usize,
}
