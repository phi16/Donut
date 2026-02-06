#[derive(Debug)]
pub enum TokenTy {
    Ident,
    Number,
    Char,
    String,
    Reserved,
    Comment,
    Whitespace,
}

#[derive(Debug, Clone)]
pub struct TokenIx(usize);

impl TokenIx {
    pub fn new(ix: usize) -> Self {
        TokenIx(ix)
    }
    pub fn into_inner(self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct TokenPos {
    pub line: usize,
    pub col: usize,
    pub len: usize,
}

#[derive(Debug)]
pub struct TokenRange {
    pub start: TokenIx,
    pub end: TokenIx,
}

#[derive(Debug)]
pub struct Token<'a> {
    pub str: &'a str,
    pub ty: TokenTy,
    pub ix: TokenIx,
    pub pos: TokenPos,
}

pub type Error = (TokenPos, String);
