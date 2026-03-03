use crate::types::common::TokenPos;

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

#[derive(Debug)]
pub struct Token<'a> {
    pub str: &'a str,
    pub ty: TokenTy,
    pub pos: TokenPos,
    pub indent: usize,
    pub connected: bool,
}
