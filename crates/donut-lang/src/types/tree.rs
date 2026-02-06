use crate::types::token::*;

#[derive(Debug)]
pub enum NodeTy {
    UnitName,   // Name ('[' (Param %0 ',') ']')?
    RefName,    // (UnitName %1 '.') (## '(' Val ')')?
    ParamDecl,  // Name+ ':' Val
    NamedParam, // Name '=' Val
    Val,
    UnitDecl,
    Mod,
    Import,
    Lit,
    Decorator,
    Decl,
    // todo...
}

#[derive(Debug)]
pub struct Node<'a> {
    pub ty: NodeTy,
    pub range: TokenRange,
    pub tokens: &'a [Token<'a>],
    pub children: Vec<Node<'a>>,
}
