use crate::types::token::TokenSpan;

#[derive(Debug)]
pub enum A<T> {
    Accepted(T, TokenSpan),
    Error(),
}

impl<T> A<T> {
    pub fn inner(&self) -> Option<&T> {
        match self {
            A::Accepted(v, _) => Some(v),
            A::Error() => None,
        }
    }
}

#[derive(Debug)]
pub struct Symbol(pub &'static str);
#[derive(Debug)]
pub struct Name(pub String);

#[derive(Debug)]
pub enum ParamTy {
    Decl,
    Named,
}
#[derive(Debug)]
pub struct Param {
    pub names: Vec<A<Name>>,
    pub ty: Option<ParamTy>,
    pub val: A<Val>,
}
#[derive(Debug)]
pub struct ParamPack(pub Vec<A<Param>>);

#[derive(Debug)]
pub struct LocalRefName(pub A<Name>, pub Option<A<ParamPack>>);
#[derive(Debug)]
pub struct RefName(pub Vec<A<LocalRefName>>, pub Option<A<Val>>);

#[derive(Debug)]
pub enum Key {
    Name(A<Name>),
    String(A<String>),
}
#[derive(Debug)]
pub enum Lit {
    Number(String),
    String(String),
    Array(Vec<A<Val>>),
    Object(Vec<(A<Key>, A<Val>)>),
}

#[derive(Debug)]
pub enum ArrowTy {
    To,
    Eq,
    Functor,
}
#[derive(Debug)]
pub enum Op {
    CompRep(u32),
    CompLit(u32),
    CompStar,
    Arrow(ArrowTy),
}
#[derive(Debug)]
pub enum Val0 {
    Ref(Box<A<RefName>>),
    Lit(A<Lit>),
    Dots,
    Paren(Box<A<Val>>),
}
#[derive(Debug)]
pub struct Val {
    pub vs: Vec<A<Val0>>,
    pub ops: Vec<(A<Op>, Option<A<ParamPack>>)>,
}

#[derive(Debug)]
pub enum Module {
    Block(Vec<A<Decl>>),
    Import(A<Lit>),
}

#[derive(Debug, Clone)]
pub enum AssignOp {
    Alias,
    Def,
    Add,
}
#[derive(Debug)]
pub enum ValMod {
    Val(A<Val>),
    Mod(A<Module>),
}
#[derive(Debug)]
pub struct DeclUnit {
    pub names: Vec<A<RefName>>,
    pub ty: Option<A<Val>>,
    pub assign: Option<(A<AssignOp>, ValMod)>,
}

#[derive(Debug, Clone)]
pub enum ClauseTy {
    With,
    Where,
}
#[derive(Debug)]
pub struct Clause(pub A<ClauseTy>, pub A<Module>);

#[derive(Debug)]
pub enum DeclMain {
    Unit(A<DeclUnit>),
    Mod(A<Module>),
}
#[derive(Debug)]
pub struct Decl {
    pub decos: Vec<A<ParamPack>>,
    pub main: Option<DeclMain>,
    pub clauses: Vec<A<Clause>>,
}

#[derive(Debug)]
pub struct Program(pub Vec<A<Decl>>);
