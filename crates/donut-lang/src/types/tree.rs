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
pub struct AppName(pub A<Name>, pub Option<A<ParamPack>>);
#[derive(Debug)]
pub struct RefName(pub Vec<A<AppName>>, pub Option<A<Val>>);

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
pub enum Op {
    Comp(u8),
    CompParam(A<ParamPack>),
    Arrow(&'static str),
}
#[derive(Debug)]
pub enum Val {
    Ref(Box<A<RefName>>),
    Dots(u8),
    Lit(A<Lit>),
    Op(Box<A<Val>>, A<Op>, Box<A<Val>>),
    Paren(Box<A<Val>>),
}

#[derive(Debug)]
pub enum Module {
    Block(A<Program>),
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
pub struct UnitDecl {
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
pub enum Unit {
    Decl(A<UnitDecl>),
    Mod(A<Module>),
}
#[derive(Debug)]
pub struct Decl {
    pub decos: Vec<A<ParamPack>>,
    pub unit: Unit,
    pub clauses: Vec<A<Clause>>,
}

#[derive(Debug)]
pub struct Program(pub Vec<A<Decl>>);
