use crate::types::common::A;
use std::rc::Rc;

#[derive(Debug)]
pub struct Name(pub String);

#[derive(Debug)]
pub struct ParamDecl {
    pub name: A<Name>,
    pub ty: Rc<A<Val>>,
}
#[derive(Debug)]
pub struct ParamVal {
    pub name: Option<A<Name>>,
    pub val: A<Val>,
}
#[derive(Debug)]
pub struct ParamsDecl(pub Vec<A<ParamDecl>>);
#[derive(Debug)]
pub struct ParamsVal(pub Vec<A<ParamVal>>);

#[derive(Debug)]
pub struct SegmentDecl(pub A<Name>, pub ParamsDecl);
#[derive(Debug)]
pub struct PathDecl(pub Vec<A<SegmentDecl>>, pub Option<A<Val>>);

#[derive(Debug)]
pub struct Segment(pub A<Name>, pub ParamsVal);
#[derive(Debug)]
pub struct Path(pub Vec<A<Segment>>, pub Option<A<Val>>);

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
    Comp(u32),
    CompStar,
    Arrow(ArrowTy),
}
#[derive(Debug)]
pub enum Val {
    Path(Box<A<Path>>),
    Lit(A<Lit>),
    Op(Box<Val>, A<Op>, Option<A<ParamsVal>>, Box<Val>),
}

#[derive(Debug)]
pub enum Module {
    Block(Vec<A<Decl>>),
    Import(A<Lit>),
}

#[derive(Debug, Clone)]
pub enum AssignOp {
    Decl,
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
    pub names: Vec<A<PathDecl>>,
    pub ty: Option<A<Val>>,
    pub op: A<AssignOp>,
    pub body: Option<ValMod>,
}

#[derive(Debug)]
pub enum DeclMain {
    Unit(A<DeclUnit>),
    Mod(A<Module>),
}
#[derive(Debug)]
pub enum Decorator {
    Param(A<Name>, Rc<A<Val>>),
    Deco(A<Val>),
}
#[derive(Debug)]
pub struct Decl {
    pub decos: Vec<A<Decorator>>,
    pub main: DeclMain,
    pub with_clauses: Vec<A<Module>>,
    pub where_clauses: Vec<A<Module>>,
}

#[derive(Debug)]
pub struct Program(pub Vec<A<Decl>>);
