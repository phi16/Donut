use crate::types::common::S;
use std::rc::Rc;

#[derive(Debug)]
pub struct Name(pub String);

#[derive(Debug)]
pub struct ParamDecl {
    pub name: Name,
    pub ty: Rc<S<Val>>,
}
#[derive(Debug)]
pub struct ParamVal {
    pub name: Option<Name>,
    pub val: S<Val>,
}
#[derive(Debug)]
pub struct ParamsDecl(pub Vec<ParamDecl>);
#[derive(Debug)]
pub struct ParamsVal(pub Vec<ParamVal>);

#[derive(Debug)]
pub struct SegmentDecl(pub Name, pub ParamsDecl);
#[derive(Debug)]
pub struct PathDecl(pub Vec<S<SegmentDecl>>, pub Option<S<Val>>);

#[derive(Debug)]
pub struct Segment(pub Name, pub ParamsVal);
#[derive(Debug)]
pub struct Path(pub Vec<S<Segment>>, pub Option<S<Val>>);

#[derive(Debug)]
pub enum Key {
    Name(Name),
    String(String),
}
#[derive(Debug)]
pub enum Lit {
    Number(String),
    String(String),
    Array(Vec<S<Val>>),
    Object(Vec<(S<Key>, S<Val>)>),
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
    Path(Box<S<Path>>),
    Lit(S<Lit>),
    Op(Box<Val>, S<Op>, Option<ParamsVal>, Box<Val>),
    Any,
}

#[derive(Debug)]
pub enum Module {
    Block(Vec<Decl>),
    Import(S<Lit>),
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
    Val(S<Val>),
    Mod(S<Module>),
}
#[derive(Debug)]
pub struct DeclUnit {
    pub names: Vec<S<PathDecl>>,
    pub ty: Option<S<Val>>,
    pub op: S<AssignOp>,
    pub body: Option<ValMod>,
}

#[derive(Debug)]
pub enum DeclMain {
    Unit(DeclUnit),
    Mod(S<Module>),
}
#[derive(Debug)]
pub enum Decorator {
    Param(Name, Rc<S<Val>>),
    Deco(S<Val>),
}
#[derive(Debug)]
pub struct Decl {
    pub decos: Vec<Decorator>,
    pub main: DeclMain,
    pub with_clauses: Vec<S<Module>>,
    pub where_clauses: Vec<S<Module>>,
}

#[derive(Debug)]
pub struct Program(pub Vec<Decl>);
