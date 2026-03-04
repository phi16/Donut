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
pub struct Params<T>(pub Vec<T>);

#[derive(Debug)]
pub struct Segment<P>(pub Name, pub Params<P>);
#[derive(Debug)]
pub struct Path<P>(pub Vec<S<Segment<P>>>, pub Option<S<Val>>);

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
    Path(Box<S<Path<ParamVal>>>),
    Lit(S<Lit>),
    Op(Box<S<Val>>, S<Op>, Option<Params<ParamVal>>, Box<S<Val>>),
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
    pub names: Vec<S<Path<ParamDecl>>>,
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
