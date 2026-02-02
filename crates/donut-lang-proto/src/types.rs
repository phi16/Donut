pub type Ident = String;
pub type Axis = u8;

#[derive(Debug, PartialEq)]
pub struct Decl {
    pub decos: Vec<Decorator>,
    pub names: Vec<Ident>,
    pub args: Vec<ArgDecl>,
    pub ty: Option<CellType>,
    pub body: Option<Cell>,
}

#[derive(Debug, PartialEq)]
pub struct ArgDecl {
    pub names: Vec<Ident>,
    pub ty: Option<ArgType>,
}

#[derive(Debug, PartialEq)]
pub enum CellType {
    Star,
    Arr(Cell, Cell),
}

#[derive(Debug, PartialEq)]
pub enum LitType {
    Nat,
    Int,
    Rat,
    Str,
}

#[derive(Debug, PartialEq)]
pub enum Lit {
    Nat(u64),
    Int(i64),
    Rat(f64),
    Str(String),
}

#[derive(Debug, PartialEq)]
pub enum ArgType {
    Lit(LitType),
    Cell(CellType),
}

#[derive(Debug, PartialEq)]
pub enum Arg {
    Lit(Lit),
    Cell(Cell),
}

#[derive(Debug, PartialEq)]
pub enum Cell {
    Var(Ident, Vec<Arg>),
    Comp(Axis, Vec<Cell>),
}

#[derive(Debug, PartialEq)]
pub struct Decorator {
    pub name: Ident,
    pub args: Vec<Arg>,
}
