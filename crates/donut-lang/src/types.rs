pub type Ident = String;
pub type Axis = u8;

pub struct Decl {
    pub decos: Vec<Decorator>,
    pub names: Vec<Ident>,
    pub args: Vec<ArgDecl>,
    pub ty: Option<CellType>,
    pub body: Option<Cell>,
}

pub struct ArgDecl {
    pub names: Vec<Ident>,
    pub ty: Option<ArgType>,
}

pub enum CellType {
    Star,
    Arr(Cell, Cell),
}

pub enum LitType {
    Nat,
    Int,
    Rat,
    Str,
}

pub enum Lit {
    Nat(u64),
    Int(i64),
    Rat(f64),
    Str(String),
}

pub enum ArgType {
    Lit(LitType),
    Cell(CellType),
}

pub enum Arg {
    Lit(Lit),
    Cell(Cell),
}

pub enum Cell {
    Var(Ident, Vec<Arg>),
    Comp(Axis, Vec<Cell>),
}

pub struct Decorator {
    pub name: Ident,
    pub args: Vec<Arg>,
}
