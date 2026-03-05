pub type Level = u8;
pub type Axis = u8;

pub type N = u32;
pub type Q = num_rational::Rational32;
pub type CoordN = Vec<N>;
pub type CoordQ = Vec<Q>;

pub type Vec1<T> = Vec<T>;
pub type Vec2<T> = Vec<T>;

pub type PrimId = u64;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimArg {
    Cell(crate::pure_cell::PureCell),
    Nat(u64),
    App(PrimId, Vec<PrimArg>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Prim {
    pub id: PrimId,
    pub args: Vec<PrimArg>,
}

impl Prim {
    pub fn new(id: PrimId) -> Self {
        Prim {
            id,
            args: Vec::new(),
        }
    }

    pub fn with_args(id: PrimId, args: Vec<PrimArg>) -> Self {
        Prim { id, args }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Dim {
    pub effective: Level,
    pub in_space: Level,
}

impl Dim {
    pub fn zero() -> Self {
        Dim {
            effective: 0,
            in_space: 0,
        }
    }

    pub fn new(effective: Level, in_space: Level) -> Self {
        Dim {
            effective,
            in_space,
        }
    }

    pub fn shifted(self) -> Self {
        Dim {
            effective: self.effective,
            in_space: self.in_space + 1,
        }
    }

    pub fn sliced(self) -> Self {
        assert!(self.in_space > 0);
        Dim {
            effective: self.effective,
            in_space: self.in_space - 1,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Side {
    Source,
    Target,
}

pub type Result<T> = std::result::Result<T, String>;

pub fn dedent(code: &str) -> String {
    let lines: Vec<&str> = code.lines().collect();
    let min_indent = lines
        .iter()
        .filter(|l| !l.trim().is_empty())
        .map(|l| l.len() - l.trim_start().len())
        .min()
        .unwrap_or(0);
    lines
        .iter()
        .map(|l| {
            if l.len() >= min_indent {
                &l[min_indent..]
            } else {
                l
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}
