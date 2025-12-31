pub type Level = u8;
pub type Axis = u8;
pub type N = u32;
pub type CoordN = Vec<N>;
pub type Q = num_rational::Rational32;
pub type CoordQ = Vec<Q>;
pub type Vec1<T> = Vec<T>;
pub type Vec2<T> = Vec<T>;

pub const BORDER_WIDTH: N = 32;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Prim {
    pub id: u64,
}

impl Prim {
    pub fn new(id: u64) -> Self {
        Prim { id }
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
