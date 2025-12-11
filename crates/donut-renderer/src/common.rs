pub use donut_core::cell::{Coord, Level, Padding};

pub type Q = num_rational::Rational32;
pub type CoordQ = Vec<Q>;

pub type Vec1<T> = Vec<T>;
pub type Vec2<T> = Vec<T>;

pub fn from_coord(c: Coord) -> CoordQ {
    c.into_iter().map(|x| Q::from(x as i32)).collect()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dimensions {
    pub effective: Level,
    pub in_space: Level,
}

impl Dimensions {
    pub fn zero() -> Self {
        Dimensions {
            effective: 0,
            in_space: 0,
        }
    }

    pub fn from_point(d: Level) -> Self {
        Dimensions {
            effective: d,
            in_space: d,
        }
    }

    pub fn shifted(&self) -> Self {
        Dimensions {
            effective: self.effective,
            in_space: self.in_space + 1,
        }
    }

    pub fn sliced(&self) -> Self {
        Dimensions {
            effective: self.effective,
            in_space: self.in_space - 1,
        }
    }
}
