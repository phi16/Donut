pub type Level = u8;
pub type Axis = u8;
pub type N = u32;
pub type CoordN = Vec<N>;
pub type Vec1<T> = Vec<T>;
pub type Vec2<T> = Vec<T>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Prim {
    id: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Dim {
    pub effective: Level,
    pub in_space: Level,
}

impl Dim {
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

pub fn add_coord(a: &mut CoordN, b: &CoordN) {
    assert_eq!(a.len(), b.len());
    for (x, y) in a.iter_mut().zip(b.iter()) {
        *x += *y;
    }
}

pub fn max_coord(a: &mut CoordN, b: &CoordN) {
    assert_eq!(a.len(), b.len());
    for (x, y) in a.iter_mut().zip(b.iter()) {
        *x = (*x).max(*y);
    }
}
