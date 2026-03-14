use std::rc::Rc;

pub type Level = u8;
pub type Axis = u8;

pub type N = u32;
pub type Q = num_rational::Rational32;
pub type CoordN = Vec<N>;
pub type CoordQ = Vec<Q>;

pub type Vec1<T> = Vec<T>;
pub type Vec2<T> = Vec<T>;

/// Reference ID — identifies a defined item (Decl/DeclDef).
/// Used in PureVal::Ref. In donut-lang, this corresponds to ItemId for non-param items.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RefId(pub usize);

impl std::fmt::Display for RefId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Bound variable ID — identifies a parameter placeholder.
/// Used in PureVal::Bound. Substituted by SubstMap.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BoundId(pub usize);

impl std::fmt::Display for BoundId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Primitive cell identifier — separate indexing from RefId.
/// Used in donut-lang for prim_decls output.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PrimId(pub u64);

impl std::fmt::Display for PrimId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

// --- AnyBox ---

pub struct AnyBox {
    data: Rc<dyn std::any::Any>,
    eq_fn: fn(&dyn std::any::Any, &dyn std::any::Any) -> bool,
    debug_fn: fn(&dyn std::any::Any, &mut std::fmt::Formatter<'_>) -> std::fmt::Result,
}

impl AnyBox {
    pub fn new<T: PartialEq + std::fmt::Debug + 'static>(val: T) -> Self {
        fn eq_impl<T: PartialEq + 'static>(a: &dyn std::any::Any, b: &dyn std::any::Any) -> bool {
            a.downcast_ref::<T>()
                .zip(b.downcast_ref::<T>())
                .map_or(false, |(a, b)| a == b)
        }
        fn debug_impl<T: std::fmt::Debug + 'static>(
            data: &dyn std::any::Any,
            f: &mut std::fmt::Formatter<'_>,
        ) -> std::fmt::Result {
            std::fmt::Debug::fmt(data.downcast_ref::<T>().unwrap(), f)
        }
        AnyBox {
            data: Rc::new(val),
            eq_fn: eq_impl::<T>,
            debug_fn: debug_impl::<T>,
        }
    }

    pub fn inner<T: 'static>(&self) -> &T {
        self.data.downcast_ref::<T>().unwrap()
    }
}

impl Clone for AnyBox {
    fn clone(&self) -> Self {
        AnyBox {
            data: Rc::clone(&self.data),
            eq_fn: self.eq_fn,
            debug_fn: self.debug_fn,
        }
    }
}

impl PartialEq for AnyBox {
    fn eq(&self, other: &Self) -> bool {
        (self.eq_fn)(self.data.as_ref(), other.data.as_ref())
    }
}

impl std::fmt::Debug for AnyBox {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (self.debug_fn)(self.data.as_ref(), f)
    }
}

// --- PureVal ---

#[derive(Debug, Clone, PartialEq)]
pub enum PureVal {
    Cell(crate::pure_cell::PureCell),
    Ref(RefId, Vec<PureVal>),
    Bound(BoundId),
    Any(AnyBox),
}

impl PureVal {
    /// Validate all PureCells contained in this value.
    /// `any_handler` is called for Any values to validate their contents.
    pub fn validate(&self, any_handler: &impl Fn(&AnyBox)) {
        match self {
            PureVal::Cell(pc) => pc.validate(),
            PureVal::Ref(_, args) => {
                for arg in args {
                    arg.validate(any_handler);
                }
            }
            PureVal::Bound(_) => {}
            PureVal::Any(v) => any_handler(v),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Prim {
    pub id: PrimId,
    pub args: Vec<PureVal>,
}

impl Prim {
    pub fn new(id: u64) -> Self {
        Prim {
            id: PrimId(id),
            args: Vec::new(),
        }
    }

    pub fn with_id(id: PrimId) -> Self {
        Prim {
            id,
            args: Vec::new(),
        }
    }

    pub fn with_id_args(id: PrimId, args: Vec<PureVal>) -> Self {
        Prim { id, args }
    }
}

/// Describes what kind of value a BoundId slot expects.
/// Used in substitution mappings to enable auto-lift and dimension checking.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExtType {
    Cell(Level),
    NonCell,
}

pub type SubstMap = std::collections::HashMap<BoundId, (PureVal, ExtType)>;

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

use crate::pure_cell::PureCell;

#[derive(Debug, Clone)]
pub enum Error {
    NotConvertible(PureCell, PureCell),
    IncompatibleDimension {
        expected: Level,
        got_dim: Level,
        got: PureCell,
    },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::NotConvertible(a, b) => {
                write!(f, "{}\nis not convertible to\n{}", a, b)
            }
            Error::IncompatibleDimension { expected, got_dim, got } => {
                write!(f, "expected {}-cell, got {}-cell `{}`", expected, got_dim, got)
            }
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

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
