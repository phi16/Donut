use crate::cell::*;
use crate::common::*;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Shape {
    Zero,
    Succ {
        source: Box<PureCell>,
        target: Box<PureCell>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PureCell {
    Prim(Prim, Shape, Dim),
    Comp(Axis, Vec2<PureCell>, Dim),
}

impl Globular for PureCell {
    fn dim(&self) -> Dim {
        match self {
            PureCell::Prim(_, _, dim) => dim.clone(),
            PureCell::Comp(_, _, dim) => dim.clone(),
        }
    }
    fn s(&self) -> Self {
        match self {
            PureCell::Prim(prim, shape, dim) if dim.effective < dim.in_space => {
                PureCell::Prim(prim.clone(), shape.clone(), dim.sliced())
            }
            PureCell::Prim(_, shape, _) => match shape {
                Shape::Zero => panic!("zero-cell has no source"),
                Shape::Succ { source, .. } => source.as_ref().clone(),
            },
            PureCell::Comp(axis, children, dim) => {
                if *axis == dim.in_space - 1 {
                    children.first().unwrap().s()
                } else {
                    let children = children.iter().map(|child| child.s()).collect::<Vec<_>>();
                    PureCell::comp(*axis, children).unwrap()
                }
            }
        }
    }

    fn t(&self) -> Self {
        match self {
            PureCell::Prim(prim, shape, dim) if dim.effective < dim.in_space => {
                PureCell::Prim(prim.clone(), shape.clone(), dim.sliced())
            }
            PureCell::Prim(_, shape, _) => match shape {
                Shape::Zero => panic!("zero-cell has no target"),
                Shape::Succ { target, .. } => target.as_ref().clone(),
            },
            PureCell::Comp(axis, children, dim) => {
                if *axis == dim.in_space - 1 {
                    children.last().unwrap().t()
                } else {
                    let children = children.iter().map(|child| child.t()).collect::<Vec<_>>();
                    PureCell::comp(*axis, children).unwrap()
                }
            }
        }
    }

    fn to_pure(&self) -> PureCell {
        self.clone()
    }
}

impl Diagram for PureCell {
    fn zero(prim: Prim) -> Self {
        PureCell::Prim(prim, Shape::Zero, Dim::new(0, 0))
    }

    fn prim(prim: Prim, source: Self, target: Self) -> Result<Self> {
        check_prim(&source, &target)?;
        let d = source.dim().in_space;
        assert_eq!(d, target.dim().in_space);
        Ok(PureCell::Prim(
            prim,
            Shape::Succ {
                source: Box::new(source),
                target: Box::new(target),
            },
            Dim::new(d + 1, d + 1),
        ))
    }

    fn id(face: Self) -> Self {
        match face {
            PureCell::Prim(prim, shape, dim) => PureCell::Prim(prim, shape, dim.shifted()),
            PureCell::Comp(axis, children, dim) => PureCell::Comp(
                axis,
                children.into_iter().map(|child| Self::id(child)).collect(),
                dim.shifted(),
            ),
        }
    }

    fn comp(axis: Axis, children: Vec2<Self>) -> Result<Self> {
        let n = children.len();
        if n == 0 {
            return Err("No elements".to_string());
        }
        assert!(n >= 1);

        let children = children
            .into_iter()
            .map(|child| {
                if child.dim().in_space <= axis {
                    // handles `f; f` where `f` is a 1-cell
                    Self::id(child)
                } else {
                    child
                }
            })
            .collect::<Vec<_>>();

        let first_source_face = source_face(&children[0], axis);
        let last_target_face = target_face(&children[n - 1], axis);

        for i in 0..n - 1 {
            let t = target_face(&children[i], axis);
            let s = source_face(&children[i + 1], axis);
            compatible(&t, &s)?;
        }

        let mut cs = vec![];
        let mut dim = children.first().unwrap().dim();
        for c in children {
            let d = c.dim();
            assert_eq!(d.in_space, dim.in_space);
            dim.effective = dim.effective.max(d.effective);
            if d.effective <= axis {
                // do nothing
            } else {
                match c {
                    PureCell::Comp(a, mut cc, _) if a == axis => {
                        cs.append(&mut cc);
                    }
                    _ => {
                        cs.push(c);
                    }
                }
            }
        }

        Ok(match cs.len() {
            0 => {
                assert!(first_source_face.is_convertible(&last_target_face));
                Self::id(first_source_face)
            }
            1 => cs.into_iter().next().unwrap(),
            _ => PureCell::Comp(axis, cs, dim),
        })
    }
}

// --- Substitution ---

impl PrimArg {
    pub fn subst(&self, mapping: &HashMap<PrimId, PrimArg>) -> PrimArg {
        match self {
            PrimArg::Cell(pc) => PrimArg::Cell(pc.subst(mapping)),
            PrimArg::Nat(n) => PrimArg::Nat(*n),
            PrimArg::Rat(bits) => PrimArg::Rat(*bits),
            PrimArg::App(id, args) => {
                let new_args: Vec<PrimArg> = args.iter().map(|a| a.subst(mapping)).collect();
                if new_args.is_empty() {
                    if let Some(replacement) = mapping.get(id) {
                        return replacement.clone();
                    }
                }
                PrimArg::App(*id, new_args)
            }
        }
    }
}

impl Shape {
    pub fn subst(&self, mapping: &HashMap<PrimId, PrimArg>) -> Shape {
        match self {
            Shape::Zero => Shape::Zero,
            Shape::Succ { source, target } => Shape::Succ {
                source: Box::new(source.subst(mapping)),
                target: Box::new(target.subst(mapping)),
            },
        }
    }
}

impl PureCell {
    pub fn extract_prim_id(&self) -> Option<PrimId> {
        match self {
            PureCell::Prim(prim, _, dim) if dim.effective == dim.in_space => Some(prim.id),
            _ => None,
        }
    }

    pub fn extract_prim(&self) -> Option<&Prim> {
        match self {
            PureCell::Prim(prim, _, dim) if dim.effective == dim.in_space => Some(prim),
            _ => None,
        }
    }

    pub fn subst(&self, mapping: &HashMap<PrimId, PrimArg>) -> PureCell {
        match self {
            PureCell::Prim(prim, shape, dim) => {
                // Cell-level fresh prim substitution
                if prim.args.is_empty() {
                    if let Some(PrimArg::Cell(replacement)) = mapping.get(&prim.id) {
                        let mut result = replacement.clone();
                        while result.dim().in_space < dim.in_space {
                            result = PureCell::id(result);
                        }
                        return result;
                    }
                }
                // Substitute in args and shape
                let new_args = prim.args.iter().map(|a| a.subst(mapping)).collect();
                let new_prim = Prim::with_args(prim.id, new_args);
                let new_shape = shape.subst(mapping);
                PureCell::Prim(new_prim, new_shape, *dim)
            }
            PureCell::Comp(axis, children, dim) => {
                let new_children = children.iter().map(|c| c.subst(mapping)).collect();
                PureCell::Comp(*axis, new_children, *dim)
            }
        }
    }
}

// --- Display ---

impl fmt::Display for PrimArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrimArg::Cell(pc) => write!(f, "{}", pc),
            PrimArg::Nat(n) => write!(f, "{}", n),
            PrimArg::Rat(bits) => write!(f, "{}", f64::from_bits(*bits)),
            PrimArg::App(id, args) => {
                write!(f, "P{}", id)?;
                if !args.is_empty() {
                    write!(f, "[")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", arg)?;
                    }
                    write!(f, "]")?;
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for PureCell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PureCell::Prim(prim, _, _) => {
                write!(f, "P{}", prim.id)?;
                if !prim.args.is_empty() {
                    write!(f, "[")?;
                    for (i, arg) in prim.args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", arg)?;
                    }
                    write!(f, "]")?;
                }
                Ok(())
            }
            PureCell::Comp(axis, children, _) => {
                write!(f, "[{}: ", axis)?;
                for (i, child) in children.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", child)?;
                }
                write!(f, "]")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pure_cell_assoc() {
        let assoc = crate::cell::tests::assoc::<PureCell>();
        assert_eq!(assoc.s().s(), assoc.t().s());
        assert_eq!(assoc.s().t(), assoc.t().t());
        let pentagon = crate::cell::tests::pentagon::<PureCell>();
        assert_eq!(pentagon.s().s(), pentagon.t().s());
        assert_eq!(pentagon.s().t(), pentagon.t().t());
    }
}
