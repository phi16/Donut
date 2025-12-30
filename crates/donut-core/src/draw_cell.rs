type X = crate::lins::X;

#[derive(Debug, Clone, Copy)]
pub enum Tangent {
    Perp,
    Shrink,
}

#[derive(Debug, Clone)]
pub enum Cuboid {
    Point(Vec<X>),
    Bridge {
        source: (Box<Cuboid>, X, Tangent),
        target: (Box<Cuboid>, X, Tangent),
    },
}

impl Cuboid {
    fn s(&self) -> Self {
        match self {
            Cuboid::Point(_) => unreachable!(),
            Cuboid::Bridge { source, .. } => source.0.as_ref().clone(),
        }
    }
    fn t(&self) -> Self {
        match self {
            Cuboid::Point(_) => unreachable!(),
            Cuboid::Bridge { target, .. } => target.0.as_ref().clone(),
        }
    }

    fn shift(&mut self, s: &X, t: &X) {
        let mut face = Cuboid::Point(vec![]);
        std::mem::swap(self, &mut face);
        *self = Cuboid::Bridge {
            source: (Box::new(face.clone()), s.clone(), Tangent::Perp),
            target: (Box::new(face), t.clone(), Tangent::Perp),
        };
    }
}
