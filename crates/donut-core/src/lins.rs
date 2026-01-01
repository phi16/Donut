use crate::common::{N, Q};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(u64);

#[derive(Debug, Clone)]
pub enum Factor {
    Var(Id),
    Const(N),
}

#[derive(Clone)]
pub struct X(Vec<Factor>);

pub struct Lins {
    next_id: u64,
    names: HashMap<Id, String>,
    constraints: Vec<(X, X)>,
}

pub struct Cloner<'a> {
    lins: &'a mut Lins,
    map: HashMap<Id, Id>,
}

pub struct Solution {
    pub map: std::collections::HashMap<Id, Q>,
}

impl X {
    pub fn zero() -> Self {
        X(vec![])
    }

    pub fn var(id: Id) -> Self {
        X(vec![Factor::Var(id)])
    }

    pub fn k(n: N) -> Self {
        X(vec![Factor::Const(n)])
    }

    pub fn one() -> Self {
        X::k(1)
    }

    pub fn add(&self, other: &X) -> X {
        let mut factors = self.0.clone();
        factors.extend(other.0.iter().cloned());
        X(factors)
    }
}

impl std::fmt::Debug for X {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() {
            return write!(f, "0");
        }
        let parts: Vec<String> = self
            .0
            .iter()
            .map(|factor| match factor {
                Factor::Var(id) => format!("x{}", id.0),
                Factor::Const(n) => format!("{}", n),
            })
            .collect();
        write!(f, "{}", parts.join(" + "))
    }
}

impl Lins {
    pub fn new() -> Self {
        Lins {
            next_id: 0,
            names: HashMap::new(),
            constraints: Vec::new(),
        }
    }

    pub fn fresh_var(&mut self, name: String) -> Id {
        let id = self.next_id;
        let id = Id(id);
        self.next_id += 1;
        self.names.insert(id, name);
        id
    }

    pub fn add_constraint(&mut self, a: &X, b: &X) {
        self.constraints.push((a.clone(), b.clone()));
    }

    pub fn cloner(&mut self) -> Cloner<'_> {
        Cloner {
            lins: self,
            map: HashMap::new(),
        }
    }

    fn show(&self, x: &X) -> String {
        if x.0.is_empty() {
            return "0".to_string();
        }
        let parts: Vec<String> =
            x.0.iter()
                .map(|factor| match factor {
                    Factor::Var(id) => {
                        format!("{}(x{})", self.names.get(id).unwrap(), id.0)
                    }
                    Factor::Const(n) => format!("{}", n),
                })
                .collect();
        parts.join(" + ")
    }

    pub fn solve(self, minimize: &X) -> Option<Solution> {
        eprintln!("Constraints = {{");
        for (l, r) in &self.constraints {
            eprintln!("  {} = {},", self.show(l), self.show(r));
        }
        eprintln!("}}");
        eprintln!("Minimize: {}", self.show(minimize));

        use microlp::{ComparisonOp, LinearExpr, OptimizationDirection, Problem};
        let mut p = Problem::new(OptimizationDirection::Minimize);
        let vars = (0..self.next_id)
            .map(|_| p.add_var(1.0, (0.0, f64::INFINITY)))
            .collect::<Vec<_>>();
        for (l, r) in &self.constraints {
            let mut lhs: HashMap<Id, i32> = HashMap::new();
            let mut rhs = 0.0;
            for factor in &l.0 {
                match factor {
                    Factor::Var(id) => {
                        lhs.entry(id.clone()).and_modify(|e| *e += 1).or_insert(1);
                    }
                    Factor::Const(n) => {
                        rhs -= *n as f64;
                    }
                }
            }
            for factor in &r.0 {
                match factor {
                    Factor::Var(id) => {
                        lhs.entry(id.clone()).and_modify(|e| *e -= 1).or_insert(-1);
                    }
                    Factor::Const(n) => {
                        rhs += *n as f64;
                    }
                }
            }
            let mut e = LinearExpr::empty();
            for (id, coeff) in lhs {
                e.add(vars[id.0 as usize], coeff as f64);
            }
            p.add_constraint(e, ComparisonOp::Eq, rhs);
        }
        let solution = p.solve().ok()?;
        eprintln!("Solution found: {:?}", solution);
        for i in 0..self.next_id {
            eprintln!(
                "  {}(x{}) = {}",
                self.names.get(&Id(i)).unwrap(),
                i,
                solution[vars[i as usize]]
            );
        }

        Some(Solution {
            map: (0..self.next_id)
                .map(|i| {
                    let value = solution[vars[i as usize]];
                    (Id(i), Q::approximate_float(value).unwrap())
                })
                .collect(),
        })
    }
}

impl<'a> Cloner<'a> {
    pub fn clone(&mut self, x: &X) -> X {
        let mut new_factors = Vec::new();
        for factor in &x.0 {
            match factor {
                Factor::Var(id) => {
                    let new_id = if let Some(mapped_id) = self.map.get(id) {
                        mapped_id.clone()
                    } else {
                        let name = self.lins.names.get(id).unwrap().clone();
                        let fresh_id = self.lins.fresh_var(name);
                        self.map.insert(id.clone(), fresh_id.clone());
                        fresh_id
                    };
                    new_factors.push(Factor::Var(new_id));
                }
                Factor::Const(n) => {
                    new_factors.push(Factor::Const(*n));
                }
            }
        }
        X(new_factors)
    }

    pub fn translate(&self, x: &X) -> Option<X> {
        let mut new_factors = Vec::new();
        for factor in &x.0 {
            match factor {
                Factor::Var(id) => match self.map.get(id) {
                    Some(new_id) => new_factors.push(Factor::Var(new_id.clone())),
                    None => return None,
                },
                Factor::Const(n) => {
                    new_factors.push(Factor::Const(*n));
                }
            }
        }
        Some(X(new_factors))
    }

    pub fn drop(self) {
        let mut new_constraints = vec![];
        let n = self.lins.constraints.len();
        for i in 0..n {
            let (l, r) = &self.lins.constraints[i];
            if let Some(l_new) = self.translate(l) {
                if let Some(r_new) = self.translate(r) {
                    new_constraints.push((l_new, r_new));
                }
            }
        }
        self.lins.constraints.append(&mut new_constraints);
    }
}

impl Solution {
    pub fn eval(&self, x: &X) -> Q {
        let mut result = Q::from(0);
        for factor in x.0.iter() {
            match factor {
                Factor::Var(id) => {
                    result += Q::from(*self.map.get(id).unwrap());
                }
                Factor::Const(n) => {
                    result += Q::from(*n as i32);
                }
            }
        }
        result
    }
}
