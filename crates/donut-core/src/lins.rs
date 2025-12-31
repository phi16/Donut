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
    pub fn var(id: Id) -> Self {
        X(vec![Factor::Var(id)])
    }

    pub fn k(n: N) -> Self {
        X(vec![Factor::Const(n)])
    }

    pub fn add(&self, other: &X) -> X {
        let mut factors = self.0.clone();
        factors.extend(other.0.iter().cloned());
        X(factors)
    }
}

impl std::fmt::Debug for X {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

    pub fn solve(self) -> Option<Solution> {
        eprintln!("Constraints: {{");
        for (l, r) in &self.constraints {
            eprintln!("  {:?} = {:?},", l, r);
        }
        eprintln!("}}");
        unimplemented!()
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
