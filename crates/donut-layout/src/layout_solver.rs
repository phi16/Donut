use donut_core::cell::*;
use donut_core::common::*;

use crate::layout_cell::*;
use crate::lins;
use crate::lins::Lins;
use crate::visual_cell;
use crate::visual_cell::VisualCell;

pub struct LayoutSolver(Lins);

pub struct Solution(lins::Solution);

impl LayoutSolver {
    pub fn new() -> Self {
        LayoutSolver(Lins::new())
    }

    fn var(&mut self, name: String) -> X {
        X::var(self.0.fresh_var(name))
    }

    fn eq(&mut self, a: &X, b: &X) {
        self.0.add_constraint(a, b);
    }

    fn eqs(&mut self, va: &Vec<X>, vb: &Vec<X>) {
        assert_eq!(va.len(), vb.len());
        va.iter().zip(vb.iter()).for_each(|(a, b)| {
            self.eq(a, b);
        });
    }

    fn fuse_cube(&mut self, a: &Cube, b: &Cube) {
        self.eqs(&a.mins, &b.mins);
        self.eqs(&a.maxs, &b.maxs);
    }

    fn fuse_layout(&mut self, a: &Layout, b: &Layout) {
        assert_eq!(a.dim, b.dim);
        self.fuse_cube(&a.cube, &b.cube);
    }

    fn fuse(&mut self, a: &LayoutCell, b: &LayoutCell) {
        self.fuse_layout(&a.1, &b.1);
        match (a.0.as_ref(), b.0.as_ref()) {
            (RawCell::Prim(pa, sa, ca), RawCell::Prim(pb, sb, cb)) => {
                assert_eq!(pa, pb);
                match (sa, sb) {
                    (Shape::Zero, Shape::Zero) => {}
                    (
                        Shape::Succ {
                            source: sa,
                            center_doubled: ca,
                            target: ta,
                        },
                        Shape::Succ {
                            source: sb,
                            center_doubled: cb,
                            target: tb,
                        },
                    ) => {
                        self.fuse(&sa.0, &sb.0);
                        self.eq(&sa.1, &sb.1);
                        self.eqs(&ca, &cb);
                        self.fuse(&ta.0, &tb.0);
                        self.eq(&ta.1, &tb.1);
                    }
                    _ => unreachable!(),
                }
                self.fuse_cube(ca, cb);
            }
            (RawCell::Comp(aa, csa), RawCell::Comp(ab, csb)) => {
                assert_eq!(aa, ab);
                assert_eq!(csa.len(), csb.len());
                for (ca, cb) in csa.iter().zip(csb.iter()) {
                    self.fuse(ca, cb);
                }
            }
            _ => unreachable!(),
        }
    }

    fn collect_vars(&self, cell: &LayoutCell) -> X {
        let mut x = X::zero();
        fn collect(x: &mut X, cell: &LayoutCell) {
            for v in &cell.1.vars {
                *x = x.add(v);
            }
            match cell.0.as_ref() {
                RawCell::Prim(_, shape, _) => match shape {
                    Shape::Zero => {}
                    Shape::Succ { source, target, .. } => {
                        collect(x, &source.0);
                        collect(x, &target.0);
                    }
                },
                RawCell::Comp(_, children) => {
                    for child in children {
                        collect(x, child);
                    }
                }
            }
        }
        collect(&mut x, cell);
        x
    }

    pub fn solve(&mut self, cell: &LayoutCell) -> Solution {
        let d = cell.1.dim.in_space;
        let ws = (0..d)
            .map(|i| self.var(format!("Mw{}", i)))
            .collect::<Vec<_>>();
        for i in 0..d as usize {
            let min = X::zero();
            let w = &ws[i];
            let base = X::one();
            let max = min.add(&base).add(w);
            self.eq(&cell.1.cube.mins[i], &min);
            self.eq(&cell.1.cube.maxs[i], &max);
        }
        let vars = self.collect_vars(cell);
        let mut lins = Lins::new();
        std::mem::swap(&mut self.0, &mut lins);
        let sol = match lins.solve(&vars) {
            Some(s) => s,
            None => panic!("infeasible"),
        };
        Solution(sol)
    }
}

impl DiagramMap for LayoutSolver {
    type Cell = LayoutCell;

    fn zero(&mut self, prim: Prim) -> LayoutCell {
        let cell = RawCell::Prim(prim, Shape::Zero, Cube::zero());
        LayoutCell(Box::new(cell), Layout::zero())
    }

    fn prim(&mut self, prim: Prim, source: LayoutCell, target: LayoutCell) -> LayoutCell {
        self.fuse_cube(&source.1.cube, &target.1.cube);
        let d = source.dim().in_space;
        if d > 0 {
            self.fuse(&source.s(), &target.s());
            self.fuse(&source.t(), &target.t());
        }
        let dim = Dim::new(d + 1, d + 1);

        // s |--u+w--> x <----> y <--v+w--| t

        let s = self.var(format!("P[{}]s{}", prim.id, d));
        let u = self.var(format!("P[{}]u{}", prim.id, d));
        let v = self.var(format!("P[{}]v{}", prim.id, d));
        let w = self.var(format!("P[{}]w{}", prim.id, d));
        let x = s.add(&u.add(&w));
        let y = x.add(&X::one());
        let t = y.add(&v.add(&w));

        let mut cube = source.1.cube.clone();
        let center_doubled = cube // TODO: this is not an appropriate center, maybe
            .mins
            .iter()
            .zip(cube.maxs.iter())
            .map(|(s, t)| s.add(t))
            .collect::<Vec<_>>();

        let shape = Shape::Succ {
            source: (source, x),
            center_doubled,
            target: (target, y),
        };

        let cell = RawCell::Prim(prim, shape, Cube::zero());
        cube.shift(&s, &t);
        let vars = vec![s, u, v, w];
        LayoutCell(Box::new(cell), Layout { dim, cube, vars })
    }

    fn id(&mut self, face: LayoutCell) -> LayoutCell {
        let d = face.dim().in_space;
        let s = self.var(format!("Is{}", d));
        let w = self.var(format!("Iw{}", d));
        let t = s.add(&w);
        let mut cell = face;
        cell.shift(&s, &t);
        cell.1.vars.push(s);
        cell.1.vars.push(w);
        cell
    }

    fn comp(&mut self, axis: Axis, children: Vec2<LayoutCell>) -> LayoutCell {
        let n = children.len();
        for i in 0..n - 1 {
            let t = children[i].face(axis, Side::Target);
            let s = children[i + 1].face(axis, Side::Source);
            assert!(t.is_convertible(&s));
            self.fuse(&t, &s);
        }
        let mins = children[0].1.cube.mins.clone();
        let maxs = children[n - 1].1.cube.maxs.clone();
        let (cell, dim) = LayoutCell::comp_unchecked(axis, children);

        let cube = Cube { mins, maxs };
        let layout = Layout {
            dim,
            cube,
            vars: vec![],
        };
        let mut c = LayoutCell(cell, layout);

        // s |--u+w--> x <----> y <--v+w--| t

        let s = self.var(format!("Cs{}", axis));
        let u = self.var(format!("Cu{}", axis));
        let v = self.var(format!("Cv{}", axis));
        let w = self.var(format!("Cw{}", axis));

        let x = s.add(&u.add(&w));
        self.eq(&c.1.cube.mins[axis as usize], &x);
        let y = c.1.cube.maxs[axis as usize].clone();
        let t = y.add(&v.add(&w));

        c.move_face_to(axis, Side::Source, &s);
        c.move_face_to(axis, Side::Target, &t);
        c.1.vars.push(s);
        c.1.vars.push(u);
        c.1.vars.push(v);
        c.1.vars.push(w);
        c
    }
}

impl Solution {
    pub fn convert(&self, cell: &LayoutCell) -> VisualCell {
        fn convert_cube(cube: &Cube, sol: &lins::Solution) -> visual_cell::Cube {
            let mins = cube.mins.iter().map(|x| sol.eval(x)).collect();
            let maxs = cube.maxs.iter().map(|x| sol.eval(x)).collect();
            visual_cell::Cube { mins, maxs }
        }
        fn convert_cell(cell: &LayoutCell, sol: &lins::Solution) -> VisualCell {
            let layout = visual_cell::Layout {
                dim: cell.1.dim,
                cube: convert_cube(&cell.1.cube, sol),
            };
            let cell = match cell.0.as_ref() {
                RawCell::Prim(prim, shape, cube) => {
                    let draw_shape = match shape {
                        Shape::Zero => visual_cell::Shape::Zero,
                        Shape::Succ {
                            source,
                            center_doubled,
                            target,
                        } => visual_cell::Shape::Succ {
                            source: (convert_cell(&source.0, sol), sol.eval(&source.1)),
                            center: center_doubled.iter().map(|x| sol.eval(x) / 2).collect(),
                            target: (convert_cell(&target.0, sol), sol.eval(&target.1)),
                        },
                    };
                    let draw_cube = convert_cube(cube, sol);
                    visual_cell::RawCell::Prim(prim.clone(), draw_shape, draw_cube)
                }
                RawCell::Comp(axis, children) => {
                    let draw_children = children
                        .iter()
                        .map(|child| convert_cell(child, sol))
                        .collect();
                    visual_cell::RawCell::Comp(*axis, draw_children)
                }
            };
            VisualCell(Box::new(cell), layout)
        }
        convert_cell(cell, &self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assoc_layout() {
        use crate::cell::tests::*;
        let mut cb = LayoutSolver::new();
        let a = pentagon::<FreeCell>();
        let cell = cb.convert(a);
        let sol = cb.solve(&cell);
        let cell = sol.convert(&cell);
        // eprintln!("assoc layout:\n{}", cell);
        assert!(false);
    }
}
