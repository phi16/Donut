use nonempty::{nonempty, NonEmpty};
use std::collections::HashMap;
use std::rc::Rc;
use std::vec;

use crate::cell::*;

pub type Color = (u8, u8, u8, u8);
pub struct Prim {
    pub name: String,
    pub level: Level,
    pub color: Color,
    pub base_cell: Rc<LayoutCell>,
}

pub struct PrimTable {
    id_map: HashMap<String, PrimId>,
    table: HashMap<PrimId, Prim>,
}

impl PrimTable {
    fn new() -> Self {
        PrimTable {
            id_map: HashMap::new(),
            table: HashMap::new(),
        }
    }

    fn add_zero(&mut self, name: &str, level: Level, color: Color) -> Rc<LayoutCell> {
        let id = self.table.len() as PrimId;
        self.id_map.insert(name.to_string(), id);
        let cell = Rc::new(LayoutCell(CellF::Prim(id, ShapeF::Zero), Layout::zero()));
        let p = Prim {
            name: name.to_string(),
            level,
            color,
            base_cell: Rc::clone(&cell),
        };
        self.table.insert(id, p);
        cell
    }

    fn add(
        &mut self,
        name: &str,
        level: Level,
        size: Coord,
        bound: u32,
        color: Color,
        source: &Rc<LayoutCell>,
        target: &Rc<LayoutCell>,
    ) -> Rc<LayoutCell> {
        assert_eq!(size.len(), level as usize);
        let id = self.table.len() as PrimId;
        self.id_map.insert(name.to_string(), id);

        let mut bounds = vec![0; level as usize - 1];
        for i in 0..(level as usize - 1) {
            bounds[i] = source.1.size[i].max(target.1.size[i]);
        }

        let pad_to_bounds = |cell: &Rc<LayoutCell>, bounds: &Coord| -> PaddedCell {
            let mut pad = vec![0; level as usize - 1];
            for i in 0..(level as usize - 1) {
                let b = bounds[i];
                let s = cell.1.size[i];
                assert!((b - s) % 2 == 0);
                pad[i] = (b - s) / 2;
            }
            PaddedCell {
                cell: Rc::clone(cell),
                pad: Padding::centered(pad),
            }
        };

        let source = pad_to_bounds(source, &bounds);
        let target = pad_to_bounds(target, &bounds);

        bounds.push(bound);

        let cell = Rc::new(LayoutCell(
            CellF::Prim(id, ShapeF::Succ(source, target)),
            Layout {
                size: bounds.clone(),
            },
        ));
        let p = Prim {
            name: name.to_string(),
            level,
            color,
            base_cell: Rc::clone(&cell),
        };
        self.table.insert(id, p);
        cell
    }

    pub fn default() -> Self {
        let mut ps = Self::new();
        let a = ps.add_zero("A", 0, (32, 32, 128, 192));
        let b = ps.add_zero("B", 0, (128, 192, 255, 255));
        let f = ps.add("f", 1, vec![10], 100, (192, 64, 64, 255), &a, &b);
        let g = ps.add("g", 1, vec![10], 100, (96, 96, 192, 255), &a, &b);
        ps.add("a", 2, vec![20, 20], 100, (255, 255, 255, 255), &f, &g);
        let i = ps.add("i", 1, vec![10], 100, (64, 128, 255, 255), &a, &a);
        ps.add("b", 2, vec![20, 20], 100, (255, 255, 255, 255), &i, &i);
        let ii = ps.comp(nonempty![Rc::clone(&i), Rc::clone(&i)], 0, 20);
        let ij = ps.comp(nonempty![Rc::clone(&i), Rc::clone(&i)], 0, 40);
        ps.add(
            "k",
            2,
            vec![20, 20],
            100,
            (64, 64, 64, 255),
            &i,
            &ps.id(&a, 100),
        );
        ps.add("m", 2, vec![20, 20], 100, (255, 128, 128, 255), &i, &ii);
        ps.add("w", 2, vec![20, 20], 100, (128, 255, 128, 255), &ij, &i);

        let f = ps.add("F", 1, vec![10], 50, (192, 64, 64, 255), &a, &b);
        let g = ps.add("G", 1, vec![10], 100, (64, 128, 255, 255), &a, &b);
        let u = ps.add("U", 2, vec![20, 20], 100, (255, 255, 255, 255), &f, &g);
        ps
    }

    pub fn prim(&self, name: &str) -> Rc<LayoutCell> {
        let prim_id = self
            .id_map
            .get(name)
            .expect(&format!("Prim \"{}\" not found", name));
        let prim = self.table.get(prim_id).unwrap();
        Rc::clone(&prim.base_cell)
    }

    pub fn id(&self, cell: &Rc<LayoutCell>, len: u32) -> Rc<LayoutCell> {
        let mut layout = cell.1.clone();
        let cell = CellF::Id(Rc::clone(cell));
        layout.size.push(len);
        Rc::new(LayoutCell(cell, layout))
    }

    pub fn comp(&self, cells: Vec1<Rc<LayoutCell>>, level: Level, len: u32) -> Rc<LayoutCell> {
        let n = cells.len();
        let dim = cells.first().dim();
        let mut bounds = vec![0; dim];
        for cell in &cells {
            let layout = &cell.1;
            for i in 0..dim {
                if i == level as usize {
                    bounds[i] += layout.size[i];
                } else {
                    bounds[i] = bounds[i].max(layout.size[i]);
                }
            }
        }
        let mut pcs = vec![];
        for cell in cells {
            let mut pad = vec![0; dim];
            for i in 0..dim {
                if i != level as usize {
                    let b = bounds[i];
                    let s = cell.1.size[i];
                    assert!((b - s) % 2 == 0);
                    pad[i] = (b - s) / 2;
                }
            }
            pcs.push(PaddedCell {
                cell: Rc::clone(&cell),
                pad: Padding::centered(pad),
            });
        }
        let inner_pads = vec![len; n - 1];
        bounds[level as usize] += len * (n as u32 - 1);
        let layout = Layout { size: bounds };
        Rc::new(LayoutCell(
            CellF::Comp(NonEmpty::from_vec(pcs).unwrap(), level, inner_pads),
            layout,
        ))
    }

    pub fn get(&self, id: PrimId) -> Option<&Prim> {
        self.table.get(&id)
    }
}
