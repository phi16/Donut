use nonempty::nonempty;
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
            bounds[i] = source.1.bounds[i].max(target.1.bounds[i]);
        }

        let modify_bounds = |cell: &Rc<LayoutCell>, bounds: &Coord| {
            let need_modify = bounds
                .iter()
                .enumerate()
                .any(|(i, &b)| cell.1.bounds[i] < b);
            if !need_modify {
                return Rc::clone(cell);
            }
            let mut new_cell = cell.as_ref().clone();
            for i in 0..(level as usize - 1) {
                let b = bounds[i];
                let cell_b = new_cell.1.bounds[i];
                assert!(b >= cell_b);
                if cell_b < b {
                    assert!((b - cell_b) % 2 == 0);
                    let offset = (b - cell_b) / 2;
                    new_cell.1.min_pad[i] += offset;
                    new_cell.1.max_pad[i] += offset;
                    new_cell.1.bounds[i] = b;
                }
            }
            Rc::new(new_cell)
        };

        let source = modify_bounds(source, &bounds);
        let target = modify_bounds(target, &bounds);

        bounds.push(bound);
        let mut offset = vec![0; level as usize];
        for i in 0..level as usize {
            assert!((bounds[i] - size[i]) % 2 == 0);
            offset[i] = (bounds[i] - size[i]) / 2;
        }

        let cell = Rc::new(LayoutCell(
            CellF::Prim(id, ShapeF::Succ(source, target)),
            Layout {
                size,
                min_pad: offset.clone(),
                max_pad: offset,
                bounds,
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
        let a = ps.add_zero("A", 0, (255, 192, 128, 255));
        let b = ps.add_zero("B", 0, (128, 192, 255, 255));
        let f = ps.add("f", 1, vec![10], 100, (192, 64, 64, 255), &a, &b);
        let g = ps.add("g", 1, vec![10], 100, (96, 96, 192, 255), &a, &b);
        ps.add("a", 2, vec![20, 20], 100, (255, 255, 255, 255), &f, &g);
        let i = ps.add("i", 1, vec![10], 100, (128, 128, 128, 255), &a, &a);
        ps.add("b", 2, vec![20, 20], 100, (255, 255, 255, 255), &i, &i);
        let ii = ps.comp(nonempty![Rc::clone(&i), Rc::clone(&i)], 0, 20);
        ps.add(
            "k",
            2,
            vec![20, 20],
            100,
            (64, 64, 64, 255),
            &i,
            &ps.id(&a, 100),
        );
        ps.add("m", 2, vec![20, 20], 100, (64, 64, 64, 255), &i, &ii);
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
        layout.push(len);
        Rc::new(LayoutCell(cell, layout))
    }

    pub fn comp(&self, cells: Vec1<Rc<LayoutCell>>, level: Level, len: u32) -> Rc<LayoutCell> {
        assert!(len % 2 == 0);
        let n = cells.len();
        let dim = cells.first().dim();
        let mut bounds = vec![0; dim];
        for cell in &cells {
            let layout = &cell.1;
            for i in 0..dim {
                if i == level as usize {
                    bounds[i] += layout.bounds[i];
                } else {
                    bounds[i] = bounds[i].max(layout.bounds[i]);
                }
            }
        }
        bounds[level as usize] += len * (n as u32 - 1);
        let mut cells = cells;
        for (index, cell) in cells.iter_mut().enumerate() {
            let mut new_cell = cell.as_ref().clone();
            let layout = &mut new_cell.1;
            for i in 0..dim {
                if i == level as usize {
                    // [0]-|-[1]-|-[2]
                    if index != 0 {
                        layout.min_pad[i] += len / 2;
                        layout.bounds[i] += len / 2;
                    }
                    if index != n - 1 {
                        layout.max_pad[i] += len / 2;
                        layout.bounds[i] += len / 2;
                    }
                } else {
                    let b = bounds[i];
                    let cell_b = layout.bounds[i];
                    assert!(b >= cell_b);
                    if cell_b < b {
                        assert!((b - cell_b) % 2 == 0);
                        let offset = (b - cell_b) / 2;
                        layout.min_pad[i] += offset;
                        layout.max_pad[i] += offset;
                        layout.bounds[i] = b;
                    }
                }
            }
            *cell = Rc::new(new_cell);
        }
        let layout = Layout {
            size: bounds.clone(),
            min_pad: vec![0; dim],
            max_pad: vec![0; dim],
            bounds,
        };
        Rc::new(LayoutCell(CellF::Comp(cells, level), layout))
    }

    pub fn get(&self, id: PrimId) -> Option<&Prim> {
        self.table.get(&id)
    }
}
