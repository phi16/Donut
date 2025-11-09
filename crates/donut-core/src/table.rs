use nonempty::nonempty;
use std::collections::HashMap;
use std::rc::Rc;

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
        bounds: Coord,
        color: Color,
        source: &Rc<LayoutCell>,
        target: &Rc<LayoutCell>,
    ) -> Rc<LayoutCell> {
        assert_eq!(size.len(), level as usize);
        let id = self.table.len() as PrimId;
        self.id_map.insert(name.to_string(), id);
        let mut offset = vec![0; level as usize];
        for i in 0..level as usize {
            offset[i] = (bounds[i] - size[i]) / 2;
        }

        let source = source.as_ref().clone();
        let target = target.as_ref().clone();

        let cell = Rc::new(LayoutCell(
            CellF::Prim(id, ShapeF::Succ(Rc::new(source), Rc::new(target))),
            Layout {
                size,
                offset,
                bounds,
                children: vec![],
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
        let f = ps.add("f", 1, vec![10], vec![100], (192, 64, 64, 255), &a, &b);
        let g = ps.add("g", 1, vec![10], vec![100], (96, 96, 192, 255), &a, &b);
        ps.add(
            "a",
            2,
            vec![20, 20],
            vec![100, 100],
            (255, 255, 255, 255),
            &f,
            &g,
        );
        let i = ps.add("i", 1, vec![10], vec![100], (128, 128, 128, 255), &a, &a);
        ps.add(
            "b",
            2,
            vec![20, 20],
            vec![100, 100],
            (255, 255, 255, 255),
            &i,
            &i,
        );
        let ii = ps.comp(nonempty![Rc::clone(&i), Rc::clone(&f)], 0, 20);
        ps.add(
            "k",
            2,
            vec![20, 20],
            vec![100, 100],
            (64, 64, 64, 255),
            &i,
            &ps.id(&a, 100),
        );
        ps.add(
            "m",
            2,
            vec![20, 20],
            vec![100, 100],
            (64, 64, 64, 255),
            &i,
            &ii,
        );
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
        bounds[level as usize] += len * (cells.len() as u32 - 1);
        let mut offset = 0;
        let mut children = vec![];
        for cell in &cells {
            let layout = &cell.1;
            let mut min = vec![0; dim];
            let mut max = vec![0; dim];
            for i in 0..dim {
                if i == level as usize {
                    min[i] = offset;
                    max[i] = min[i] + layout.bounds[i];
                    offset += layout.bounds[i] + len;
                } else {
                    min[i] = (bounds[i] - layout.bounds[i]) / 2;
                    max[i] = min[i] + layout.bounds[i];
                }
            }
            children.push(Bounds { min, max });
        }
        let layout = Layout {
            size: bounds.clone(),
            offset: vec![0; dim],
            bounds,
            children,
        };
        Rc::new(LayoutCell(CellF::Comp(cells, level), layout))
    }

    pub fn get(&self, id: PrimId) -> Option<&Prim> {
        self.table.get(&id)
    }
}
