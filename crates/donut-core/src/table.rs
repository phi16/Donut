use std::cell::RefCell;
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
        size: Vec<u32>,
        color: Color,
        source: Rc<LayoutCell>,
        target: Rc<LayoutCell>,
    ) -> Rc<LayoutCell> {
        assert_eq!(size.len(), level as usize);
        let id = self.table.len() as PrimId;
        self.id_map.insert(name.to_string(), id);
        let cell = Rc::new(LayoutCell(
            CellF::Prim(id, ShapeF::Succ(source, target)),
            Layout {
                size,
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
        ps.add("f", 1, vec![100], (64, 64, 64, 255), a, b);
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
        for b in &mut layout.children {
            b.min.push(0);
            b.max.push(len);
        }
        Rc::new(LayoutCell(cell, layout))
    }

    pub fn get(&self, id: PrimId) -> Option<&Prim> {
        self.table.get(&id)
    }
}
