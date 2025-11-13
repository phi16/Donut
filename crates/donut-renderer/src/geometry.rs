use donut_core::cell::*;
use donut_util::println;
use std::rc::Rc;

#[derive(Debug)]
pub enum WireStyle {
    Smooth,
    Shrink0,
    Shrink1,
}

#[derive(Debug)]
pub enum Cube {
    Point(Coord),
    Wire {
        x0: u32,
        g0: Rc<Cube>,
        x1: u32,
        g1: Rc<Cube>,
        shape: WireStyle,
    },
}

impl Cube {
    pub fn dim(&self) -> usize {
        match self {
            Cube::Point(_) => 0,
            Cube::Wire { g0, g1, .. } => g0.dim().max(g1.dim()) + 1,
        }
    }
}

#[derive(Debug)]
pub struct Element {
    // pub offset: Coord,
    pub cube: Cube,
    pub prim_id: PrimId,
}

#[derive(Debug)]
pub struct Geometry {
    pub elements: Vec<Vec<Element>>, // [dim][element index]
}

impl Geometry {
    pub fn new() -> Self {
        Self { elements: vec![] }
    }

    pub fn add_element(&mut self, dim: usize, element: Element) {
        assert_eq!(dim, element.cube.dim());
        while self.elements.len() <= dim {
            self.elements.push(vec![]);
        }
        self.elements[dim].push(element);
    }
}

struct Builder {}

impl Builder {
    fn new() -> Self {
        Self {}
    }

    fn cell(&mut self, pc: &PaddedCell) -> Geometry {
        let layout = &pc.cell.1;
        let mut geometry = Geometry::new();
        match &pc.cell.0 {
            CellF::Prim(prim_id, shape) => {
                let prim_id = *prim_id;
                match shape {
                    ShapeF::Zero => {
                        let point = Cube::Point(vec![]);
                        geometry.add_element(
                            0,
                            Element {
                                cube: point,
                                prim_id,
                            },
                        );
                    }
                    ShapeF::Succ(source, target) => {
                        let dim = layout.size.len();
                        assert!(dim > 0);
                        let source = self.cell(&source.extend(&pc.pad));
                        let target = self.cell(&target.extend(&pc.pad));

                        // assert_eq!(pc.pad.min[dim - 1], 0);
                        // assert_eq!(pc.pad.max[dim - 1], 0);

                        let size = layout.size[dim - 1];
                        let center = layout.size.iter().map(|s| *s / 2).collect::<Vec<_>>();
                        let center_point = {
                            let mut center_slice = center.clone();
                            center_slice.pop();
                            Rc::new(Cube::Point(center_slice.clone()))
                        };
                        // Note: this^ is not sufficient...

                        // source --- self
                        for (dim, elements) in source.elements.into_iter().enumerate() {
                            for element in elements {
                                let cube = Cube::Wire {
                                    x0: 0,
                                    g0: Rc::new(element.cube),
                                    x1: size / 2,
                                    g1: Rc::clone(&center_point),
                                    shape: WireStyle::Smooth,
                                };
                                let prim_id = element.prim_id;
                                let e = Element { cube, prim_id };
                                geometry.add_element(dim + 1, e);
                            }
                        }
                        // self --- target
                        for (dim, elements) in target.elements.into_iter().enumerate() {
                            for element in elements {
                                let cube = Cube::Wire {
                                    x0: size / 2,
                                    g0: Rc::clone(&center_point),
                                    x1: size,
                                    g1: Rc::new(element.cube),
                                    shape: WireStyle::Smooth,
                                };
                                let prim_id = element.prim_id;
                                let e = Element { cube, prim_id };
                                geometry.add_element(dim + 1, e);
                            }
                        }

                        // self
                        let center = layout.size.iter().map(|s| *s / 2).collect::<Vec<_>>();
                        let point = Cube::Point(center);
                        geometry.add_element(
                            0,
                            Element {
                                cube: point,
                                prim_id,
                            },
                        );
                    }
                }
            }
            CellF::Id(inner) => {
                let dim = layout.size.len();
                assert!(dim > 0);
                let level = dim - 1;
                let bound = pc.pad.min[level] + layout.size[level] + pc.pad.max[level];

                let inner_pc = PaddedCell {
                    cell: Rc::clone(inner),
                    pad: pc.pad.clone(),
                };
                let inner = self.cell(&inner_pc);
                for (dim, elements) in inner.elements.into_iter().enumerate() {
                    for element in elements {
                        let face = Rc::new(element.cube);
                        let cube = Cube::Wire {
                            x0: 0,
                            g0: face.clone(),
                            x1: bound,
                            g1: face,
                            shape: WireStyle::Smooth,
                        };
                        let prim_id = element.prim_id;
                        let e = Element { cube, prim_id };
                        geometry.add_element(dim + 1, e);
                    }
                }
            }
            CellF::Comp(points, level, subcell_ids) => {
                unimplemented!()
            }
        }
        geometry
    }
}

pub fn extract_geometry(cell: &Rc<LayoutCell>) -> Geometry {
    let mut builder = Builder::new();
    builder.cell(&PaddedCell::from_cell(Rc::clone(cell)))
}
