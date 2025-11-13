use donut_core::cell::*;
use donut_util::println;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum WireStyle {
    Smooth,
    Shrink0,
    Shrink1,
}

#[derive(Debug, Clone)]
pub enum Cube {
    Point(Coord),
    Wire {
        g0: Rc<Cube>,
        x0: u32,
        g1: Rc<Cube>,
        x1: u32,
        style: WireStyle,
    },
}

impl Cube {
    pub fn dim(&self) -> usize {
        match self {
            Cube::Point(_) => 0,
            Cube::Wire { g0, .. } => g0.dim() + 1,
        }
    }

    fn shrink(&self, center: &[u32], size: &[u32]) -> Cube {
        let dim = center.len();
        assert_eq!(size.len(), dim);
        match self {
            Cube::Point(p) => {
                assert_eq!(p.len(), dim);
                let mut p = p.clone();
                for (i, p) in p.iter_mut().enumerate() {
                    if 0 < *p && *p < size[i] {
                        *p = center[i];
                    }
                }
                Cube::Point(p)
            }
            Cube::Wire {
                g0,
                x0,
                g1,
                x1,
                style,
            } => {
                assert!(dim > 0);
                let xc = center[dim - 1];
                let bound = size[dim - 1];
                let mut x0 = *x0;
                let mut x1 = *x1;
                if 0 < x0 && x0 < bound {
                    x0 = xc;
                }
                if 0 < x1 && x1 < bound {
                    x1 = xc;
                }

                Cube::Wire {
                    g0: Rc::new(g0.shrink(&center[..dim - 1], &size[..dim - 1])),
                    x0,
                    g1: Rc::new(g1.shrink(&center[..dim - 1], &size[..dim - 1])),
                    x1,
                    style: style.clone(),
                }
            }
        }
    }

    fn offset(&mut self, offset: &[u32]) {
        let dim = offset.len();
        match self {
            Cube::Point(p) => {
                assert_eq!(p.len(), dim);
                for (i, o) in offset.iter().enumerate() {
                    p[i] += *o;
                }
            }
            Cube::Wire {
                g0,
                x0,
                g1,
                x1,
                style: _,
            } => {
                *x0 += offset[dim - 1];
                *x1 += offset[dim - 1];
                let mut c0 = g0.as_ref().clone();
                c0.offset(&offset[..dim - 1]);
                *g0 = Rc::new(c0);
                let mut c1 = g1.as_ref().clone();
                c1.offset(&offset[..dim - 1]);
                *g1 = Rc::new(c1);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Element {
    pub cube: Cube,
    pub prim_id: PrimId,
}

#[derive(Debug, Clone)]
pub struct Geometry {
    pub size: Coord,
    pub elements: Vec<Vec<Element>>, // [dim][element index]
}

impl Geometry {
    pub fn new(size: Coord) -> Self {
        let elements = vec![vec![]; size.len() + 1];
        Self { size, elements }
    }

    pub fn add_element(&mut self, dim: usize, element: Element) {
        assert_eq!(dim, element.cube.dim());
        assert!(dim < self.elements.len());
        self.elements[dim].push(element);
    }

    pub fn insert(&mut self, other: Vec<Vec<Element>>) {
        assert_eq!(self.elements.len(), other.len());
        for (dim, elements) in other.into_iter().enumerate() {
            self.elements[dim].extend(elements);
        }
    }
    pub fn insert_with_offset(&mut self, offset: &Coord, other: Vec<Vec<Element>>) {
        let dim = offset.len();
        assert_eq!(dim, self.size.len());
        assert_eq!(dim + 1, self.elements.len());
        assert_eq!(dim + 1, other.len());
        for (dim, elements) in other.into_iter().enumerate() {
            for element in elements {
                let mut element = element;
                element.cube.offset(offset);
                self.elements[dim].push(element);
            }
        }
    }

    pub fn id(&self, x0: u32, x1: u32) -> Vec<Vec<Element>> {
        let mut ess = vec![vec![]; self.elements.len() + 1];
        for (dim, elements) in self.elements.iter().enumerate() {
            for element in elements {
                let face = Rc::new(element.cube.clone());
                let cube = Cube::Wire {
                    g0: Rc::clone(&face),
                    x0,
                    g1: face,
                    x1,
                    style: WireStyle::Smooth,
                };
                let prim_id = element.prim_id;
                let e = Element { cube, prim_id };
                ess[dim + 1].push(e);
            }
        }
        ess
    }
}

struct Builder {}

impl Builder {
    fn new() -> Self {
        Self {}
    }

    fn cell(&mut self, pc: &PaddedCell) -> Geometry {
        let layout = &pc.cell.1;
        let mut geometry = Geometry::new(pc.size());
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
                        let dim = pc.cell.dim();
                        assert!(dim > 0);
                        let source = self.cell(&source.extend(&pc.pad));
                        let target = self.cell(&target.extend(&pc.pad));

                        let offset = pc.pad.min[dim - 1];
                        let size = layout.size[dim - 1];
                        let bound = geometry.size[dim - 1];
                        let center = layout.size.iter().map(|s| *s / 2).collect::<Vec<_>>();
                        let center_slice = &center[..dim - 1];

                        // padding
                        if offset > 0 {
                            geometry.insert(source.id(0, offset));
                        }
                        if offset + size < bound {
                            geometry.insert(target.id(offset + size, bound));
                        }

                        // source --- self
                        for (dim, elements) in source.elements.into_iter().enumerate() {
                            for element in elements {
                                let shrinked = element.cube.shrink(center_slice, &source.size[..]);
                                let cube = Cube::Wire {
                                    g0: Rc::new(element.cube),
                                    x0: offset,
                                    g1: Rc::new(shrinked),
                                    x1: offset + size / 2,
                                    style: WireStyle::Shrink1,
                                };
                                let prim_id = element.prim_id;
                                let e = Element { cube, prim_id };
                                geometry.add_element(dim + 1, e);
                            }
                        }
                        // self --- target
                        for (dim, elements) in target.elements.into_iter().enumerate() {
                            for element in elements {
                                let shrinked = element.cube.shrink(center_slice, &target.size[..]);
                                let cube = Cube::Wire {
                                    g0: Rc::new(shrinked),
                                    x0: offset + size / 2,
                                    g1: Rc::new(element.cube),
                                    x1: offset + size,
                                    style: WireStyle::Shrink0,
                                };
                                let prim_id = element.prim_id;
                                let e = Element { cube, prim_id };
                                geometry.add_element(dim + 1, e);
                            }
                        }

                        // self
                        let mut center = layout.size.iter().map(|s| *s / 2).collect::<Vec<_>>();
                        center[dim - 1] += offset;
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
                let bound = geometry.size[dim - 1];

                let inner_pc = PaddedCell {
                    cell: Rc::clone(inner),
                    pad: pc.pad.clone(),
                };
                let inner = self.cell(&inner_pc);
                geometry.elements = inner.id(0, bound);
            }
            CellF::Comp(children, level, inner_pads) => {
                let mut offset = pc.pad.min.clone(); // vec![0; layout.size.len()];
                for (index, child) in children.iter().enumerate() {
                    let child_geometry = self.cell(child);
                    geometry.insert_with_offset(&offset, child_geometry.elements);
                    if index < children.len() - 1 {
                        offset[*level as usize] +=
                            inner_pads[index] + child.size()[*level as usize];
                    }
                }
            }
        }
        geometry
    }
}

pub fn extract_geometry(cell: &Rc<LayoutCell>) -> Geometry {
    let mut builder = Builder::new();
    builder.cell(&PaddedCell::from_cell(Rc::clone(cell)))
}
