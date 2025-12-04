use donut_core::cell::*;
use donut_util::println;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum EndStyle {
    Smooth,
    Shrink,
}

#[derive(Debug, Clone)]
pub struct WireStyle {
    pub s0: EndStyle,
    pub s1: EndStyle,
}

impl WireStyle {
    pub const Smooth: Self = Self {
        s0: EndStyle::Smooth,
        s1: EndStyle::Smooth,
    };

    pub const Shrink0: Self = Self {
        s0: EndStyle::Shrink,
        s1: EndStyle::Smooth,
    };

    pub const Shrink1: Self = Self {
        s0: EndStyle::Smooth,
        s1: EndStyle::Shrink,
    };
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
                let mut g0 = Rc::clone(g0);
                let mut x1 = *x1;
                let mut g1 = Rc::clone(g1);
                let mut style = style.clone();
                if 0 < x0 && x0 < bound {
                    x0 = xc;
                    g0 = Rc::new(g0.shrink(&center[..dim - 1], &size[..dim - 1]));
                } else {
                    style.s1 = EndStyle::Shrink; // TODO
                }
                if 0 < x1 && x1 < bound {
                    x1 = xc;
                    g1 = Rc::new(g1.shrink(&center[..dim - 1], &size[..dim - 1]));
                } else {
                    style.s0 = EndStyle::Shrink; // TODO
                }

                Cube::Wire {
                    g0,
                    x0,
                    g1,
                    x1,
                    style,
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

    fn cell(&mut self, pc: &LayoutCell) -> Geometry {
        let layout = &pc.1;
        let mut geometry = Geometry::new(pc.size());
        match pc.0.as_ref() {
            Cell::Prim(prim_id, shape) => {
                let prim_id = *prim_id;
                match shape {
                    Shape::Zero => {
                        let point = Cube::Point(vec![]);
                        geometry.add_element(
                            0,
                            Element {
                                cube: point,
                                prim_id,
                            },
                        );
                    }
                    Shape::Succ(source, target) => {
                        let dim = pc.dim();
                        assert!(dim > 0);
                        let source = self.cell(&source.extend(&pc.1.pad));
                        let target = self.cell(&target.extend(&pc.1.pad));

                        let offset = pc.1.pad.min[dim - 1];
                        let size = layout.size[dim - 1];
                        let bound = geometry.size[dim - 1];
                        let center = layout
                            .size
                            .iter()
                            .zip(pc.1.pad.min.iter())
                            .map(|(s, p)| *s / 2 + *p)
                            .collect::<Vec<_>>();
                        let center_slice = &center[..dim - 1];

                        // always insert paddings to keep geometry structure always the same
                        geometry.insert(source.id(0, offset));
                        geometry.insert(target.id(offset + size, bound));

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
            Cell::Id(inner) => {
                let dim = layout.size.len();
                assert!(dim > 0);
                let bound = geometry.size[dim - 1];

                let mut inner_layout = inner.1.clone();
                inner_layout.pad = pc.1.pad.clone();
                let inner_pc = LayoutCell(Rc::clone(&inner.0), inner_layout);
                let inner = self.cell(&inner_pc);
                geometry.elements = inner.id(0, bound);
            }
            Cell::Comp(level, children, inner_pads) => {
                let n = children.len();
                let mut pad = pc.1.pad.clone();
                let children = children
                    .iter()
                    .enumerate()
                    .map(|(index, c)| {
                        pad.min[*level as usize] = if index == 0 {
                            pc.1.pad.min[*level as usize]
                        } else {
                            0
                        };
                        pad.max[*level as usize] = if index == n - 1 {
                            pc.1.pad.max[*level as usize]
                        } else {
                            0
                        };
                        c.extend(&pad)
                    })
                    .collect::<Vec<_>>();
                let mut offset = vec![0; layout.size.len()];
                for index in 0..n {
                    let child = &children[index];
                    let child_geometry = self.cell(&child);
                    geometry.insert_with_offset(&offset, child_geometry.elements);
                    offset[*level as usize] += child.size()[*level as usize];
                    if index < children.len() - 1 {
                        let width = inner_pads[index];
                        let mut bridge_size = child.size();
                        bridge_size[*level as usize] = width;
                        let g = self.bridge(
                            &child.target(*level),
                            &children[index + 1].source(*level),
                            *level,
                            bridge_size,
                        );
                        geometry.insert_with_offset(&offset, g.elements);
                        offset[*level as usize] += width;
                    }
                }
            }
        }
        geometry
    }

    fn bridge(&mut self, s: &LayoutCell, t: &LayoutCell, level: Level, size: Coord) -> Geometry {
        // level:         2
        // s.size: [A, B, X]
        // t.size: [A, B, Y]
        // size:   [A, B, Z, D, E]
        assert_eq!(s.dim(), t.dim());
        assert_eq!(s.dim(), level as usize);
        assert_eq!(size[..level as usize], s.size()[..level as usize]);
        assert_eq!(size[..level as usize], t.size()[..level as usize]);
        if (level as usize) + 1 < s.size().len() {
            assert_eq!(
                s.size()[level as usize + 1..],
                t.size()[level as usize + 1..]
            );
        }

        // contains only Prims and Comps (different level)
        // Note: Id-only case? (id-id-id ?)
        /* fn extract(pc: &LayoutCell, level: Level) -> Vec<LayoutCell> {
            match &pc.0 {
                Cell::Prim(_, _) => unimplemented!(),
                Cell::Id(inner) => {
                    unimplemented!()
                }
                Cell::Comp(l, ref children, _) if l != level => children
                    .iter()
                    .map(|c| {
                        let mut child_layout = c.1.clone();
                        child_layout.pad = pc.1.pad.clone();
                        let child_pc = LayoutCell(Rc::clone(&c.0), child_layout);
                        extract(&child_pc, level)
                    })
                    .flatten()
                    .collect(),
                _ => panic!(
                    "Bridge source/target must contain only Prims and Comps (different level)"
                ),
            }
            unimplemented!()
        } */

        println(&format!("Bridge: level={}, size={:?}", level, size));
        Geometry::new(size)
    }
}

pub fn extract_geometry(cell: &LayoutCell) -> Geometry {
    let mut builder = Builder::new();
    builder.cell(cell)
}
