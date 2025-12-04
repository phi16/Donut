use donut_core::cell::*;
use donut_core::table::*;
use donut_util::println;
use std::rc::Rc;

use crate::geometry_old::Cube;
use crate::geometry_old::Element;
use crate::geometry_old::Geometry;
use crate::geometry_old::{EndStyle, WireStyle};

type R = f32;
type RCoord = Vec<R>;

fn lerp(a: R, b: R, r: R) -> R {
    a * (1.0 - r) + b * r
}

#[derive(Debug, Clone)]
pub struct RWireStyle {
    // 0: smooth, 1: shrink
    pub s0: R,
    pub s1: R,
}

impl RWireStyle {
    pub fn from(style: &WireStyle) -> Self {
        let s0 = match style.s0 {
            EndStyle::Smooth => 0.0,
            EndStyle::Shrink => 1.0,
        };
        let s1 = match style.s1 {
            EndStyle::Smooth => 0.0,
            EndStyle::Shrink => 1.0,
        };
        RWireStyle { s0, s1 }
    }

    pub fn lerp(a: &RWireStyle, b: &RWireStyle, r: R) -> Self {
        RWireStyle {
            s0: lerp(a.s0, b.s0, r),
            s1: lerp(a.s1, b.s1, r),
        }
    }
}

#[derive(Debug, Clone)]
pub enum RCube {
    Point(RCoord),
    Wire {
        g0: Rc<RCube>,
        x0: R,
        g1: Rc<RCube>,
        x1: R,
        style: RWireStyle,
    },
}

impl RCube {
    pub fn from(cube: &Cube) -> Self {
        match cube {
            Cube::Point(p) => RCube::Point(p.iter().map(|&x| x as R).collect()),
            Cube::Wire {
                g0,
                x0,
                g1,
                x1,
                style,
            } => RCube::Wire {
                g0: Rc::new(RCube::from(g0)),
                x0: *x0 as R,
                g1: Rc::new(RCube::from(g1)),
                x1: *x1 as R,
                style: RWireStyle::from(style),
            },
        }
    }

    pub fn lerp(a: &RCube, b: &RCube, r: R) -> Self {
        match (a, b) {
            (RCube::Point(p0), RCube::Point(p1)) => {
                let p: RCoord = p0
                    .iter()
                    .zip(p1.iter())
                    .map(|(&x0, &x1)| lerp(x0, x1, r))
                    .collect();
                RCube::Point(p)
            }
            (
                RCube::Wire {
                    g0: g0a,
                    x0: x0a,
                    g1: g1a,
                    x1: x1a,
                    style: style1a,
                },
                RCube::Wire {
                    g0: g0b,
                    x0: x0b,
                    g1: g1b,
                    x1: x1b,
                    style: style1b,
                },
            ) => {
                let g0 = Rc::new(RCube::lerp(g0a.as_ref(), g0b.as_ref(), r));
                let x0 = lerp(*x0a, *x0b, r);
                let g1 = Rc::new(RCube::lerp(g1a.as_ref(), g1b.as_ref(), r));
                let x1 = lerp(*x1a, *x1b, r);
                RCube::Wire {
                    g0,
                    x0,
                    g1,
                    x1,
                    style: RWireStyle::lerp(style1a, style1b, r),
                }
            }
            _ => panic!(),
        }
    }

    pub fn slice(&self, slice: R) -> Option<Self> {
        match self {
            RCube::Point(_) => None, // TODO!
            RCube::Wire {
                g0,
                x0,
                g1,
                x1,
                style: _,
            } => {
                if slice < *x0 || slice > *x1 {
                    return None;
                }
                let r = (slice - *x0) / (*x1 - *x0);
                // TODO: compute `r` using `style`
                Some(RCube::lerp(g0.as_ref(), g1.as_ref(), r))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct RElement {
    pub cube: RCube,
    pub prim_id: PrimId,
}

impl RElement {
    pub fn from(element: &Element) -> Self {
        RElement {
            cube: RCube::from(&element.cube),
            prim_id: element.prim_id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct RGeometry {
    pub size: Coord,
    pub elements: Vec<Vec<RElement>>, // [dim][element index]
}

impl RGeometry {
    pub fn from(geometry: &Geometry) -> Self {
        RGeometry {
            size: geometry.size.clone(),
            elements: geometry
                .elements
                .iter()
                .map(|elements| elements.iter().map(RElement::from).collect())
                .collect(),
        }
    }

    pub fn slice(&self, slice: R) -> Self {
        let mut size = self.size.clone();
        size.pop();
        let elements = self
            .elements
            .iter()
            .skip(1)
            .map(|elements| {
                elements
                    .iter()
                    .flat_map(|element| {
                        element.cube.slice(slice).map(|cube| RElement {
                            cube,
                            prim_id: element.prim_id,
                        })
                    })
                    .collect()
            })
            .collect();

        RGeometry { size, elements }
    }
}

pub struct Renderer {
    context: web_sys::CanvasRenderingContext2d,
    prim_table: Rc<PrimTable>,
}

impl Renderer {
    pub fn new(context: web_sys::CanvasRenderingContext2d, prim_table: Rc<PrimTable>) -> Self {
        Self {
            context,
            prim_table,
        }
    }

    pub fn push(&self) {
        self.context.save();
    }

    pub fn pop(&self) {
        self.context.restore();
    }

    pub fn set_fill_color(&self, color: (u8, u8, u8, u8)) {
        let (r, g, b, a) = color;
        let str = if a == 255 {
            format!("rgb({} {} {})", r, g, b)
        } else {
            format!("rgb({} {} {} / {})", r, g, b, a as f64 / 255.0)
        };
        self.context.set_fill_style_str(&str);
    }

    pub fn set_stroke_color(&self, color: (u8, u8, u8, u8), width: u32) {
        let (r, g, b, a) = color;
        let str = if a == 255 {
            format!("rgb({} {} {})", r, g, b)
        } else {
            format!("rgb({} {} {} / {})", r, g, b, a as f64 / 255.0)
        };
        self.context.set_stroke_style_str(&str);
        self.context.set_line_width(width as f64);
    }

    pub fn offset(&self, x: u32, y: u32) {
        self.context.translate(x as f64, y as f64).unwrap();
    }

    pub fn rect(&self, x: u32, y: u32, width: u32, height: u32) {
        self.context
            .fill_rect(x as f64, y as f64, width as f64, height as f64);
    }

    pub fn frame(&self, x: u32, y: u32, width: u32, height: u32) {
        self.context.set_stroke_style_str("cyan");
        self.context.set_line_width(1.0);
        self.context.begin_path();
        self.context.rect(
            x as f64 + 0.5,
            y as f64 + 0.5,
            width as f64 - 1.0,
            height as f64 - 1.0,
        );
        self.context.stroke();
    }

    pub fn circle(&self, x: u32, y: u32, radius: u32) {
        self.context.begin_path();
        self.context
            .arc(
                x as f64,
                y as f64,
                radius as f64,
                0.0,
                std::f64::consts::PI * 2.0,
            )
            .unwrap();
        self.context.fill();
    }

    pub fn circle_r(&self, x: R, y: R, radius: R) {
        self.context.begin_path();
        self.context
            .arc(
                x as f64,
                y as f64,
                radius as f64,
                0.0,
                std::f64::consts::PI * 2.0,
            )
            .unwrap();
        self.context.fill();
    }

    pub fn render(&self, cell: &LayoutCell, x_axis: usize, y_axis: usize) {
        let slicer = Slicer::new(self, x_axis, y_axis);
        slicer.render_2d(cell);
    }

    pub fn render_geometry(&self, g: &RGeometry) {
        for (dim, elements) in g.elements.iter().enumerate().rev() {
            if dim >= 3 {
                continue;
            }
            for element in elements {
                let color = self.prim_table.get(element.prim_id).unwrap().color;
                if dim == 0 {
                    self.set_fill_color(color);
                    let (x, y) = match &element.cube {
                        RCube::Point(p) => (p[0], p[1]),
                        _ => panic!(),
                    };
                    // self.circle(x, y, 10);
                    self.circle_r(x, y, 10.0);
                } else if dim == 1 {
                    self.set_stroke_color(color, 10);
                    let (x0, y0, x1, y1, style) = match &element.cube {
                        RCube::Wire {
                            g0,
                            x0,
                            g1,
                            x1,
                            style,
                        } => {
                            let p0 = match &**g0 {
                                RCube::Point(p) => p[0],
                                _ => panic!(),
                            };
                            let p1 = match &**g1 {
                                RCube::Point(p) => p[0],
                                _ => panic!(),
                            };
                            (p0, *x0, p1, *x1, style)
                        }
                        _ => panic!(),
                    };
                    let xc = (x0 + x1) / 2.0;
                    let yc = (y0 + y1) / 2.0;

                    let RWireStyle { s0, s1 } = style;
                    let x0k = lerp(x0, xc, *s0);
                    let y0k = lerp(yc, y0, *s0);
                    let x1k = lerp(x1, xc, *s1);
                    let y1k = lerp(yc, y1, *s1);

                    self.context.begin_path();
                    self.context.move_to(x0 as f64, y0 as f64);
                    self.context.bezier_curve_to(
                        x0k as f64, y0k as f64, x1k as f64, y1k as f64, x1 as f64, y1 as f64,
                    );
                    self.context.stroke();
                } else if dim == 2 {
                    self.set_fill_color(color);
                    let (x0l, x0r, y0, x1l, x1r, y1, style) = match &element.cube {
                        RCube::Wire {
                            g0,
                            x0,
                            g1,
                            x1,
                            style,
                        } => {
                            let (p0l, p0r) = match &**g0 {
                                RCube::Wire {
                                    g0: _,
                                    x0,
                                    g1: _,
                                    x1,
                                    style: _,
                                } => (*x0, *x1),
                                _ => panic!(),
                            };
                            let (p1l, p1r) = match &**g1 {
                                RCube::Wire {
                                    g0: _,
                                    x0,
                                    g1: _,
                                    x1,
                                    style: _,
                                } => (*x0, *x1),
                                _ => panic!(),
                            };
                            (p0l, p0r, *x0, p1l, p1r, *x1, style)
                        }
                        _ => panic!(),
                    };
                    let xcl = (x0l + x1l) / 2.0;
                    let xcr = (x0r + x1r) / 2.0;
                    let yc = (y0 + y1) / 2.0;

                    let RWireStyle { s0, s1 } = style;
                    let x0lk = lerp(x0l, xcl, *s0);
                    let y0lk = lerp(yc, y0, *s0);
                    let x1lk = lerp(x1l, xcl, *s1);
                    let y1lk = lerp(yc, y1, *s1);
                    let x0rk = lerp(x0r, xcr, *s0);
                    let y0rk = lerp(yc, y0, *s0);
                    let x1rk = lerp(x1r, xcr, *s1);
                    let y1rk = lerp(yc, y1, *s1);

                    self.context.begin_path();
                    self.context.move_to(x0l as f64, y0 as f64);
                    self.context.bezier_curve_to(
                        x0lk as f64,
                        y0lk as f64,
                        x1lk as f64,
                        y1lk as f64,
                        x1l as f64,
                        y1 as f64,
                    );
                    self.context.line_to(x1r as f64, y1 as f64);
                    self.context.bezier_curve_to(
                        x1rk as f64,
                        y1rk as f64,
                        x0rk as f64,
                        y0rk as f64,
                        x0r as f64,
                        y0 as f64,
                    );

                    self.context.fill();
                }
            }
        }
        let x_axis = 0;
        let y_axis = 1;
        self.frame(0, 0, g.size[x_axis], g.size[y_axis]);
    }
}

pub struct Slicer<'a> {
    renderer: &'a Renderer,
    x_axis: usize,
    y_axis: usize,
}

impl Slicer<'_> {
    pub fn new(renderer: &Renderer, x_axis: usize, y_axis: usize) -> Slicer {
        assert!(x_axis < y_axis);
        Slicer {
            renderer,
            x_axis,
            y_axis,
        }
    }

    pub fn render_0d(&self, cell: &LayoutCell, x_len: u32, y_len: u32) {
        let prim = match cell.0.as_ref() {
            Cell::Prim(id, _) => self.renderer.prim_table.get(*id).unwrap(),
            Cell::Id(_) => unreachable!(), // TODO: extract prim
            Cell::Comp(_, _, _) => unreachable!(), // TODO: extract prim
        };

        self.renderer.set_fill_color(prim.color);
        self.renderer.rect(0, 0, x_len, y_len);
    }

    pub fn render_1d(&self, cell: &LayoutCell, y_len: u32) {
        let dim = cell.dim();
        assert!(self.x_axis < dim);
        let layout = &cell.1;

        match cell.0.as_ref() {
            Cell::Prim(id, shape) => {
                let (source, target) = match shape {
                    Shape::Zero => unreachable!(),
                    Shape::Succ(s, t) => (s, t),
                };
                let prim = self.renderer.prim_table.get(*id).unwrap();

                let size_x = layout.size[self.x_axis];

                self.padded_0d(source, size_x / 2, y_len);
                self.renderer.push();
                self.renderer.offset(size_x / 2, 0);
                self.padded_0d(target, size_x / 2, y_len);
                self.renderer.pop();

                let center_x = size_x / 2;
                self.renderer.set_fill_color(prim.color);
                self.renderer.rect(center_x - 5, 0, 10, y_len);
            }
            Cell::Id(inner) => {
                if self.x_axis == dim - 1 {
                    self.render_0d(inner, layout.size[self.x_axis], y_len);
                } else {
                    unimplemented!()
                }
            }
            Cell::Comp(level, children, inner_pads) => {
                let n = children.len();
                if *level == 0 && self.x_axis == 0 {
                    let mut offset = 0;
                    for (index, child) in children.iter().enumerate() {
                        self.renderer.push();
                        self.renderer.offset(offset, 0);
                        self.padded_1d(child, y_len);
                        offset += child.1.size[self.x_axis];
                        if index < n - 1 {
                            offset += inner_pads[index];
                        }
                        self.renderer.pop();
                    }
                    let mut offset = 0;
                    for index in 0..children.len() - 1 {
                        self.renderer.push();
                        let c0 = &children[index + 0];
                        let c1 = &children[index + 1];
                        offset += c0.1.size[self.x_axis];
                        self.renderer.offset(offset, 0);
                        let x_len = inner_pads[index];
                        self.bridge_0d(&c0.t(), &c1.s(), x_len, y_len);
                        offset += inner_pads[index];
                        self.renderer.pop();
                    }
                } else {
                    unimplemented!()
                }
            }
        }
    }

    pub fn render_2d(&self, cell: &LayoutCell) {
        let dim = cell.dim();
        assert!(self.x_axis < dim && self.y_axis < dim);
        let layout = &cell.1;

        match cell.0.as_ref() {
            Cell::Prim(id, shape) => {
                let (source, target) = match shape {
                    Shape::Zero => unreachable!(),
                    Shape::Succ(s, t) => (s, t),
                };
                let prim = self.renderer.prim_table.get(*id).unwrap();

                let size_x = layout.size[self.x_axis];
                let size_y = layout.size[self.y_axis];

                // self.padded_1d(source, size_y / 2);
                self.shrink_1d(source, size_y / 2, 0);
                self.shrink_1d(target, size_y / 2, size_y);

                let center_x = size_x / 2;
                let center_y = size_y / 2;
                self.renderer.set_fill_color(prim.color);
                self.renderer.circle(center_x, center_y, 10);

                self.renderer.frame(0, 0, size_x, size_y);
            }
            Cell::Id(inner) => {
                if self.y_axis == dim - 1 {
                    self.render_1d(inner, layout.size[self.y_axis]);
                } else {
                    unimplemented!()
                }
            }
            Cell::Comp(level, children, inner_pads) => {
                let n = children.len();
                if *level == 0 && self.x_axis == 0 {
                    let mut offset = 0;
                    for (index, child) in children.iter().enumerate() {
                        self.renderer.push();
                        self.renderer.offset(offset, 0);
                        self.padded_2d(child);
                        offset += child.1.size[self.x_axis];
                        if index < n - 1 {
                            offset += inner_pads[index];
                        }
                        self.renderer.pop();
                    }
                    let y_len = layout.size[self.y_axis];
                    let mut offset = 0;
                    for index in 0..children.len() - 1 {
                        self.renderer.push();
                        let c0 = &children[index + 0];
                        let c1 = &children[index + 1];
                        offset += c0.1.size[self.x_axis];
                        self.renderer.offset(offset, 0);
                        let x_len = inner_pads[index];
                        self.bridge_0d(&c0.s().t(), &c1.s().s(), x_len, y_len);
                        offset += inner_pads[index];
                        self.renderer.pop();
                    }
                } else if *level == 1 && self.y_axis == 1 {
                    let mut offset = 0;
                    for (index, child) in children.iter().enumerate() {
                        self.renderer.push();
                        self.renderer.offset(0, offset);
                        self.padded_2d(child);
                        offset += child.1.size[self.y_axis];
                        if index < n - 1 {
                            offset += inner_pads[index];
                        }
                        self.renderer.pop();
                    }
                    let mut offset = 0;
                    for index in 0..children.len() - 1 {
                        self.renderer.push();
                        let c0 = &children[index + 0];
                        let c1 = &children[index + 1];
                        offset += c0.1.size[self.y_axis];
                        self.renderer.offset(0, offset);
                        let y_len = inner_pads[index];
                        self.bridge_1d(&c0.t(), &c1.s(), y_len);
                        offset += inner_pads[index];
                        self.renderer.pop();
                    }
                } else {
                    unimplemented!()
                }
            }
        }
    }

    pub fn padded_0d(&self, pc: &LayoutCell, x_len: u32, y_len: u32) {
        let cell = pc;
        self.render_0d(cell, x_len, y_len);
    }

    pub fn padded_1d(&self, pc: &LayoutCell, y_len: u32) {
        let cell = pc;
        let min_x = pc.1.pad.min[self.x_axis];
        let max_x = pc.1.pad.max[self.x_axis];
        let size_x = cell.1.size[self.x_axis];

        self.renderer.push();
        self.padded_0d(&pc.s(), min_x, y_len);
        self.renderer.offset(min_x + size_x, 0);
        self.padded_0d(&pc.t(), max_x, y_len);
        self.renderer.pop();

        self.renderer.push();
        self.renderer.offset(min_x, 0);
        self.render_1d(cell, y_len);
        self.renderer.pop();
    }

    pub fn padded_2d(&self, pc: &LayoutCell) {
        let cell = pc;
        let min_x = pc.1.pad.min[self.x_axis];
        let min_y = pc.1.pad.min[self.y_axis];
        let max_x = pc.1.pad.max[self.x_axis];
        let max_y = pc.1.pad.max[self.y_axis];
        let size_x = cell.1.size[self.x_axis];
        let size_y = cell.1.size[self.y_axis];
        let bounds_x = min_x + size_x + max_x;
        let bounds_y = min_y + size_y + max_y;

        self.renderer.push();
        self.padded_0d(&pc.s().s(), min_x, bounds_y);
        self.renderer.offset(min_x + size_x, 0);
        self.padded_0d(&pc.t().s(), max_x, bounds_y);
        self.renderer.pop();

        self.renderer.push();
        self.padded_1d(&pc.s(), min_y);
        self.renderer.offset(0, min_y + size_y);
        self.padded_1d(&pc.t(), max_y);
        self.renderer.pop();

        self.renderer.push();
        self.renderer.offset(min_x, min_y);
        self.render_2d(cell);
        self.renderer.pop();

        self.renderer.frame(0, 0, bounds_x, bounds_y);
    }

    pub fn bridge_0d(&self, pc0: &LayoutCell, _pc1: &LayoutCell, x_len: u32, y_len: u32) {
        self.render_0d(pc0, x_len, y_len);
    }

    pub fn extract_0d(&self, cell: &LayoutCell) -> PrimId {
        assert!(cell.dim() == 0);
        match cell.0.as_ref() {
            Cell::Prim(id, _) => *id,
            Cell::Id(_) => unreachable!(),
            Cell::Comp(_, _, _) => unreachable!(),
        }
    }

    pub fn extract_1d(&self, pc: &LayoutCell, cs: &mut Vec<(PrimId, u32)>) {
        let cell = pc;
        let add = |cs: &mut Vec<(PrimId, u32)>, p: PrimId, l: u32| {
            if let Some((last_p, last_l)) = cs.last_mut() {
                if *last_p == p {
                    *last_l += l;
                    return;
                }
            }
            cs.push((p, l));
        };
        add(cs, self.extract_0d(&pc.s()), pc.1.pad.min[self.x_axis]);
        match cell.0.as_ref() {
            Cell::Prim(prim_id, shape) => {
                let (source, target) = match shape {
                    Shape::Zero => unreachable!(),
                    Shape::Succ(s, t) => (s, t),
                };
                add(cs, self.extract_0d(source), cell.1.size[self.x_axis] / 2);
                add(cs, *prim_id, 0);
                add(cs, self.extract_0d(target), cell.1.size[self.x_axis] / 2);
            }
            Cell::Id(inner) => {
                if self.x_axis == cell.dim() - 1 {
                    add(cs, self.extract_0d(inner), cell.1.size[self.x_axis]);
                } else {
                    unimplemented!()
                }
            }
            Cell::Comp(_, children, inner_pads) => {
                for (index, child) in children.iter().enumerate() {
                    self.extract_1d(child, cs);
                    if index < children.len() - 1 {
                        cs.last_mut().unwrap().1 += inner_pads[index];
                    }
                }
            }
        }
        add(cs, self.extract_0d(&pc.t()), pc.1.pad.max[self.x_axis]);
    }

    pub fn bridge_1d(&self, pc0: &LayoutCell, pc1: &LayoutCell, y_len: u32) {
        let mut cs0 = vec![];
        let mut cs1 = vec![];
        self.extract_1d(pc0, &mut cs0);
        self.extract_1d(pc1, &mut cs1);
        assert_eq!(cs0.len(), cs1.len());
        assert!(cs0.len() % 2 == 1);

        self.renderer.push();
        let mut offset0 = 0;
        let mut offset1 = 0;
        for (index, ((c0, l0), (c1, l1))) in cs0.iter().zip(cs1.iter()).enumerate() {
            if index % 2 == 0 {
                let x0 = offset0;
                let x1 = offset1;
                let y0 = 0;
                let y1 = y_len;
                let yc = (y0 + y1) / 2;
                self.renderer.context.begin_path();
                self.renderer.context.move_to(x0 as f64, y0 as f64);
                self.renderer.context.bezier_curve_to(
                    x0 as f64, yc as f64, x1 as f64, yc as f64, x1 as f64, y1 as f64,
                );
                let x0 = offset0 + l0;
                let x1 = offset1 + l1;
                self.renderer.context.line_to(x1 as f64, y1 as f64);
                self.renderer.context.bezier_curve_to(
                    x1 as f64, yc as f64, x0 as f64, yc as f64, x0 as f64, y0 as f64,
                );
                assert_eq!(c0, c1);
                let color = self.renderer.prim_table.get(*c0).unwrap().color;
                self.renderer.set_fill_color(color);
                self.renderer.context.fill();
            }
            offset0 += l0;
            offset1 += l1;
        }
        let mut offset0 = 0;
        let mut offset1 = 0;
        for (index, ((c0, l0), (c1, l1))) in cs0.iter().zip(cs1.iter()).enumerate() {
            if index % 2 == 1 {
                let x0 = offset0;
                let x1 = offset1;
                let y0 = 0;
                let y1 = y_len;
                let yc = (y0 + y1) / 2;
                self.renderer.context.begin_path();
                self.renderer.context.move_to(x0 as f64, y0 as f64);
                self.renderer.context.bezier_curve_to(
                    x0 as f64, yc as f64, x1 as f64, yc as f64, x1 as f64, y1 as f64,
                );
                assert_eq!(c0, c1);
                let color = self.renderer.prim_table.get(*c0).unwrap().color;
                self.renderer.set_stroke_color(color, 10);
                self.renderer.context.stroke();
            }
            offset0 += l0;
            offset1 += l1;
        }
        self.renderer.pop();
    }

    pub fn shrink_1d(&self, pc: &LayoutCell, y_0d: u32, y_1d: u32) {
        let mut cs = vec![];
        self.extract_1d(pc, &mut cs);
        assert!(cs.len() % 2 == 1);
        let last = cs.len() - 1;

        let right = pc.1.pad.min[self.x_axis] + pc.1.size[self.x_axis] + pc.1.pad.max[self.x_axis];
        let left = 0;
        let center = right / 2;

        self.renderer.push();
        let mut offset = 0;
        for (index, (c, l)) in cs.iter().enumerate() {
            if index % 2 == 0 {
                let x0 = if index == 0 { left } else { center };
                let x1 = offset;
                let xc = (x0 + x1) / 2;
                let y0 = y_0d;
                let y1 = y_1d;
                let yc = (y0 + y1) / 2;
                self.renderer.context.begin_path();
                self.renderer.context.move_to(x0 as f64, y0 as f64);
                self.renderer.context.bezier_curve_to(
                    xc as f64, y0 as f64, x1 as f64, yc as f64, x1 as f64, y1 as f64,
                );
                let x0 = if index == last { right } else { center };
                let x1 = offset + l;
                let xc = (x0 + x1) / 2;
                self.renderer.context.line_to(x1 as f64, y1 as f64);
                self.renderer.context.bezier_curve_to(
                    x1 as f64, yc as f64, xc as f64, y0 as f64, x0 as f64, y0 as f64,
                );
                let color = self.renderer.prim_table.get(*c).unwrap().color;
                self.renderer.set_fill_color(color);
                self.renderer.context.fill();
            }
            offset += l;
        }
        let mut offset = 0;
        for (index, (c, l)) in cs.iter().enumerate() {
            if index % 2 == 1 {
                let x0 = center;
                let x1 = offset;
                let xc = (x0 + x1) / 2;
                let y0 = y_0d;
                let y1 = y_1d;
                let yc = (y0 + y1) / 2;
                self.renderer.context.begin_path();
                self.renderer.context.move_to(x0 as f64, y0 as f64);
                self.renderer.context.bezier_curve_to(
                    xc as f64, y0 as f64, x1 as f64, yc as f64, x1 as f64, y1 as f64,
                );
                let color = self.renderer.prim_table.get(*c).unwrap().color;
                self.renderer.set_stroke_color(color, 10);
                self.renderer.context.stroke();
            }
            offset += l;
        }
        self.renderer.pop();
    }
}
