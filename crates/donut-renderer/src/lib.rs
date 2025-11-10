use donut_core::cell::*;
use donut_core::table::*;
use donut_util::println;
use std::rc::Rc;

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

    pub fn set_color(&self, color: (u8, u8, u8, u8)) {
        let (r, g, b, a) = color;
        let str = if a == 255 {
            format!("rgb({} {} {})", r, g, b)
        } else {
            format!("rgb({} {} {} / {})", r, g, b, a as f64 / 255.0)
        };
        self.context.set_fill_style_str(&str);
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

    pub fn test(&self) {
        self.set_color((255, 0, 0, 128));
        self.context.begin_path();
        let p0 = (100.0, 150.0);
        let p1 = (200.0, 50.0);
        self.context.move_to(p0.0, p0.1);
        self.context.bezier_curve_to(
            p0.0,
            (p0.1 * 2.0 + p1.1) / 3.0,
            p1.0,
            (p0.1 + p1.1 * 2.0) / 3.0,
            p1.0,
            p1.1,
        );
        self.context.set_stroke_style_str("white");
        self.context.set_line_width(5.0);
        self.context.stroke();

        self.context.begin_path();
        self.context.translate(100.0, 0.0).unwrap();
        self.context.move_to(p0.0, p0.1);
        for i in 1..=10 {
            let t = i as f64 / 10.0;
            let x = (1.0 - t).powi(3) * p0.0
                + 3.0 * (1.0 - t).powi(2) * t * p0.0
                + 3.0 * (1.0 - t) * t.powi(2) * p1.0
                + t.powi(3) * p1.0;
            let y = p0.1 * (1.0 - t) + p1.1 * t;
            self.context.line_to(x, y);
        }
        self.context.stroke();
    }

    pub fn render(&self, cell: &Rc<LayoutCell>, x_axis: usize, y_axis: usize) {
        let slicer = Slicer::new(self, x_axis, y_axis);
        slicer.render_2d(cell);
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

    pub fn render_0d(&self, cell: &Rc<LayoutCell>, x_len: u32, y_len: u32) {
        let prim = match &cell.0 {
            CellF::Prim(id, _) => self.renderer.prim_table.get(*id).unwrap(),
            CellF::Id(_) => unreachable!(), // TODO: extract prim
            CellF::Comp(_, _, _) => unreachable!(), // TODO: extract prim
        };

        self.renderer.set_color(prim.color);
        self.renderer.rect(0, 0, x_len, y_len);
    }

    pub fn render_1d(&self, cell: &Rc<LayoutCell>, y_len: u32) {
        let dim = cell.dim();
        assert!(self.x_axis < dim);
        let layout = &cell.1;

        match &cell.0 {
            CellF::Prim(id, shape) => {
                let (source, target) = match shape {
                    ShapeF::Zero => unreachable!(),
                    ShapeF::Succ(s, t) => (s, t),
                };
                let prim = self.renderer.prim_table.get(*id).unwrap();

                let size_x = layout.size[self.x_axis];

                self.padded_0d(source, size_x / 2, y_len);
                self.renderer.push();
                self.renderer.offset(size_x / 2, 0);
                self.padded_0d(target, size_x / 2, y_len);
                self.renderer.pop();

                let center_x = size_x / 2;
                self.renderer.set_color(prim.color);
                self.renderer.rect(center_x - 5, 0, 10, y_len);
            }
            CellF::Id(inner) => {
                if self.x_axis == dim - 1 {
                    self.render_0d(inner, layout.size[self.x_axis], y_len);
                } else {
                    unimplemented!()
                }
            }
            CellF::Comp(children, level, inner_pads) => {
                let n = children.len();
                if *level == 0 && self.x_axis == 0 {
                    let mut offset = 0;
                    for (index, child) in children.iter().enumerate() {
                        self.renderer.push();
                        self.renderer.offset(offset, 0);
                        self.padded_1d(child, y_len);
                        offset += child.cell.1.size[self.x_axis];
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
                        offset += c0.cell.1.size[self.x_axis];
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

    pub fn render_2d(&self, cell: &Rc<LayoutCell>) {
        let dim = cell.dim();
        assert!(self.x_axis < dim && self.y_axis < dim);
        let layout = &cell.1;

        match &cell.0 {
            CellF::Prim(id, shape) => {
                let (source, target) = match shape {
                    ShapeF::Zero => unreachable!(),
                    ShapeF::Succ(s, t) => (s, t),
                };
                let prim = self.renderer.prim_table.get(*id).unwrap();

                let size_x = layout.size[self.x_axis];
                let size_y = layout.size[self.y_axis];

                self.padded_1d(source, size_y / 2);
                self.renderer.push();
                self.renderer.offset(0, size_y / 2);
                self.padded_1d(target, size_y / 2);
                self.renderer.pop();

                let center_x = size_x / 2;
                let center_y = size_y / 2;
                self.renderer.set_color(prim.color);
                self.renderer.circle(center_x, center_y, 10);

                self.renderer.frame(0, 0, size_x, size_y);
            }
            CellF::Id(inner) => {
                if self.y_axis == dim - 1 {
                    self.render_1d(inner, layout.size[self.y_axis]);
                } else {
                    unimplemented!()
                }
            }
            CellF::Comp(children, level, inner_pads) => {
                let n = children.len();
                if *level == 0 && self.x_axis == 0 {
                    let mut offset = 0;
                    for (index, child) in children.iter().enumerate() {
                        self.renderer.push();
                        self.renderer.offset(offset, 0);
                        self.padded_2d(child);
                        offset += child.cell.1.size[self.x_axis];
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
                        offset += c0.cell.1.size[self.x_axis];
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
                        offset += child.cell.1.size[self.y_axis];
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
                        offset += c0.cell.1.size[self.y_axis];
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

    pub fn padded_0d(&self, pc: &PaddedCell, x_len: u32, y_len: u32) {
        let cell = &pc.cell;
        self.render_0d(cell, x_len, y_len);
    }

    pub fn padded_1d(&self, pc: &PaddedCell, y_len: u32) {
        let cell = &pc.cell;
        let min_x = pc.pad.min[self.x_axis];
        let max_x = pc.pad.max[self.x_axis];
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

    pub fn padded_2d(&self, pc: &PaddedCell) {
        let cell = &pc.cell;
        let min_x = pc.pad.min[self.x_axis];
        let min_y = pc.pad.min[self.y_axis];
        let max_x = pc.pad.max[self.x_axis];
        let max_y = pc.pad.max[self.y_axis];
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

    pub fn bridge_0d(&self, pc0: &PaddedCell, _pc1: &PaddedCell, x_len: u32, y_len: u32) {
        self.render_0d(&pc0.cell, x_len, y_len);
    }

    pub fn bridge_1d(&self, pc0: &PaddedCell, pc1: &PaddedCell, y_len: u32) {
        self.padded_1d(pc0, y_len);
        self.padded_1d(pc1, y_len);
    }
}
