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
            CellF::Id(_) => unreachable!(),      // TODO: extract prim
            CellF::Comp(_, _) => unreachable!(), // TODO: extract prim
        };

        self.renderer.set_color(prim.color);
        self.renderer.rect(0, 0, x_len, y_len);
    }

    pub fn render_1d(&self, cell: &Rc<LayoutCell>, y_len: u32) {
        let dim = cell.dim();
        assert!(self.x_axis < dim);
        let layout = &cell.1;

        let (prim, source, target) = match &cell.0 {
            CellF::Prim(id, shape) => {
                let (source, target) = match shape {
                    ShapeF::Zero => unreachable!(),
                    ShapeF::Succ(s, t) => (s, t),
                };
                let prim = self.renderer.prim_table.get(*id).unwrap();
                (prim, source, target)
            }
            CellF::Id(inner) => {
                self.renderer.push();
                self.renderer.offset(layout.min_pad[self.x_axis], 0);
                if self.x_axis == dim - 1 {
                    self.render_0d(inner, layout.bounds[self.x_axis], y_len);
                } else {
                    unimplemented!()
                }
                self.renderer.pop();
                return;
            }
            CellF::Comp(children, level) => {
                if *level == 0 {
                    self.renderer.push();
                    self.renderer.offset(layout.min_pad[self.x_axis], 0);
                    let mut offset = 0;
                    for child in children {
                        self.renderer.push();
                        self.renderer.offset(offset, 0);
                        self.render_1d(child, y_len);
                        offset += child.1.bounds[self.x_axis];
                        self.renderer.pop();
                    }
                    self.renderer.pop();
                } else {
                    unimplemented!()
                }
                return;
            }
        };

        let size_x = layout.size[self.x_axis];
        let offset_x = layout.min_pad[self.x_axis];
        let bounds_x = layout.bounds[self.x_axis];
        let center_x = offset_x + size_x / 2;

        self.render_0d(source, center_x, y_len);
        self.renderer.push();
        self.renderer.offset(center_x, 0);
        self.render_0d(target, bounds_x - center_x, y_len);
        self.renderer.pop();

        self.renderer.set_color(prim.color);
        self.renderer.rect(offset_x, 0, size_x, y_len);
    }

    pub fn render_2d(&self, cell: &Rc<LayoutCell>) {
        let dim = cell.dim();
        assert!(self.x_axis < dim && self.y_axis < dim);
        let layout = &cell.1;

        let (prim, source, target) = match &cell.0 {
            CellF::Prim(id, shape) => {
                let (source, target) = match shape {
                    ShapeF::Zero => unreachable!(),
                    ShapeF::Succ(s, t) => (s, t),
                };
                let prim = self.renderer.prim_table.get(*id).unwrap();
                (prim, source, target)
            }
            CellF::Id(inner) => {
                self.renderer.push();
                self.renderer.offset(0, layout.min_pad[self.y_axis]);
                if self.y_axis == dim - 1 {
                    self.render_1d(inner, layout.bounds[self.y_axis]);
                } else {
                    unimplemented!()
                }
                self.renderer.pop();
                return;
            }
            CellF::Comp(children, level) => {
                self.renderer.push();
                self.renderer.offset(0, layout.min_pad[self.y_axis]);
                if *level == 0 {
                    let mut offset = 0;
                    for child in children {
                        self.renderer.push();
                        self.renderer.offset(offset, 0);
                        self.render_2d(child);
                        offset += child.1.bounds[self.x_axis];
                        self.renderer.pop();
                    }
                } else if *level == 1 {
                    let mut offset = 0;
                    for child in children {
                        self.renderer.push();
                        self.renderer.offset(0, offset);
                        self.render_2d(child);
                        offset += child.1.bounds[self.y_axis];
                        self.renderer.pop();
                    }
                } else {
                    unimplemented!()
                }
                self.renderer.pop();
                return;
            }
        };

        let size_y = layout.size[self.y_axis];
        let offset_y = layout.min_pad[self.y_axis];
        let bounds_y = layout.bounds[self.y_axis];
        let center_y = offset_y + size_y / 2;

        self.render_1d(source, center_y);
        self.renderer.push();
        self.renderer.offset(0, center_y);
        self.render_1d(target, bounds_y - center_y);
        self.renderer.pop();

        let size_x = layout.size[self.x_axis];
        let offset_x = layout.min_pad[self.x_axis];
        let bounds_x = layout.bounds[self.x_axis];
        let center_x = offset_x + size_x / 2;

        self.renderer.set_color(prim.color);
        self.renderer
            .circle(center_x, center_y, size_x.min(size_y) / 2);

        self.renderer.frame(0, 0, bounds_x, bounds_y);
    }
}
