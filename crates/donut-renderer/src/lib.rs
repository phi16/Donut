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
        self.renderer
            .context
            .fill_rect(0.0, 0.0, x_len as f64, y_len as f64);
    }

    pub fn render_1d(&self, cell: &Rc<LayoutCell>, y_len: u32) {
        let dim = cell.1.size.len();
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
                if self.x_axis == dim - 1 {
                    self.render_0d(inner, layout.size[self.x_axis], y_len);
                    return;
                } else {
                    unimplemented!()
                }
            }
            CellF::Comp(children, level) => {
                unimplemented!()
            }
        };

        let width = layout.size[self.x_axis];
        self.render_0d(source, width * 2 / 5, y_len);
        self.renderer.push();
        self.renderer.offset(width * 3 / 5, 0);
        self.render_0d(target, width * 2 / 5, y_len);
        self.renderer.pop();

        self.renderer.set_color(prim.color);
        self.renderer.context.fill_rect(
            (width * 2 / 5) as f64,
            0.0,
            (width / 5) as f64,
            y_len as f64,
        );
    }

    pub fn render_2d(&self, cell: &Rc<LayoutCell>) {
        let dim = cell.1.size.len();
        assert!(self.x_axis < dim && self.y_axis < dim);
        let layout = &cell.1;

        match &cell.0 {
            CellF::Prim(id, shape) => {
                unimplemented!()
            }
            CellF::Id(inner) => {
                if self.y_axis == dim - 1 {
                    self.render_1d(inner, layout.size[self.y_axis]);
                } else {
                    unimplemented!()
                }
            }
            CellF::Comp(children, level) => {
                unimplemented!()
            }
        }
    }
}
