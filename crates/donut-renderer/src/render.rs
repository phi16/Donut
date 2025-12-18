use donut_core::common::N;
use donut_core::layout_cell::{LayoutCell, RawCell, Shape};

pub struct Renderer {
    context: web_sys::CanvasRenderingContext2d,
}

impl Renderer {
    pub fn new(context: web_sys::CanvasRenderingContext2d) -> Self {
        Self { context }
    }

    pub fn cell_1d(&self, cell: &LayoutCell) {
        self.context.begin_path();
        self.context
            .set_stroke_style_str("rgba(255, 255, 255, 0.2)");
        let w = cell.1.size[0] as f64;
        self.context.rect(0.0, -4.0, w, 8.0);
        self.context.stroke();

        assert_eq!(cell.1.dim.in_space, 1);
        match cell.0.as_ref() {
            RawCell::Prim(prim, shape) => {
                assert_eq!(cell.1.dim.effective, 1);
                let x = match shape {
                    Shape::Zero => unreachable!(),
                    Shape::Succ {
                        source_extend,
                        width,
                        ..
                    } => (*source_extend as f64) + (*width as f64) / 2.0,
                };
                self.context.begin_path();
                self.context.set_fill_style_str("rgb(0 100 255)");
                self.context
                    .arc(x, 0.0, 4.0, 0.0, std::f64::consts::PI * 2.0)
                    .unwrap();
                self.context.fill();
            }
            RawCell::Id(face, width) => {
                unimplemented!()
            }
            RawCell::Comp(axis, children) => {
                self.context.save();
                for child in children {
                    self.cell_1d(&child);
                    match axis {
                        0 => self.context.translate(child.1.size[0] as f64, 0.0).unwrap(),
                        _ => unreachable!(),
                    }
                }
                self.context.restore();
            }
        }
    }

    pub fn cell_2d(&self, cell: &LayoutCell) {
        self.context.begin_path();
        self.context
            .set_stroke_style_str("rgba(255, 255, 255, 0.2)");
        let w = cell.1.size[0] as f64;
        let h = cell.1.size[1] as f64;
        self.context.rect(0.0, 0.0, w, h);
        self.context.stroke();

        assert_eq!(cell.1.dim.in_space, 2);
        match cell.0.as_ref() {
            RawCell::Prim(prim, shape) => {
                assert_eq!(cell.1.dim.effective, 2);
                let w = cell.1.size[0] as f64;
                let h = cell.1.size[1] as f64;
                self.context.begin_path();
                self.context.set_fill_style_str("rgb(255 100 0)");
                self.context
                    .arc(w / 2.0, h / 2.0, 8.0, 0.0, std::f64::consts::PI * 2.0)
                    .unwrap();
                self.context.fill();

                match shape {
                    Shape::Zero => unreachable!(),
                    Shape::Succ { source, target, .. } => {
                        self.context.save();
                        self.cell_1d(source);
                        self.context.translate(0.0, h).unwrap();
                        self.cell_1d(target);
                        self.context.restore();
                    }
                }
            }
            RawCell::Id(face, width) => {
                self.context.save();
                let h = cell.1.size[1] as f64;
                self.context.translate(0.0, h / 2.0).unwrap();
                self.cell_1d(face);
                self.context.restore();
            }
            RawCell::Comp(axis, children) => {
                self.context.save();
                for child in children {
                    self.cell_2d(&child);
                    match axis {
                        0 => self.context.translate(child.1.size[0] as f64, 0.0).unwrap(),
                        1 => self.context.translate(0.0, child.1.size[1] as f64).unwrap(),
                        _ => unreachable!(),
                    }
                }
                self.context.restore();
            }
        }
    }
}
