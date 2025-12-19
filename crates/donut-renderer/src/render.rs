use crate::render_cell::{Cube, RenderCell, R};
use donut_core::common::{Prim, Q};

pub struct Renderer {
    context: web_sys::CanvasRenderingContext2d,
}

impl Renderer {
    pub fn new(context: web_sys::CanvasRenderingContext2d) -> Self {
        Self { context }
    }

    /* pub fn cell_1d(&self, cell: &LayoutCell) {
        self.context.save();
        self.context
            .translate(cell.1.origin[0] as f64, 0.0)
            .unwrap();
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
                        source_limit,
                        source_coord,
                        target_coord,
                        ..
                    } => {
                        let source_width = ((*target_coord - *source_coord) as f64) / 2.0;
                        (*source_limit as f64) + source_width
                    }
                };
                self.context.begin_path();
                self.context.set_fill_style_str("rgb(0 100 255)");
                self.context
                    .arc(x, 0.0, 4.0, 0.0, std::f64::consts::PI * 2.0)
                    .unwrap();
                self.context.fill();
            }
            RawCell::Id(face, s, t) => {
                unimplemented!()
            }
            RawCell::Comp(axis, children) => {
                self.context.save();
                for child in children {
                    self.cell_1d(&child);
                    /* match axis {
                        0 => self.context.translate(child.1.size[0] as f64, 0.0).unwrap(),
                        _ => unreachable!(),
                    } */
                }
                self.context.restore();
            }
        }
        self.context.restore();
    }

    pub fn cell_2d(&self, cell: &LayoutCell) {
        self.context.save();
        self.context
            .translate(cell.1.origin[0] as f64, cell.1.origin[1] as f64)
            .unwrap();
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
                        // self.context.translate(0.0, h).unwrap();
                        self.cell_1d(target);
                        self.context.restore();
                    }
                }
            }
            RawCell::Id(face, _, _) => {
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
                    /* match axis {
                        0 => self.context.translate(child.1.size[0] as f64, 0.0).unwrap(),
                        1 => self.context.translate(0.0, child.1.size[1] as f64).unwrap(),
                        _ => unreachable!(),
                    } */
                }
                self.context.restore();
            }
        }
        self.context.restore();
    } */

    pub fn circle(&self, x: R, y: R) {
        self.context.begin_path();
        self.context
            .arc(x, y, 8.0, 0.0, std::f64::consts::PI * 2.0)
            .unwrap();
        self.context.close_path();
        self.context.set_fill_style_str("rgb(255 100 0)");
        self.context.fill();
    }
    pub fn path(&self, x0: R, y0: R, x1: R, y1: R) {
        let yh = (y0 + y1) / 2.0;
        self.context.begin_path();
        self.context.move_to(x0, y0);
        self.context.bezier_curve_to(x0, yh, x1, yh, x1, y1);
        self.context.set_stroke_style_str("rgb(0 100 255)");
        self.context.set_line_width(6.0);
        self.context.stroke();
    }
    pub fn region(&self, x0: (R, R), y0: R, x1: (R, R), y1: R) {
        let x0s = x0.0;
        let x0t = x0.1;
        let x1s = x1.0;
        let x1t = x1.1;
        let yh = (y0 + y1) / 2.0;
        // (x0s, y0)      (x0t, y0)
        //     |              ^
        //     |              |
        //     v              |
        // (x1s, y1) ---> (x1t, y1)
        self.context.begin_path();
        self.context.move_to(x0s, y0);
        self.context.bezier_curve_to(x0s, yh, x1s, yh, x1s, y1);
        self.context.line_to(x1t, y1);
        self.context.bezier_curve_to(x1t, yh, x0t, yh, x0t, y0);
        self.context.close_path();
        self.context.set_fill_style_str("rgb(20 20 20)");
        // self.context.fill();
        self.context.set_stroke_style_str("rgba(255 255 255 0.1)");
        self.context.set_line_width(1.0);
        self.context.stroke();
    }

    pub fn cube(&self, prim: &Prim, cube: &Cube) {
        match cube {
            Cube::Point(p) => {
                assert_eq!(p.len(), 2);
                self.circle(p[0], p[1]);
            }
            Cube::Bridge { source, target } => match (source.0.as_ref(), target.0.as_ref()) {
                (Cube::Point(s), Cube::Point(t)) => {
                    assert_eq!(s.len(), 1);
                    assert_eq!(t.len(), 1);
                    self.path(s[0], source.1, t[0], target.1);
                }
                (
                    Cube::Bridge {
                        source: ss,
                        target: st,
                    },
                    Cube::Bridge {
                        source: ts,
                        target: tt,
                    },
                ) => {
                    self.region((ss.1, st.1), source.1, (ts.1, tt.1), target.1);
                }
                _ => unreachable!(),
            },
        }
    }

    pub fn cell(&self, cell: &RenderCell) {
        for cubes in cell.cubes.iter() {
            for (prim, cube) in cubes {
                self.cube(prim, cube);
            }
        }
    }
}
