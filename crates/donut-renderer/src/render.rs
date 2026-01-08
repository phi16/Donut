use crate::geometry::*;
use donut_core::common::Prim;

fn lerp(a: R, b: R, t: R) -> R {
    a * (1.0 - t) + b * t
}

pub struct Renderer {
    context: web_sys::CanvasRenderingContext2d,
}

impl Renderer {
    pub fn new(context: web_sys::CanvasRenderingContext2d) -> Self {
        Self { context }
    }

    pub fn circle(&self, x: R, y: R) {
        self.context.begin_path();
        self.context
            .arc(x, y, 8.0, 0.0, std::f64::consts::PI * 2.0)
            .unwrap();
        self.context.close_path();
        self.context.set_fill_style_str("rgb(255 100 0)");
        self.context.fill();
    }
    pub fn path(&self, x0: R, y0: R, t0: &CoordR, x1: R, y1: R, t1: &CoordR) {
        assert_eq!(t0.len(), 2);
        assert_eq!(t1.len(), 2);
        let (x0t, y0t) = (lerp(x0, x1, t0[0]), lerp(y0, y1, t0[1]));
        let (x1t, y1t) = (lerp(x1, x0, t1[0]), lerp(y1, y0, t1[1]));
        self.context.begin_path();
        self.context.move_to(x0, y0);
        self.context.bezier_curve_to(x0t, y0t, x1t, y1t, x1, y1);
        self.context.set_stroke_style_str("rgb(0 100 255)");
        self.context.set_line_width(6.0);
        self.context.stroke();
    }
    pub fn region(&self, x0: (R, R), y0: R, t0: &CoordR, x1: (R, R), y1: R, t1: &CoordR) {
        let x0s = x0.0;
        let x0t = x0.1;
        let x1s = x1.0;
        let x1t = x1.1;
        let (x0st, y0st) = (lerp(x0s, x1s, t0[0]), lerp(y0, y1, t0[1]));
        let (x0tt, y0tt) = (lerp(x0t, x1t, t0[0]), lerp(y0, y1, t0[1]));
        let (x1st, y1st) = (lerp(x1s, x0s, t1[0]), lerp(y1, y0, t1[1]));
        let (x1tt, y1tt) = (lerp(x1t, x0t, t1[0]), lerp(y1, y0, t1[1]));
        // (x0s, y0)      (x0t, y0)
        //     |              ^
        //     |              |
        //     v              |
        // (x1s, y1) ---> (x1t, y1)
        self.context.begin_path();
        self.context.move_to(x0s, y0);
        self.context
            .bezier_curve_to(x0st, y0st, x1st, y1st, x1s, y1);
        self.context.line_to(x1t, y1);
        self.context
            .bezier_curve_to(x1tt, y1tt, x0tt, y0tt, x0t, y0);
        self.context.close_path();
        self.context.set_fill_style_str("rgb(20 20 20)");
        // self.context.fill();
        self.context.set_stroke_style_str("rgba(255 255 255 0.1)");
        self.context.set_line_width(1.0);
        self.context.stroke();
    }

    pub fn cube(&self, prim: &Prim, cube: &Cuboid) {
        match cube {
            Cuboid::Point(p) => {
                assert_eq!(p.len(), 2);
                self.circle(p[0], p[1]);
            }
            Cuboid::Bridge { source, target } => match (source.0.as_ref(), target.0.as_ref()) {
                (Cuboid::Point(s), Cuboid::Point(t)) => {
                    assert_eq!(s.len(), 1);
                    assert_eq!(t.len(), 1);
                    self.path(s[0], source.1, &source.2, t[0], target.1, &target.2);
                }
                (
                    Cuboid::Bridge {
                        source: ss,
                        target: st,
                    },
                    Cuboid::Bridge {
                        source: ts,
                        target: tt,
                    },
                ) => {
                    self.region(
                        (ss.1, st.1),
                        source.1,
                        &source.2,
                        (ts.1, tt.1),
                        target.1,
                        &target.2,
                    );
                }
                _ => unreachable!(),
            },
        }
    }

    pub fn cell(&self, cell: &Geometry) {
        for cubes in cell.cubes.iter() {
            for (prim, cube) in cubes {
                self.cube(prim, cube);
            }
        }

        for (_, center, r2) in &cell.spheres {
            assert_eq!(center.len(), 2);
            let r = r2.sqrt();
            let x = center[0];
            let y = center[1];
            self.context.begin_path();
            self.context
                .arc(x, y, r, 0.0, std::f64::consts::PI * 2.0)
                .unwrap();
            self.context.close_path();
            self.context.set_fill_style_str("rgb(255 255 0)");
            self.context.fill();
        }
    }
}
