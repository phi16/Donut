use crate::geometry::*;
use crate::prim_table::PrimTable;
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

    pub fn circle(&self, x: R, y: R, color: &str) {
        self.context.begin_path();
        self.context
            .arc(x, y, 8.0, 0.0, std::f64::consts::PI * 2.0)
            .unwrap();
        self.context.close_path();
        self.context.set_fill_style_str(color);
        self.context.fill();
    }
    pub fn path(&self, x0: R, y0: R, t0: &CoordR, x1: R, y1: R, t1: &CoordR, color: &str) {
        assert_eq!(t0.len(), 2);
        assert_eq!(t1.len(), 2);
        let (x0t, y0t) = (lerp(x0, x1, t0[0]), lerp(y0, y1, t0[1]));
        let (x1t, y1t) = (lerp(x1, x0, t1[0]), lerp(y1, y0, t1[1]));
        self.context.begin_path();
        self.context.move_to(x0, y0);
        self.context.bezier_curve_to(x0t, y0t, x1t, y1t, x1, y1);
        self.context.set_stroke_style_str(color);
        self.context.set_line_width(6.0);
        self.context.stroke();
    }
    pub fn region(
        &self,
        x0: (R, R),
        y0: R,
        t0: &CoordR,
        x1: (R, R),
        y1: R,
        t1: &CoordR,
        color: &str,
    ) {
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
        self.context.set_fill_style_str(color);
        self.context.fill();
        // self.context.set_stroke_style_str("rgba(255 255 255 0.1)");
        // self.context.set_line_width(1.0);
        // self.context.stroke();
    }

    fn color(&self, color: (u8, u8, u8)) -> String {
        format!("rgb({} {} {})", color.0, color.1, color.2)
    }

    fn brighten(color: (u8, u8, u8)) -> (u8, u8, u8) {
        let f = |c: u8| (c as f64 + (255.0 - c as f64) * 0.2) as u8;
        (f(color.0), f(color.1), f(color.2))
    }

    pub fn cube(&self, cube: &Cuboid, color: (u8, u8, u8)) {
        match cube {
            Cuboid::Point(p) => {
                assert_eq!(p.len(), 2);
                self.circle(p[0], p[1], &self.color(color));
            }
            Cuboid::Bridge { source, target } => match (source.0.as_ref(), target.0.as_ref()) {
                (Cuboid::Point(s), Cuboid::Point(t)) => {
                    assert_eq!(s.len(), 1);
                    assert_eq!(t.len(), 1);
                    self.path(
                        s[0],
                        source.1,
                        &source.2,
                        t[0],
                        target.1,
                        &target.2,
                        &self.color(color),
                    );
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
                        &self.color((color.0 / 2, color.1 / 2, color.2 / 2)),
                    );
                }
                _ => unreachable!(),
            },
        }
    }

    pub fn cell(&self, cell: &Geometry, table: &PrimTable, highlight: Option<&Prim>) {
        for cubes in cell.cubes.iter() {
            for (prim, cube) in cubes {
                let entry = table.get(prim).unwrap();
                let color = if highlight == Some(prim) {
                    Self::brighten(entry.color)
                } else {
                    entry.color
                };
                self.cube(cube, color);
            }
        }

        for (prim, center, r2) in &cell.spheres {
            let entry = table.get(prim).unwrap();
            let color = if highlight == Some(prim) {
                Self::brighten(entry.color)
            } else {
                entry.color
            };
            let str = format!("rgb({} {} {})", color.0, color.1, color.2);

            assert_eq!(center.len(), 2);
            let r = r2.sqrt();
            let x = center[0];
            let y = center[1];
            self.context.begin_path();
            self.context
                .arc(x, y, r, 0.0, std::f64::consts::PI * 2.0)
                .unwrap();
            self.context.close_path();
            self.context.set_fill_style_str(&str);
            self.context.fill();
        }
    }

    pub fn hit_test(&self, cell: &Geometry, mx: R, my: R) -> Option<Prim> {
        // Reset transform so isPointInPath/isPointInStroke work in local coordinates
        self.context.save();
        let _ = self.context.set_transform(1.0, 0.0, 0.0, 1.0, 0.0, 0.0);

        let mut hit: Option<Prim> = None;

        for cubes in cell.cubes.iter() {
            for (prim, cube) in cubes {
                if self.hit_test_cube(cube, mx, my) {
                    hit = Some(prim.clone());
                }
            }
        }

        // Spheres are drawn on top of cubes, so they take priority
        for (prim, center, r2) in &cell.spheres {
            assert_eq!(center.len(), 2);
            let dx = center[0] - mx;
            let dy = center[1] - my;
            if dx * dx + dy * dy < *r2 {
                hit = Some(prim.clone());
            }
        }

        self.context.restore();
        hit
    }

    fn hit_test_cube(&self, cube: &Cuboid, mx: R, my: R) -> bool {
        match cube {
            Cuboid::Point(p) => {
                assert_eq!(p.len(), 2);
                let dx = p[0] - mx;
                let dy = p[1] - my;
                dx * dx + dy * dy < 8.0 * 8.0
            }
            Cuboid::Bridge { source, target } => match (source.0.as_ref(), target.0.as_ref()) {
                (Cuboid::Point(s), Cuboid::Point(t)) => {
                    // 1-cell: bezier curve path, use isPointInStroke
                    assert_eq!(s.len(), 1);
                    assert_eq!(t.len(), 1);
                    self.context.begin_path();
                    self.context.move_to(s[0], source.1);
                    let (x0t, y0t) = (
                        lerp(s[0], t[0], source.2[0]),
                        lerp(source.1, target.1, source.2[1]),
                    );
                    let (x1t, y1t) = (
                        lerp(t[0], s[0], target.2[0]),
                        lerp(target.1, source.1, target.2[1]),
                    );
                    self.context
                        .bezier_curve_to(x0t, y0t, x1t, y1t, t[0], target.1);
                    self.context.set_line_width(6.0);
                    self.context.is_point_in_stroke_with_x_and_y(mx, my)
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
                    // 2-cell: filled bezier region, use isPointInPath
                    let (x0s, y0) = (ss.1, source.1);
                    let x0t = st.1;
                    let (x1s, y1) = (ts.1, target.1);
                    let x1t = tt.1;
                    let (x0st, y0st) = (
                        lerp(x0s, x1s, source.2[0]),
                        lerp(y0, y1, source.2[1]),
                    );
                    let (x0tt, y0tt) = (
                        lerp(x0t, x1t, source.2[0]),
                        lerp(y0, y1, source.2[1]),
                    );
                    let (x1st, y1st) = (
                        lerp(x1s, x0s, target.2[0]),
                        lerp(y1, y0, target.2[1]),
                    );
                    let (x1tt, y1tt) = (
                        lerp(x1t, x0t, target.2[0]),
                        lerp(y1, y0, target.2[1]),
                    );
                    self.context.begin_path();
                    self.context.move_to(x0s, y0);
                    self.context
                        .bezier_curve_to(x0st, y0st, x1st, y1st, x1s, y1);
                    self.context.line_to(x1t, y1);
                    self.context
                        .bezier_curve_to(x1tt, y1tt, x0tt, y0tt, x0t, y0);
                    self.context.close_path();
                    self.context.is_point_in_path_with_f64(mx, my)
                }
                _ => false,
            },
        }
    }
}
