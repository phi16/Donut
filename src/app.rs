use std::cell::RefCell;
use std::rc::Rc;

use donut_core::cell::*;
use donut_core::common::*;
use donut_core::free_cell::FreeCell;
use donut_layout::layout_solver::LayoutSolver;
use donut_renderer::render::Renderer;
use donut_renderer::render_cell::{RenderCell, R};
use donut_util::println;

pub fn assoc<T: Diagram>() -> T {
    let a = T::zero(Prim::new(0));
    let x = T::prim_c(Prim::new(1), &a, &a);
    let xx = T::comp_c(0, vec![&x, &x]);
    let m = T::prim_c(Prim::new(2), &xx, &x);
    let xi = T::id_c(&x);
    let mx = T::comp_c(0, vec![&m, &xi]);
    let xm = T::comp_c(0, vec![&xi, &m]);
    let mm_l = T::comp_c(1, vec![&mx, &m]);
    let mm_r = T::comp_c(1, vec![&xm, &m]);
    let assoc = T::prim_c(Prim::new(3), &mm_l, &mm_r);
    let xii = T::id_c(&xi);
    let ax = T::comp_c(0, vec![&assoc, &xii]);

    let chl = {
        let mmx = T::comp_c(0, vec![&mm_r, &xi]);
        let xmx = T::comp_c(0, vec![&xi, &m, &xi]);
        let mmx2 = T::comp_c(1, vec![&xmx, &mx]);
        let ch = T::prim_c(Prim::new(4), &mmx, &mmx2);
        let mi = T::id_c(&m);
        T::comp_c(1, vec![&ch, &mi])
    };

    let chr = {
        let xmm = T::comp_c(0, vec![&xi, &mm_l]);
        let xmx = T::comp_c(0, vec![&xi, &m, &xi]);
        let xmm2 = T::comp_c(1, vec![&xmx, &xm]);
        let ch = T::prim_c(Prim::new(5), &xmm2, &xmm);
        let mi = T::id_c(&m);
        T::comp_c(1, vec![&ch, &mi])
    };

    let mi = T::id_c(&m);
    let am = T::comp_c(1, vec![&ax, &mi]);
    let xmx = T::comp_c(0, vec![&xi, &m, &xi]);
    let xmxi = T::id_c(&xmx);
    let ma = T::comp_c(1, vec![&xmxi, &assoc]);

    let xa = T::comp_c(0, vec![&xii, &assoc]);
    let am2 = T::comp_c(1, vec![&xa, &mi]);

    let aaa = T::comp_c(2, vec![&am, &chl, &ma, &chr, &am2]);

    let mxx = T::comp_c(0, vec![&m, &xi, &xi]);
    let mxxi = T::id_c(&mxx);
    let oa = T::comp_c(1, vec![&mxxi, &assoc]);

    let xxm = T::comp_c(0, vec![&xi, &xi, &m]);
    let xxmi = T::id_c(&xxm);
    let ao = T::comp_c(1, vec![&xxmi, &assoc]);

    let chx = {
        let vl = T::comp_c(1, vec![&mxx, &xm]);
        let vx = T::comp_c(0, vec![&m, &m]);
        let vr = T::comp_c(1, vec![&xxm, &mx]);
        let ch0 = T::prim_c(Prim::new(6), &vl, &vx);
        let ch1 = T::prim_c(Prim::new(7), &vx, &vr);
        let cc = T::comp_c(2, vec![&ch0, &ch1]);
        T::comp_c(1, vec![&cc, &mi])
    };

    let ichl = {
        let k1 = T::comp_c(0, vec![&mm_l, &xi]);
        let k2 = T::comp_c(1, vec![&mxx, &mx]);
        let k = T::prim_c(Prim::new(8), &k1, &k2);
        T::comp_c(1, vec![&k, &mi])
    };
    let ichr = {
        let k1 = T::comp_c(0, vec![&xi, &mm_r]);
        let k2 = T::comp_c(1, vec![&xxm, &xm]);
        let k = T::prim_c(Prim::new(9), &k2, &k1);
        T::comp_c(1, vec![&k, &mi])
    };

    let oao = T::comp_c(2, vec![&ichl, &oa, &chx, &ao, &ichr]);

    T::prim_c(Prim::new(10), &aaa, &oao)
}

pub struct App {
    canvas: web_sys::HtmlCanvasElement,
    context: web_sys::CanvasRenderingContext2d,
    mouse: Rc<RefCell<(f64, f64)>>,
    cell: RenderCell,
    t: R,
}

impl App {
    pub fn new(
        canvas: web_sys::HtmlCanvasElement,
        context: web_sys::CanvasRenderingContext2d,
        mouse: Rc<RefCell<(f64, f64)>>,
    ) -> Self {
        let cell = assoc::<FreeCell>();
        let mut f = LayoutSolver::new();
        let cell = f.from_free(cell);
        let sol = f.solve(&cell);
        let cell = sol.convert(&cell);
        // println(&format!("Cell: {}", cell));
        let cell = cell.render();
        let cell = RenderCell::from(&cell);
        Self {
            canvas,
            context,
            mouse,
            cell,
            t: 0.0,
        }
    }

    pub fn step(&mut self) {
        let width = self.canvas.width() as f64;
        let height = self.canvas.height() as f64;
        self.context.set_fill_style_str("rgb(40 40 40)");
        self.context.fill_rect(0.0, 0.0, width, height);
        self.context.set_fill_style_str("rgb(50 50 50)");
        self.context.fill_rect(100.0, 150.0, 600.0, 400.0);

        /* self.context.begin_path();
        self.context
            .arc(
                width / 2.0,
                height / 2.0,
                100.0,
                0.0,
                std::f64::consts::PI * 2.0,
            )
            .unwrap();
        self.context.close_path();
        self.context.set_fill_style_str("rgb(255 255 255)"); // why flickers
        self.context.fill(); */

        let mouse = *self.mouse.borrow();
        self.context.save();
        self.context.translate(200.0, 200.0).unwrap();
        let renderer = Renderer::new(self.context.clone());
        // let x = (self.t * 0.125).fract() * 610.0; // (self.t.sin() * 0.5 + 0.5) * 300.0;
        let x = (mouse.0 - 100.0).min(600.0).max(0.0);
        // let y = (mouse.1 - 150.0).min(400.0).max(0.0) / 4.0;
        let y = (mouse.1 - 75.0).min(100.0).max(0.0);
        let rc = &self.cell;
        let rc = rc.sliced(y);
        let rc = rc.sliced(x);
        renderer.cell(&rc);
        self.context.translate(-100.0, -125.0).unwrap();
        let rc = &self.cell;
        let rc = rc.squashed();
        let rc = rc.squashed();
        renderer.cell(&rc);
        self.context.restore();

        self.t += 0.03;
    }
}
