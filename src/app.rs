use donut_core::cell::*;
use donut_core::common::*;
use donut_core::layout_cell::{LayoutCell, LayoutCellFactory};
use donut_renderer::render::Renderer;
use donut_renderer::render_cell::{RenderCell, R};
use donut_util::println;

// pub fn assoc<T: CellFactory>(f: &mut T) -> T::Cell {
pub fn assoc(f: &mut LayoutCellFactory) -> LayoutCell {
    let a = f.zero(Prim::new(0));
    let x = f.prim_c(Prim::new(1), &a, &a);
    let xx = f.comp_c(0, vec![&x, &x]);
    let m = f.prim_c(Prim::new(2), &xx, &x);
    let xi = f.id_c(&x);
    let mx = f.comp_c(0, vec![&m, &xi]);
    let xm = f.comp_c(0, vec![&xi, &m]);
    let mm_l = f.comp_c(1, vec![&mx, &m]);
    let mm_r = f.comp_c(1, vec![&xm, &m]);
    let assoc = f.prim_c(Prim::new(3), &mm_l, &mm_r);
    let xii = f.id_c(&xi);
    let ax = f.comp_c(0, vec![&assoc, &xii]);

    let interchange = {
        let mmx = f.comp_c(0, vec![&mm_r, &xi]);
        let xmx = f.comp_c(0, vec![&xi, &m, &xi]);
        let mmx2 = f.comp_c(1, vec![&xmx, &mx]);
        let ch = f.prim_c(Prim::new(4), &mmx, &mmx2);
        let mi = f.id_c(&m);
        f.comp_c(1, vec![&ch, &mi])
    };

    let mi = f.id_c(&m);
    let am = f.comp_c(1, vec![&ax, &mi]);
    let xmx = f.comp_c(0, vec![&xi, &m, &xi]);
    let xmxi = f.id_c(&xmx);
    let ma = f.comp_c(1, vec![&xmxi, &assoc]);
    f.comp_c(2, vec![&am, &interchange, &ma])
}

pub struct App {
    canvas: web_sys::HtmlCanvasElement,
    context: web_sys::CanvasRenderingContext2d,
    cell: RenderCell,
    t: R,
}

impl App {
    pub fn new(
        canvas: web_sys::HtmlCanvasElement,
        context: web_sys::CanvasRenderingContext2d,
    ) -> Self {
        let mut f = LayoutCellFactory::new();
        let cell = assoc(&mut f);
        let sol = f.solve(&cell);
        let cell = sol.convert(&cell);
        let cell = cell.render();
        let cell = RenderCell::from(&cell);
        // println(&format!("Cell: {:?}", cell));
        Self {
            canvas,
            context,
            cell,
            t: 0.0,
        }
    }

    pub fn step(&mut self) {
        let width = self.canvas.width() as f64;
        let height = self.canvas.height() as f64;
        self.context.set_fill_style_str("rgb(40 40 40)");
        self.context.fill_rect(0.0, 0.0, width, height);

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
        self.context.fill();

        self.context.save();
        self.context.translate(100.0, 50.0).unwrap();
        let renderer = Renderer::new(self.context.clone());
        let x = (self.t * 0.125).fract() * 300.0; // (self.t.sin() * 0.5 + 0.5) * 300.0;
        let rc = self.cell.sliced(x);
        // let rc = &self.cell;
        renderer.cell(&rc);
        self.context.restore();

        self.t += 0.03;
    }
}
