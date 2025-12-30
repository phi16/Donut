use donut_core::cell::*;
use donut_core::common::*;
use donut_util::println;

type R = f64;

pub fn assoc<T: CellFactory>(cb: &mut T) -> T::Cell {
    let a = cb.zero(Prim::new(0));
    let x = cb.prim_c(Prim::new(1), &a, &a);
    let xx = cb.comp_c(0, vec![&x, &x]).unwrap();
    let m = cb.prim_c(Prim::new(2), &xx, &x);
    let xi = cb.id_c(&x);
    let mx = cb.comp_c(0, vec![&m, &xi]).unwrap();
    let xm = cb.comp_c(0, vec![&xi, &m]).unwrap();
    let mm_l = cb.comp_c(1, vec![&mx, &m]).unwrap();
    let mm_r = cb.comp_c(1, vec![&xm, &m]).unwrap();
    let assoc = cb.prim_c(Prim::new(3), &mm_l, &mm_r);
    assoc
}

pub struct App {
    canvas: web_sys::HtmlCanvasElement,
    context: web_sys::CanvasRenderingContext2d,
    t: R,
}

impl App {
    pub fn new(
        canvas: web_sys::HtmlCanvasElement,
        context: web_sys::CanvasRenderingContext2d,
    ) -> Self {
        Self {
            canvas,
            context,
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

        self.t += 0.03;
    }
}
