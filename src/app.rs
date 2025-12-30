use donut_core::cell::*;
use donut_core::common::*;
use donut_util::println;

type R = f64;

pub fn assoc<T: Cellular>() -> T {
    let a = T::zero(Prim::new(0));
    let x = T::prim(Prim::new(1), a.clone(), a.clone());
    let xx = T::comp(0, vec![x.clone(), x.clone()]).unwrap();
    let m = T::prim(Prim::new(2), xx.clone(), x.clone());
    let xi = T::id(x.clone());
    let mx = T::comp(0, vec![m.clone(), xi.clone()]).unwrap();
    let xm = T::comp(0, vec![xi.clone(), m.clone()]).unwrap();
    let mm_l = T::comp(1, vec![mx, m.clone()]).unwrap();
    let mm_r = T::comp(1, vec![xm, m.clone()]).unwrap();
    let assoc = T::prim(Prim::new(3), mm_l, mm_r);
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
