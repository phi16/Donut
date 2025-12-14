use donut_core::layout_cell::*;
// use donut_renderer::geometry::Geometry;
use donut_renderer::render::Renderer;
use donut_util::println;
use std::rc::Rc;

pub struct App {
    renderer: Renderer,
    canvas: web_sys::HtmlCanvasElement,
    context: web_sys::CanvasRenderingContext2d,
}

impl App {
    pub fn new(
        canvas: web_sys::HtmlCanvasElement,
        context: web_sys::CanvasRenderingContext2d,
    ) -> Self {
        Self {
            renderer: Renderer::new(context.clone()),
            canvas,
            context,
        }
    }

    pub fn step(&mut self) {
        let width = self.canvas.width() as f64;
        let height = self.canvas.height() as f64;
        self.context.set_fill_style_str("rgb(40 40 40)");
        self.context.fill_rect(0.0, 0.0, width, height);

        self.context.set_fill_style_str("rgb(255 255 255)"); // why flickers
        self.context
            .arc(
                width / 2.0,
                height / 2.0,
                100.0,
                0.0,
                std::f64::consts::PI * 2.0,
            )
            .unwrap();
        self.context.fill();
    }
}
