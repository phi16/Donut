use donut_core::cell::*;
use donut_core::table::*;
use donut_renderer::Renderer;
use std::rc::Rc;

pub struct App {
    renderer: Renderer,
    canvas: web_sys::HtmlCanvasElement,
    context: web_sys::CanvasRenderingContext2d,
    prim_table: Rc<PrimTable>,
    cell: Rc<LayoutCell>,
}

impl App {
    pub fn new(
        canvas: web_sys::HtmlCanvasElement,
        context: web_sys::CanvasRenderingContext2d,
    ) -> Self {
        let prim_table = Rc::new(PrimTable::default());
        let cell = prim_table.prim("m");
        Self {
            renderer: Renderer::new(context.clone(), Rc::clone(&prim_table)),
            canvas,
            context,
            prim_table,
            cell,
        }
    }

    pub fn step(&self) {
        let width = self.canvas.width() as f64;
        let height = self.canvas.height() as f64;
        self.context.set_fill_style_str("rgb(20 20 20)");
        self.context.fill_rect(0.0, 0.0, width, height);
        self.renderer.push();
        self.renderer.offset(50, 50);
        self.renderer.render(&self.cell, 0, 1);
        self.renderer.pop();
    }
}
