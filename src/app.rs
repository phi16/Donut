use donut_core::cell::*;
use donut_core::table::*;
use donut_renderer::render::Renderer;
use donut_util::println;
use nonempty::nonempty;
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
        let i = prim_table.prim("i");
        let ii = prim_table.id(&i, 50);
        let ij = prim_table.id(&i, 150);
        let m = prim_table.prim("m");
        let w = prim_table.prim("w");
        let g1 = prim_table.comp(nonempty![Rc::clone(&ii), Rc::clone(&m)], 0, 80);
        let g2 = prim_table.comp(nonempty![Rc::clone(&w), Rc::clone(&ij)], 0, 20);
        let cell = prim_table.comp(nonempty![g1, g2], 1, 40);

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
        self.context.set_fill_style_str("rgb(40 40 40)");
        self.context.fill_rect(0.0, 0.0, width, height);
        self.renderer.push();
        self.renderer.offset(50, 50);
        self.renderer.render(&self.cell, 0, 1);
        self.renderer.pop();
    }
}
