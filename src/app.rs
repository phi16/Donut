use donut_core::*;
use donut_renderer::Renderer;
use std::rc::Rc;

pub struct App {
    renderer: Renderer,
    canvas: web_sys::HtmlCanvasElement,
    context: web_sys::CanvasRenderingContext2d,
    cell: Rc<LayoutCell>,
}

impl App {
    pub fn new(
        canvas: web_sys::HtmlCanvasElement,
        context: web_sys::CanvasRenderingContext2d,
    ) -> Self {
        let prim_table = Rc::new(PrimTable::new());
        let cell = Rc::new(LayoutCell(
            CellF::Prim(0, ShapeF::Zero),
            Layout {
                size: vec![],
                children: vec![],
            },
        ));
        Self {
            renderer: Renderer::new(context.clone(), prim_table),
            canvas,
            context,
            cell,
        }
    }

    pub fn step(&self) {
        let width = self.canvas.width() as f64;
        let height = self.canvas.height() as f64;
        self.context.set_fill_style_str("white");
        self.context.fill_rect(0.0, 0.0, width, height);
        self.context.set_fill_style_str("rgb(0 0 255 / 0.5)");
        self.context
            .fill_rect(5.0, 5.0, width - 10.0, height - 10.0);

        self.renderer.render(&self.cell);
    }
}
