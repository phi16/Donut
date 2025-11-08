use donut_renderer::Renderer;

pub struct App {
    renderer: Renderer,
    context: web_sys::CanvasRenderingContext2d,
}

impl App {
    pub fn new(context: web_sys::CanvasRenderingContext2d) -> Self {
        Self {
            renderer: Renderer::new(context.clone()),
            context,
        }
    }

    pub fn step(&self) {
        self.context.set_fill_style_str("blue");
        self.context.fill_rect(50.0, 50.0, 100.0, 100.0);
    }
}
