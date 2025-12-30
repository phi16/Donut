pub struct Renderer {
    context: web_sys::CanvasRenderingContext2d,
}

impl Renderer {
    pub fn new(context: web_sys::CanvasRenderingContext2d) -> Self {
        Self { context }
    }
}
