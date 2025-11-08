use wasm_bindgen::prelude::*;

pub struct App {
    context: web_sys::CanvasRenderingContext2d,
}

impl App {
    pub fn new(context: web_sys::CanvasRenderingContext2d) -> Self {
        Self { context }
    }

    pub fn step(&self) {
        self.context.set_fill_style(&JsValue::from_str("blue"));
        self.context.fill_rect(50.0, 50.0, 100.0, 100.0);
    }
}
