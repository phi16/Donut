pub mod api;
mod canvas;
pub mod engine;
mod shader_view;

use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn main_js() -> Result<(), JsValue> {
    #[cfg(debug_assertions)]
    console_error_panic_hook::set_once();
    wasm_logger::init(wasm_logger::Config::default());
    Ok(())
}

// --- Canvas rendering exported for TS ---

#[wasm_bindgen]
pub fn canvas_step(
    engine: &mut api::WasmEngine,
    context: &web_sys::CanvasRenderingContext2d,
    width: f64,
    height: f64,
    mouse_x: f64,
    mouse_y: f64,
    pressing: bool,
) {
    canvas::step(
        engine.engine_mut(),
        context,
        width,
        height,
        mouse_x,
        mouse_y,
        pressing,
    );
}

// --- Shader view wrapper for TS ---

#[wasm_bindgen]
pub struct WasmShaderView {
    inner: shader_view::ShaderView,
}

#[wasm_bindgen]
pub fn create_shader_view(canvas: web_sys::HtmlCanvasElement) -> Option<WasmShaderView> {
    shader_view::ShaderView::new(canvas).map(|sv| WasmShaderView { inner: sv })
}

#[wasm_bindgen]
impl WasmShaderView {

    pub fn set_shader(&mut self, fragment_source: &str) -> Result<(), JsValue> {
        self.inner
            .set_shader(fragment_source)
            .map_err(|e| JsValue::from_str(&e))
    }

    pub fn render(&self) {
        self.inner.render();
    }

    pub fn show(&self) {
        self.inner.show();
    }

    pub fn hide(&self) {
        self.inner.hide();
    }
}

// --- Default code ---

#[wasm_bindgen]
pub fn default_code() -> String {
    include_str!("default.donut").to_string()
}
