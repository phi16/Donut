use wasm_bindgen::JsValue;
use web_sys::console;

pub fn println(s: &str) {
  console::log_1(&JsValue::from_str(s));
}
