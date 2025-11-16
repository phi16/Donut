mod app;

use crate::app::App;
use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn main_js() -> Result<(), JsValue> {
    #[cfg(debug_assertions)]
    console_error_panic_hook::set_once();

    start().unwrap();
    Ok(())
}

fn set_resize_callback(canvas: web_sys::HtmlCanvasElement) -> Option<()> {
    let window = web_sys::window()?;
    let on_resize = {
        let window = window.clone();
        move || {
            let width = window.inner_width().unwrap().as_f64().unwrap() as u32;
            let height = window.inner_height().unwrap().as_f64().unwrap() as u32;
            canvas.set_width(width);
            canvas.set_height(height);
        }
    };
    on_resize();
    let resize_callback = Closure::new(Box::new(on_resize) as Box<dyn FnMut()>);
    window
        .add_event_listener_with_callback("resize", resize_callback.as_ref().unchecked_ref())
        .ok()?;
    resize_callback.forget();
    Some(())
}

fn request_animation_frame(f: &Closure<dyn FnMut()>) {
    web_sys::window()
        .unwrap()
        .request_animation_frame(f.as_ref().unchecked_ref())
        .unwrap();
}

pub fn start() -> Option<()> {
    let window = web_sys::window()?;
    let document = window.document()?;
    let canvas = document
        .get_element_by_id("canvas")?
        .dyn_into::<web_sys::HtmlCanvasElement>()
        .ok()?;
    set_resize_callback(canvas.clone())?;
    let context = canvas
        .get_context("2d")
        .ok()??
        .dyn_into::<web_sys::CanvasRenderingContext2d>()
        .ok()?;
    let mut app = App::new(canvas, context);

    let step: Rc<RefCell<Option<Closure<dyn FnMut()>>>> = Rc::new(RefCell::new(None));
    let step_c = Rc::clone(&step);
    *step_c.borrow_mut() = Some(Closure::new(move || {
        app.step();
        request_animation_frame(step.borrow().as_ref().unwrap());
    }));
    request_animation_frame(step_c.borrow().as_ref().unwrap());

    Some(())
}
