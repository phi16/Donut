mod app;

use crate::app::App;
use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn main_js() -> Result<(), JsValue> {
    #[cfg(debug_assertions)]
    console_error_panic_hook::set_once();
    wasm_logger::init(wasm_logger::Config::default());

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

fn set_mouse_callback(
    canvas: &web_sys::HtmlCanvasElement,
    mouse: Rc<RefCell<(f64, f64)>>,
    pressing: Rc<RefCell<bool>>,
) -> Option<()> {
    let on_mouse_move: Closure<dyn Fn(web_sys::MouseEvent)> = {
        let mouse = Rc::clone(&mouse);
        Closure::new(move |event: web_sys::MouseEvent| {
            *mouse.borrow_mut() = (event.client_x() as f64, event.client_y() as f64);
        })
    };
    // mousemove on window so we track cursor even outside canvas
    let window = web_sys::window()?;
    window
        .add_event_listener_with_callback("mousemove", on_mouse_move.as_ref().unchecked_ref())
        .ok()?;
    on_mouse_move.forget();

    // mousedown/mouseup on canvas only, so editor clicks don't affect slicing
    let on_mouse_down: Closure<dyn Fn(web_sys::MouseEvent)> = {
        let pressing = Rc::clone(&pressing);
        Closure::new(move |_: web_sys::MouseEvent| {
            *pressing.borrow_mut() = true;
        })
    };
    canvas
        .add_event_listener_with_callback("mousedown", on_mouse_down.as_ref().unchecked_ref())
        .ok()?;
    on_mouse_down.forget();

    let on_mouse_up: Closure<dyn Fn(web_sys::MouseEvent)> = {
        Closure::new(move |_: web_sys::MouseEvent| {
            *pressing.borrow_mut() = false;
        })
    };
    window
        .add_event_listener_with_callback("mouseup", on_mouse_up.as_ref().unchecked_ref())
        .ok()?;
    on_mouse_up.forget();

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
    let mouse = Rc::new(RefCell::new((0.0, 0.0)));
    let pressing = Rc::new(RefCell::new(false));
    set_mouse_callback(&canvas, Rc::clone(&mouse), Rc::clone(&pressing))?;
    let context = canvas
        .get_context("2d")
        .ok()??
        .dyn_into::<web_sys::CanvasRenderingContext2d>()
        .ok()?;
    let entry_select = document
        .get_element_by_id("entry-select")?
        .dyn_into::<web_sys::HtmlSelectElement>()
        .ok()?;
    let eval_result_el = document
        .get_element_by_id("eval-result")?
        .dyn_into::<web_sys::HtmlElement>()
        .ok()?;
    let diagnostics_el = document
        .get_element_by_id("diagnostics")?
        .dyn_into::<web_sys::HtmlElement>()
        .ok()?;

    let textarea = document
        .get_element_by_id("code-input")?
        .dyn_into::<web_sys::HtmlTextAreaElement>()
        .ok()?;

    // Populate textarea with default code
    textarea.set_value(include_str!("default.donut"));

    let app = Rc::new(RefCell::new(App::new(
        canvas,
        context,
        mouse,
        pressing,
        entry_select.clone(),
        eval_result_el,
        diagnostics_el,
    )));

    // Set up entry selection handler
    {
        let app_clone = Rc::clone(&app);
        let select_clone = entry_select.clone();
        let on_change: Closure<dyn FnMut()> = Closure::new(move || {
            let value = select_clone.value();
            if let Ok(index) = value.parse::<usize>() {
                app_clone.borrow_mut().select_entry(index);
            }
        });
        entry_select
            .add_event_listener_with_callback("change", on_change.as_ref().unchecked_ref())
            .ok()?;
        on_change.forget();
    }

    // Set up Ctrl+Enter shortcut
    let app_clone = Rc::clone(&app);
    let textarea_clone = textarea.clone();
    let on_keydown: Closure<dyn FnMut(web_sys::KeyboardEvent)> =
        Closure::new(move |event: web_sys::KeyboardEvent| {
            if event.key() == "Enter" && event.ctrl_key() {
                event.prevent_default();
                let code = textarea_clone.value();
                app_clone.borrow_mut().update_code(&code);
            }
        });
    textarea
        .add_event_listener_with_callback("keydown", on_keydown.as_ref().unchecked_ref())
        .ok()?;
    on_keydown.forget();

    // Animation loop
    let step: Rc<RefCell<Option<Closure<dyn FnMut()>>>> = Rc::new(RefCell::new(None));
    let step_c = Rc::clone(&step);
    let app_clone = Rc::clone(&app);
    *step_c.borrow_mut() = Some(Closure::new(move || {
        app_clone.borrow_mut().step();
        request_animation_frame(step.borrow().as_ref().unwrap());
    }));
    request_animation_frame(step_c.borrow().as_ref().unwrap());

    Some(())
}
