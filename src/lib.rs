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

fn set_mouse_callback(mouse: Rc<RefCell<(f64, f64)>>) -> Option<()> {
    let window = web_sys::window()?;
    let on_mouse_move: Closure<dyn Fn(web_sys::MouseEvent)> = {
        Closure::new(move |event: web_sys::MouseEvent| {
            let pos = (event.client_x() as f64, event.client_y() as f64);
            *mouse.borrow_mut() = pos;
        })
    };
    window
        .add_event_listener_with_callback("mousemove", on_mouse_move.as_ref().unchecked_ref())
        .ok()?;
    on_mouse_move.forget();
    Some(())
}

fn request_animation_frame(f: &Closure<dyn FnMut()>) {
    web_sys::window()
        .unwrap()
        .request_animation_frame(f.as_ref().unchecked_ref())
        .unwrap();
}

fn show_notification(notification: &web_sys::HtmlElement, message: &str) {
    notification.set_inner_text(message);
    let _ = notification.class_list().add_1("show");

    let notification_clone = notification.clone();
    let hide_notification = Closure::once(move || {
        let _ = notification_clone.class_list().remove_1("show");
    });

    web_sys::window()
        .unwrap()
        .set_timeout_with_callback_and_timeout_and_arguments_0(
            hide_notification.as_ref().unchecked_ref(),
            3000,
        )
        .unwrap();

    hide_notification.forget();
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
    set_mouse_callback(Rc::clone(&mouse))?;
    let context = canvas
        .get_context("2d")
        .ok()??
        .dyn_into::<web_sys::CanvasRenderingContext2d>()
        .ok()?;
    let app = Rc::new(RefCell::new(App::new(canvas, context, mouse)));

    // Set up button click handler
    let button = document
        .get_element_by_id("update-button")?
        .dyn_into::<web_sys::HtmlElement>()
        .ok()?;
    let textarea = document
        .get_element_by_id("code-input")?
        .dyn_into::<web_sys::HtmlTextAreaElement>()
        .ok()?;

    let notification = document
        .get_element_by_id("notification")?
        .dyn_into::<web_sys::HtmlElement>()
        .ok()?;

    let app_clone = Rc::clone(&app);
    let textarea_clone = textarea.clone();
    let notification_clone = notification.clone();
    let on_click: Closure<dyn FnMut()> = Closure::new(move || {
        let code = textarea_clone.value();
        if let Err(e) = app_clone.borrow_mut().update_code(&code) {
            log::error!("Failed to update code: {}", e);
            show_notification(&notification_clone, &e);
        }
    });
    button
        .add_event_listener_with_callback("click", on_click.as_ref().unchecked_ref())
        .ok()?;
    on_click.forget();

    // Set up Ctrl+Enter shortcut
    let app_clone = Rc::clone(&app);
    let textarea_clone = textarea.clone();
    let on_keydown: Closure<dyn FnMut(web_sys::KeyboardEvent)> =
        Closure::new(move |event: web_sys::KeyboardEvent| {
            if event.key() == "Enter" && event.ctrl_key() {
                event.prevent_default();
                let code = textarea_clone.value();
                if let Err(e) = app_clone.borrow_mut().update_code(&code) {
                    log::error!("Failed to update code: {}", e);
                    show_notification(&notification, &e);
                }
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
