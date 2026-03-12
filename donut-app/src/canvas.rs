use donut_renderer::geometry::{Geometry, R};
use donut_renderer::prim_table::PrimTable;
use donut_renderer::render::Renderer;

use crate::engine::Engine;

const MARGIN: R = 100.0;
const GAP: R = 100.0;

pub fn step(
    engine: &mut Engine,
    context: &web_sys::CanvasRenderingContext2d,
    width: f64,
    height: f64,
    mouse_x: f64,
    mouse_y: f64,
    pressing: bool,
) {
    context.set_fill_style_str("rgb(40 40 40)");
    context.fill_rect(0.0, 0.0, width, height);

    let Some(ref cell) = engine.cell else { return };

    let size = &cell.size;
    let n_extra = size.len() - 2;
    let n_views = n_extra / 2;

    let renderer = Renderer::new(context.clone());

    let slice_origin = (MARGIN, MARGIN);
    let mut squash_origins = vec![];
    let mut cursor_x = MARGIN + size[0] + GAP;
    for i in 0..n_views {
        let d = 2 + 2 * i;
        squash_origins.push((cursor_x, MARGIN));
        cursor_x += size[d] + GAP;
    }

    if pressing {
        for i in 0..n_views {
            let d = 2 + 2 * i;
            let (ox, oy) = squash_origins[i];
            let lx = mouse_x - ox;
            let ly = mouse_y - oy;
            if lx >= 0.0 && lx <= size[d] && ly >= 0.0 && ly <= size[d + 1] {
                engine.slice_pos[2 * i] = lx;
                engine.slice_pos[2 * i + 1] = ly;
                break;
            }
        }
    }

    let table = engine.table();

    // --- Squashed views ---
    for i in 0..n_views {
        let d = 2 + 2 * i;
        let (ox, oy) = squash_origins[i];
        let vw = size[d];
        let vh = size[d + 1];

        let rc = engine.build_squash_view(i);
        draw_view(table, &renderer, context, &rc, ox, oy, vw, vh, mouse_x, mouse_y);

        draw_crosshair(context, ox, oy, vw, vh,
            engine.slice_pos[2 * i], engine.slice_pos[2 * i + 1]);
    }

    // --- Params label ---
    draw_params(engine, context, 20.0, 30.0);

    // --- Slice view ---
    {
        let (ox, oy) = slice_origin;
        let rc = engine.build_slice_view();
        draw_view(table, &renderer, context, &rc, ox, oy, size[0], size[1], mouse_x, mouse_y);
    }
}

fn draw_view(
    table: &PrimTable,
    renderer: &Renderer,
    context: &web_sys::CanvasRenderingContext2d,
    geom: &Geometry,
    ox: R, oy: R, vw: R, vh: R,
    mouse_x: R, mouse_y: R,
) {
    context.set_fill_style_str("rgb(50 50 50)");
    context.fill_rect(ox - 25.0, oy - 25.0, vw + 50.0, vh + 50.0);

    context.save();
    context.translate(ox, oy).unwrap();

    let lmx = mouse_x - ox;
    let lmy = mouse_y - oy;
    let hit = if lmx >= 0.0 && lmx <= vw && lmy >= 0.0 && lmy <= vh {
        renderer.hit_test(geom, lmx, lmy)
    } else {
        None
    };

    renderer.cell(geom, table, hit.as_ref());

    if let Some(prim) = &hit {
        let label = table.format_prim(prim);
        draw_tooltip(context, &label, lmx + 12.0, lmy - 8.0);
    }

    context.restore();
}

fn draw_crosshair(
    context: &web_sys::CanvasRenderingContext2d,
    ox: R, oy: R, vw: R, vh: R,
    sx: R, sy: R,
) {
    context.save();
    context.translate(ox, oy).unwrap();
    context.set_stroke_style_str("rgba(255 255 255 / 0.4)");
    context.set_line_width(1.0);
    context.begin_path();
    context.move_to(sx, 0.0);
    context.line_to(sx, vh);
    context.move_to(0.0, sy);
    context.line_to(vw, sy);
    context.stroke();
    context.restore();
}

fn draw_tooltip(context: &web_sys::CanvasRenderingContext2d, text: &str, x: R, y: R) {
    context.set_fill_style_str("rgb(255 255 255)");
    context.set_font("14px Noto Sans Mono, monospace");
    let _ = context.fill_text(text, x, y);
}

fn draw_params(engine: &Engine, context: &web_sys::CanvasRenderingContext2d, x: R, y: R) {
    let text = engine.selected_params_text();
    if text.is_empty() {
        return;
    }
    context.set_fill_style_str("rgba(200, 200, 200, 0.8)");
    context.set_font("20px Noto Sans Mono, monospace");
    let _ = context.fill_text(&text, x, y);
}
