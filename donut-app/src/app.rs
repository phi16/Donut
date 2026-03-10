use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::shader_view::ShaderView;
use donut_core::cell::*;
use donut_core::common::*;
use donut_core::free_cell::FreeCell;
use donut_lang::types::env::{Color, Env};
use donut_lang::types::item::DefId;
use donut_layout::layout_solver::LayoutSolver;
use donut_renderer::geometry::{Geometry, R};
use donut_renderer::prim_table::PrimTable;
use donut_renderer::render::Renderer;
use donut_runtime::Runtime;
use wasm_bindgen::JsCast;

const MARGIN: R = 100.0;
const GAP: R = 100.0;

pub struct App {
    canvas: web_sys::HtmlCanvasElement,
    context: web_sys::CanvasRenderingContext2d,
    mouse: Rc<RefCell<(f64, f64)>>,
    pressing: Rc<RefCell<bool>>,
    entry_select: web_sys::HtmlSelectElement,
    eval_result_el: web_sys::HtmlElement,
    diagnostics_el: web_sys::HtmlElement,
    shader_view: Option<ShaderView>,
    root_entries: Vec<DefId>,
    table: PrimTable,
    runtime: Runtime,
    selected: Option<DefId>,
    cell: Option<Geometry>,
    slice_pos: Vec<R>,
    diagnostics: Vec<String>,
}

fn format_css_color(c: &Color) -> String {
    format!("rgb({}, {}, {})", c.r(), c.g(), c.b())
}

fn def_cell(env: &Env, def_id: DefId) -> Option<FreeCell> {
    match &env.defs[def_id.0].val {
        PureVal::Cell(pc) => Some(FreeCell::from_pure(pc)),
        _ => None,
    }
}

fn root_entries(env: &Env) -> Vec<DefId> {
    let mut entries = Vec::new();
    for (_, child) in &env.root.lookup {
        if let Some(def_id) = child.this {
            entries.push(def_id);
        }
    }
    entries
}

fn prim_lookup(env: &Env) -> HashMap<String, PrimId> {
    env.prim_item
        .iter()
        .map(|(&prim_id, &item_id)| (env.items[item_id.0].cname.clone(), prim_id))
        .collect()
}

fn prim_names(env: &Env) -> HashMap<PrimId, String> {
    env.prim_item
        .iter()
        .map(|(&prim_id, &item_id)| (prim_id, env.items[item_id.0].cname.clone()))
        .collect()
}

impl App {
    pub fn new(
        canvas: web_sys::HtmlCanvasElement,
        context: web_sys::CanvasRenderingContext2d,
        mouse: Rc<RefCell<(f64, f64)>>,
        pressing: Rc<RefCell<bool>>,
        entry_select: web_sys::HtmlSelectElement,
        eval_result_el: web_sys::HtmlElement,
        diagnostics_el: web_sys::HtmlElement,
        shader_view: Option<ShaderView>,
    ) -> Self {
        let input = include_str!("default.donut");
        let (table, runtime, diagnostics) = Self::load(input);
        let root_entries = root_entries(table.env());
        let selected = Self::find_last_cell(table.env(), &root_entries);
        let cell = selected.and_then(|id| {
            def_cell(table.env(), id).map(|c| Self::build_geometry(&c))
        });
        let slice_pos = cell
            .as_ref()
            .map(|c| Self::init_slice_pos(&c.size))
            .unwrap_or_default();

        let mut app = Self {
            canvas,
            context,
            mouse,
            pressing,
            entry_select,
            eval_result_el,
            diagnostics_el,
            shader_view,
            root_entries,
            table,
            runtime,
            selected,
            cell,
            slice_pos,
            diagnostics,
        };
        app.populate_select();
        app.update_eval_result();
        app
    }

    fn env(&self) -> &Env {
        self.table.env()
    }

    fn find_last_cell(env: &Env, root_entries: &[DefId]) -> Option<DefId> {
        root_entries.iter().rev().find_map(|&def_id| {
            let def = &env.defs[def_id.0];
            if def.origin.is_some() {
                return None;
            }
            if let PureVal::Cell(_) = &def.val {
                Some(def_id)
            } else {
                None
            }
        })
    }

    fn load(code: &str) -> (PrimTable, Runtime, Vec<String>) {
        let user_code = dedent(code);
        let (env, errors) = donut_lang::load::load(&user_code);
        let diagnostics: Vec<String> = errors
            .into_iter()
            .map(|(pos, msg)| format!("{}:{}: {}", pos.line + 1, pos.col + 1, msg))
            .collect();

        let lookup = prim_lookup(&env);
        let table = PrimTable::new(env);

        let mut runtime = Runtime::new();
        donut_runtime::env::register_sys(&mut runtime, &lookup);

        (table, runtime, diagnostics)
    }

    fn build_geometry(free_cell: &FreeCell) -> Geometry {
        let mut f = LayoutSolver::new();
        let cell = f.from_free(free_cell.clone());
        let sol = f.solve(&cell);
        let cell = sol.convert(&cell);
        let mut cell = cell.render();
        while cell.max.len() < 4 || cell.max.len() % 2 != 0 {
            cell.shift(&Q::from(0), &Q::from(1));
        }
        Geometry::from(&cell)
    }

    fn init_slice_pos(size: &[R]) -> Vec<R> {
        size[2..].iter().map(|s| s / 2.0).collect()
    }

    fn populate_select(&self) {
        self.entry_select.set_inner_html("");
        let env = self.env();

        let document = web_sys::window().unwrap().document().unwrap();
        for &def_id in &self.root_entries {
            let def = &env.defs[def_id.0];
            if def.origin.is_some() {
                continue;
            }
            let Some(free) = def_cell(env, def_id) else {
                continue;
            };
            let option = document
                .create_element("option")
                .unwrap()
                .dyn_into::<web_sys::HtmlOptionElement>()
                .unwrap();
            let dim = free.pure.dim().in_space;
            let label = format!("{} ({}d)", def.lname, dim);
            option.set_text_content(Some(&label));
            option.set_value(&def_id.0.to_string());
            let color = env.def_color(def);
            let el: &web_sys::HtmlElement = option.unchecked_ref();
            el.style()
                .set_property("color", &format_css_color(color))
                .unwrap();
            if self.selected == Some(def_id) {
                option.set_selected(true);
            }
            self.entry_select.append_child(&option).unwrap();
        }
    }

    pub fn update_code(&mut self, code: &str) {
        let (table, runtime, diagnostics) = Self::load(code);
        self.table = table;
        self.runtime = runtime;
        self.diagnostics = diagnostics;
        self.root_entries = root_entries(self.env());
        self.selected = Self::find_last_cell(self.env(), &self.root_entries);
        self.cell = self.selected.and_then(|id| {
            def_cell(self.env(), id).map(|c| Self::build_geometry(&c))
        });
        self.slice_pos = self
            .cell
            .as_ref()
            .map(|c| Self::init_slice_pos(&c.size))
            .unwrap_or_default();
        self.populate_select();
        self.update_eval_result();
    }

    pub fn select_entry(&mut self, index: usize) {
        let def_id = DefId(index);
        if index >= self.env().defs.len() {
            return;
        }
        let Some(free) = def_cell(self.env(), def_id) else {
            return;
        };
        self.selected = Some(def_id);
        let geom = Self::build_geometry(&free);
        self.slice_pos = Self::init_slice_pos(&geom.size);
        self.cell = Some(geom);
        self.update_eval_result();
    }

    fn build_squash_view(&self, i: usize) -> Geometry {
        let cell = self.cell.as_ref().unwrap();
        let d = 2 + 2 * i;
        let n_extra = cell.size.len() - 2;

        let mut rc = cell.squashed();
        for _ in 1..d {
            rc = rc.squashed();
        }
        for k in (2 * (i + 1)..n_extra).rev() {
            rc = rc.sliced(self.slice_pos[k]);
        }
        rc
    }

    fn build_slice_view(&self) -> Geometry {
        let cell = self.cell.as_ref().unwrap();
        let n_extra = cell.size.len() - 2;
        let mut rc = cell.sliced(self.slice_pos[n_extra - 1]);
        for k in (0..n_extra - 1).rev() {
            rc = rc.sliced(self.slice_pos[k]);
        }
        rc
    }

    pub fn step(&mut self) {
        let width = self.canvas.width() as f64;
        let height = self.canvas.height() as f64;
        self.context.set_fill_style_str("rgb(40 40 40)");
        self.context.fill_rect(0.0, 0.0, width, height);

        let Some(ref cell) = self.cell else { return };

        let size = &cell.size;
        let n_extra = size.len() - 2;
        let n_views = n_extra / 2;

        let mouse = *self.mouse.borrow();
        let renderer = Renderer::new(self.context.clone());

        let slice_origin = (MARGIN, MARGIN);
        let mut squash_origins = vec![];
        let mut cursor_x = MARGIN + size[0] + GAP;
        for i in 0..n_views {
            let d = 2 + 2 * i;
            squash_origins.push((cursor_x, MARGIN));
            cursor_x += size[d] + GAP;
        }

        if *self.pressing.borrow() {
            for i in 0..n_views {
                let d = 2 + 2 * i;
                let (ox, oy) = squash_origins[i];
                let lx = mouse.0 - ox;
                let ly = mouse.1 - oy;
                if lx >= 0.0 && lx <= size[d] && ly >= 0.0 && ly <= size[d + 1] {
                    self.slice_pos[2 * i] = lx;
                    self.slice_pos[2 * i + 1] = ly;
                    break;
                }
            }
        }

        // --- Squashed views ---
        for i in 0..n_views {
            let d = 2 + 2 * i;
            let (ox, oy) = squash_origins[i];
            let vw = size[d];
            let vh = size[d + 1];

            self.context.set_fill_style_str("rgb(50 50 50)");
            self.context
                .fill_rect(ox - 25.0, oy - 25.0, vw + 50.0, vh + 50.0);

            self.context.save();
            self.context.translate(ox, oy).unwrap();

            let rc = self.build_squash_view(i);
            let lmx = mouse.0 - ox;
            let lmy = mouse.1 - oy;
            let hit = if lmx >= 0.0 && lmx <= vw && lmy >= 0.0 && lmy <= vh {
                renderer.hit_test(&rc, lmx, lmy)
            } else {
                None
            };

            renderer.cell(&rc, &self.table, hit.as_ref());

            let sx = self.slice_pos[2 * i];
            let sy = self.slice_pos[2 * i + 1];
            self.context.set_stroke_style_str("rgba(255 255 255 / 0.4)");
            self.context.set_line_width(1.0);
            self.context.begin_path();
            self.context.move_to(sx, 0.0);
            self.context.line_to(sx, vh);
            self.context.move_to(0.0, sy);
            self.context.line_to(vw, sy);
            self.context.stroke();

            if let Some(prim) = &hit {
                let label = self.table.format_prim(prim);
                self.draw_tooltip(&label, lmx + 12.0, lmy - 8.0);
            }

            self.context.restore();
        }

        // --- Params label ---
        self.draw_params(20.0, 30.0);

        // --- Slice view ---
        {
            let (ox, oy) = slice_origin;
            let vw = size[0];
            let vh = size[1];

            self.context.set_fill_style_str("rgb(50 50 50)");
            self.context
                .fill_rect(ox - 25.0, oy - 25.0, vw + 50.0, vh + 50.0);

            self.context.save();
            self.context.translate(ox, oy).unwrap();

            let rc = self.build_slice_view();
            let lmx = mouse.0 - ox;
            let lmy = mouse.1 - oy;
            let hit = if lmx >= 0.0 && lmx <= vw && lmy >= 0.0 && lmy <= vh {
                renderer.hit_test(&rc, lmx, lmy)
            } else {
                None
            };

            renderer.cell(&rc, &self.table, hit.as_ref());

            if let Some(prim) = &hit {
                let label = self.table.format_prim(prim);
                self.draw_tooltip(&label, lmx + 12.0, lmy - 8.0);
            }

            self.context.restore();
        }
    }

    fn update_eval_result(&mut self) {
        // Update diagnostics
        if self.diagnostics.is_empty() {
            let _ = self.diagnostics_el.class_list().remove_1("has-errors");
            self.diagnostics_el.set_inner_text("");
        } else {
            let _ = self.diagnostics_el.class_list().add_1("has-errors");
            self.diagnostics_el
                .set_inner_text(&self.diagnostics.join("\n"));
        }

        // Update eval result
        let Some(selected) = self.selected else {
            self.eval_result_el.set_inner_text("");
            let _ = self.eval_result_el.class_list().remove_1("evaluable");
            self.hide_shader();
            return;
        };
        let env = self.env();
        let def = &env.defs[selected.0];
        let Some(free) = def_cell(env, selected) else {
            self.eval_result_el
                .set_inner_text(&format!("{}: meta", def.lname));
            let _ = self.eval_result_el.class_list().remove_1("evaluable");
            self.hide_shader();
            return;
        };
        let type_str = self.table.format_cell_type(&free.pure);

        let evaluable = self.runtime.is_evaluable(&free);
        let names = prim_names(env);
        let eval_str = match self.runtime.eval_check(&free, &names) {
            Some(reason) => reason,
            None => match self.runtime.eval(&free, &[], &names) {
                Ok(values) => format!("= {}", donut_runtime::format_values(&values)),
                Err(e) => format!("BUG: {}", e),
            },
        };

        if evaluable {
            let _ = self.eval_result_el.class_list().add_1("evaluable");
        } else {
            let _ = self.eval_result_el.class_list().remove_1("evaluable");
        }
        let mut text = format!("{}: {}\n{}", def.lname, type_str, eval_str);

        // GLSL compilation + shader preview
        let mut shader_shown = false;
        match donut_runtime::glsl::compile_to_glsl(&free, &names) {
            Ok(func) => {
                text.push_str("\n\n--- GLSL ---\n");
                text.push_str(&func.to_function(&def.lname));

                if let Some(ref mut sv) = self.shader_view {
                    if let Ok(frag) = func.to_fragment_shader() {
                        match sv.set_shader(&frag) {
                            Ok(()) => {
                                sv.render();
                                sv.show();
                                shader_shown = true;
                            }
                            Err(e) => {
                                text.push_str(&format!("\nshader error: {}", e));
                            }
                        }
                    }
                }
            }
            Err(_) => {}
        }
        if !shader_shown {
            self.hide_shader();
        }

        self.eval_result_el.set_inner_text(&text);
    }

    fn hide_shader(&self) {
        if let Some(ref sv) = self.shader_view {
            sv.hide();
        }
    }

    fn draw_tooltip(&self, text: &str, x: R, y: R) {
        self.context.set_fill_style_str("rgb(255 255 255)");
        self.context.set_font("14px monospace");
        let _ = self.context.fill_text(text, x, y);
    }

    fn draw_params(&self, x: R, y: R) {
        let Some(selected) = self.selected else {
            return;
        };
        let env = self.env();
        let def = &env.defs[selected.0];
        let color = env.def_color(def);
        let text = env.display_params(def);
        if text.is_empty() {
            return;
        }
        self.context
            .set_fill_style_str(&format!("rgba({}, {}, {}, 0.8)", color.r(), color.g(), color.b()));
        self.context.set_font("20px monospace");
        let _ = self.context.fill_text(&text, x, y);
    }
}
