use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use donut_core::cell::*;
use donut_core::common::*;
use donut_core::free_cell::FreeCell;
use donut_lang::check::Env;
use donut_runtime::Runtime;
use wasm_bindgen::JsCast;
use donut_layout::layout_solver::LayoutSolver;
use donut_renderer::geometry::{Geometry, R};
use donut_renderer::prim_table::PrimTable;
use donut_renderer::render::Renderer;

const MARGIN: R = 100.0;
const GAP: R = 100.0;

pub struct App {
    canvas: web_sys::HtmlCanvasElement,
    context: web_sys::CanvasRenderingContext2d,
    mouse: Rc<RefCell<(f64, f64)>>,
    pressing: Rc<RefCell<bool>>,
    entry_select: web_sys::HtmlSelectElement,
    eval_result_el: web_sys::HtmlElement,
    env: Env,
    table: PrimTable,
    runtime: Runtime,
    env_entry_count: usize,
    selected: usize,
    cell: Geometry,
    slice_pos: Vec<R>,
}

impl App {
    pub fn new(
        canvas: web_sys::HtmlCanvasElement,
        context: web_sys::CanvasRenderingContext2d,
        mouse: Rc<RefCell<(f64, f64)>>,
        pressing: Rc<RefCell<bool>>,
        entry_select: web_sys::HtmlSelectElement,
        eval_result_el: web_sys::HtmlElement,
    ) -> Self {
        let input = include_str!("default.donut");
        let (env, table, runtime) = Self::load(input).unwrap();
        let env_entry_count = Self::env_entry_count();
        let selected = env.entries.len() - 1;
        let cell = Self::build_geometry(env.entries[selected].body.as_cell().unwrap());

        let mut app = Self {
            canvas,
            context,
            mouse,
            pressing,
            entry_select,
            eval_result_el,
            env,
            table,
            runtime,
            env_entry_count,
            selected,
            cell,
            slice_pos: vec![],
        };
        app.slice_pos = Self::init_slice_pos(&app.cell.size);
        app.populate_select();
        app.update_eval_result();
        app
    }

    const PRELUDE: &str = "import \"base\"\nimport \"ui\"\nenv = import \"sys\"\n";

    fn env_entry_count() -> usize {
        use std::sync::OnceLock;
        static COUNT: OnceLock<usize> = OnceLock::new();
        *COUNT.get_or_init(|| {
            let (env_only, _) = donut_lang::load::load(Self::PRELUDE);
            env_only.entries.len()
        })
    }

    fn load(code: &str) -> Result<(Env, PrimTable, Runtime)> {
        let user_code = dedent(code);
        let full_code = format!("{}{}", Self::PRELUDE, user_code);
        let (env, errors) = donut_lang::load::load(&full_code);
        if !errors.is_empty() {
            for (_, msg) in &errors {
                log::warn!("Load warning: {}", msg);
            }
        }

        let mut table = PrimTable::new();
        for (i, e) in env.entries.iter().enumerate() {
            if let Some(cell) = e.body.as_cell() {
                let prim = Prim::new(i as PrimId);
                table.insert(
                    prim,
                    &e.name,
                    cell.pure.dim().in_space,
                    e.color,
                    e.param_counts.clone(),
                );
            }
        }

        // Build runtime
        let mut prim_lookup: HashMap<String, PrimId> = HashMap::new();
        for (name, &idx) in &env.lookup {
            let entry = &env.entries[idx];
            if let Some(cell) = entry.body.as_cell() {
                if let Some(id) = cell.pure.extract_prim_id() {
                    prim_lookup.insert(name.clone(), id);
                }
            }
        }
        let mut runtime = Runtime::new();
        donut_runtime::env::register_env(&mut runtime, &prim_lookup);

        Ok((env, table, runtime))
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

        let document = web_sys::window().unwrap().document().unwrap();
        for (i, entry) in self.env.entries.iter().enumerate() {
            if i < self.env_entry_count {
                continue;
            }
            let option = document
                .create_element("option")
                .unwrap()
                .dyn_into::<web_sys::HtmlOptionElement>()
                .unwrap();
            let Some(cell) = entry.body.as_cell() else { continue };
            let dim = cell.pure.dim().in_space;
            let label = format!("{} ({}d)", entry.name, dim);
            option.set_text_content(Some(&label));
            option.set_value(&i.to_string());
            if i == self.selected {
                option.set_selected(true);
            }
            self.entry_select.append_child(&option).unwrap();
        }
    }

    pub fn update_code(&mut self, code: &str) -> Result<()> {
        let (env, table, runtime) = Self::load(code)?;
        let selected = env.entries.len() - 1;
        let cell = Self::build_geometry(env.entries[selected].body.as_cell().unwrap());
        self.slice_pos = Self::init_slice_pos(&cell.size);
        self.env = env;
        self.table = table;
        self.runtime = runtime;
        self.selected = selected;
        self.cell = cell;
        self.populate_select();
        self.update_eval_result();
        Ok(())
    }

    pub fn select_entry(&mut self, index: usize) {
        if index >= self.env.entries.len() {
            return;
        }
        self.selected = index;
        self.cell = Self::build_geometry(self.env.entries[index].body.as_cell().unwrap());
        self.slice_pos = Self::init_slice_pos(&self.cell.size);
        self.update_eval_result();
    }

    fn build_squash_view(&self, i: usize) -> Geometry {
        let d = 2 + 2 * i;
        let n_extra = self.cell.size.len() - 2;

        let mut rc = self.cell.squashed();
        for _ in 1..d {
            rc = rc.squashed();
        }
        for k in (2 * (i + 1)..n_extra).rev() {
            rc = rc.sliced(self.slice_pos[k]);
        }
        rc
    }

    fn build_slice_view(&self) -> Geometry {
        let n_extra = self.cell.size.len() - 2;
        let mut rc = self.cell.sliced(self.slice_pos[n_extra - 1]);
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

        let size = self.cell.size.clone();
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
            self.context
                .set_stroke_style_str("rgba(255 255 255 / 0.4)");
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

    fn update_eval_result(&self) {
        let entry = &self.env.entries[self.selected];
        let Some(cell) = entry.body.as_cell() else {
            self.eval_result_el.set_inner_text(&format!("{}: meta", entry.name));
            return;
        };
        let type_str = self.table.format_cell_type(&cell.pure);

        let evaluable = self.runtime.is_evaluable(cell);
        let eval_str = match self.runtime.eval_check(cell) {
            Some(reason) => reason,
            None => match self.runtime.eval(cell, &[]) {
                Ok(values) => format!("= {}", donut_runtime::format_values(&values)),
                Err(e) => format!("BUG: {}", e),
            },
        };

        if evaluable {
            let _ = self.eval_result_el.class_list().add_1("evaluable");
        } else {
            let _ = self.eval_result_el.class_list().remove_1("evaluable");
        }
        self.eval_result_el.set_inner_text(&format!("{}: {}\n{}", entry.name, type_str, eval_str));
    }

    fn draw_tooltip(&self, text: &str, x: R, y: R) {
        self.context.set_fill_style_str("rgb(255 255 255)");
        self.context.set_font("14px monospace");
        let _ = self.context.fill_text(text, x, y);
    }
}
