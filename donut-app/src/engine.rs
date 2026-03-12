use std::collections::HashMap;

use donut_core::cell::*;
use donut_core::common::*;
use donut_core::free_cell::FreeCell;
use donut_lang::types::env::{Color, Env};
use donut_lang::types::item::DefId;
use donut_layout::layout_solver::LayoutSolver;
use donut_renderer::geometry::{Geometry, R};
use donut_renderer::prim_table::PrimTable;
use donut_runtime::Runtime;

pub struct Engine {
    pub(crate) table: PrimTable,
    pub(crate) runtime: Runtime,
    pub(crate) root_entries: Vec<DefId>,
    pub(crate) selected: Option<DefId>,
    pub(crate) cell: Option<Geometry>,
    pub(crate) slice_pos: Vec<R>,
    pub(crate) diagnostics: Vec<String>,
}

pub struct EntryDesc {
    pub index: usize,
    pub name: String,
    pub dimension: u8,
    pub color: [u8; 3],
}

fn def_cell(env: &Env, def_id: DefId) -> Option<FreeCell> {
    match &env.defs[def_id.0].val {
        PureVal::Cell(pc) => Some(FreeCell::from_pure(pc)),
        _ => None,
    }
}

fn root_entries(env: &Env) -> Vec<DefId> {
    env.defs
        .iter()
        .enumerate()
        .filter(|(_, def)| def.origin.is_none())
        .map(|(i, _)| DefId(i))
        .collect()
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

impl Engine {
    pub fn new(code: &str) -> Self {
        let (table, runtime, diagnostics) = Self::load(code);
        let root_entries = root_entries(table.env());
        let selected = Self::find_last_cell(table.env(), &root_entries);
        let cell = selected.and_then(|id| {
            def_cell(table.env(), id).map(|c| Self::build_geometry(&c))
        });
        let slice_pos = cell
            .as_ref()
            .map(|c| Self::init_slice_pos(&c.size))
            .unwrap_or_default();

        Self {
            table,
            runtime,
            root_entries,
            selected,
            cell,
            slice_pos,
            diagnostics,
        }
    }

    pub fn env(&self) -> &Env {
        self.table.env()
    }

    pub fn table(&self) -> &PrimTable {
        &self.table
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
        let user_code = donut_core::common::dedent(code);
        let (env, errors) = donut_lang::load::load(&user_code);
        let diagnostics: Vec<String> = errors
            .into_iter()
            .map(|(pos, msg)| format!("{}:{}: {}", pos.line + 1, pos.col + 1, msg))
            .collect();

        let lookup = prim_lookup(&env);
        let def_subs = env.def_subs.clone();
        let table = PrimTable::new(env);

        let mut runtime = Runtime::new();
        donut_runtime::env::register_sys(&mut runtime, &lookup);
        runtime.set_def_subs(def_subs);

        (table, runtime, diagnostics)
    }

    pub fn build_geometry(free_cell: &FreeCell) -> Geometry {
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

    pub fn init_slice_pos(size: &[R]) -> Vec<R> {
        size[2..].iter().map(|s| s / 2.0).collect()
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
    }

    pub fn root_entry_descs(&self) -> Vec<EntryDesc> {
        let env = self.env();
        let mut descs = Vec::new();
        for &def_id in &self.root_entries {
            let def = &env.defs[def_id.0];
            let Some(free) = def_cell(env, def_id) else {
                continue;
            };
            let dim = free.pure.dim().in_space;
            let color = env.def_color(def);
            descs.push(EntryDesc {
                index: def_id.0,
                name: def.qname.clone(),
                dimension: dim as u8,
                color: [color.r(), color.g(), color.b()],
            });
        }
        descs
    }

    pub fn eval_result_text(&self) -> String {
        let Some(selected) = self.selected else {
            return String::new();
        };
        let env = self.env();
        let def = &env.defs[selected.0];
        let Some(free) = def_cell(env, selected) else {
            return format!("{}: meta", def.lname);
        };
        let sig = env.display_def_signature(def);
        let expanded = self.runtime.expand(&free.pure);
        let names = prim_names(env);
        let eval_str = match self.runtime.eval_check(&expanded, &names) {
            Some(reason) => reason,
            None => match self.runtime.eval(&expanded, &[], &names) {
                Ok(values) => format!("= {}", donut_runtime::format_values(&values)),
                Err(e) => format!("BUG: {}", e),
            },
        };
        format!("{}\n{}", sig, eval_str)
    }

    pub fn is_evaluable(&self) -> bool {
        let Some(selected) = self.selected else {
            return false;
        };
        let env = self.env();
        let Some(free) = def_cell(env, selected) else {
            return false;
        };
        let expanded = self.runtime.expand(&free.pure);
        let names = prim_names(env);
        self.runtime.is_evaluable(&expanded) && self.runtime.eval_check(&expanded, &names).is_none()
    }

    pub fn compile_glsl(&self) -> Option<String> {
        let selected = self.selected?;
        let env = self.env();
        let free = def_cell(env, selected)?;
        let def = &env.defs[selected.0];
        let expanded = self.runtime.expand(&free.pure);
        let names = prim_names(env);
        let func = donut_runtime::glsl::compile_to_glsl(&expanded, &names).ok()?;
        Some(func.to_function(&def.lname))
    }

    pub fn compile_fragment_shader(&self) -> Option<std::result::Result<String, String>> {
        let selected = self.selected?;
        let env = self.env();
        let free = def_cell(env, selected)?;
        let expanded = self.runtime.expand(&free.pure);
        let names = prim_names(env);
        let func = donut_runtime::glsl::compile_to_glsl(&expanded, &names).ok()?;
        Some(func.to_fragment_shader().map_err(|e| e.to_string()))
    }

    pub fn format_css_color(c: &Color) -> String {
        format!("rgb({}, {}, {})", c.r(), c.g(), c.b())
    }

    pub fn selected_color(&self) -> Option<&Color> {
        let selected = self.selected?;
        let env = self.env();
        let def = &env.defs[selected.0];
        Some(env.def_color(def))
    }

    pub fn selected_params_text(&self) -> String {
        let Some(selected) = self.selected else {
            return String::new();
        };
        let env = self.env();
        let def = &env.defs[selected.0];
        env.display_params(def)
    }

    // --- Geometry access for canvas ---

    pub fn build_squash_view(&self, i: usize) -> Geometry {
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

    pub fn build_slice_view(&self) -> Geometry {
        let cell = self.cell.as_ref().unwrap();
        let n_extra = cell.size.len() - 2;
        let mut rc = cell.sliced(self.slice_pos[n_extra - 1]);
        for k in (0..n_extra - 1).rev() {
            rc = rc.sliced(self.slice_pos[k]);
        }
        rc
    }
}
