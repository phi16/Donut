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
use serde::Serialize;

pub struct Engine {
    pub(crate) table: PrimTable,
    pub(crate) runtime: Runtime,
    pub(crate) root_entries: Vec<DefId>,
    pub(crate) selected: Option<DefId>,
    pub(crate) cell: Option<Geometry>,
    pub(crate) slice_pos: Vec<R>,
    pub(crate) diagnostics: Vec<String>,
    prim_names: HashMap<PrimId, String>,
}

#[derive(Serialize)]
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

fn build_prim_maps(env: &Env) -> (HashMap<String, PrimId>, HashMap<PrimId, String>) {
    let mut lookup = HashMap::new();
    let mut names = HashMap::new();
    for (&prim_id, &item_id) in &env.prim_item {
        let cname = env.items[item_id.0].cname.clone();
        lookup.insert(cname.clone(), prim_id);
        names.insert(prim_id, cname);
    }
    (lookup, names)
}

impl Engine {
    pub fn new(code: &str) -> Self {
        let (table, runtime, prim_names, diagnostics) = Self::load(code);
        let mut engine = Self {
            table,
            runtime,
            root_entries: Vec::new(),
            selected: None,
            cell: None,
            slice_pos: Vec::new(),
            diagnostics,
            prim_names,
        };
        engine.refresh_selection();
        engine
    }

    pub fn env(&self) -> &Env {
        self.table.env()
    }

    pub fn table(&self) -> &PrimTable {
        &self.table
    }

    fn find_last_cell(env: &Env, root_entries: &[DefId]) -> Option<DefId> {
        root_entries.iter().rev().find_map(|&def_id| {
            if matches!(&env.defs[def_id.0].val, PureVal::Cell(_)) {
                Some(def_id)
            } else {
                None
            }
        })
    }

    fn load(code: &str) -> (PrimTable, Runtime, HashMap<PrimId, String>, Vec<String>) {
        let user_code = donut_core::common::dedent(code);
        let (env, errors) = donut_lang::load::load(&user_code);
        let diagnostics: Vec<String> = errors
            .into_iter()
            .map(|(pos, msg)| format!("{}:{}: {}", pos.line + 1, pos.col + 1, msg))
            .collect();

        let (lookup, names) = build_prim_maps(&env);
        let def_subs = env.def_subs.clone();
        let table = PrimTable::new(env);

        let mut runtime = Runtime::new();
        donut_runtime::env::register_sys(&mut runtime, &lookup);
        runtime.set_def_subs(def_subs);

        (table, runtime, names, diagnostics)
    }

    /// Rebuild root_entries, selected, cell, and slice_pos from current env.
    fn refresh_selection(&mut self) {
        self.root_entries = root_entries(self.env());
        self.selected = Self::find_last_cell(self.env(), &self.root_entries);
        self.cell = self
            .selected
            .and_then(|id| def_cell(self.env(), id).map(|c| Self::build_geometry(&c)));
        self.slice_pos = self
            .cell
            .as_ref()
            .map(|c| Self::init_slice_pos(&c.size))
            .unwrap_or_default();
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
        let (table, runtime, prim_names, diagnostics) = Self::load(code);
        if !diagnostics.is_empty() {
            // Errors present: only update diagnostics, keep previous valid state
            self.diagnostics = diagnostics;
            return;
        }
        self.table = table;
        self.runtime = runtime;
        self.diagnostics = diagnostics;
        self.prim_names = prim_names;
        self.refresh_selection();
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

    /// Get the selected definition's cell expanded for runtime use.
    fn selected_expanded(&self) -> Option<(DefId, FreeCell)> {
        let selected = self.selected?;
        let free = def_cell(self.env(), selected)?;
        let expanded = self.runtime.expand(&free.pure);
        Some((selected, expanded))
    }

    pub fn eval_result_text(&self) -> String {
        let Some(selected) = self.selected else {
            return String::new();
        };
        let env = self.env();
        let def = &env.defs[selected.0];
        let Some((_, expanded)) = self.selected_expanded() else {
            return format!("{}: meta", def.lname);
        };
        let sig = env.display_def_signature(def);
        if env.is_parametric(def) {
            return format!("{}\nfailed to eval: parametric", sig);
        }
        let eval_str = match self.runtime.eval_check(&expanded, &self.prim_names) {
            Some(reason) => format!("failed to eval: {}", reason),
            None => match self.runtime.eval(&expanded, &[], &self.prim_names) {
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
        if self.env().is_parametric(&self.env().defs[selected.0]) {
            return false;
        }
        let Some((_, expanded)) = self.selected_expanded() else {
            return false;
        };
        self.runtime.is_evaluable(&expanded)
            && self
                .runtime
                .eval_check(&expanded, &self.prim_names)
                .is_none()
    }

    pub fn compile_glsl(&self) -> Option<String> {
        let (selected, expanded) = self.selected_expanded()?;
        let def = &self.env().defs[selected.0];
        let func = donut_runtime::glsl::compile_to_glsl(&expanded, &self.prim_names).ok()?;
        Some(func.to_function(&def.lname))
    }

    /// Returns (cell_function, color_wrapper) for the selected entry.
    /// cell_function: `void cell(in ... , out ...)` definition
    /// color_wrapper: `vec3 cell_color(vec2 uv)` wrapper that calls cell and returns RGB
    pub fn compile_fragment_parts(&self) -> Option<std::result::Result<(String, String), String>> {
        let (_, expanded) = self.selected_expanded()?;
        let func = donut_runtime::glsl::compile_to_glsl(&expanded, &self.prim_names).ok()?;
        Some(
            func.to_color_wrapper("cell_color")
                .map(|wrapper| (func.to_function("cell"), wrapper))
                .map_err(|e| e.to_string()),
        )
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
        env.display_all_params(def)
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
