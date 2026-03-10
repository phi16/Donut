use donut_core::common::Prim;
use donut_core::pure_cell::PureCell;
use donut_lang::types::env::{Color, Env};

pub struct PrimTable {
    env: Env,
}

impl PrimTable {
    pub fn new(env: Env) -> Self {
        PrimTable { env }
    }

    pub fn env(&self) -> &Env {
        &self.env
    }

    pub fn prim_color(&self, prim: &Prim) -> Color {
        let prim_id = prim.id;
        if let Some(&item_id) = self.env.prim_item.get(&prim_id) {
            // Find the def that owns this item and check its decorators
            for def in &self.env.defs {
                if def.item == Some(item_id) {
                    if let Some(c) = self.env.def_color(def) {
                        return c.clone();
                    }
                }
            }
        }
        Color::gray()
    }

    pub fn format_prim(&self, prim: &Prim) -> String {
        let name = self.env.prim_name(prim.id);
        if prim.args.is_empty() {
            name.to_string()
        } else {
            let args_str: Vec<String> = prim.args.iter().map(|a| self.env.display_pure_val(a)).collect();
            format!("{}[{}]", name, args_str.join(", "))
        }
    }

    pub fn format_cell_type(&self, pure: &PureCell) -> String {
        self.env.display_cell(pure)
    }
}
