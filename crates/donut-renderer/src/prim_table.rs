use std::collections::HashMap;

use donut_core::common::{Prim, PrimId};
use donut_core::pure_cell::PureCell;
use donut_lang::types::env::{Color, DisplayContext, Env};

pub struct PrimTable {
    env: Env,
    prim_color: HashMap<PrimId, Color>,
}

impl PrimTable {
    pub fn new(env: Env) -> Self {
        let mut prim_color = HashMap::new();
        for def in &env.defs {
            if let Some(item_id) = def.item {
                if let Some(prim_id) = env.items[item_id.0].prim_id {
                    prim_color.insert(prim_id, def.style.color);
                }
            }
        }
        PrimTable { env, prim_color }
    }

    pub fn env(&self) -> &Env {
        &self.env
    }

    pub fn prim_color(&self, prim: &Prim) -> Color {
        self.prim_color
            .get(&prim.id)
            .copied()
            .unwrap_or_else(Color::gray)
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
