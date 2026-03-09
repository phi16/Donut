use donut_lang::check::Env;
use donut_lang::types::item::*;
use std::collections::HashMap;

use super::{HoverInfo, TokenType};

pub(super) struct HoverBuilder<'a> {
    program: &'a Program,
    env: &'a Env,
    map: HashMap<usize, HoverInfo>,
    styles: HashMap<usize, TokenType>,
}

impl<'a> HoverBuilder<'a> {
    pub fn new(program: &'a Program, env: &'a Env) -> Self {
        Self {
            program,
            env,
            map: HashMap::new(),
            styles: HashMap::new(),
        }
    }

    fn make_hover(&self, qname: &str) -> Option<HoverInfo> {
        let &idx = self.env.lookup.get(qname)?;
        let entry = &self.env.entries[idx];
        let detail = entry.display_kind();
        let type_expr = entry.display_type(self.env);
        let params = self.env.display_params(idx);
        Some(HoverInfo {
            name: qname.to_string(),
            detail,
            type_expr,
            params,
        })
    }

    fn try_resolve(&self, name: &str, prefixes: &[&str]) -> Option<String> {
        for i in (0..=prefixes.len()).rev() {
            let qname = if i == 0 {
                name.to_string()
            } else {
                format!("{}.{}", prefixes[..i].join("."), name)
            };
            if self.env.lookup.contains_key(&qname) {
                return Some(qname);
            }
        }
        None
    }

    fn walk_val(&mut self, val_id: ValId, prefixes: &[&str]) {
        let val_s = self.program.val(val_id);
        match &val_s.0 {
            Val::Path(path) => {
                let parts: Vec<_> =
                    path.segments.iter().map(|s| s.0.name.clone()).collect();
                if parts.is_empty() {
                    return;
                }
                let path_name = parts.join(".");
                if path_name == "*" {
                    let info = HoverInfo {
                        name: "*".to_string(),
                        detail: "meta".to_string(),
                        type_expr: Some("meta".to_string()),
                        params: String::new(),
                    };
                    self.map.insert(path.segments[0].1.start, info);
                } else if let Some(resolved) = self.try_resolve(&path_name, prefixes) {
                    if let Some(info) = self.make_hover(&resolved) {
                        // 最後のセグメントにはエントリの hover info
                        let last = path.segments.len() - 1;
                        self.map.insert(path.segments[last].1.start, info);
                        // 中間セグメントにはモジュールプレフィックスの hover info
                        for i in 0..last {
                            let prefix = parts[..=i].join(".");
                            let module_info = HoverInfo {
                                name: prefix.clone(),
                                detail: "module".to_string(),
                                type_expr: None,
                                params: self.env.display_module_params(&prefix),
                            };
                            self.map.insert(path.segments[i].1.start, module_info);
                        }
                    }
                    // . の左側のセグメントを Namespace として色付け
                    if parts.len() > 1 {
                        for i in 0..parts.len() - 1 {
                            self.styles
                                .insert(path.segments[i].1.start, TokenType::Namespace);
                        }
                    }
                } else {
                    // エントリとしては解決できなかったが、モジュール名かもしれない
                    for i in (0..parts.len()).rev() {
                        let prefix = parts[..=i].join(".");
                        if self.env.module_members.contains_key(&prefix) {
                            for j in 0..=i {
                                let seg_prefix = parts[..=j].join(".");
                                let module_info = HoverInfo {
                                    name: seg_prefix.clone(),
                                    detail: "module".to_string(),
                                    type_expr: None,
                                    params: self.env.display_module_params(&seg_prefix),
                                };
                                self.map.insert(path.segments[j].1.start, module_info);
                            }
                            // . の左側のセグメントを Namespace として色付け
                            for j in 0..i {
                                self.styles
                                    .insert(path.segments[j].1.start, TokenType::Namespace);
                            }
                            break;
                        }
                    }
                }
                // Walk param vals
                let param_vals: Vec<_> = path
                    .segments
                    .iter()
                    .flat_map(|s| s.0.params.iter().map(|pv| pv.val))
                    .collect();
                for pv in param_vals {
                    self.walk_val(pv, prefixes);
                }
                // Walk applicand
                if let Some(app_id) = path.applicand {
                    self.walk_val(app_id, prefixes);
                }
            }
            Val::Arrow(_, l, r) => {
                self.walk_val(*l, prefixes);
                self.walk_val(*r, prefixes);
            }
            Val::Comp(_, children) | Val::CompStar(children) => {
                for &c in children {
                    self.walk_val(c, prefixes);
                }
            }
            _ => {}
        }
    }

    fn walk_module(&mut self, module: &Module, prefixes: &[&str]) {
        // Collect entry data to avoid borrow issues
        let entries: Vec<_> = module.entries.clone();
        let internal: Vec<_> = module.internal.clone();

        for (name, item_id) in &entries {
            let item = self.program.item(*item_id);
            let qname = if prefixes.is_empty() {
                name.clone()
            } else {
                format!("{}.{}", prefixes.join("."), name)
            };

            let has_members = item
                .members()
                .map_or(false, |m| !m.entries.is_empty());
            let is_module_def = has_members && item.ty.is_none();

            // Definition site hover
            if is_module_def {
                let params = self.env.display_module_params(&qname);
                self.map.insert(
                    item.span.start,
                    HoverInfo {
                        name: qname.clone(),
                        detail: "module".to_string(),
                        type_expr: None,
                        params,
                    },
                );
            } else if let Some(info) = self.make_hover(&qname) {
                self.map.insert(item.span.start, info);
            }

            // Walk type expression
            if let Some(ty_id) = item.ty {
                self.walk_val(ty_id, prefixes);
            }

            // Walk body value
            if let Some(val_id) = item.val() {
                self.walk_val(val_id, prefixes);
            }

            // Walk decorators
            for &deco_id in &item.decos.clone() {
                self.walk_val(deco_id, prefixes);
            }

            // Walk params' type expressions
            for ty in item.params.iter().map(|p| p.ty).collect::<Vec<_>>() {
                self.walk_val(ty, prefixes);
            }

            // Walk functor mappings
            if let ItemBody::Functor { mappings } = &item.body {
                let mapping_data: Vec<_> = mappings
                    .iter()
                    .map(|m| {
                        let ptys: Vec<_> = m.params.iter().map(|p| p.ty).collect();
                        (m.applicand, m.val, ptys)
                    })
                    .collect();
                for (app, val, ptys) in mapping_data {
                    self.walk_val(app, prefixes);
                    self.walk_val(val, prefixes);
                    for ty in ptys {
                        self.walk_val(ty, prefixes);
                    }
                }
            }

            // Walk members (nested module)
            if has_members {
                let members = self.program.item(*item_id).members().unwrap().clone();
                let mut new_prefixes: Vec<&str> = prefixes.to_vec();
                new_prefixes.push(name);
                self.walk_module(&members, &new_prefixes);
            }
        }

        // Internal items (from use/import)
        for (_name, item_id) in &internal {
            let item = self.program.item(*item_id);
            if let Some(ty_id) = item.ty {
                self.walk_val(ty_id, prefixes);
            }
            if let Some(val_id) = item.val() {
                self.walk_val(val_id, prefixes);
            }
        }
    }

    pub fn build(mut self) -> (HashMap<usize, HoverInfo>, HashMap<usize, TokenType>) {
        let root = self.program.root.clone();
        self.walk_module(&root, &[]);
        (self.map, self.styles)
    }
}
