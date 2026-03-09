use donut_lang::check::{EntryKind, Env};
use donut_lang::types::item::*;
use std::collections::HashMap;

use super::{build_entry_info, build_module_info, EntryInfo, HoverInfo, TokenType};

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

    /// Try to resolve a path name in the given prefix context.
    /// Tries from innermost scope to outermost (top-level).
    fn resolve_name(&self, name: &str, prefixes: &[&str]) -> Option<String> {
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
                    path.segments.iter().map(|s| s.0.name.as_str()).collect();
                if parts.is_empty() {
                    return;
                }
                let path_name = parts.join(".");
                if path_name == "*" {
                    self.map.insert(path.segments[0].1.start, HoverInfo {
                        name: "*".to_string(),
                        entry: EntryInfo {
                            kind: EntryKind::Meta,
                            is_module: false,
                            type_expr: Some("meta".to_string()),
                            params: String::new(),
                        },
                    });
                } else if let Some(resolved) = self.resolve_name(&path_name, prefixes) {
                    if let Some(entry) = build_entry_info(&resolved, self.env) {
                        // 最後のセグメントにはエントリの hover info
                        let last = path.segments.len() - 1;
                        self.map.insert(path.segments[last].1.start, HoverInfo {
                            name: resolved.clone(),
                            entry,
                        });
                        // 中間セグメントにはモジュールプレフィックスの hover info
                        for i in 0..last {
                            let prefix = parts[..=i].join(".");
                            self.insert_module_hover(path.segments[i].1.start, &prefix);
                        }
                    }
                    // . の左側のセグメントを Namespace として色付け
                    for i in 0..parts.len().saturating_sub(1) {
                        self.styles.insert(path.segments[i].1.start, TokenType::Namespace);
                    }
                } else {
                    // エントリとしては解決できなかったが、モジュール名かもしれない
                    for i in (0..parts.len()).rev() {
                        let prefix = parts[..=i].join(".");
                        if self.env.module_members.contains_key(&prefix) {
                            for j in 0..=i {
                                let seg_prefix = parts[..=j].join(".");
                                self.insert_module_hover(path.segments[j].1.start, &seg_prefix);
                            }
                            for j in 0..i {
                                self.styles.insert(path.segments[j].1.start, TokenType::Namespace);
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

    fn insert_module_hover(&mut self, token_index: usize, qname: &str) {
        self.map.insert(token_index, HoverInfo {
            name: qname.to_string(),
            entry: build_module_info(qname, self.env),
        });
    }

    fn walk_item(&mut self, qname: &str, item: &Item, prefixes: &[&str]) {
        let has_members = item.members().map_or(false, |m| !m.entries.is_empty());
        let is_module_def = has_members && item.ty.is_none();

        // Definition site hover
        if is_module_def {
            self.insert_module_hover(item.span.start, qname);
        } else if let Some(entry) = build_entry_info(qname, self.env) {
            self.map.insert(item.span.start, HoverInfo {
                name: qname.to_string(),
                entry,
            });
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
        for deco_id in &item.decos {
            self.walk_val(*deco_id, prefixes);
        }

        // Walk params' type expressions
        for param in &item.params {
            self.walk_val(param.ty, prefixes);
        }

        // Walk functor mappings
        if let ItemBody::Functor { mappings } = &item.body {
            for m in mappings {
                self.walk_val(m.applicand, prefixes);
                self.walk_val(m.val, prefixes);
                for p in &m.params {
                    self.walk_val(p.ty, prefixes);
                }
            }
        }
    }

    fn walk_module(&mut self, module: &Module, prefixes: &[&str]) {
        for (name, item_id) in &module.entries {
            let qname = if prefixes.is_empty() {
                name.clone()
            } else {
                format!("{}.{}", prefixes.join("."), name)
            };

            let item = self.program.item(*item_id);
            let has_members = item.members().map_or(false, |m| !m.entries.is_empty());

            self.walk_item(&qname, item, prefixes);

            // Walk members (nested module)
            if has_members {
                let members = item.members().unwrap();
                let mut new_prefixes: Vec<&str> = prefixes.to_vec();
                new_prefixes.push(name);
                // Clone to avoid borrow conflict (members borrows program, walk_module borrows self mutably)
                let members = members.clone();
                self.walk_module(&members, &new_prefixes);
            }
        }

        // Internal items (from use/import) — walk val expressions only
        for (_, item_id) in &module.internal {
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
