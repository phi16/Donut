use donut_lang::types::env::Env;
use donut_lang::types::item::*;
use donut_lang::types::token;
use std::collections::HashMap;

use super::{build_entry_info, build_module_info, EntryInfo, FlatEnv, HoverInfo, TokenType};

/// Check if a path target refers to a parameter item.
fn is_param_ref(program: &Program, target: &Ref) -> bool {
    matches!(target, Ref::Item(item_id) if matches!(program.item(*item_id).kind, ItemKind::Param))
}

pub(super) struct HoverBuilder<'a> {
    program: &'a Program,
    env: &'a Env,
    flat: &'a FlatEnv,
    tokens: &'a [token::Token<'a>],
    map: HashMap<usize, HoverInfo>,
    styles: HashMap<usize, TokenType>,
}

impl<'a> HoverBuilder<'a> {
    pub fn new(
        program: &'a Program,
        env: &'a Env,
        flat: &'a FlatEnv,
        tokens: &'a [token::Token<'a>],
    ) -> Self {
        Self {
            program,
            env,
            flat,
            tokens,
            map: HashMap::new(),
            styles: HashMap::new(),
        }
    }

    /// Get the qname for a Ref target.
    fn ref_qname(&self, target: &Ref) -> &str {
        match target {
            Ref::Def(def_id) => &self.program.def(*def_id).qname,
            Ref::Item(item_id) => &self.program.item(*item_id).cname,
        }
    }

    fn walk_val(&mut self, val_id: ValId) {
        let val = self.program.val(val_id);
        let span = self.program.val_span(val_id);
        match val {
            Val::Path(path) => {
                let qname = self.ref_qname(&path.target).to_string();

                if qname == "*" {
                    // Star literal
                    let entry = EntryInfo {
                        kind: super::EntryKind::Meta,
                        module_kind: None,
                        type_expr: Some("meta".to_string()),
                        params: String::new(),
                    };
                    let tags = entry.tags();
                    self.map.insert(
                        span.start,
                        HoverInfo {
                            name: "*".to_string(),
                            entry,
                            tags,
                        },
                    );
                } else {
                    // Find name tokens in the span (before any '[')
                    let mut name_tokens = Vec::new();
                    for i in span.start..span.end {
                        if let Some(t) = self.tokens.get(i) {
                            if t.str == "[" {
                                break;
                            }
                            if t.ty == token::TokenTy::Name
                                && !donut_lang::convert::is_number_str(t.str)
                            {
                                name_tokens.push(i);
                            }
                        }
                    }

                    if !name_tokens.is_empty() {
                        let last = name_tokens.len() - 1;

                        // Last token → hover for the resolved target
                        let is_param = is_param_ref(self.program, &path.target);
                        if is_param {
                            // Parameter: build hover from env Item + parent def context
                            if let Ref::Item(item_id) = &path.target {
                                let item = &self.env.items[item_id.0];
                                let parent_def = self.env.defs.iter().find(|d| {
                                    d.params.contains(item_id)
                                });
                                let type_expr = if let Some(def) = parent_def {
                                    self.env.display_ty_in_def(&item.ty, def)
                                } else {
                                    self.env.display_ty(&item.ty)
                                };
                                let entry = EntryInfo {
                                    kind: super::item_to_kind(item),
                                    module_kind: None,
                                    type_expr: Some(type_expr),
                                    params: String::new(),
                                };
                                let mut tags = entry.tags();
                                tags.push(if let Some(def) = parent_def {
                                    format!("parameter of {}", def.lname)
                                } else {
                                    "parameter".to_string()
                                });
                                self.map.insert(
                                    name_tokens[last],
                                    HoverInfo {
                                        name: item.lname.clone(),
                                        entry,
                                        tags,
                                    },
                                );
                            }
                            self.styles
                                .insert(name_tokens[last], TokenType::Parameter);
                        } else if let Some(entry) =
                            build_entry_info(&qname, self.env, self.flat)
                        {
                            let tags = entry.tags();
                            self.map.insert(
                                name_tokens[last],
                                HoverInfo {
                                    name: qname.clone(),
                                    entry,
                                    tags,
                                },
                            );
                        } else if !self.flat.defs.contains_key(&qname)
                            && self.flat.modules.contains_key(&qname)
                        {
                            // Unresolved as def but known as module
                            self.insert_module_hover(name_tokens[last], &qname);
                        }

                        // Intermediate tokens → namespace marking
                        // Reconstruct prefix from source token strings
                        let mut prefix_parts = Vec::new();
                        for i in 0..last {
                            prefix_parts.push(self.tokens[name_tokens[i]].str);
                            let prefix = prefix_parts.join(".");
                            if let Some(entry) =
                                build_entry_info(&prefix, self.env, self.flat)
                            {
                                let tags = entry.tags();
                                self.map.insert(
                                    name_tokens[i],
                                    HoverInfo {
                                        name: prefix,
                                        entry,
                                        tags,
                                    },
                                );
                            } else {
                                self.insert_module_hover(name_tokens[i], &prefix);
                            }
                            self.styles
                                .insert(name_tokens[i], TokenType::Namespace);
                        }
                    }
                }

                // Walk args
                for &arg in &path.args {
                    self.walk_val(arg);
                }
                // Walk applicand
                if let Some(app) = path.applicand {
                    self.walk_val(app);
                }
            }
            Val::Arrow(_, l, r) => {
                self.walk_val(*l);
                self.walk_val(*r);
            }
            Val::Comp(_, children) | Val::CompStar(children) => {
                for &c in children {
                    self.walk_val(c);
                }
            }
            Val::Subst(inner, mapping) => {
                self.walk_val(*inner);
                for (_, &v) in mapping {
                    self.walk_val(v);
                }
            }
            Val::Lit(_) | Val::Hole(_) => {}
        }
    }

    fn insert_module_hover(&mut self, token_index: usize, qname: &str) {
        let entry = build_module_info();
        let tags = entry.tags();
        self.map.insert(
            token_index,
            HoverInfo {
                name: qname.to_string(),
                entry,
                tags,
            },
        );
    }

    fn walk_def(&mut self, def_id: DefId) {
        let def = self.program.def(def_id);

        // Skip imported defs — their spans refer to the source file's token indices,
        // not the current file's tokens.
        if def.origin.is_some() {
            return;
        }

        let qname = def.qname.clone();
        let has_members = !def.members.entries.is_empty();
        let is_module_def = has_members && def.ty.is_none();

        // Definition site hover
        if is_module_def {
            self.insert_module_hover(def.span.start, &qname);
        } else if let Some(entry) = build_entry_info(&qname, self.env, self.flat) {
            let tags = entry.tags();
            self.map.insert(
                def.span.start,
                HoverInfo {
                    name: qname.clone(),
                    entry,
                    tags,
                },
            );
        }

        // Walk type expression
        if let Some(ty_id) = def.ty {
            self.walk_val(ty_id);
        }

        // Walk body value
        if let Some(val_id) = def.val() {
            self.walk_val(val_id);
        }

        // Walk decorators
        for &deco_id in &def.decos {
            self.walk_val(deco_id);
        }

        // Walk params' type expressions
        for param in &def.params {
            self.walk_val(param.ty);
        }

        // Walk functor mappings
        if let DefBody::Functor { mappings } = &def.body {
            for m in mappings {
                self.walk_val(m.applicand);
                self.walk_val(m.val);
                for p in &m.params {
                    self.walk_val(p.ty);
                }
            }
        }

        // Walk members (nested module)
        for &child_def_id in &def.members.entries {
            self.walk_def(child_def_id);
        }
    }

    pub fn build(mut self) -> (HashMap<usize, HoverInfo>, HashMap<usize, TokenType>) {
        // Walk all root defs
        for &def_id in &self.program.root.entries {
            self.walk_def(def_id);
        }
        (self.map, self.styles)
    }
}
