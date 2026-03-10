use donut_lang::types::env::Env;
use donut_lang::types::item::*;
use donut_lang::types::token;
use std::collections::HashMap;

use super::{build_entry_info, build_module_info, EntryInfo, FlatEnv, HoverInfo, TokenType};

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
                    self.map.insert(
                        span.start,
                        HoverInfo {
                            name: "*".to_string(),
                            entry: EntryInfo {
                                kind: super::EntryKind::Meta,
                                is_module: false,
                                type_expr: Some("meta".to_string()),
                                params: String::new(),
                            },
                        },
                    );
                } else {
                    let segments: Vec<&str> = qname.split('.').collect();

                    // Find name tokens in the span for per-segment hover
                    // We need to be careful: only consider name tokens BEFORE any '[' (args)
                    let mut name_tokens = Vec::new();
                    for i in span.start..=span.end {
                        if let Some(t) = self.tokens.get(i) {
                            if t.str == "[" {
                                break; // stop at first bracket (args follow)
                            }
                            if t.ty == token::TokenTy::Name
                                && !donut_lang::convert::is_number_str(t.str)
                            {
                                name_tokens.push(i);
                            }
                        }
                    }

                    if name_tokens.len() == segments.len() {
                        // Last token → entry hover
                        let last = segments.len() - 1;
                        if let Some(entry) =
                            build_entry_info(&qname, self.env, self.flat)
                        {
                            self.map.insert(
                                name_tokens[last],
                                HoverInfo {
                                    name: qname.clone(),
                                    entry,
                                },
                            );
                        }
                        // Intermediate tokens → module prefix hover + namespace marking
                        for i in 0..last {
                            let prefix = segments[..=i].join(".");
                            self.insert_module_hover(name_tokens[i], &prefix);
                            self.styles
                                .insert(name_tokens[i], TokenType::Namespace);
                        }
                    } else if !name_tokens.is_empty() {
                        // Fallback: put hover on last name token
                        if let Some(entry) =
                            build_entry_info(&qname, self.env, self.flat)
                        {
                            self.map.insert(
                                *name_tokens.last().unwrap(),
                                HoverInfo {
                                    name: qname.clone(),
                                    entry,
                                },
                            );
                        }
                    }

                    // Check if unresolved path might be a module name
                    if !self.flat.defs.contains_key(&qname)
                        && self.flat.modules.contains_key(&qname)
                    {
                        if let Some(&tok) = name_tokens.last() {
                            self.insert_module_hover(tok, &qname);
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
        self.map.insert(
            token_index,
            HoverInfo {
                name: qname.to_string(),
                entry: build_module_info(),
            },
        );
    }

    fn walk_def(&mut self, def_id: DefId) {
        let def = self.program.def(def_id);
        let qname = def.qname.clone();
        let has_members = !def.members.entries.is_empty();
        let is_module_def = has_members && def.ty.is_none();

        // Definition site hover
        if is_module_def {
            self.insert_module_hover(def.span.start, &qname);
        } else if let Some(entry) = build_entry_info(&qname, self.env, self.flat) {
            self.map.insert(
                def.span.start,
                HoverInfo {
                    name: qname.clone(),
                    entry,
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
