use donut_lang::types::env::Env;
use donut_lang::types::item::*;
use donut_lang::types::token;
use std::collections::HashMap;

use crate::{
    build_entry_info, EntryInfo, FlatEnv, HoverInfo, TokenType,
};

pub(crate) struct HoverBuilder<'a> {
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

    // --- Val walking ---

    fn walk_val(&mut self, val_id: ValId) {
        let val = self.program.val(val_id);
        let span = self.program.val_span(val_id);
        match val {
            Val::Path(path) => {
                // Find name tokens in the span (before any '[' or '(')
                let mut name_tokens = Vec::new();
                for i in span.start..span.end {
                    if let Some(t) = self.tokens.get(i) {
                        if t.str == "[" || t.str == "(" {
                            break;
                        }
                        if t.ty == token::TokenTy::Name
                            && !donut_lang::convert::is_number_str(t.str)
                        {
                            name_tokens.push(i);
                        }
                    }
                }

                // Map name tokens to segments 1:1
                let last = name_tokens.len().saturating_sub(1);
                for (i, &tok_idx) in name_tokens.iter().enumerate() {
                    if let Some(seg_ref) = path.segments.get(i) {
                        let is_last = i == last;
                        self.insert_ref_hover(tok_idx, seg_ref, !is_last);
                    }
                }

                // Walk args and applicand
                for &arg in &path.args {
                    self.walk_val(arg);
                }
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

    // --- Hover insertion ---

    fn insert_ref_hover(&mut self, token_index: usize, target: &Ref, is_namespace: bool) {
        match target {
            Ref::Item(item_id) => {
                let prog_item = self.program.item(*item_id);
                if matches!(prog_item.kind, ItemKind::Param) {
                    self.insert_param_hover(token_index, *item_id);
                    self.styles.insert(token_index, TokenType::Parameter);
                } else {
                    // Non-param item (e.g., star literal)
                    let item = &self.env.items[item_id.0];
                    let ty_str = self.env.display_ty(&item.ty);
                    let entry = EntryInfo {
                        kind: crate::item_to_kind(item),
                        module_kind: None,
                        type_expr: Some(ty_str.clone()),
                        params: String::new(),
                    };
                    let tags = entry.tags();
                    self.map.insert(
                        token_index,
                        HoverInfo {
                            name: item.lname.clone(),
                            signature: format!("{}: {}", item.lname, ty_str),
                            entry,
                            tags,
                        },
                    );
                }
            }
            Ref::Def(def_id) => {
                let env_def = &self.env.defs[def_id.0];
                let signature = self.env.display_def_signature(env_def);
                let entry = build_entry_info(*def_id, self.env, self.flat);
                let tags = entry.tags();
                self.map.insert(
                    token_index,
                    HoverInfo {
                        name: env_def.qname.clone(),
                        signature,
                        entry,
                        tags,
                    },
                );
                if is_namespace {
                    self.styles.insert(token_index, TokenType::Namespace);
                }
            }
        }
    }

    fn insert_param_hover(&mut self, token_index: usize, item_id: ItemId) {
        let item = &self.env.items[item_id.0];
        let parent_def = self.env.defs.iter().find(|d| d.params.contains(&item_id));
        let type_expr = if let Some(def) = parent_def {
            self.env.display_ty_in_def(&item.ty, def)
        } else {
            self.env.display_ty(&item.ty)
        };
        let entry = EntryInfo {
            kind: crate::item_to_kind(item),
            module_kind: None,
            type_expr: Some(type_expr.clone()),
            params: String::new(),
        };
        let mut tags = entry.tags();
        tags.push(if let Some(def) = parent_def {
            format!("parameter of {}", def.lname)
        } else {
            "parameter".to_string()
        });
        self.map.insert(
            token_index,
            HoverInfo {
                name: item.lname.clone(),
                signature: format!("{}: {}", item.lname, type_expr),
                entry,
                tags,
            },
        );
    }

    // --- Def processing ---

    fn walk_def(&mut self, def_id: DefId) {
        let def = self.program.def(def_id);

        // Skip imported defs — their spans refer to the source file's token indices.
        if def.origin.is_some() {
            return;
        }

        // Skip auto-generated DeclDef members (x.def) — they share the parent's span.
        if def.lname == donut_lang::types::item::DEF_MEMBER_NAME {
            return;
        }

        // Definition site hover
        let env_def = &self.env.defs[def_id.0];
        let signature = self.env.display_def_signature(env_def);
        let entry = build_entry_info(def_id, self.env, self.flat);
        let tags = entry.tags();
        self.map.insert(
            def.span.start,
            HoverInfo {
                name: env_def.qname.clone(),
                signature,
                entry,
                tags,
            },
        );

        // Walk type expression
        if let Some(ty_id) = def.ty {
            self.walk_val(ty_id);
        }

        // Walk body value
        match &def.body {
            DefBody::Alias { val } => self.walk_val(*val),
            DefBody::Decl { def_val: Some(val), .. } => self.walk_val(*val),
            _ => {}
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

        // No members recursion — build() iterates all defs directly.
    }

    // --- Entry point ---

    pub fn build(mut self) -> (HashMap<usize, HoverInfo>, HashMap<usize, TokenType>) {
        for i in 0..self.program.defs.len() {
            self.walk_def(DefId(i));
        }
        (self.map, self.styles)
    }
}
