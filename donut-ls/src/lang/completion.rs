use donut_lang::types::env::Env;
use donut_lang::types::item::Program;
use donut_lang::types::token;
use std::collections::HashMap;

use super::{build_entry_info, build_module_info, CompletionCandidate, CompletionData, FlatEnv};

pub(super) fn build_completion_data(
    program: &Program,
    env: &Env,
    flat: &FlatEnv,
    tokens: &[token::Token],
    dot_prefixes: HashMap<usize, String>,
) -> CompletionData {
    let mut scopes: HashMap<String, Vec<CompletionCandidate>> = HashMap::new();

    // Top-level entries from root module
    let mut top_level = Vec::new();
    for &def_id in &program.root.entries {
        let def = program.def(def_id);
        let name = &def.lname;
        let def_line = tokens
            .get(def.span.start)
            .map(|t| t.pos.line as u32)
            .unwrap_or(0);
        let is_imported = def.origin.is_some();

        let entry = if let Some(info) = build_entry_info(&def.qname, env, flat) {
            Some(info)
        } else {
            let has_members = !def.members.entries.is_empty();
            let is_module = flat.modules.contains_key(&def.qname);
            if is_module || has_members {
                Some(build_module_info())
            } else {
                None
            }
        };
        if let Some(entry) = entry {
            top_level.push(CompletionCandidate {
                label: name.clone(),
                entry,
                def_line,
                is_imported,
            });
        }
    }
    scopes.insert(String::new(), top_level);

    // Module member scopes — walk env's Module tree
    collect_module_members(env, flat, "", &env.root, &mut scopes);

    CompletionData {
        scopes,
        dot_prefixes,
    }
}

fn collect_module_members(
    env: &Env,
    flat: &FlatEnv,
    prefix: &str,
    module: &donut_lang::types::env::Module,
    scopes: &mut HashMap<String, Vec<CompletionCandidate>>,
) {
    // For each child that is a module (has sub-entries), create a scope
    for (name, child) in &module.lookup {
        let qname = if prefix.is_empty() {
            name.clone()
        } else {
            format!("{}.{}", prefix, name)
        };

        // If this child has children, it's a module scope
        if !child.lookup.is_empty() {
            let mut candidates = Vec::new();
            for (member_name, member_child) in &child.lookup {
                let full_name = format!("{}.{}", qname, member_name);
                let entry = if let Some(info) = build_entry_info(&full_name, env, flat) {
                    let is_imported = env.defs[flat.defs[&full_name].0].origin.is_some();
                    Some((info, is_imported))
                } else if flat.modules.contains_key(&full_name) {
                    Some((build_module_info(), true))
                } else {
                    // Child has a this but no entry_info — try as module
                    if member_child.this.is_some() {
                        let def = &env.defs[member_child.this.unwrap().0];
                        let is_imported = def.origin.is_some();
                        let info = super::EntryInfo {
                            kind: super::def_to_kind(def),
                            is_module: !member_child.lookup.is_empty(),
                            type_expr: if member_child.lookup.is_empty() {
                                Some(env.display_ty(&def.ty))
                            } else {
                                None
                            },
                            params: env.display_params(def),
                        };
                        Some((info, is_imported))
                    } else {
                        None
                    }
                };
                if let Some((entry, is_imported)) = entry {
                    candidates.push(CompletionCandidate {
                        label: member_name.clone(),
                        entry,
                        def_line: 0,
                        is_imported,
                    });
                }
            }
            scopes.insert(qname.clone(), candidates);

            // Recurse into nested modules
            collect_module_members(env, flat, &qname, child, scopes);
        }
    }
}

#[cfg(test)]
mod tests {
    use donut_lang::types::token;

    use super::super::marking::{Context, Marking};
    use super::super::TokenType;

    fn collect_dot_prefixes(
        code: &str,
    ) -> (
        std::collections::HashMap<usize, String>,
        Vec<token::Token<'_>>,
    ) {
        let (tokens, _, _) = donut_lang::tokenize::tokenize(code);
        let (program, _) = donut_lang::parse::parse(&tokens);

        let token_data: Vec<_> = tokens
            .iter()
            .enumerate()
            .map(|(i, t)| super::super::TokenData {
                line: t.pos.line as u32,
                column: t.pos.col as u32,
                length: t.pos.len as u32,
                token_index: Some(i),
                token_type: TokenType::Unknown,
            })
            .collect();

        let mut ctx = Context::new(token_data);
        program.mark(&mut ctx);
        let (_, dot_prefixes) = ctx.into_parts();
        (dot_prefixes, tokens)
    }

    fn last_dot_index(tokens: &[token::Token]) -> usize {
        tokens
            .iter()
            .rposition(|t| t.str == ".")
            .expect("no dot found")
    }

    fn nth_dot_index(tokens: &[token::Token], n: usize) -> usize {
        tokens
            .iter()
            .enumerate()
            .filter(|(_, t)| t.str == ".")
            .nth(n)
            .map(|(i, _)| i)
            .expect("dot not found")
    }

    #[test]
    fn dot_prefix_simple() {
        let (prefixes, tokens) = collect_dot_prefixes("x = {\n  a = 1\n}\ny = x.");
        let dot = last_dot_index(&tokens);
        assert_eq!(prefixes.get(&dot).map(|s| s.as_str()), Some("x"));
    }

    #[test]
    fn dot_prefix_nested() {
        let (prefixes, tokens) =
            collect_dot_prefixes("x = {\n  y = {\n    z = 1\n  }\n}\nw = x.y.");
        let dot = last_dot_index(&tokens);
        assert_eq!(prefixes.get(&dot).map(|s| s.as_str()), Some("x.y"));
    }

    #[test]
    fn dot_prefix_with_params() {
        let (prefixes, tokens) = collect_dot_prefixes("[a] x = {\n  b = 1\n}\ny = x[1].");
        let dot = last_dot_index(&tokens);
        assert_eq!(prefixes.get(&dot).map(|s| s.as_str()), Some("x"));
    }

    #[test]
    fn dot_prefix_middle() {
        let (prefixes, tokens) =
            collect_dot_prefixes("x = {\n  y = {\n    z = 1\n  }\n}\nw = x.y.z");
        let dot0 = nth_dot_index(&tokens, 0);
        assert_eq!(prefixes.get(&dot0).map(|s| s.as_str()), Some("x"));

        let dot1 = nth_dot_index(&tokens, 1);
        assert_eq!(prefixes.get(&dot1).map(|s| s.as_str()), Some("x.y"));
    }

    #[test]
    fn dot_prefix_no_match() {
        let (prefixes, _) = collect_dot_prefixes("x = 1");
        assert_eq!(prefixes.get(&999), None);
    }

    #[test]
    fn dot_prefix_decl_name() {
        let (prefixes, tokens) = collect_dot_prefixes("f32.");
        let dot = last_dot_index(&tokens);
        assert_eq!(prefixes.get(&dot).map(|s| s.as_str()), Some("f32"));
    }
}
