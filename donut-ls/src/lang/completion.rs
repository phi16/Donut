use donut_lang::old_check::Env;
use donut_lang::types::old_item::Program;
use donut_lang::types::token;
use std::collections::HashMap;

use super::{build_entry_info, build_module_info, CompletionCandidate, CompletionData};

pub(super) fn build_completion_data(
    program: &Program,
    env: &Env,
    tokens: &[token::Token],
    dot_prefixes: HashMap<usize, String>,
) -> CompletionData {
    let mut scopes: HashMap<String, Vec<CompletionCandidate>> = HashMap::new();

    // Top-level entries from root module
    let mut top_level = Vec::new();
    let all_root = program
        .root
        .internal
        .iter()
        .chain(program.root.entries.iter());
    for (name, item_id) in all_root {
        let item = program.item(*item_id);
        let def_line = tokens
            .get(item.span.start)
            .map(|t| t.pos.line as u32)
            .unwrap_or(0);
        let is_imported = item.origin.is_some();

        let entry = if let Some(info) = build_entry_info(name, env) {
            Some(info)
        } else {
            // Not in env.lookup — check if it's a known module
            let has_members = item.members().map_or(false, |m| !m.entries.is_empty());
            let is_module = env.module_members.contains_key(name);
            if is_module || has_members {
                Some(build_module_info(name, env))
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

    // Module member scopes from env.module_members
    for (prefix, members) in &env.module_members {
        let mut candidates = Vec::new();
        for member_name in members {
            let full_name = format!("{}.{}", prefix, member_name);
            let entry = if let Some(info) = build_entry_info(&full_name, env) {
                Some((info, env.entries[env.lookup[&full_name]].origin.is_some()))
            } else if env.module_members.contains_key(&full_name) {
                Some((build_module_info(&full_name, env), true))
            } else {
                None
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
        scopes.insert(prefix.clone(), candidates);
    }

    CompletionData {
        scopes,
        dot_prefixes,
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
