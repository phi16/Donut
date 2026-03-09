use donut_lang::check::{Entry, Env};
use donut_lang::types::common;
use donut_lang::types::item::Program;
use donut_lang::types::syntree;
use donut_lang::types::token;
use std::collections::HashMap;

use super::{CompletionCandidate, CompletionData, CompletionKind};

pub(super) fn build_completion_data(
    program: &Program,
    env: &Env,
    tokens: &[token::Token],
    dot_prefixes: HashMap<usize, String>,
) -> CompletionData {
    let mut scopes: HashMap<String, Vec<CompletionCandidate>> = HashMap::new();

    // Top-level entries from root module
    let mut top_level = Vec::new();
    let all_root = program.root.internal.iter().chain(program.root.entries.iter());
    for (name, item_id) in all_root {
        let item = program.item(*item_id);
        let def_line = tokens.get(item.span.start).map(|t| t.pos.line as u32).unwrap_or(0);
        let is_imported = item.origin.is_some();
        let has_members = item.members().map_or(false, |m| !m.entries.is_empty());
        let is_module = env.module_members.contains_key(name);

        if let Some(&idx) = env.lookup.get(name) {
            let entry = &env.entries[idx];
            let kind = if is_module {
                CompletionKind::Module
            } else {
                entry_to_kind(entry)
            };
            top_level.push(CompletionCandidate {
                label: name.clone(),
                kind,
                type_expr: if is_module { None } else { entry.display_type(env) },
                params: if is_module {
                    env.display_module_params(name)
                } else {
                    env.display_params(idx)
                },
                def_line,
                is_imported,
            });
        } else if is_module || has_members {
            top_level.push(CompletionCandidate {
                label: name.clone(),
                kind: CompletionKind::Module,
                type_expr: None,
                params: env.display_module_params(name),
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
            let is_sub_module = env.module_members.contains_key(&full_name);

            if let Some(&idx) = env.lookup.get(&full_name) {
                let entry = &env.entries[idx];
                let kind = if is_sub_module {
                    CompletionKind::Module
                } else {
                    entry_to_kind(entry)
                };
                candidates.push(CompletionCandidate {
                    label: member_name.clone(),
                    kind,
                    type_expr: if is_sub_module { None } else { entry.display_type(env) },
                    params: if is_sub_module {
                        env.display_module_params(&full_name)
                    } else {
                        env.display_params(idx)
                    },
                    def_line: 0,
                    is_imported: entry.origin.is_some(),
                });
            } else if is_sub_module {
                candidates.push(CompletionCandidate {
                    label: member_name.clone(),
                    kind: CompletionKind::Module,
                    type_expr: None,
                    params: env.display_module_params(&full_name),
                    def_line: 0,
                    is_imported: true,
                });
            }
        }
        scopes.insert(prefix.clone(), candidates);
    }

    CompletionData { scopes, dot_prefixes }
}

fn entry_to_kind(entry: &Entry) -> CompletionKind {
    match entry.display_kind().as_str() {
        "meta" => CompletionKind::Meta,
        "type" => CompletionKind::Type,
        s if s.ends_with("-cell") => {
            let dim: u8 = s.strip_suffix("-cell").and_then(|d| d.parse().ok()).unwrap_or(0);
            CompletionKind::Cell(dim)
        }
        _ => CompletionKind::Cell(0),
    }
}

/// Walk the syntree and collect all segment boundaries from paths.
/// Returns a map from token index (where a "." sits) to the prefix string.
/// Uses the syntree (before convert/resolve) so that ALL paths are included,
/// including incomplete declarations that convert would discard.
pub(super) fn collect_dot_prefixes(syn: &syntree::Program) -> HashMap<usize, String> {
    use common::A;

    let mut result = HashMap::new();

    fn record_path_segments(segments: &[A<syntree::Segment>], result: &mut HashMap<usize, String>) {
        let mut names = Vec::new();
        for seg_a in segments {
            if let A::Accepted(seg, span) = seg_a {
                if let A::Accepted(name, _) = &seg.0 {
                    names.push(name.0.as_str());
                    let prefix = names.join(".");
                    result.insert(span.end, prefix);
                }
            }
        }
    }

    fn walk_val(val_a: &A<syntree::Val>, result: &mut HashMap<usize, String>) {
        let val = match val_a.inner() {
            Some(v) => v,
            None => return,
        };
        for v0_a in &val.vs {
            if let Some(v0) = v0_a.inner() {
                match v0 {
                    syntree::Val0::Path(path_a) => {
                        if let Some(path) = path_a.inner() {
                            record_path_segments(&path.0, result);
                            // Walk param vals in segments
                            for seg_a in &path.0 {
                                if let Some(seg) = seg_a.inner() {
                                    if let Some(A::Accepted(params, _)) = &seg.1 {
                                        for param_a in &params.1 {
                                            if let Some(param) = param_a.inner() {
                                                walk_val(&param.val, result);
                                            }
                                        }
                                    }
                                }
                            }
                            // Walk applicand
                            if let Some(app) = &path.1 {
                                walk_val(app, result);
                            }
                        }
                    }
                    syntree::Val0::Lit(lit_a) => {
                        if let Some(lit) = lit_a.inner() {
                            match lit {
                                syntree::Lit::Array(vs) => {
                                    for v in vs { walk_val(v, result); }
                                }
                                syntree::Lit::Object(kvs) => {
                                    for (_, v) in kvs { walk_val(v, result); }
                                }
                                _ => {}
                            }
                        }
                    }
                    syntree::Val0::Paren(inner) => walk_val(inner, result),
                    syntree::Val0::Dots => {}
                }
            }
        }
        // Walk operator RHS params
        for (_, params_opt) in &val.ops {
            if let Some(A::Accepted(params, _)) = params_opt {
                for param_a in &params.1 {
                    if let Some(param) = param_a.inner() {
                        walk_val(&param.val, result);
                    }
                }
            }
        }
    }

    fn walk_module(m_a: &A<syntree::Module>, result: &mut HashMap<usize, String>) {
        if let Some(syntree::Module::Block(decls)) = m_a.inner() {
            for d_a in decls {
                if let Some(d) = d_a.inner() {
                    walk_decl(d, result);
                }
            }
        }
    }

    fn walk_decl(decl: &syntree::Decl, result: &mut HashMap<usize, String>) {
        // Decorators
        for deco_a in &decl.decos {
            if let Some(deco) = deco_a.inner() {
                for param_a in &deco.1 {
                    if let Some(param) = param_a.inner() {
                        walk_val(&param.val, result);
                    }
                }
            }
        }
        // Main
        if let Some(main) = &decl.main {
            match main {
                syntree::DeclMain::Unit(unit_a) => {
                    if let Some(unit) = unit_a.inner() {
                        // Declaration name paths
                        for name_path_a in &unit.names {
                            if let Some(path) = name_path_a.inner() {
                                record_path_segments(&path.0, result);
                                // Walk param type exprs in decl names
                                for seg_a in &path.0 {
                                    if let Some(seg) = seg_a.inner() {
                                        if let Some(A::Accepted(params, _)) = &seg.1 {
                                            for param_a in &params.1 {
                                                if let Some(param) = param_a.inner() {
                                                    walk_val(&param.val, result);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        // Type annotation
                        if let Some(ty) = &unit.ty {
                            walk_val(ty, result);
                        }
                        // Body
                        if let Some((_, body)) = &unit.assign {
                            match body {
                                syntree::ValMod::Val(v) => walk_val(v, result),
                                syntree::ValMod::Mod(m) => walk_module(m, result),
                            }
                        }
                    }
                }
                syntree::DeclMain::Mod(m) => walk_module(m, result),
                syntree::DeclMain::Dots => {}
            }
        }
        // Clauses
        for clause_a in &decl.clauses {
            if let Some(clause) = clause_a.inner() {
                walk_module(&clause.1, result);
            }
        }
    }

    for decl_a in &syn.0 {
        if let Some(decl) = decl_a.inner() {
            walk_decl(decl, &mut result);
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run_parse(code: &str) -> (common::A<syntree::Program>, Vec<token::Token<'_>>) {
        let (tokens, _, _) = donut_lang::tokenize::tokenize(code);
        let (program, _) = donut_lang::parse::parse(&tokens);
        (program, tokens)
    }

    /// Find the token index of the last "." in the token list.
    fn last_dot_index(tokens: &[token::Token]) -> usize {
        tokens.iter().rposition(|t| t.str == ".").expect("no dot found")
    }

    /// Find the token index of the nth "." in the token list (0-indexed).
    fn nth_dot_index(tokens: &[token::Token], n: usize) -> usize {
        tokens.iter().enumerate()
            .filter(|(_, t)| t.str == ".")
            .nth(n)
            .map(|(i, _)| i)
            .expect("dot not found")
    }

    fn dot_prefix_at(syn_a: &common::A<syntree::Program>, dot_token: usize) -> Option<String> {
        let syn = syn_a.inner()?;
        let prefixes = collect_dot_prefixes(syn);
        prefixes.get(&dot_token).cloned()
    }

    #[test]
    fn dot_prefix_simple() {
        let code = "x = {\n  a = 1\n}\ny = x.";
        let (syn, tokens) = run_parse(code);
        let dot = last_dot_index(&tokens);
        assert_eq!(dot_prefix_at(&syn, dot), Some("x".to_string()));
    }

    #[test]
    fn dot_prefix_nested() {
        let code = "x = {\n  y = {\n    z = 1\n  }\n}\nw = x.y.";
        let (syn, tokens) = run_parse(code);
        let dot = last_dot_index(&tokens);
        assert_eq!(dot_prefix_at(&syn, dot), Some("x.y".to_string()));
    }

    #[test]
    fn dot_prefix_with_params() {
        let code = "[a] x = {\n  b = 1\n}\ny = x[1].";
        let (syn, tokens) = run_parse(code);
        let dot = last_dot_index(&tokens);
        assert_eq!(dot_prefix_at(&syn, dot), Some("x".to_string()));
    }

    #[test]
    fn dot_prefix_middle() {
        let code = "x = {\n  y = {\n    z = 1\n  }\n}\nw = x.y.z";
        let (syn, tokens) = run_parse(code);
        let dot0 = nth_dot_index(&tokens, 0);
        assert_eq!(dot_prefix_at(&syn, dot0), Some("x".to_string()));

        let dot1 = nth_dot_index(&tokens, 1);
        assert_eq!(dot_prefix_at(&syn, dot1), Some("x.y".to_string()));
    }

    #[test]
    fn dot_prefix_no_match() {
        let code = "x = 1";
        let (syn, _tokens) = run_parse(code);
        assert_eq!(dot_prefix_at(&syn, 999), None);
    }

    #[test]
    fn dot_prefix_decl_name() {
        // Declaration name path (not a value): f32. at top level
        let code = "f32.";
        let (syn, tokens) = run_parse(code);
        let dot = last_dot_index(&tokens);
        assert_eq!(dot_prefix_at(&syn, dot), Some("f32".to_string()));
    }
}
