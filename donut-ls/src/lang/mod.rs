mod completion;
mod hover;
mod marking;

use donut_lang::types;
use std::collections::HashMap;

use completion::{build_completion_data, collect_dot_prefixes};
use hover::HoverBuilder;
use marking::{Context, Marking};

#[derive(Debug, Clone)]
pub enum TokenType {
    Unknown,
    Keyword,
    Operator,
    Symbol,
    Number,
    String,
    Comment,
    Parameter, // [x: T] の x などパラメータ名
    Namespace, // module 名
}

pub struct Diagnostic {
    pub begin_line: u32,
    pub begin_column: u32,
    pub end_line: u32,
    pub end_column: u32,
    pub message: String,
    pub source: &'static str,
}

pub struct TokenData {
    pub line: u32,
    pub column: u32,
    pub length: u32,
    pub token_type: TokenType,
    pub token_index: Option<usize>,
}

#[derive(Clone)]
pub struct HoverInfo {
    pub name: String,
    pub detail: String,
    pub type_expr: Option<String>,
    pub params: String,
}

// --- Completion data ---

#[derive(Clone)]
pub enum CompletionKind {
    Cell(u8),
    Meta,
    Type,
    Module,
}

#[derive(Clone)]
pub struct CompletionCandidate {
    pub label: String,
    pub kind: CompletionKind,
    pub type_expr: Option<String>,
    pub params: String,
    pub def_line: u32,
    pub is_imported: bool,
}

#[derive(Clone)]
pub struct CompletionData {
    /// prefix → candidates. "" for top-level.
    pub scopes: HashMap<String, Vec<CompletionCandidate>>,
    /// dot token index → prefix string (e.g. token for "." after "sys.f32" → "sys.f32").
    pub dot_prefixes: HashMap<usize, String>,
}

impl CompletionData {
    pub fn empty() -> Self {
        CompletionData {
            scopes: HashMap::new(),
            dot_prefixes: HashMap::new(),
        }
    }
}

pub struct AnalysisResult {
    pub tokens: Vec<TokenData>,
    pub diagnostics: Vec<Diagnostic>,
    pub hover_map: HashMap<usize, HoverInfo>,
    pub completion: CompletionData,
}

// --- Helpers ---

fn to_diag(pos: &types::common::TokenPos, msg: &str, source: &'static str) -> Diagnostic {
    Diagnostic {
        begin_line: pos.line as u32,
        begin_column: pos.col as u32,
        end_line: pos.line as u32,
        end_column: (pos.col + pos.len) as u32,
        message: msg.to_string(),
        source,
    }
}

fn to_utf16(lines: &[&str], line: usize, col: usize, len: usize) -> (u32, u32) {
    let mut chars = lines.get(line).unwrap().chars();
    let utf16_col = chars
        .by_ref()
        .take(col)
        .map(|c| c.len_utf16())
        .sum::<usize>() as u32;
    let utf16_len = chars.take(len).map(|c| c.len_utf16()).sum::<usize>() as u32;
    (utf16_col, utf16_len)
}

// --- Main analysis ---

pub fn analyze(code: &str) -> AnalysisResult {
    let lines = code.lines().collect::<Vec<_>>();

    let (tokens, comments, tokenize_errors) = donut_lang::tokenize::tokenize(code);

    // tokenizer の型付けを初期値として TokenData を構築
    let token_data = tokens
        .iter()
        .enumerate()
        .map(|(i, t)| {
            let (utf16_col, utf16_len) = to_utf16(&lines, t.pos.line, t.pos.col, t.pos.len);
            TokenData {
                line: t.pos.line as u32,
                column: utf16_col,
                length: utf16_len,
                token_index: Some(i),
                token_type: match t.ty {
                    types::token::TokenTy::Name => {
                        if donut_lang::convert::is_number_str(t.str) {
                            TokenType::Number
                        } else {
                            TokenType::Unknown
                        }
                    }
                    types::token::TokenTy::Keyword => TokenType::Keyword,
                    types::token::TokenTy::Operator => TokenType::Operator,
                    types::token::TokenTy::Symbol => TokenType::Symbol,
                    types::token::TokenTy::Number => TokenType::Number,
                    types::token::TokenTy::String => TokenType::String,
                    types::token::TokenTy::Whitespace => TokenType::Unknown,
                },
            }
        })
        .collect::<Vec<_>>();

    // tokenize エラーを診断として収集
    let mut diags: Vec<Diagnostic> = tokenize_errors
        .iter()
        .map(|(pos, msg)| to_diag(pos, msg, "[tokenize]"))
        .collect();

    // parse は常に実行（エラー回復があるので tokenize エラーがあっても走らせる）
    let mut ctx = Context::new(token_data);
    let (program, parse_errors) = donut_lang::parse::parse(&tokens);
    program.mark(&mut ctx);
    for (pos, msg) in &parse_errors {
        diags.push(to_diag(pos, msg, "[parse]"));
    }

    // dot completion 用のプレフィックス情報を syntree から抽出（convert が消費する前に）
    let dot_prefixes = match program.inner() {
        Some(p) => collect_dot_prefixes(p),
        None => HashMap::new(),
    };

    // convert（意味解析）を実行
    let (sem_program, convert_errors) = donut_lang::convert::convert(program, &tokens);
    for (pos, msg) in &convert_errors {
        diags.push(to_diag(pos, msg, "[convert]"));
    }

    // resolve（名前解決）を実行
    let (resolved, resolve_errors) = donut_lang::resolve::resolve(sem_program, &tokens);
    for (pos, msg) in &resolve_errors {
        diags.push(to_diag(pos, msg, "[resolve]"));
    }

    // check（型検査）を実行
    let (env, check_errors) = donut_lang::check::check(&resolved, &tokens);
    for (pos, msg) in &check_errors {
        diags.push(to_diag(pos, msg, "[check]"));
    }

    // hover map とスタイルオーバーライドを構築
    let (hover_map, style_overrides) = HoverBuilder::new(&resolved, &env).build();

    // コメントトークンを構築
    let comments_iter = comments.into_iter().map(|pos| {
        let (utf16_col, utf16_len) = to_utf16(&lines, pos.line, pos.col, pos.len);
        TokenData {
            line: pos.line as u32,
            column: utf16_col,
            length: utf16_len,
            token_type: TokenType::Comment,
            token_index: None,
        }
    });

    // スタイルオーバーライドを適用
    let mut token_data = ctx.into_inner();
    for td in &mut token_data {
        if let Some(idx) = td.token_index {
            if let Some(style) = style_overrides.get(&idx) {
                td.token_type = style.clone();
            }
        }
    }

    // トークンとコメントを行・列順にマージ
    let mut tokens_iter = token_data.into_iter().peekable();
    let mut comments_iter = comments_iter.peekable();
    let mut res = Vec::new();
    loop {
        match (tokens_iter.peek(), comments_iter.peek()) {
            (Some(t), Some(c)) => {
                if (t.line, t.column) <= (c.line, c.column) {
                    res.push(tokens_iter.next().unwrap());
                } else {
                    res.push(comments_iter.next().unwrap());
                }
            }
            (Some(_), None) => {
                res.extend(tokens_iter);
                break;
            }
            (None, _) => {
                res.extend(comments_iter);
                break;
            }
        }
    }

    // 補完データを構築
    let completion = build_completion_data(&resolved, &env, &tokens, dot_prefixes);

    AnalysisResult {
        tokens: res,
        diagnostics: diags,
        hover_map,
        completion,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // --- Helpers ---

    fn find_hover<'a>(r: &'a AnalysisResult, name: &str) -> Option<&'a HoverInfo> {
        r.hover_map.values().find(|h| h.name == name)
    }

    fn hover_names(r: &AnalysisResult) -> Vec<String> {
        let mut names: Vec<_> = r.hover_map.values().map(|h| h.name.clone()).collect();
        names.sort();
        names.dedup();
        names
    }

    fn completion_labels(r: &AnalysisResult, scope: &str) -> Vec<String> {
        let mut labels: Vec<_> = r.completion.scopes
            .get(scope)
            .map_or(vec![], |cs| cs.iter().map(|c| c.label.clone()).collect());
        labels.sort();
        labels
    }

    fn find_completion<'a>(r: &'a AnalysisResult, scope: &str, label: &str) -> Option<&'a CompletionCandidate> {
        r.completion.scopes.get(scope)?.iter().find(|c| c.label == label)
    }

    fn token_type_at(r: &AnalysisResult, line: u32, col: u32) -> Option<&TokenType> {
        r.tokens.iter()
            .find(|t| t.line == line && t.column <= col && col < t.column + t.length)
            .map(|t| &t.token_type)
    }

    fn diag_sources(r: &AnalysisResult) -> Vec<&str> {
        r.diagnostics.iter().map(|d| d.source).collect()
    }

    // --- Hover tests ---

    #[test]
    fn hover_basic_cells() {
        let r = analyze("u: *\nx: u → u");
        let u = find_hover(&r, "u").unwrap();
        assert_eq!(u.detail, "0-cell");
        assert_eq!(u.type_expr.as_deref(), Some("*"));

        let x = find_hover(&r, "x").unwrap();
        assert_eq!(x.detail, "1-cell");
        assert_eq!(x.type_expr.as_deref(), Some("u → u"));
    }

    #[test]
    fn hover_module() {
        let r = analyze("cat = {\n  u: *\n  x: u → u\n}");
        let cat = find_hover(&r, "cat").unwrap();
        assert_eq!(cat.detail, "module");
        assert_eq!(cat.type_expr, None);

        let u = find_hover(&r, "cat.u").unwrap();
        assert_eq!(u.detail, "0-cell");
    }

    #[test]
    fn hover_nested_path() {
        let r = analyze("cat = {\n  u: *\n  x: u → u\n}\ny = cat.x");
        // "cat" in the reference should have module hover
        let cat = find_hover(&r, "cat").unwrap();
        assert_eq!(cat.detail, "module");
        // "x" in the reference should resolve to cat.x
        let x = find_hover(&r, "cat.x").unwrap();
        assert_eq!(x.detail, "1-cell");
    }

    #[test]
    fn hover_parametric() {
        // Parameters are attached to name: x[u: *], not [u: *] x
        let r = analyze("x[u: *]: u → u");
        let x = find_hover(&r, "x").unwrap();
        assert_eq!(x.detail, "1-cell");
        assert!(!x.params.is_empty());
    }

    #[test]
    fn hover_meta() {
        let r = analyze("nat: meta");
        let n = find_hover(&r, "nat").unwrap();
        assert_eq!(n.detail, "meta");
    }

    #[test]
    fn hover_type_alias() {
        let r = analyze("u: *\nv = u → u\nx: v");
        let v = find_hover(&r, "v").unwrap();
        assert_eq!(v.detail, "type");
    }

    #[test]
    fn hover_2cell() {
        let r = analyze("u: *\nx: u → u\ny: u → u\nm: x → y");
        let m = find_hover(&r, "m").unwrap();
        assert_eq!(m.detail, "2-cell");
        assert_eq!(m.type_expr.as_deref(), Some("x → y"));
    }

    #[test]
    fn hover_star_literal() {
        let r = analyze("u: *");
        // "*" should get a hover
        let star = find_hover(&r, "*");
        assert!(star.is_some());
    }

    // --- Completion tests ---

    #[test]
    fn completion_top_level() {
        let r = analyze("u: *\nx: u → u");
        let labels = completion_labels(&r, "");
        assert!(labels.contains(&"u".to_string()));
        assert!(labels.contains(&"x".to_string()));
    }

    #[test]
    fn completion_module_members() {
        let r = analyze("cat = {\n  u: *\n  x: u → u\n}");
        let labels = completion_labels(&r, "cat");
        assert!(labels.contains(&"u".to_string()));
        assert!(labels.contains(&"x".to_string()));
    }

    #[test]
    fn completion_nested_module() {
        let r = analyze("a = {\n  b = {\n    c: *\n  }\n}");
        let a_labels = completion_labels(&r, "a");
        assert!(a_labels.contains(&"b".to_string()));

        let ab_labels = completion_labels(&r, "a.b");
        assert!(ab_labels.contains(&"c".to_string()));
    }

    #[test]
    fn completion_module_kind() {
        let r = analyze("cat = {\n  u: *\n}");
        let cat = find_completion(&r, "", "cat").unwrap();
        assert!(matches!(cat.kind, CompletionKind::Module));

        let u = find_completion(&r, "cat", "u").unwrap();
        assert!(matches!(u.kind, CompletionKind::Cell(0)));
    }

    #[test]
    fn completion_cell_kind() {
        let r = analyze("u: *\nx: u → u\nm: x → x");
        let u = find_completion(&r, "", "u").unwrap();
        assert!(matches!(u.kind, CompletionKind::Cell(0)));
        let x = find_completion(&r, "", "x").unwrap();
        assert!(matches!(x.kind, CompletionKind::Cell(1)));
        let m = find_completion(&r, "", "m").unwrap();
        assert!(matches!(m.kind, CompletionKind::Cell(2)));
    }

    #[test]
    fn completion_meta_kind() {
        let r = analyze("nat: meta");
        let n = find_completion(&r, "", "nat").unwrap();
        assert!(matches!(n.kind, CompletionKind::Meta));
    }

    #[test]
    fn completion_type_kind() {
        let r = analyze("u: *\nv = u → u");
        let v = find_completion(&r, "", "v").unwrap();
        assert!(matches!(v.kind, CompletionKind::Type));
    }

    #[test]
    fn completion_type_expr() {
        let r = analyze("u: *\nx: u → u");
        let u = find_completion(&r, "", "u").unwrap();
        assert_eq!(u.type_expr.as_deref(), Some("*"));
        let x = find_completion(&r, "", "x").unwrap();
        assert_eq!(x.type_expr.as_deref(), Some("u → u"));
    }

    #[test]
    fn completion_def_line() {
        let r = analyze("u: *\nx: u → u");
        let u = find_completion(&r, "", "u").unwrap();
        assert_eq!(u.def_line, 0);
        let x = find_completion(&r, "", "x").unwrap();
        assert_eq!(x.def_line, 1);
    }

    // --- Token type / marking tests ---

    #[test]
    fn token_type_keyword() {
        let r = analyze("u: *\ncat = {\n  x: u → u\n} with {\n  y: u → u\n}");
        // "with" at line 3 after "} "
        let ty = token_type_at(&r, 3, 2);
        assert!(matches!(ty, Some(TokenType::Keyword)));
    }

    #[test]
    fn token_type_operator() {
        // "=" is an operator
        let r = analyze("u: *\nx = u");
        // "=" at line 1, col 2
        let ty = token_type_at(&r, 1, 2);
        assert!(matches!(ty, Some(TokenType::Operator)));
    }

    #[test]
    fn token_type_parameter() {
        // x[a: *] — "a" is a param name
        let r = analyze("x[a: *]: a → a");
        // "a" at col 2 (after "x[")
        let ty = token_type_at(&r, 0, 2);
        assert!(matches!(ty, Some(TokenType::Parameter)));
    }

    #[test]
    fn token_type_namespace() {
        let r = analyze("cat = {\n  u: *\n}\nx = cat.u");
        // "cat" in "cat.u" (line 3) should be Namespace
        let ty = token_type_at(&r, 3, 4);
        assert!(matches!(ty, Some(TokenType::Namespace)));
    }

    #[test]
    fn token_type_number() {
        let r = analyze("nat: meta\nx[n: nat]: *\ny = x[0]");
        let ty = token_type_at(&r, 2, 6);
        assert!(matches!(ty, Some(TokenType::Number)));
    }

    #[test]
    fn token_type_comment() {
        let r = analyze("// hello\nu: *");
        // comment on line 0
        let ty = token_type_at(&r, 0, 0);
        assert!(matches!(ty, Some(TokenType::Comment)));
    }

    // --- Diagnostics tests ---

    #[test]
    fn diag_no_errors() {
        let r = analyze("u: *\nx: u → u");
        assert!(r.diagnostics.is_empty());
    }

    #[test]
    fn diag_undefined_name() {
        let r = analyze("x: undefined → undefined");
        assert!(!r.diagnostics.is_empty());
    }

    #[test]
    fn diag_source_tag() {
        let r = analyze("x: undefined → undefined");
        // Error should come from resolve or check
        for d in &r.diagnostics {
            assert!(d.source.starts_with("["));
            assert!(d.source.ends_with("]"));
        }
    }

    #[test]
    fn diag_parse_error() {
        let r = analyze("x =");
        let sources = diag_sources(&r);
        // Should have parse or convert error
        assert!(!sources.is_empty());
    }

    // --- Integration tests ---

    #[test]
    fn empty_code() {
        let r = analyze("");
        assert!(r.tokens.is_empty());
        assert!(r.diagnostics.is_empty());
        assert!(r.hover_map.is_empty());
    }

    #[test]
    fn comment_only() {
        let r = analyze("// just a comment");
        assert!(r.diagnostics.is_empty());
        assert!(r.hover_map.is_empty());
        // Should have one comment token
        assert!(r.tokens.iter().any(|t| matches!(t.token_type, TokenType::Comment)));
    }

    #[test]
    fn hover_does_not_leak_module_internals() {
        // Module definition creates entries, but hover should only show
        // what's in the current file's scope
        let r = analyze("m = {\n  a: *\n}\nx = m.a");
        let names = hover_names(&r);
        // "m" and "m.a" should be present, but no bare "a"
        assert!(names.contains(&"m".to_string()));
        assert!(names.contains(&"m.a".to_string()));
        assert!(!names.contains(&"a".to_string()));
    }

    #[test]
    fn completion_module_not_duplicated() {
        // A module should appear exactly once in top-level completions
        let r = analyze("m = {\n  a: *\n  b: *\n}");
        let count = r.completion.scopes.get("")
            .map_or(0, |cs| cs.iter().filter(|c| c.label == "m").count());
        assert_eq!(count, 1);
    }

    #[test]
    fn namespace_marking_deep() {
        // In "a.b.c", both "a" and "b" should be Namespace
        let r = analyze("a = {\n  b = {\n    c: *\n  }\n}\nx = a.b.c");
        // line 5: "x = a.b.c"
        // a at col 4
        let ty_a = token_type_at(&r, 5, 4);
        assert!(matches!(ty_a, Some(TokenType::Namespace)), "a should be Namespace");
        // b at col 6
        let ty_b = token_type_at(&r, 5, 6);
        assert!(matches!(ty_b, Some(TokenType::Namespace)), "b should be Namespace");
        // c at col 8 should NOT be Namespace
        let ty_c = token_type_at(&r, 5, 8);
        assert!(!matches!(ty_c, Some(TokenType::Namespace)), "c should not be Namespace");
    }

    #[test]
    fn hover_with_clause() {
        let r = analyze("u: *\ncat = {\n  x: u → u\n} with {\n  m: x → x\n}");
        let m = find_hover(&r, "cat.m");
        assert!(m.is_some());
        assert_eq!(m.unwrap().detail, "2-cell");
    }

    #[test]
    fn completion_with_clause() {
        let r = analyze("u: *\ncat = {\n  x: u → u\n} with {\n  m: x → x\n}");
        let labels = completion_labels(&r, "cat");
        assert!(labels.contains(&"x".to_string()));
        assert!(labels.contains(&"m".to_string()));
    }

    #[test]
    fn completion_params_display() {
        let r = analyze("x[u: *]: u → u");
        let x = find_completion(&r, "", "x").unwrap();
        assert!(!x.params.is_empty());
    }

    #[test]
    fn dot_prefixes_present() {
        let r = analyze("m = {\n  a: *\n}\nx = m.a");
        // There should be a dot_prefix entry for the "." in "m.a"
        assert!(!r.completion.dot_prefixes.is_empty());
        assert!(r.completion.dot_prefixes.values().any(|v| v == "m"));
    }
}
