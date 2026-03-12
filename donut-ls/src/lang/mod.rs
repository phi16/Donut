mod completion;
mod hover;
mod marking;

use donut_lang::types;
use donut_lang::types::env::{self, Env, Ty};
use donut_lang::types::item::DefId;
use std::collections::HashMap;

use completion::build_completion_data;
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

#[derive(Debug, Clone, PartialEq)]
pub enum EntryKind {
    Cell(u8),
    Meta,
    Type,
}

impl EntryKind {
    pub fn display(&self) -> String {
        match self {
            EntryKind::Cell(dim) => format!("{}-cell", dim),
            EntryKind::Meta => "meta".to_string(),
            EntryKind::Type => "type".to_string(),
        }
    }
}

fn ty_to_kind(ty: &Ty) -> EntryKind {
    match ty {
        Ty::Star => EntryKind::Cell(0),
        Ty::Arrow(level, _, _, _) => EntryKind::Cell(*level as u8),
        Ty::Nat | Ty::Rat | Ty::Color | Ty::Deco | Ty::Meta => EntryKind::Meta,
        Ty::Functor(_, _) => EntryKind::Type,
        Ty::Hole => EntryKind::Cell(0),
    }
}

fn def_to_kind(def: &env::Def) -> EntryKind {
    match &def.ty {
        Ty::Meta => {
            if def.item.is_some() {
                EntryKind::Meta // declaration like `nat: meta`
            } else {
                EntryKind::Type // alias like `v = u → u`
            }
        }
        ty => ty_to_kind(ty),
    }
}

pub(crate) fn item_to_kind(item: &env::Item) -> EntryKind {
    ty_to_kind(&item.ty)
}

#[derive(Clone)]
pub struct EntryInfo {
    pub kind: EntryKind,
    pub module_kind: Option<ModuleKind>,
    pub type_expr: Option<String>,
    pub params: String,
}

impl EntryInfo {
    pub fn is_module(&self) -> bool {
        self.module_kind.is_some()
    }

    pub fn tags(&self) -> Vec<String> {
        let mut tags = vec![self.kind.display()];
        match self.module_kind {
            Some(ModuleKind::Module) => tags.push("module".to_string()),
            Some(ModuleKind::Definition) => tags.push("definition".to_string()),
            None => {}
        }
        tags
    }

    pub fn display_detail(&self) -> String {
        self.tags().join(", ")
    }

    /// Completion アイテムの detail 文字列を生成
    pub fn display_completion_detail(&self) -> String {
        let kind_str = self.display_detail();
        match (&self.type_expr, self.params.is_empty()) {
            (Some(ty), true) => format!("{} ({})", ty, kind_str),
            (Some(ty), false) => format!("{} {} ({})", self.params, ty, kind_str),
            (None, true) => format!("({})", kind_str),
            (None, false) => format!("{} ({})", self.params, kind_str),
        }
    }
}

#[derive(Clone)]
pub struct HoverInfo {
    pub name: String,
    pub entry: EntryInfo,
    /// Tags describing the entry (e.g. ["1-cell", "parameter of rep4"])
    pub tags: Vec<String>,
}

impl HoverInfo {
    pub fn display_markdown(&self) -> String {
        let detail = self.tags.join(", ");
        if let Some(ty) = &self.entry.type_expr {
            format!(
                "```donut\n{}{}: {}\n```\n{}",
                self.name, self.entry.params, ty, detail
            )
        } else {
            format!(
                "```donut\n{}{}\n```\n{}",
                self.name, self.entry.params, detail
            )
        }
    }
}

// --- Completion data ---

#[derive(Clone)]
pub struct CompletionCandidate {
    pub label: String,
    pub entry: EntryInfo,
    pub def_line: u32,
    pub is_imported: bool,
}

#[derive(Clone, Default)]
pub struct CompletionData {
    /// prefix → candidates. "" for top-level.
    pub scopes: HashMap<String, Vec<CompletionCandidate>>,
    /// dot token index → prefix string (e.g. token for "." after "sys.f32" → "sys.f32").
    pub dot_prefixes: HashMap<usize, String>,
}

pub struct AnalysisResult {
    pub tokens: Vec<TokenData>,
    pub diagnostics: Vec<Diagnostic>,
    pub hover_map: HashMap<usize, HoverInfo>,
    pub completion: CompletionData,
}

// --- Flat lookup from env Module tree ---

/// def メンバーのみ持つ (DeclDef) か、他のメンバーも持つ (module) かを区別
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum ModuleKind {
    /// Only contains "def" member (DeclDef)
    Definition,
    /// Contains other members
    Module,
}

pub(crate) struct FlatEnv {
    pub defs: HashMap<String, DefId>,
    pub modules: HashMap<String, ModuleKind>,
}

impl FlatEnv {
    pub fn build(env: &Env) -> Self {
        let mut flat = FlatEnv {
            defs: HashMap::new(),
            modules: HashMap::new(),
        };
        Self::walk(&env.root, "", &mut flat);
        flat
    }

    fn walk(module: &env::Module, prefix: &str, flat: &mut FlatEnv) {
        for (name, child) in &module.lookup {
            let qname = if prefix.is_empty() {
                name.clone()
            } else {
                format!("{}.{}", prefix, name)
            };
            if let Some(def_id) = child.this {
                flat.defs.insert(qname.clone(), def_id);
            }
            if !child.lookup.is_empty() {
                let kind = if child.lookup.len() == 1
                    && child.lookup.contains_key(donut_lang::types::item::DEF_MEMBER_NAME)
                {
                    ModuleKind::Definition
                } else {
                    ModuleKind::Module
                };
                flat.modules.insert(qname.clone(), kind);
            }
            Self::walk(child, &qname, flat);
        }
    }
}

// --- Shared hover/completion info construction ---

/// Build EntryInfo for an entry by qualified name.
fn build_entry_info(qname: &str, env: &Env, flat: &FlatEnv) -> Option<EntryInfo> {
    let &def_id = flat.defs.get(qname)?;
    let def = &env.defs[def_id.0];
    let module_kind = flat.modules.get(qname).copied();
    let kind = def_to_kind(def);
    Some(EntryInfo {
        kind,
        module_kind,
        type_expr: Some(env.display_def_ty(def)),
        params: env.display_params(def),
    })
}

/// Build EntryInfo for a module (not a def, but known as a module).
fn build_module_info() -> EntryInfo {
    EntryInfo {
        kind: EntryKind::Cell(0), // unused when module_kind=Some
        module_kind: Some(ModuleKind::Module),
        type_expr: None,
        params: String::new(),
    }
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

fn collect_diags(
    diags: &mut Vec<Diagnostic>,
    errors: &[(types::common::TokenPos, String)],
    source: &'static str,
) {
    diags.extend(errors.iter().map(|(pos, msg)| to_diag(pos, msg, source)));
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
    let mut diags = Vec::new();
    collect_diags(&mut diags, &tokenize_errors, "[tokenize]");

    // parse + marking + dot prefix 収集を一度の syntree 走査で実行
    let mut ctx = Context::new(token_data);
    let (program, parse_errors) = donut_lang::parse::parse(&tokens);
    program.mark(&mut ctx);
    collect_diags(&mut diags, &parse_errors, "[parse]");
    let (token_data, dot_prefixes) = ctx.into_parts();

    // convert（意味解析）を実行
    let (sem_program, convert_errors) = donut_lang::convert::convert(program, &tokens);
    collect_diags(&mut diags, &convert_errors, "[convert]");

    // resolve（名前解決）を実行
    let (resolved, resolve_errors) = donut_lang::resolve::resolve(sem_program, &tokens);
    collect_diags(&mut diags, &resolve_errors, "[resolve]");

    // check（型検査）を実行
    let (env, check_errors) = donut_lang::check::check(&resolved, &tokens);
    collect_diags(&mut diags, &check_errors, "[check]");

    // hover map とスタイルオーバーライドを構築
    let flat = FlatEnv::build(&env);
    let (hover_map, style_overrides) = HoverBuilder::new(&resolved, &env, &flat, &tokens).build();

    // スタイルオーバーライドを適用
    let mut token_data = token_data;
    for td in &mut token_data {
        if let Some(idx) = td.token_index {
            if let Some(style) = style_overrides.get(&idx) {
                td.token_type = style.clone();
            }
        }
    }

    // コメントトークンを追加し、行・列順にソート
    let comment_tokens = comments.into_iter().map(|pos| {
        let (utf16_col, utf16_len) = to_utf16(&lines, pos.line, pos.col, pos.len);
        TokenData {
            line: pos.line as u32,
            column: utf16_col,
            length: utf16_len,
            token_type: TokenType::Comment,
            token_index: None,
        }
    });
    let mut res: Vec<_> = token_data.into_iter().chain(comment_tokens).collect();
    res.sort_by_key(|t| (t.line, t.column));

    // 補完データを構築
    let completion = build_completion_data(&resolved, &env, &flat, &tokens, dot_prefixes);

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
        let mut labels: Vec<_> = r
            .completion
            .scopes
            .get(scope)
            .map_or(vec![], |cs| cs.iter().map(|c| c.label.clone()).collect());
        labels.sort();
        labels
    }

    fn find_completion<'a>(
        r: &'a AnalysisResult,
        scope: &str,
        label: &str,
    ) -> Option<&'a CompletionCandidate> {
        r.completion
            .scopes
            .get(scope)?
            .iter()
            .find(|c| c.label == label)
    }

    fn token_type_at(r: &AnalysisResult, line: u32, col: u32) -> Option<&TokenType> {
        r.tokens
            .iter()
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
        assert_eq!(u.entry.kind, EntryKind::Cell(0));
        assert_eq!(u.entry.type_expr.as_deref(), Some("*"));

        let x = find_hover(&r, "x").unwrap();
        assert_eq!(x.entry.kind, EntryKind::Cell(1));
        assert_eq!(x.entry.type_expr.as_deref(), Some("u → u"));
    }

    #[test]
    fn hover_module() {
        let r = analyze("cat = {\n  u: *\n  x: u → u\n}");
        let cat = find_hover(&r, "cat").unwrap();
        assert!(cat.entry.is_module());

        let u = find_hover(&r, "cat.u").unwrap();
        assert_eq!(u.entry.kind, EntryKind::Cell(0));
    }

    #[test]
    fn hover_nested_path() {
        let r = analyze("cat = {\n  u: *\n  x: u → u\n}\ny = cat.x");
        let cat = find_hover(&r, "cat").unwrap();
        assert!(cat.entry.is_module());
        let x = find_hover(&r, "cat.x").unwrap();
        assert_eq!(x.entry.kind, EntryKind::Cell(1));
    }

    #[test]
    fn hover_parametric() {
        // Parameters are attached to name: x[u: *], not [u: *] x
        let r = analyze("x[u: *]: u → u");
        let x = find_hover(&r, "x").unwrap();
        assert_eq!(x.entry.kind, EntryKind::Cell(1));
        assert!(!x.entry.params.is_empty());
    }

    #[test]
    fn hover_meta() {
        let r = analyze("nat: meta");
        let n = find_hover(&r, "nat").unwrap();
        assert_eq!(n.entry.kind, EntryKind::Meta);
    }

    #[test]
    fn hover_type_alias() {
        let r = analyze("u: *\nv = u → u\nx: v");
        let v = find_hover(&r, "v").unwrap();
        assert_eq!(v.entry.kind, EntryKind::Type);
    }

    #[test]
    fn hover_2cell() {
        let r = analyze("u: *\nx: u → u\ny: u → u\nm: x → y");
        let m = find_hover(&r, "m").unwrap();
        assert_eq!(m.entry.kind, EntryKind::Cell(2));
        assert_eq!(m.entry.type_expr.as_deref(), Some("x → y"));
    }

    #[test]
    fn hover_star_literal() {
        let r = analyze("u: *");
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
        assert!(cat.entry.is_module());

        let u = find_completion(&r, "cat", "u").unwrap();
        assert_eq!(u.entry.kind, EntryKind::Cell(0));
        assert!(!u.entry.is_module());
    }

    #[test]
    fn completion_cell_kind() {
        let r = analyze("u: *\nx: u → u\nm: x → x");
        let u = find_completion(&r, "", "u").unwrap();
        assert_eq!(u.entry.kind, EntryKind::Cell(0));
        let x = find_completion(&r, "", "x").unwrap();
        assert_eq!(x.entry.kind, EntryKind::Cell(1));
        let m = find_completion(&r, "", "m").unwrap();
        assert_eq!(m.entry.kind, EntryKind::Cell(2));
    }

    #[test]
    fn completion_meta_kind() {
        let r = analyze("nat: meta");
        let n = find_completion(&r, "", "nat").unwrap();
        assert_eq!(n.entry.kind, EntryKind::Meta);
    }

    #[test]
    fn completion_type_kind() {
        let r = analyze("u: *\nv = u → u");
        let v = find_completion(&r, "", "v").unwrap();
        assert_eq!(v.entry.kind, EntryKind::Type);
    }

    #[test]
    fn completion_type_expr() {
        let r = analyze("u: *\nx: u → u");
        let u = find_completion(&r, "", "u").unwrap();
        assert_eq!(u.entry.type_expr.as_deref(), Some("*"));
        let x = find_completion(&r, "", "x").unwrap();
        assert_eq!(x.entry.type_expr.as_deref(), Some("u → u"));
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
        for d in &r.diagnostics {
            assert!(d.source.starts_with("["));
            assert!(d.source.ends_with("]"));
        }
    }

    #[test]
    fn diag_parse_error() {
        let r = analyze("x =");
        let sources = diag_sources(&r);
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
        assert!(r
            .tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::Comment)));
    }

    #[test]
    fn hover_does_not_leak_module_internals() {
        let r = analyze("m = {\n  a: *\n}\nx = m.a");
        let names = hover_names(&r);
        assert!(names.contains(&"m".to_string()));
        assert!(names.contains(&"m.a".to_string()));
        assert!(!names.contains(&"a".to_string()));
    }

    #[test]
    fn completion_module_not_duplicated() {
        let r = analyze("m = {\n  a: *\n  b: *\n}");
        let count = r
            .completion
            .scopes
            .get("")
            .map_or(0, |cs| cs.iter().filter(|c| c.label == "m").count());
        assert_eq!(count, 1);
    }

    #[test]
    fn imported_defs_do_not_leak_styles() {
        // Imported defs' spans refer to the source file's token indices.
        // They must not pollute the current file's token styling.
        let r = analyze("import \"sys\"\nstep: f32x2 → f32x2");
        // No token should be marked as Namespace (f32x2 is a 0-cell, not a module prefix)
        let namespace_tokens: Vec<_> = r
            .tokens
            .iter()
            .filter(|t| matches!(t.token_type, TokenType::Namespace))
            .collect();
        assert!(
            namespace_tokens.is_empty(),
            "no token should be Namespace, but found {} at line={} col={}",
            namespace_tokens.len(),
            namespace_tokens.first().map_or(0, |t| t.line),
            namespace_tokens.first().map_or(0, |t| t.column),
        );
    }

    #[test]
    fn param_ref_marked_as_parameter() {
        // Parameter `x` references in body should be marked as Parameter
        let r = analyze("C: *\nx = {\n  y[x: C → C] = x x\n}");
        // line 2: "  y[x: C → C] = x x"
        // param declaration at col 4
        let ty_decl = token_type_at(&r, 2, 4);
        assert!(
            matches!(ty_decl, Some(TokenType::Parameter)),
            "param declaration should be Parameter"
        );
        // first x in body at col 16
        let ty1 = token_type_at(&r, 2, 16);
        assert!(
            matches!(ty1, Some(TokenType::Parameter)),
            "first param ref should be Parameter, got {:?}",
            ty1
        );
        // second x in body at col 18
        let ty2 = token_type_at(&r, 2, 18);
        assert!(
            matches!(ty2, Some(TokenType::Parameter)),
            "second param ref should be Parameter, got {:?}",
            ty2
        );
    }

    #[test]
    fn namespace_marking_deep() {
        let r = analyze("a = {\n  b = {\n    c: *\n  }\n}\nx = a.b.c");
        let ty_a = token_type_at(&r, 5, 4);
        assert!(
            matches!(ty_a, Some(TokenType::Namespace)),
            "a should be Namespace"
        );
        let ty_b = token_type_at(&r, 5, 6);
        assert!(
            matches!(ty_b, Some(TokenType::Namespace)),
            "b should be Namespace"
        );
        let ty_c = token_type_at(&r, 5, 8);
        assert!(
            !matches!(ty_c, Some(TokenType::Namespace)),
            "c should not be Namespace"
        );
    }

    #[test]
    fn hover_with_clause() {
        let r = analyze("u: *\ncat = {\n  x: u → u\n} with {\n  m: x → x\n}");
        let m = find_hover(&r, "cat.m");
        assert!(m.is_some());
        assert_eq!(m.unwrap().entry.kind, EntryKind::Cell(2));
    }

    #[test]
    fn hover_where_clause() {
        let r = analyze("u: *\nx: u → u\nf: x → x\ng = h where {\n  h = f; f\n}");
        let names = hover_names(&r);
        eprintln!("hover names: {:?}", names);
        // h should have hover
        let h = find_hover(&r, "h");
        eprintln!("h hover: {:?}", h.map(|h| &h.name));
        // f reference in where body should have hover
        assert!(find_hover(&r, "f").is_some(), "f should have hover");
        // g should have hover
        assert!(find_hover(&r, "g").is_some(), "g should have hover");
    }

    #[test]
    fn param_in_where_clause() {
        let code = "import \"sys\"\nr[C: *, x: C → C, f: x → x]: x → x = g where {\n  g = f; f\n}";
        let r = analyze(code);
        let names = hover_names(&r);
        eprintln!("hover names: {:?}", names);
        eprintln!("diagnostics: {:?}", r.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>());
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
        assert!(!x.entry.params.is_empty());
    }

    #[test]
    fn dot_prefixes_present() {
        let r = analyze("m = {\n  a: *\n}\nx = m.a");
        assert!(!r.completion.dot_prefixes.is_empty());
        assert!(r.completion.dot_prefixes.values().any(|v| v == "m"));
    }
}
