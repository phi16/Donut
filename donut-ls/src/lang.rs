use donut_lang::check::Env;
use donut_lang::types;
use donut_lang::types::common;
use donut_lang::types::item::*;
use donut_lang::types::syntree;
use std::collections::HashMap;

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

struct Context {
    tokens: Vec<TokenData>,
}

impl Context {
    fn new(tokens: Vec<TokenData>) -> Self {
        Self { tokens }
    }

    fn into_inner(self) -> Vec<TokenData> {
        self.tokens
    }

    fn mark_as(&mut self, index: usize, t: TokenType) {
        let d = match self.tokens.get_mut(index) {
            Some(token) => token,
            None => return,
        };
        d.token_type = t;
    }
    fn mark_elem_as<T>(&mut self, e: &common::A<T>, t: TokenType) {
        if let common::A::Accepted(_, span) = e {
            // TODO: end?
            self.mark_as(span.start, t);
        }
    }
}

trait Marking {
    fn mark(&self, x: &mut Context);
}

impl<T: Marking> Marking for common::A<T> {
    fn mark(&self, x: &mut Context) {
        if let common::A::Accepted(t, _) = self {
            t.mark(x);
        }
    }
}
impl<T: Marking> Marking for Vec<T> {
    fn mark(&self, x: &mut Context) {
        for t in self {
            t.mark(x);
        }
    }
}
impl<T: Marking> Marking for Option<T> {
    fn mark(&self, x: &mut Context) {
        if let Some(t) = self {
            t.mark(x);
        }
    }
}
impl<T: Marking, U: Marking> Marking for (T, U) {
    fn mark(&self, x: &mut Context) {
        self.0.mark(x);
        self.1.mark(x);
    }
}

impl Marking for syntree::Symbol {
    fn mark(&self, _: &mut Context) {}
}
impl Marking for syntree::Name {
    fn mark(&self, _: &mut Context) {}
}
impl Marking for syntree::ParamTy {
    fn mark(&self, _: &mut Context) {}
}
impl Marking for syntree::Param {
    fn mark(&self, x: &mut Context) {
        for name in &self.names {
            x.mark_elem_as(name, TokenType::Parameter);
        }
        self.ty.mark(x);
        self.val.mark(x);
    }
}
impl Marking for syntree::Params {
    fn mark(&self, x: &mut Context) {
        self.1.mark(x);
    }
}
impl Marking for syntree::Decorator {
    fn mark(&self, x: &mut Context) {
        x.mark_elem_as(&self.0, TokenType::Keyword);
        self.1.mark(x);
        x.mark_elem_as(&self.2, TokenType::Keyword);
    }
}
impl Marking for syntree::Segment {
    fn mark(&self, x: &mut Context) {
        self.0.mark(x);
        self.1.mark(x);
    }
}
impl Marking for syntree::Path {
    fn mark(&self, x: &mut Context) {
        self.0.mark(x);
        self.1.mark(x);
    }
}
impl Marking for syntree::Key {
    fn mark(&self, x: &mut Context) {
        match self {
            syntree::Key::Name(n) => n.mark(x),
            syntree::Key::String(_) => {}
        }
    }
}
impl Marking for syntree::Lit {
    fn mark(&self, x: &mut Context) {
        match self {
            syntree::Lit::Number(_) => {}
            syntree::Lit::String(_) => {}
            syntree::Lit::Array(vs) => vs.mark(x),
            syntree::Lit::Object(kvs) => kvs.mark(x),
        }
    }
}
impl Marking for syntree::Op {
    fn mark(&self, _: &mut Context) {}
}
impl Marking for syntree::Val0 {
    fn mark(&self, x: &mut Context) {
        match self {
            syntree::Val0::Path(p) => p.mark(x),
            syntree::Val0::Lit(l) => l.mark(x),
            syntree::Val0::Dots => {}
            syntree::Val0::Paren(v) => v.mark(x),
        }
    }
}
impl Marking for syntree::Val {
    fn mark(&self, x: &mut Context) {
        self.vs.mark(x);
        self.ops.mark(x);
    }
}
impl Marking for syntree::Module {
    fn mark(&self, x: &mut Context) {
        match self {
            syntree::Module::Block(ds) => ds.mark(x),
            syntree::Module::Import(l) => l.mark(x),
            syntree::Module::Use(l) => l.mark(x),
        }
    }
}
impl Marking for syntree::AssignOp {
    fn mark(&self, _: &mut Context) {}
}
impl Marking for syntree::ValMod {
    fn mark(&self, x: &mut Context) {
        match self {
            syntree::ValMod::Val(v) => v.mark(x),
            syntree::ValMod::Mod(m) => m.mark(x),
        }
    }
}
impl Marking for syntree::DeclUnit {
    fn mark(&self, x: &mut Context) {
        self.names.mark(x);
        self.ty.mark(x);
        self.assign.mark(x);
    }
}
impl Marking for syntree::ClauseTy {
    fn mark(&self, _: &mut Context) {}
}
impl Marking for syntree::Clause {
    fn mark(&self, x: &mut Context) {
        self.0.mark(x);
        self.1.mark(x);
    }
}
impl Marking for syntree::DeclMain {
    fn mark(&self, x: &mut Context) {
        match self {
            syntree::DeclMain::Unit(d) => d.mark(x),
            syntree::DeclMain::Mod(m) => m.mark(x),
            syntree::DeclMain::Dots => {}
        }
    }
}
impl Marking for syntree::Decl {
    fn mark(&self, x: &mut Context) {
        self.decos.mark(x);
        self.main.mark(x);
        self.clauses.mark(x);
    }
}
impl Marking for syntree::Program {
    fn mark(&self, x: &mut Context) {
        self.0.mark(x);
    }
}

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

// --- Hover map construction ---

struct HoverBuilder<'a> {
    program: &'a Program,
    env: &'a Env,
    map: HashMap<usize, HoverInfo>,
    styles: HashMap<usize, TokenType>,
}

impl<'a> HoverBuilder<'a> {
    fn new(program: &'a Program, env: &'a Env) -> Self {
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

    fn build(mut self) -> (HashMap<usize, HoverInfo>, HashMap<usize, TokenType>) {
        let root = self.program.root.clone();
        self.walk_module(&root, &[]);
        (self.map, self.styles)
    }
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

fn build_completion_data(
    program: &Program,
    env: &Env,
    tokens: &[types::token::Token],
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

fn entry_to_kind(entry: &donut_lang::check::Entry) -> CompletionKind {
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

// --- Dot completion prefix ---

/// Walk the syntree and collect all segment boundaries from paths.
/// Returns a map from token index (where a "." sits) to the prefix string.
/// Uses the syntree (before convert/resolve) so that ALL paths are included,
/// including incomplete declarations that convert would discard.
fn collect_dot_prefixes(syn: &syntree::Program) -> HashMap<usize, String> {
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

    fn run_parse(code: &str) -> (common::A<syntree::Program>, Vec<donut_lang::types::token::Token<'_>>) {
        let (tokens, _, _) = donut_lang::tokenize::tokenize(code);
        let (program, _) = donut_lang::parse::parse(&tokens);
        (program, tokens)
    }

    /// Find the token index of the last "." in the token list.
    fn last_dot_index(tokens: &[donut_lang::types::token::Token]) -> usize {
        tokens.iter().rposition(|t| t.str == ".").expect("no dot found")
    }

    /// Find the token index of the nth "." in the token list (0-indexed).
    fn nth_dot_index(tokens: &[donut_lang::types::token::Token], n: usize) -> usize {
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
