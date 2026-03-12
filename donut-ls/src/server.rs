use crate::doc::Doc;
use crate::lang::{analyze_utf16, CompletionCandidate, TokenType};
use colored::Colorize;
use lsp_server::{Connection, Message, Notification, Request, Response};
use lsp_types::*;
use std::sync::{Arc, Mutex};
use std::{collections::HashMap, error::Error};

/// SemanticTokenType の登録順。インデックスがそのまま LSP の token_type になる。
const SEMANTIC_TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::PARAMETER, // 0
    SemanticTokenType::NAMESPACE, // 1
    SemanticTokenType::VARIABLE,  // 2
    SemanticTokenType::PROPERTY,  // 3
    SemanticTokenType::MACRO,     // 4
    SemanticTokenType::KEYWORD,   // 5
    SemanticTokenType::COMMENT,   // 6
    SemanticTokenType::STRING,    // 7
    SemanticTokenType::NUMBER,    // 8
    SemanticTokenType::OPERATOR,  // 9
];

pub fn token_type_list() -> Vec<SemanticTokenType> {
    SEMANTIC_TOKEN_TYPES.to_vec()
}

pub fn token_modifiers_list() -> Vec<SemanticTokenModifier> {
    vec![
        SemanticTokenModifier::DEFINITION,
        SemanticTokenModifier::DEFAULT_LIBRARY,
    ]
}

fn from_token_type(t: &TokenType) -> (u32, u32) {
    let idx = match t {
        TokenType::Parameter => 0,
        TokenType::Namespace => 1,
        TokenType::Unknown => 2, // VARIABLE
        TokenType::Keyword => 5,
        TokenType::Comment => 6,
        TokenType::String => 7,
        TokenType::Number => 8,
        TokenType::Operator | TokenType::Symbol => 9,
    };
    (idx, 0)
}

/// 分析を実行し、結果を Doc にキャッシュ。診断を返す。
fn update_analysis(doc: &mut Doc) -> Vec<crate::lang::Diagnostic> {
    let contents = doc.to_string();
    let result = analyze_utf16(&contents);
    doc.hover_map = result.hover_map;
    doc.completion = result.completion;
    doc.tokens = result.tokens;
    result.diagnostics
}

/// Doc に格納済みのトークン列から LSP SemanticToken 列を構築。
fn build_semantic_tokens(doc: &Doc) -> Vec<SemanticToken> {
    let mut prev_line = 0;
    let mut prev_column = 0;
    doc.tokens
        .iter()
        .map(|t| {
            if prev_line != t.line {
                prev_column = 0;
            }
            let (token_type, token_modifiers_bitset) = from_token_type(&t.token_type);
            let st = SemanticToken {
                delta_line: t.line - prev_line,
                delta_start: t.column - prev_column,
                length: t.length,
                token_type,
                token_modifiers_bitset,
            };
            prev_line = t.line;
            prev_column = t.column;
            st
        })
        .collect()
}

struct Server<'a> {
    docs: HashMap<lsp_types::Uri, Arc<Mutex<Doc>>>,
    connection: &'a Connection,
    input_dict: crate::input::InputDict,
    last_backslash: Option<lsp_types::Position>,
    next_result_id: u32,
}

type Result<T> = core::result::Result<T, Box<dyn Error + Send + Sync>>;

impl<'a> Server<'a> {
    fn new(connection: &'a Connection) -> Self {
        Self {
            docs: HashMap::new(),
            connection,
            input_dict: crate::input::InputDict::new(),
            last_backslash: None,
            next_result_id: 0,
        }
    }

    fn get_doc(&self, uri: &lsp_types::Uri) -> Result<Arc<Mutex<Doc>>> {
        let doc = self
            .docs
            .get(uri)
            .ok_or(format!("Document not found: {:?}", uri.as_str()))?
            .clone();
        Ok(doc)
    }

    fn fresh_result_id(&mut self) -> u32 {
        let id = self.next_result_id;
        self.next_result_id += 1;
        id
    }

    fn process_request<R>(
        &mut self,
        handler: impl Fn(&mut Self, R::Params) -> Result<R::Result>,
        req: Request,
    ) -> Result<()>
    where
        R: lsp_types::request::Request,
        R::Params: serde::de::DeserializeOwned,
    {
        let (id, params) = req.extract(R::METHOD).unwrap();
        let result = handler(self, params)?;
        let result = serde_json::to_value(&result).unwrap();
        let resp = Response {
            id,
            result: Some(result),
            error: None,
        };
        self.connection.sender.send(Message::Response(resp))?;
        Ok(())
    }
    fn process_notification<N>(
        &mut self,
        handler: impl Fn(&mut Self, N::Params) -> Result<()>,
        not: Notification,
    ) -> Result<()>
    where
        N: lsp_types::notification::Notification,
        N::Params: serde::de::DeserializeOwned,
    {
        let params = not.extract(N::METHOD).unwrap();
        handler(self, params)
    }

    fn send_diagnostics(
        &mut self,
        uri: lsp_types::Uri,
        diags: Vec<crate::lang::Diagnostic>,
    ) -> Result<()> {
        let mut diagnostics = Vec::new();
        for d in diags {
            diagnostics.push(Diagnostic {
                range: Range {
                    start: Position {
                        line: d.begin_line,
                        character: d.begin_column,
                    },
                    end: Position {
                        line: d.end_line,
                        character: d.end_column,
                    },
                },
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some(d.source.to_string()),
                message: d.message,
                ..Default::default()
            });
        }
        let params = PublishDiagnosticsParams {
            uri,
            diagnostics,
            version: None,
        };
        self.publish_diagnostics(params)
    }

    fn run_and_publish(&mut self, uri: lsp_types::Uri) -> Result<SemanticTokens> {
        let result_id = self.fresh_result_id();
        let doc = self.get_doc(&uri)?;
        let (data, diags) = {
            let mut doc = doc.lock().unwrap();
            let diags = update_analysis(&mut doc);
            let data = build_semantic_tokens(&doc);
            doc.last_result_id = Some(result_id);
            (data, diags)
        };
        self.send_diagnostics(uri, diags)?;
        Ok(SemanticTokens {
            result_id: Some(result_id.to_string()),
            data,
        })
    }

    fn semantic_tokens_full(
        &mut self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let tokens = self.run_and_publish(params.text_document.uri)?;
        Ok(Some(SemanticTokensResult::Tokens(tokens)))
    }
    fn semantic_tokens_full_delta(
        &mut self,
        params: SemanticTokensDeltaParams,
    ) -> Result<Option<SemanticTokensFullDeltaResult>> {
        // TODO: TokensDelta
        let tokens = self.run_and_publish(params.text_document.uri)?;
        Ok(Some(SemanticTokensFullDeltaResult::Tokens(tokens)))
    }
    fn semantic_tokens_range(
        &mut self,
        _params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        Ok(None)
    }

    fn hover(&mut self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let doc = self.get_doc(&uri)?;
        let doc = doc.lock().unwrap();
        let hover = doc
            .token_index_at(pos.line, pos.character)
            .and_then(|idx| doc.hover_map.get(&idx))
            .map(|info| Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: info.display_markdown(),
                }),
                range: None,
            });
        Ok(hover)
    }

    fn completion(&mut self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let cursor_pos = params.text_document_position.position;
        let trigger = params.context.as_ref().and_then(|c| {
            if c.trigger_kind == CompletionTriggerKind::TRIGGER_CHARACTER {
                c.trigger_character.clone()
            } else if c.trigger_kind == CompletionTriggerKind::TRIGGER_FOR_INCOMPLETE_COMPLETIONS {
                Some("\\continue".to_string())
            } else {
                None
            }
        });

        match trigger.as_deref() {
            Some("\\") => self.completion_unicode(cursor_pos, &params),
            Some("\\continue") => {
                let last_backslash = std::mem::take(&mut self.last_backslash);
                match last_backslash {
                    Some(pos) => self.completion_unicode_continue(cursor_pos, pos, &params),
                    None => Ok(None),
                }
            }
            Some(".") => self.completion_dot(cursor_pos, &params),
            _ => self.completion_invoked(cursor_pos, &params),
        }
    }

    fn completion_unicode(
        &mut self,
        cursor_pos: Position,
        params: &CompletionParams,
    ) -> Result<Option<CompletionResponse>> {
        let backslash_pos = Position {
            line: cursor_pos.line,
            character: cursor_pos.character.saturating_sub(1),
        };
        self.completion_unicode_continue(cursor_pos, backslash_pos, params)
    }

    fn completion_unicode_continue(
        &mut self,
        cursor_pos: Position,
        backslash_pos: Position,
        params: &CompletionParams,
    ) -> Result<Option<CompletionResponse>> {
        let range = Range {
            start: backslash_pos,
            end: cursor_pos,
        };
        let doc = self.get_doc(&params.text_document_position.text_document.uri)?;
        let input = {
            let doc = doc.lock().unwrap();
            let s = doc.loc_utf16(
                backslash_pos.line as usize,
                backslash_pos.character as usize + 1,
            );
            let e = doc.loc_utf16(cursor_pos.line as usize, cursor_pos.character as usize);
            doc.substr(s, e)
        };

        use crate::input::InputEntry;
        let mut is_incomplete = false;
        let cands = self
            .input_dict
            .get_candidates(&input)
            .iter()
            .map(|entry| match entry {
                InputEntry::Many(label, count) => {
                    is_incomplete = true;
                    CompletionItem {
                        label: label.clone(),
                        label_details: Some(CompletionItemLabelDetails {
                            detail: Some("...".to_string()),
                            description: Some(format!("[{}]", count)),
                        }),
                        text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                            range: range.clone(),
                            new_text: label.clone(),
                        })),
                        ..Default::default()
                    }
                }
                InputEntry::Symbol(label, new_text) => CompletionItem {
                    label: label.clone(),
                    label_details: Some(CompletionItemLabelDetails {
                        detail: None,
                        description: Some(new_text.to_string()),
                    }),
                    text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                        range: range.clone(),
                        new_text: new_text.clone(),
                    })),
                    ..Default::default()
                },
            })
            .collect();
        if is_incomplete {
            self.last_backslash = Some(range.start);
        }
        Ok(Some(CompletionResponse::List(CompletionList {
            is_incomplete,
            items: cands,
        })))
    }

    fn completion_dot(
        &mut self,
        cursor_pos: Position,
        params: &CompletionParams,
    ) -> Result<Option<CompletionResponse>> {
        let doc = self.get_doc(&params.text_document_position.text_document.uri)?;
        let mut doc = doc.lock().unwrap();
        update_analysis(&mut doc);

        // Find the dot token at cursor - 1 (cursor is after the dot)
        let dot_col = cursor_pos.character.saturating_sub(1);
        let scope = doc
            .token_index_at(cursor_pos.line, dot_col)
            .and_then(|idx| doc.completion.dot_prefixes.get(&idx).map(|s| s.as_str()));

        match scope {
            Some(scope) => Ok(build_completion_list(&doc, scope, cursor_pos.line)),
            None => Ok(None),
        }
    }

    fn completion_invoked(
        &mut self,
        cursor_pos: Position,
        params: &CompletionParams,
    ) -> Result<Option<CompletionResponse>> {
        let doc = self.get_doc(&params.text_document_position.text_document.uri)?;
        let mut doc = doc.lock().unwrap();
        update_analysis(&mut doc);
        Ok(build_completion_list(&doc, "", cursor_pos.line))
    }

    fn publish_diagnostics(&mut self, params: PublishDiagnosticsParams) -> Result<()> {
        self.connection
            .sender
            .send(Message::Notification(Notification {
                method: method_of_n::<notification::PublishDiagnostics>().to_string(),
                params: serde_json::to_value(&params).unwrap(),
            }))?;
        Ok(())
    }

    fn did_open(&mut self, params: DidOpenTextDocumentParams) -> Result<()> {
        let uri = params.text_document.uri;
        let doc = Doc::new(params.text_document.text);
        self.docs.insert(uri.clone(), Arc::new(Mutex::new(doc)));
        Ok(())
    }
    fn did_close(&mut self, params: DidCloseTextDocumentParams) -> Result<()> {
        let uri = params.text_document.uri;
        self.docs.remove(&uri);
        Ok(())
    }
    fn did_change(&mut self, params: DidChangeTextDocumentParams) -> Result<()> {
        let uri = params.text_document.uri;
        let doc = self.get_doc(&uri)?;
        let mut doc = doc.lock().unwrap();
        for change in params.content_changes {
            match change.range {
                Some(range) => {
                    let s =
                        doc.loc_utf16(range.start.line as usize, range.start.character as usize);
                    let e = doc.loc_utf16(range.end.line as usize, range.end.character as usize);
                    doc.replace(s, e, change.text);
                }
                None => {
                    *doc = Doc::new(change.text);
                }
            }
        }
        Ok(())
    }
}

pub fn main_loop(connection: Connection, _params: serde_json::Value) -> Result<()> {
    eprintln!("params = {:?}", _params);

    let mut server = Server::new(&connection);
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                eprintln!("{}", format!("[Request] {:?}", req).black());
                let method = req.method.as_str();
                if method == method_of_req::<request::SemanticTokensFullRequest>() {
                    server.process_request::<request::SemanticTokensFullRequest>(
                        Server::semantic_tokens_full,
                        req,
                    )?;
                } else if method == method_of_req::<request::SemanticTokensFullDeltaRequest>() {
                    server.process_request::<request::SemanticTokensFullDeltaRequest>(
                        Server::semantic_tokens_full_delta,
                        req,
                    )?;
                } else if method == method_of_req::<request::SemanticTokensRangeRequest>() {
                    server.process_request::<request::SemanticTokensRangeRequest>(
                        Server::semantic_tokens_range,
                        req,
                    )?;
                } else if method == method_of_req::<request::HoverRequest>() {
                    server.process_request::<request::HoverRequest>(Server::hover, req)?;
                } else if method == method_of_req::<request::Completion>() {
                    server.process_request::<request::Completion>(Server::completion, req)?;
                } else {
                    eprintln!("{}", format!("- Unhandled: {}", method).yellow());
                }
            }
            Message::Response(resp) => {
                eprintln!("{}", format!("[Response] {:?}", resp).black());
            }
            Message::Notification(not) => {
                eprintln!("{}", format!("[Notification] {:?}", not).black());
                let method = not.method.as_str();
                if method == method_of_n::<notification::DidOpenTextDocument>() {
                    server.process_notification::<notification::DidOpenTextDocument>(
                        Server::did_open,
                        not,
                    )?;
                } else if method == method_of_n::<notification::DidChangeTextDocument>() {
                    server.process_notification::<notification::DidChangeTextDocument>(
                        Server::did_change,
                        not,
                    )?;
                } else if method == method_of_n::<notification::DidCloseTextDocument>() {
                    server.process_notification::<notification::DidCloseTextDocument>(
                        Server::did_close,
                        not,
                    )?;
                } else {
                    eprintln!("{}", format!("- Unhandled: {}", method).yellow());
                }
            }
        }
    }
    Ok(())
}

fn method_of_req<R>() -> &'static str
where
    R: lsp_types::request::Request,
{
    R::METHOD
}

fn method_of_n<N>() -> &'static str
where
    N: lsp_types::notification::Notification,
{
    N::METHOD
}

fn build_completion_list(doc: &Doc, scope: &str, cursor_line: u32) -> Option<CompletionResponse> {
    let candidates = doc.completion.scopes.get(scope)?;
    let items = candidates
        .iter()
        .map(|c| candidate_to_item(c, cursor_line))
        .collect();
    Some(CompletionResponse::List(CompletionList {
        is_incomplete: false,
        items,
    }))
}

fn candidate_to_item(c: &CompletionCandidate, cursor_line: u32) -> CompletionItem {
    use crate::lang::EntryKind;

    let lsp_kind = if c.entry.is_module() {
        Some(CompletionItemKind::MODULE)
    } else {
        match c.entry.kind {
            EntryKind::Cell(_) => Some(CompletionItemKind::VARIABLE),
            EntryKind::Meta => Some(CompletionItemKind::CONSTANT),
            EntryKind::Type => Some(CompletionItemKind::CLASS),
            EntryKind::Functor => Some(CompletionItemKind::FUNCTION),
        }
    };

    let detail = c.entry.display_completion_detail();

    let defined_after = !c.is_imported && c.def_line > cursor_line;
    let tags = if defined_after {
        Some(vec![CompletionItemTag::DEPRECATED])
    } else {
        None
    };

    CompletionItem {
        label: c.label.clone(),
        kind: lsp_kind,
        detail: Some(detail),
        tags,
        sort_text: Some(if defined_after {
            format!("1_{}", c.label)
        } else {
            format!("0_{}", c.label)
        }),
        ..Default::default()
    }
}
