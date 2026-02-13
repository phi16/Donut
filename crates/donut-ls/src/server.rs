use crate::doc::Doc;
use crate::lang::{tokenize_example, TokenType};
use colored::Colorize;
use lsp_server::{Connection, Message, Notification, Request, Response};
use lsp_types::*;
use std::sync::{Arc, Mutex};
use std::{collections::HashMap, error::Error};

struct TokenTypeSignature(pub u32);

impl TokenTypeSignature {
    pub const PARAMETER: Self = Self(0);
    pub const NAMESPACE: Self = Self(1);
    pub const VARIABLE: Self = Self(2);
    pub const PROPERTY: Self = Self(3);
    pub const MACRO: Self = Self(4);
    pub const KEYWORD: Self = Self(5);
    pub const COMMENT: Self = Self(6);
    pub const STRING: Self = Self(7);
    pub const NUMBER: Self = Self(8);
    pub const OPERATOR: Self = Self(9);
}

pub fn token_type_list() -> Vec<SemanticTokenType> {
    vec![
        SemanticTokenType::PARAMETER,
        SemanticTokenType::NAMESPACE,
        SemanticTokenType::VARIABLE,
        SemanticTokenType::PROPERTY,
        SemanticTokenType::MACRO,
        SemanticTokenType::KEYWORD,
        SemanticTokenType::COMMENT,
        SemanticTokenType::STRING,
        SemanticTokenType::NUMBER,
        SemanticTokenType::OPERATOR,
    ]
}

pub fn token_modifiers_list() -> Vec<SemanticTokenModifier> {
    vec![
        SemanticTokenModifier::DEFINITION,
        SemanticTokenModifier::DEFAULT_LIBRARY,
    ]
}

fn from_token_type(t: TokenType) -> Option<(u32, u32)> {
    type Sig = TokenTypeSignature;
    let res = match t {
        TokenType::Unknown => (Sig::VARIABLE, 0), // return None,
        TokenType::Keyword => (Sig::KEYWORD, 0),
        TokenType::Operator => (Sig::OPERATOR, 0),
        TokenType::Symbol => (Sig::OPERATOR, 0),
        TokenType::Number => (Sig::NUMBER, 0),
        TokenType::String => (Sig::STRING, 0),
        TokenType::Comment => (Sig::COMMENT, 0),
    };
    Some((res.0 .0, res.1))
}

fn semantic_tokens(
    doc: Arc<Mutex<Doc>>,
) -> Result<(Vec<SemanticToken>, Vec<crate::lang::Diagnostic>)> {
    let doc = doc.lock().unwrap();
    let contents = doc.to_string();
    let (tokens, diagnostics) = tokenize_example(&contents);
    let mut prev_line = 0;
    let mut prev_column = 0;
    let mut result = Vec::new();
    for t in tokens {
        if prev_line != t.line {
            prev_column = 0;
        }
        if let Some((token_type, token_modifiers_bitset)) = from_token_type(t.token_type) {
            result.push(SemanticToken {
                delta_line: t.line - prev_line,
                delta_start: t.column - prev_column,
                length: t.length,
                token_type,
                token_modifiers_bitset,
            });
            prev_line = t.line;
            prev_column = t.column;
        }
    }
    Ok((result, diagnostics))
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

    fn semantic_tokens_full(
        &mut self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;
        let doc = self.get_doc(&uri)?;
        let (result, diags) = match semantic_tokens(doc.clone()) {
            Ok((r, diags)) => (r, diags),
            Err(_) => (vec![], vec![]),
        };
        let result_id = self.fresh_result_id();
        {
            let mut doc = doc.lock().unwrap();
            doc.last_result_id = Some(result_id);
        }
        self.send_diagnostics(uri, diags)?;
        let result = SemanticTokensResult::Tokens(SemanticTokens {
            result_id: Some(result_id.to_string()),
            data: result,
        });
        Ok(Some(result))
    }
    fn semantic_tokens_full_delta(
        &mut self,
        params: SemanticTokensDeltaParams,
    ) -> Result<Option<SemanticTokensFullDeltaResult>> {
        let uri = params.text_document.uri;
        let doc = self.get_doc(&uri)?;
        let (result, diags) = match semantic_tokens(doc.clone()) {
            Ok((r, diags)) => (r, diags),
            Err(_) => (vec![], vec![]),
        };
        self.send_diagnostics(uri, diags)?;
        let result_id = self.fresh_result_id();
        {
            let mut doc = doc.lock().unwrap();
            doc.last_result_id = Some(result_id);
        }
        // TODO: TokensDelta
        let result = SemanticTokensFullDeltaResult::Tokens(SemanticTokens {
            result_id: Some(result_id.to_string()),
            data: result,
        });
        Ok(Some(result))
    }
    fn semantic_tokens_range(
        &mut self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        Ok(None)
    }

    fn completion(&mut self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let cursor_pos = params.text_document_position.position;
        let last_backslash = std::mem::take(&mut self.last_backslash);
        let backslash_pos = {
            let c = match &params.context {
                Some(c) => c,
                None => return Ok(None),
            };
            match &c.trigger_kind {
                &CompletionTriggerKind::TRIGGER_CHARACTER => match &c.trigger_character {
                    Some(s) if s == "\\" => Position {
                        line: cursor_pos.line,
                        character: cursor_pos.character - 1,
                    },
                    _ => return Ok(None),
                },
                &CompletionTriggerKind::TRIGGER_FOR_INCOMPLETE_COMPLETIONS => {
                    match last_backslash {
                        None => return Ok(None),
                        Some(last_backslash) => last_backslash,
                    }
                }
                _ => return Ok(None),
            }
        };
        let range = Range {
            start: backslash_pos,
            end: cursor_pos,
        };
        let doc = self.get_doc(&params.text_document_position.text_document.uri)?;
        let input = {
            let doc = doc.lock().unwrap();
            let s = doc.loc_utf16(
                backslash_pos.line as usize,
                backslash_pos.character as usize + 1, // without backslash
            );
            let e = doc.loc_utf16(cursor_pos.line as usize, cursor_pos.character as usize);
            doc.substr(s, e)
        };
        eprintln!("completion input = {:?}", input);

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
        let res = CompletionResponse::List(CompletionList {
            is_incomplete,
            items: cands,
        });
        if is_incomplete {
            self.last_backslash = Some(range.start);
        }
        Ok(Some(res))
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
