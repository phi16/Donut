use lsp_server::Connection;
use lsp_types::*;
use std::error::Error;
mod doc;
mod input;
mod lang;
mod server;
use crate::server::{main_loop, token_modifiers_list, token_type_list};

fn main() -> Result<(), Box<dyn Error + Send + Sync>> {
    eprintln!("donut-ls started");
    let (connection, io_handles) = Connection::stdio();

    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        diagnostic_provider: Some(DiagnosticServerCapabilities::Options(DiagnosticOptions {
            identifier: None,
            inter_file_dependencies: false,
            workspace_diagnostics: false,
            work_done_progress_options: Default::default(),
        })),
        text_document_sync: Some(TextDocumentSyncCapability::Kind(
            TextDocumentSyncKind::INCREMENTAL,
        )),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        completion_provider: Some(CompletionOptions {
            resolve_provider: Some(false),
            trigger_characters: Some(vec!["\\".to_string()]),
            all_commit_characters: Some(vec![" ".to_string(), "\t".to_string(), "\n".to_string()]),
            work_done_progress_options: Default::default(),
            completion_item: None,
        }),
        position_encoding: Some(PositionEncodingKind::UTF16),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                legend: SemanticTokensLegend {
                    token_types: token_type_list(),
                    token_modifiers: token_modifiers_list(),
                },
                range: Some(true),
                full: Some(SemanticTokensFullOptions::Delta { delta: Some(true) }),
                work_done_progress_options: Default::default(),
            },
        )),
        ..Default::default()
    })
    .unwrap();
    let params = connection.initialize(server_capabilities)?;
    main_loop(connection, params)?;
    io_handles.join()?;
    eprintln!("donut-ls terminated");
    Ok(())
}
