use serde::Serialize;
use wasm_bindgen::prelude::*;

use crate::engine;

// --- analyze() API ---

#[derive(Serialize)]
pub struct JsTokenData {
    pub line: u32,
    pub col: u32,
    pub len: u32,
    #[serde(rename = "type")]
    pub token_type: String,
    pub token_index: Option<usize>,
}

#[derive(Serialize)]
pub struct JsDiagnostic {
    pub begin_line: u32,
    pub begin_col: u32,
    pub end_line: u32,
    pub end_col: u32,
    pub message: String,
    pub source: String,
}

#[derive(Serialize)]
pub struct JsHoverInfo {
    pub token_index: usize,
    pub name: String,
    pub signature: String,
    pub detail: String,
    pub markdown: String,
}

#[derive(Serialize)]
pub struct JsCompletionCandidate {
    pub label: String,
    pub detail: String,
    pub kind: String,
    pub def_line: u32,
    pub is_imported: bool,
    pub is_module: bool,
}

#[derive(Serialize)]
pub struct JsCompletionData {
    pub scopes: std::collections::HashMap<String, Vec<JsCompletionCandidate>>,
    pub dot_prefixes: std::collections::HashMap<String, String>,
}

#[derive(Serialize)]
pub struct JsAnalysisResult {
    pub tokens: Vec<JsTokenData>,
    pub diagnostics: Vec<JsDiagnostic>,
    pub hover: Vec<JsHoverInfo>,
    pub completion: JsCompletionData,
}

fn token_type_str(t: &donut_analysis::TokenType) -> &'static str {
    match t {
        donut_analysis::TokenType::Unknown => "unknown",
        donut_analysis::TokenType::Keyword => "keyword",
        donut_analysis::TokenType::Operator => "operator",
        donut_analysis::TokenType::Symbol => "symbol",
        donut_analysis::TokenType::Number => "number",
        donut_analysis::TokenType::String => "string",
        donut_analysis::TokenType::Comment => "comment",
        donut_analysis::TokenType::Parameter => "parameter",
        donut_analysis::TokenType::Namespace => "namespace",
    }
}

fn entry_kind_str(k: &donut_analysis::EntryKind) -> String {
    match k {
        donut_analysis::EntryKind::Cell(d) => format!("cell-{}", d),
        donut_analysis::EntryKind::Meta => "meta".to_string(),
        donut_analysis::EntryKind::Type => "type".to_string(),
    }
}

#[wasm_bindgen]
pub fn analyze(code: &str) -> JsValue {
    let result = donut_analysis::analyze(code);

    let tokens = result
        .tokens
        .iter()
        .map(|t| JsTokenData {
            line: t.line,
            col: t.column,
            len: t.length,
            token_type: token_type_str(&t.token_type).to_string(),
            token_index: t.token_index,
        })
        .collect();

    let diagnostics = result
        .diagnostics
        .iter()
        .map(|d| JsDiagnostic {
            begin_line: d.begin_line,
            begin_col: d.begin_column,
            end_line: d.end_line,
            end_col: d.end_column,
            message: d.message.clone(),
            source: d.source.to_string(),
        })
        .collect();

    let hover = result
        .hover_map
        .iter()
        .map(|(&idx, h)| JsHoverInfo {
            token_index: idx,
            name: h.name.clone(),
            signature: h.signature.clone(),
            detail: h.entry.display_detail(),
            markdown: h.display_markdown(),
        })
        .collect();

    let scopes = result
        .completion
        .scopes
        .iter()
        .map(|(scope, candidates)| {
            let js_candidates = candidates
                .iter()
                .map(|c| JsCompletionCandidate {
                    label: c.label.clone(),
                    detail: c.entry.display_completion_detail(),
                    kind: entry_kind_str(&c.entry.kind),
                    def_line: c.def_line,
                    is_imported: c.is_imported,
                    is_module: c.entry.is_module(),
                })
                .collect();
            (scope.clone(), js_candidates)
        })
        .collect();

    let js_result = JsAnalysisResult {
        tokens,
        diagnostics,
        hover,
        completion: JsCompletionData {
            scopes,
            dot_prefixes: result
                .completion
                .dot_prefixes
                .iter()
                .map(|(k, v)| (k.to_string(), v.clone()))
                .collect(),
        },
    };

    let serializer = serde_wasm_bindgen::Serializer::new().serialize_maps_as_objects(true);
    js_result.serialize(&serializer).unwrap()
}

// --- Engine API ---

#[wasm_bindgen]
pub struct WasmEngine {
    inner: engine::Engine,
}

#[wasm_bindgen]
impl WasmEngine {
    #[wasm_bindgen(constructor)]
    pub fn new(code: &str) -> Self {
        Self {
            inner: engine::Engine::new(code),
        }
    }

    pub fn update_code(&mut self, code: &str) {
        self.inner.update_code(code);
    }

    pub fn select_entry(&mut self, index: usize) {
        self.inner.select_entry(index);
    }

    pub fn root_entries(&self) -> JsValue {
        let descs = self.inner.root_entry_descs();
        serde_wasm_bindgen::to_value(&descs).unwrap()
    }

    pub fn eval_result(&self) -> String {
        self.inner.eval_result_text()
    }

    pub fn is_evaluable(&self) -> bool {
        self.inner.is_evaluable()
    }

    pub fn diagnostics(&self) -> JsValue {
        serde_wasm_bindgen::to_value(&self.inner.diagnostics).unwrap()
    }

    pub fn compile_glsl(&self) -> Option<String> {
        self.inner.compile_glsl()
    }

    pub fn compile_fragment_shader(&self) -> JsValue {
        match self.inner.compile_fragment_shader() {
            Some(Ok(src)) => JsValue::from_str(&src),
            _ => JsValue::NULL,
        }
    }

    pub fn selected_index(&self) -> JsValue {
        match self.inner.selected {
            Some(def_id) => JsValue::from_f64(def_id.0 as f64),
            None => JsValue::NULL,
        }
    }
}

// Also expose the engine for canvas rendering (used internally by the old App path)
impl WasmEngine {
    pub fn engine_mut(&mut self) -> &mut engine::Engine {
        &mut self.inner
    }

    pub fn engine(&self) -> &engine::Engine {
        &self.inner
    }
}
