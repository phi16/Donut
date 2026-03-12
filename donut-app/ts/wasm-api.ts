// Type definitions for WASM exports

export interface TokenData {
  line: number;
  col: number;
  len: number;
  type: string;
  token_index: number | null;
}

export interface DiagnosticData {
  begin_line: number;
  begin_col: number;
  end_line: number;
  end_col: number;
  message: string;
  source: string;
}

export interface HoverInfo {
  token_index: number;
  name: string;
  signature: string;
  detail: string;
  markdown: string;
}

export interface CompletionCandidate {
  label: string;
  detail: string;
  kind: string;
  def_line: number;
  is_imported: boolean;
  is_module: boolean;
}

export interface CompletionData {
  scopes: Record<string, CompletionCandidate[]>;
  dot_prefixes: Record<string, string>;
}

export interface AnalysisResult {
  tokens: TokenData[];
  diagnostics: DiagnosticData[];
  hover: HoverInfo[];
  completion: CompletionData;
}

export interface EntryDesc {
  index: number;
  name: string;
  dimension: number;
  color: [number, number, number];
}

// WASM module interface — filled by dynamic import
export interface WasmModule {
  analyze(code: string): AnalysisResult;
  WasmEngine: {
    new(code: string): WasmEngine;
  };
}

export interface WasmEngine {
  free(): void;
  update_code(code: string): void;
  select_entry(index: number): void;
  root_entries(): EntryDesc[];
  eval_result(): string;
  is_evaluable(): boolean;
  diagnostics(): string[];
  compile_glsl(): string | undefined;
  compile_fragment_shader(): string | null;
  selected_index(): number | null;
}
