import { EditorState } from "@codemirror/state";
import { EditorView, keymap, lineNumbers } from "@codemirror/view";
import { defaultKeymap, history, historyKeymap, indentWithTab } from "@codemirror/commands";
import { bracketMatching, indentOnInput } from "@codemirror/language";
import { acceptCompletion, closeBrackets, closeBracketsKeymap } from "@codemirror/autocomplete";
import { lintGutter } from "@codemirror/lint";
import { donutTheme } from "./theme";
import { highlightField, setTokens } from "./highlighting";
import { applyDiagnostics } from "./diagnostics";
import { donutHoverTooltip, updateHoverIndex } from "./hover";
import { donutCompletion, updateCompletionData } from "./completion";
import type { AnalysisResult } from "../wasm-api";

export interface EditorHandle {
  view: EditorView;
  getCode(): string;
  setCode(code: string): void;
  applyAnalysis(result: AnalysisResult): void;
}

export function createEditor(
  container: HTMLElement,
  initialCode: string,
  onChange: (code: string) => void
): EditorHandle {
  const updateListener = EditorView.updateListener.of((update) => {
    if (update.docChanged) {
      onChange(update.state.doc.toString());
    }
  });

  const state = EditorState.create({
    doc: initialCode,
    extensions: [
      lineNumbers(),
      history(),
      indentOnInput(),
      bracketMatching(),
      closeBrackets(),
      keymap.of([
        { key: "Tab", run: acceptCompletion },
        indentWithTab,
        ...closeBracketsKeymap,
        ...defaultKeymap,
        ...historyKeymap,
      ]),
      donutTheme,
      highlightField,
      donutHoverTooltip,
      donutCompletion,
      lintGutter(),
      updateListener,
    ],
  });

  const view = new EditorView({
    state,
    parent: container,
  });

  return {
    view,
    getCode() {
      return view.state.doc.toString();
    },
    setCode(code: string) {
      view.dispatch({
        changes: {
          from: 0,
          to: view.state.doc.length,
          insert: code,
        },
      });
    },
    applyAnalysis(result: AnalysisResult) {
      // Apply semantic tokens
      view.dispatch({
        effects: setTokens.of(result.tokens),
      });

      // Apply diagnostics
      applyDiagnostics(view, result.diagnostics);

      // Update hover index
      updateHoverIndex(result.tokens, result.hover, view.state.doc);

      // Update completion data
      updateCompletionData(result.completion);
    },
  };
}
