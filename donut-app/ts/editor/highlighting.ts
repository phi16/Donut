import { EditorView, Decoration, DecorationSet } from "@codemirror/view";
import { StateField, StateEffect } from "@codemirror/state";
import { RangeSetBuilder } from "@codemirror/state";
import type { TokenData } from "../wasm-api";

// Effect to set new tokens
export const setTokens = StateEffect.define<TokenData[]>();

const tokenClass: Record<string, string> = {
  unknown: "tok-unknown",
  keyword: "tok-keyword",
  operator: "tok-operator",
  symbol: "tok-symbol",
  number: "tok-number",
  string: "tok-string",
  comment: "tok-comment",
  parameter: "tok-parameter",
  namespace: "tok-namespace",
};

function buildDecorations(
  tokens: TokenData[],
  doc: { line(n: number): { from: number }; lines: number }
): DecorationSet {
  const builder = new RangeSetBuilder<Decoration>();
  const sorted = [...tokens].sort((a, b) => {
    if (a.line !== b.line) return a.line - b.line;
    return a.col - b.col;
  });

  for (const tok of sorted) {
    const lineNum = tok.line + 1; // CM lines are 1-based
    if (lineNum < 1 || lineNum > doc.lines) continue;
    const lineStart = doc.line(lineNum).from;
    const from = lineStart + tok.col;
    const to = from + tok.len;
    const cls = tokenClass[tok.type] || "tok-unknown";
    builder.add(from, to, Decoration.mark({ class: cls }));
  }

  return builder.finish();
}

export const highlightField = StateField.define<DecorationSet>({
  create() {
    return Decoration.none;
  },
  update(decorations, tr) {
    for (const effect of tr.effects) {
      if (effect.is(setTokens)) {
        return buildDecorations(effect.value, tr.state.doc);
      }
    }
    if (tr.docChanged) {
      // Map existing decorations through document changes
      return decorations.map(tr.changes);
    }
    return decorations;
  },
  provide: (f) => EditorView.decorations.from(f),
});
