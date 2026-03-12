import { setDiagnostics, type Diagnostic } from "@codemirror/lint";
import type { EditorView } from "@codemirror/view";
import type { DiagnosticData } from "../wasm-api";

export function applyDiagnostics(
  view: EditorView,
  diags: DiagnosticData[]
): void {
  const doc = view.state.doc;
  const cmDiags: Diagnostic[] = [];

  for (const d of diags) {
    const fromLine = d.begin_line + 1;
    const toLine = d.end_line + 1;
    if (fromLine < 1 || fromLine > doc.lines) continue;
    if (toLine < 1 || toLine > doc.lines) continue;

    const fromLineObj = doc.line(fromLine);
    const toLineObj = doc.line(toLine);

    // Clamp to line end (not next line)
    const from = Math.min(fromLineObj.from + d.begin_col, fromLineObj.to);
    const to = Math.min(toLineObj.from + d.end_col, toLineObj.to);

    const clampedFrom = Math.min(from, doc.length);
    const clampedTo = Math.min(Math.max(to, clampedFrom), doc.length);

    cmDiags.push({
      from: clampedFrom,
      to: clampedTo,
      severity: "error",
      message: d.message,
      source: d.source,
    });
  }

  view.dispatch(setDiagnostics(view.state, cmDiags));
}
