import { EditorView } from "@codemirror/view";

export const donutTheme = EditorView.theme(
  {
    "&": {
      backgroundColor: "#2d2d2d",
      color: "#d4d4d4",
      fontFamily: "'Consolas', 'Monaco', monospace",
      fontSize: "14px",
      height: "100%",
    },
    ".cm-content": {
      caretColor: "#d4d4d4",
      padding: "10px 0",
    },
    ".cm-cursor, .cm-dropCursor": {
      borderLeftColor: "#d4d4d4",
    },
    "&.cm-focused .cm-selectionBackground, .cm-selectionBackground, .cm-content ::selection":
      {
        backgroundColor: "#264f78",
      },
    ".cm-panels": {
      backgroundColor: "#252526",
      color: "#d4d4d4",
    },
    ".cm-panels.cm-panels-top": {
      borderBottom: "1px solid #444",
    },
    ".cm-panels.cm-panels-bottom": {
      borderTop: "1px solid #444",
    },
    ".cm-activeLine": {
      backgroundColor: "#2a2d2e",
    },
    ".cm-gutters": {
      backgroundColor: "#2d2d2d",
      color: "#858585",
      borderRight: "1px solid #444",
    },
    ".cm-activeLineGutter": {
      backgroundColor: "#2a2d2e",
    },
    ".cm-tooltip": {
      backgroundColor: "#252526",
      color: "#d4d4d4",
      border: "1px solid #454545",
    },
    ".cm-tooltip-autocomplete": {
      "& > ul > li[aria-selected]": {
        backgroundColor: "#094771",
        color: "#ffffff",
      },
    },
    // Semantic token colors
    ".tok-unknown": { color: "#d4d4d4" },
    ".tok-keyword": { color: "#569cd6" },
    ".tok-operator": { color: "#d4d4d4" },
    ".tok-symbol": { color: "#d4d4d4" },
    ".tok-number": { color: "#b5cea8" },
    ".tok-string": { color: "#ce9178" },
    ".tok-comment": { color: "#6a9955" },
    ".tok-parameter": { color: "#9cdcfe" },
    ".tok-namespace": { color: "#4ec9b0" },
    // Diagnostics
    ".cm-lintRange-error": {
      backgroundImage: "none",
      textDecoration: "underline wavy #f44",
      textUnderlineOffset: "3px",
    },
    ".cm-diagnostic-error": {
      borderLeft: "3px solid #f44",
      paddingLeft: "8px",
      color: "#f88",
    },
  },
  { dark: true }
);
