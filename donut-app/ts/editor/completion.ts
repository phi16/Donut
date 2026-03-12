import {
  autocompletion,
  acceptCompletion,
  completionStatus,
  currentCompletions,
  type CompletionContext,
  type CompletionResult,
} from "@codemirror/autocomplete";
import { keymap, type EditorView } from "@codemirror/view";
import type { CompletionData } from "../wasm-api";
import dictText from "./unicode-dict.txt";

let currentCompletion: CompletionData = { scopes: {}, dot_prefixes: {} };

export function updateCompletionData(completion: CompletionData): void {
  currentCompletion = completion;
}

// --- Unicode dictionary ---

interface UnicodeEntry {
  key: string; // e.g. "alpha"
  symbol: string; // e.g. "α"
}

const unicodeDict: UnicodeEntry[] = [];

{
  const lines = dictText.split("\n");
  for (let i = 4; i < lines.length; i++) {
    const line = lines[i];
    const spaceIdx = line.indexOf(" ");
    if (spaceIdx <= 0) continue;
    const key = line.slice(0, spaceIdx).trim();
    const symbol = line.slice(spaceIdx + 1).trim();
    if (key && symbol) unicodeDict.push({ key, symbol });
  }
}

function unicodeCompletionSource(
  context: CompletionContext
): CompletionResult | null {
  const { state, pos } = context;
  const line = state.doc.lineAt(pos);
  const textBefore = line.text.slice(0, pos - line.from);

  // Match \something at the end
  const match = textBefore.match(/\\([^\s\\]*)$/);
  if (!match) return null;

  const typed = match[1]; // text after backslash
  const from = pos - match[0].length; // include the backslash

  if (typed.length === 0) return null;

  // Collect and rank candidates
  const exact: typeof options = [];
  const prefix: typeof options = [];
  const contains: typeof options = [];
  type Option = {
    label: string;
    displayLabel: string;
    apply: string;
    boost: number;
  };
  const options: Option[] = [];

  for (const entry of unicodeDict) {
    if (entry.key === typed) {
      exact.push(entry);
    } else if (entry.key.startsWith(typed)) {
      prefix.push(entry);
    } else if (entry.key.includes(typed)) {
      contains.push(entry);
    }
  }

  // Sort prefix and contains by key length (shorter = more relevant)
  prefix.sort((a, b) => a.key.length - b.key.length);
  contains.sort((a, b) => a.key.length - b.key.length);

  const ranked = [...exact, ...prefix, ...contains];
  for (let i = 0; i < ranked.length && i < 200; i++) {
    const entry = ranked[i];
    options.push({
      label: "\\" + entry.key,
      displayLabel: `${entry.symbol} \\${entry.key}`,
      apply: entry.symbol + " ",
      boost: -i,
    });
  }

  if (options.length === 0) return null;

  return {
    from,
    options,
    filter: false,
  };
}

// --- Donut language completion ---

function donutCompletionSource(
  context: CompletionContext
): CompletionResult | null {
  const { state, pos } = context;
  const line = state.doc.lineAt(pos);
  const textBefore = line.text.slice(0, pos - line.from);

  // Don't trigger language completion if we're in a backslash sequence
  if (textBefore.match(/\\[^\s\\]*$/)) return null;

  // Dot completion: check if cursor is right after a dot
  const dotMatch = textBefore.match(/(\w[\w.]*)\.$/);
  if (dotMatch) {
    const prefix = dotMatch[1];
    // Find the scope matching this prefix
    const scope = findDotScope(prefix);
    if (scope) {
      const candidates = currentCompletion.scopes[scope];
      if (candidates && candidates.length > 0) {
        return {
          from: pos,
          options: candidates.map((c) => ({
            label: c.label,
            detail: c.detail,
            type: completionType(c.kind, c.is_module),
          })),
        };
      }
    }
    return null;
  }

  // Word completion: find word being typed
  const wordMatch = textBefore.match(/(\w+)$/);
  if (!wordMatch && !context.explicit) return null;

  const from = wordMatch ? pos - wordMatch[1].length : pos;
  const cursorLine = state.doc.lineAt(pos).number - 1; // 0-based

  // Use top-level scope
  const candidates = currentCompletion.scopes[""];
  if (!candidates || candidates.length === 0) return null;

  return {
    from,
    options: candidates.map((c) => ({
      label: c.label,
      detail: c.detail,
      type: completionType(c.kind, c.is_module),
      boost: sortBoost(c, cursorLine),
    })),
  };
}

function findDotScope(prefix: string): string | null {
  // Direct scope lookup
  if (currentCompletion.scopes[prefix]) {
    return prefix;
  }
  // Also check via dot_prefixes in token data
  for (const [, scopeName] of Object.entries(
    currentCompletion.dot_prefixes
  )) {
    if (scopeName === prefix && currentCompletion.scopes[scopeName]) {
      return scopeName;
    }
  }
  return null;
}

function completionType(
  kind: string,
  isModule: boolean
): string | undefined {
  if (isModule) return "namespace";
  if (kind.startsWith("cell-")) return "variable";
  if (kind === "meta") return "constant";
  if (kind === "type") return "class";
  return "variable";
}

function sortBoost(
  c: { is_imported: boolean; def_line: number },
  cursorLine: number
): number {
  // Definitions after cursor get lower priority
  if (!c.is_imported && c.def_line > cursorLine) return -10;
  return 0;
}

export const donutCompletion = [
  autocompletion({
    override: [unicodeCompletionSource, donutCompletionSource],
    activateOnTyping: true,
  }),
  keymap.of([
    {
      key: " ",
      run: (view: EditorView) => {
        if (completionStatus(view.state) !== "active") return false;
        const completions = currentCompletions(view.state);
        if (completions.length > 0 && completions[0].label.startsWith("\\")) {
          return acceptCompletion(view);
        }
        return false;
      },
    },
  ]),
];
