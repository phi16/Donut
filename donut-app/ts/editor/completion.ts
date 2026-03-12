import {
  autocompletion,
  type CompletionContext,
  type CompletionResult,
} from "@codemirror/autocomplete";
import type { CompletionData } from "../wasm-api";

let currentCompletion: CompletionData = { scopes: {}, dot_prefixes: {} };

export function updateCompletionData(completion: CompletionData): void {
  currentCompletion = completion;
}

function donutCompletionSource(
  context: CompletionContext
): CompletionResult | null {
  const { state, pos } = context;
  const line = state.doc.lineAt(pos);
  const textBefore = line.text.slice(0, pos - line.from);

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

export const donutCompletion = autocompletion({
  override: [donutCompletionSource],
  activateOnTyping: true,
});
