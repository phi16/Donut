import { hoverTooltip, type Tooltip } from "@codemirror/view";
import type { TokenData, HoverInfo } from "../wasm-api";

interface HoverIndex {
  // Maps document offset ranges to hover info
  entries: { from: number; to: number; info: HoverInfo }[];
}

let currentHoverIndex: HoverIndex = { entries: [] };

export function updateHoverIndex(
  tokens: TokenData[],
  hoverInfos: HoverInfo[],
  doc: { line(n: number): { from: number }; lines: number }
): void {
  // Build a map from token_index to hover info
  const hoverMap = new Map<number, HoverInfo>();
  for (const h of hoverInfos) {
    hoverMap.set(h.token_index, h);
  }

  const entries: HoverIndex["entries"] = [];
  for (const tok of tokens) {
    if (tok.token_index == null) continue;
    const info = hoverMap.get(tok.token_index);
    if (!info) continue;

    const lineNum = tok.line + 1;
    if (lineNum < 1 || lineNum > doc.lines) continue;
    const lineStart = doc.line(lineNum).from;
    const from = lineStart + tok.col;
    const to = from + tok.len;
    entries.push({ from, to, info });
  }

  currentHoverIndex = { entries };
}

export const donutHoverTooltip = hoverTooltip(
  (view, pos): Tooltip | null => {
    for (const entry of currentHoverIndex.entries) {
      if (pos >= entry.from && pos <= entry.to) {
        return {
          pos: entry.from,
          end: entry.to,
          above: true,
          create() {
            const dom = document.createElement("div");
            dom.style.padding = "4px 8px";
            dom.style.fontFamily = "'Consolas', 'Monaco', monospace";
            dom.style.fontSize = "13px";
            dom.style.maxWidth = "500px";

            // Signature
            const sig = document.createElement("div");
            sig.style.color = "#e0e0e0";
            sig.style.whiteSpace = "pre-wrap";
            sig.textContent = entry.info.signature;
            dom.appendChild(sig);

            // Detail
            const detail = document.createElement("div");
            detail.style.color = "#888";
            detail.style.marginTop = "2px";
            detail.style.fontSize = "12px";
            detail.textContent = entry.info.detail;
            dom.appendChild(detail);

            return { dom };
          },
        };
      }
    }
    return null;
  },
  { hideOnChange: true }
);
