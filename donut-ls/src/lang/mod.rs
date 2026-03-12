// Re-export from donut-analysis
pub use donut_analysis::{
    analyze, AnalysisResult, CompletionCandidate, CompletionData, Diagnostic, EntryKind,
    HoverInfo, TokenData, TokenType,
};

// LSP-specific helper: UTF-8 → UTF-16 position conversion
pub fn to_utf16(lines: &[&str], line: usize, col: usize, len: usize) -> (u32, u32) {
    let mut chars = lines.get(line).unwrap().chars();
    let utf16_col = chars
        .by_ref()
        .take(col)
        .map(|c| c.len_utf16())
        .sum::<usize>() as u32;
    let utf16_len = chars.take(len).map(|c| c.len_utf16()).sum::<usize>() as u32;
    (utf16_col, utf16_len)
}

/// Wrap donut-analysis's UTF-8 analyze() with UTF-16 position conversion for LSP.
pub fn analyze_utf16(code: &str) -> AnalysisResult {
    let lines = code.lines().collect::<Vec<_>>();
    let mut result = analyze(code);

    // Convert all token positions from UTF-8 to UTF-16
    for td in &mut result.tokens {
        let (utf16_col, utf16_len) =
            to_utf16(&lines, td.line as usize, td.column as usize, td.length as usize);
        td.column = utf16_col;
        td.length = utf16_len;
    }

    // Convert diagnostic positions from UTF-8 to UTF-16
    for d in &mut result.diagnostics {
        let (begin_col_16, _) =
            to_utf16(&lines, d.begin_line as usize, d.begin_column as usize, 0);
        let (end_col_16, _) =
            to_utf16(&lines, d.end_line as usize, d.end_column as usize, 0);
        d.begin_column = begin_col_16;
        d.end_column = end_col_16;
    }

    result
}
