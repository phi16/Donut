use std::collections::HashMap;

pub struct Doc {
    pub content: ropey::Rope,
    pub tokens: Vec<crate::lang::TokenData>,
    pub last_result_id: Option<u32>,
    pub hover_map: HashMap<usize, crate::lang::HoverInfo>,
}

impl Doc {
    pub fn new(text: String) -> Self {
        Doc {
            content: ropey::Rope::from(text),
            tokens: Vec::new(),
            last_result_id: None,
            hover_map: HashMap::new(),
        }
    }

    pub fn loc_utf16(&self, line: usize, chars: usize) -> usize {
        let l = self.content.line(line);
        self.content.line_to_char(line) + l.utf16_cu_to_char(chars)
    }

    pub fn replace(&mut self, start_ix: usize, end_ix: usize, text: String) {
        self.content.remove(start_ix..end_ix);
        self.content.insert(start_ix, text.as_str());
    }

    pub fn to_string(&self) -> String {
        self.content.to_string()
    }

    pub fn substr(&self, start_ix: usize, end_ix: usize) -> String {
        self.content.slice(start_ix..end_ix).to_string()
    }

    /// Find the token index at the given UTF-16 position.
    pub fn token_index_at(&self, line: u32, character: u32) -> Option<usize> {
        for t in &self.tokens {
            if t.line == line && t.column <= character && character < t.column + t.length {
                return t.token_index;
            }
        }
        None
    }
}
