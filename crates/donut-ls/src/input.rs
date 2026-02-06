use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum InputEntry {
    Many(String, u32),
    Symbol(String, String),
}

pub struct InputDict {
    global: Vec<InputEntry>,
    // each char_map entry contains all candidates not only starting with the key but also *including* the key.
    char_map: HashMap<char, Vec<InputEntry>>,
}

// Note: VSCode client doesn't retry completion on input symbols (except for underbar)...

impl InputDict {
    pub fn new() -> Self {
        let mut head_map = HashMap::new();
        let mut char_map = HashMap::new();
        include_str!("unicode-dict.txt")
            .lines()
            .skip(4) // comments
            .for_each(|line| {
                let space_ix = match line.find(' ') {
                    Some(s) => s,
                    None => return,
                };
                let key = &line[0..space_ix];
                let value = &line[space_ix + 1..];
                let key_with_backslash = format!("\\{}", key);
                assert!(!key.is_empty());
                for c in key.chars() {
                    char_map.entry(c).or_insert(vec![]).push(InputEntry::Symbol(
                        key_with_backslash.clone(),
                        value.to_string(),
                    ));
                }
                let head = key.chars().next().unwrap();
                head_map
                    .entry(head)
                    .or_insert(vec![])
                    .push(InputEntry::Symbol(
                        key_with_backslash.clone(),
                        value.to_string(),
                    ));
            });
        let global = head_map
            .iter()
            .flat_map(|(k, v)| {
                if v.len() == 1 {
                    vec![v[0].clone()]
                } else {
                    // workaround
                    if k.is_ascii_alphanumeric() || k == &'_' {
                        vec![InputEntry::Many(format!("\\{}", k), v.len() as u32)]
                    } else {
                        v.clone()
                    }
                }
            })
            .collect::<Vec<_>>();
        InputDict { global, char_map }
    }

    pub fn get_candidates(&self, input: &str) -> Vec<InputEntry> {
        if input.is_empty() {
            return self.global.clone();
        }
        let head = input.chars().next().unwrap();
        if let Some(cands) = self.char_map.get(&head) {
            cands.clone()
        } else {
            Vec::new()
        }
    }
}
