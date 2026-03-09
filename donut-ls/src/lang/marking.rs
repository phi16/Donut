use donut_lang::types::common;
use donut_lang::types::syntree;
use std::collections::HashMap;

use super::TokenType;

pub(super) struct Context {
    tokens: Vec<super::TokenData>,
    dot_prefixes: HashMap<usize, String>,
}

impl Context {
    pub fn new(tokens: Vec<super::TokenData>) -> Self {
        Self {
            tokens,
            dot_prefixes: HashMap::new(),
        }
    }

    pub fn into_parts(self) -> (Vec<super::TokenData>, HashMap<usize, String>) {
        (self.tokens, self.dot_prefixes)
    }

    fn mark_as(&mut self, index: usize, t: TokenType) {
        let d = match self.tokens.get_mut(index) {
            Some(token) => token,
            None => return,
        };
        d.token_type = t;
    }
    fn mark_elem_as<T>(&mut self, e: &common::A<T>, t: TokenType) {
        if let common::A::Accepted(_, span) = e {
            self.mark_as(span.start, t);
        }
    }

    /// Record dot prefix entries for each segment boundary in a path.
    /// For segments [A, B, C], records:
    ///   span_A.end → "A"
    ///   span_B.end → "A.B"
    ///   span_C.end → "A.B.C"
    fn record_dot_prefixes(&mut self, segments: &[common::A<syntree::Segment>]) {
        let mut names = Vec::new();
        for seg_a in segments {
            if let common::A::Accepted(seg, span) = seg_a {
                if let common::A::Accepted(name, _) = &seg.0 {
                    names.push(name.0.as_str());
                    let prefix = names.join(".");
                    self.dot_prefixes.insert(span.end, prefix);
                }
            }
        }
    }
}

pub(super) trait Marking {
    fn mark(&self, x: &mut Context);
}

impl<T: Marking> Marking for common::A<T> {
    fn mark(&self, x: &mut Context) {
        if let common::A::Accepted(t, _) = self {
            t.mark(x);
        }
    }
}
impl<T: Marking> Marking for Vec<T> {
    fn mark(&self, x: &mut Context) {
        for t in self {
            t.mark(x);
        }
    }
}
impl<T: Marking> Marking for Option<T> {
    fn mark(&self, x: &mut Context) {
        if let Some(t) = self {
            t.mark(x);
        }
    }
}
impl<T: Marking, U: Marking> Marking for (T, U) {
    fn mark(&self, x: &mut Context) {
        self.0.mark(x);
        self.1.mark(x);
    }
}

impl Marking for syntree::Symbol {
    fn mark(&self, _: &mut Context) {}
}
impl Marking for syntree::Name {
    fn mark(&self, _: &mut Context) {}
}
impl Marking for syntree::ParamTy {
    fn mark(&self, _: &mut Context) {}
}
impl Marking for syntree::Param {
    fn mark(&self, x: &mut Context) {
        for name in &self.names {
            x.mark_elem_as(name, TokenType::Parameter);
        }
        self.ty.mark(x);
        self.val.mark(x);
    }
}
impl Marking for syntree::Params {
    fn mark(&self, x: &mut Context) {
        self.1.mark(x);
    }
}
impl Marking for syntree::Decorator {
    fn mark(&self, x: &mut Context) {
        x.mark_elem_as(&self.0, TokenType::Keyword);
        self.1.mark(x);
        x.mark_elem_as(&self.2, TokenType::Keyword);
    }
}
impl Marking for syntree::Segment {
    fn mark(&self, x: &mut Context) {
        self.0.mark(x);
        self.1.mark(x);
    }
}
impl Marking for syntree::Path {
    fn mark(&self, x: &mut Context) {
        x.record_dot_prefixes(&self.0);
        self.0.mark(x);
        self.1.mark(x);
    }
}
impl Marking for syntree::Key {
    fn mark(&self, x: &mut Context) {
        match self {
            syntree::Key::Name(n) => n.mark(x),
            syntree::Key::String(_) => {}
        }
    }
}
impl Marking for syntree::Lit {
    fn mark(&self, x: &mut Context) {
        match self {
            syntree::Lit::Number(_) => {}
            syntree::Lit::String(_) => {}
            syntree::Lit::Array(vs) => vs.mark(x),
            syntree::Lit::Object(kvs) => kvs.mark(x),
        }
    }
}
impl Marking for syntree::Op {
    fn mark(&self, _: &mut Context) {}
}
impl Marking for syntree::Val0 {
    fn mark(&self, x: &mut Context) {
        match self {
            syntree::Val0::Path(p) => p.mark(x),
            syntree::Val0::Lit(l) => l.mark(x),
            syntree::Val0::Dots => {}
            syntree::Val0::Paren(v) => v.mark(x),
        }
    }
}
impl Marking for syntree::Val {
    fn mark(&self, x: &mut Context) {
        self.vs.mark(x);
        self.ops.mark(x);
    }
}
impl Marking for syntree::Module {
    fn mark(&self, x: &mut Context) {
        match self {
            syntree::Module::Block(ds) => ds.mark(x),
            syntree::Module::Import(l) => l.mark(x),
            syntree::Module::Use(l) => l.mark(x),
        }
    }
}
impl Marking for syntree::AssignOp {
    fn mark(&self, _: &mut Context) {}
}
impl Marking for syntree::ValMod {
    fn mark(&self, x: &mut Context) {
        match self {
            syntree::ValMod::Val(v) => v.mark(x),
            syntree::ValMod::Mod(m) => m.mark(x),
        }
    }
}
impl Marking for syntree::DeclUnit {
    fn mark(&self, x: &mut Context) {
        // Declaration name paths also need dot prefix recording
        for name_path_a in &self.names {
            if let common::A::Accepted(path, _) = name_path_a {
                x.record_dot_prefixes(&path.0);
            }
        }
        self.names.mark(x);
        self.ty.mark(x);
        self.assign.mark(x);
    }
}
impl Marking for syntree::ClauseTy {
    fn mark(&self, _: &mut Context) {}
}
impl Marking for syntree::Clause {
    fn mark(&self, x: &mut Context) {
        self.0.mark(x);
        self.1.mark(x);
    }
}
impl Marking for syntree::DeclMain {
    fn mark(&self, x: &mut Context) {
        match self {
            syntree::DeclMain::Unit(d) => d.mark(x),
            syntree::DeclMain::Mod(m) => m.mark(x),
            syntree::DeclMain::Dots => {}
        }
    }
}
impl Marking for syntree::Decl {
    fn mark(&self, x: &mut Context) {
        self.decos.mark(x);
        self.main.mark(x);
        self.clauses.mark(x);
    }
}
impl Marking for syntree::Program {
    fn mark(&self, x: &mut Context) {
        self.0.mark(x);
    }
}
