use crate::types::token::*;
use crate::types::tree::*;

// num-lit = ...
// str-lit = ...
// name = ...
// param = (name+ (':' | '='))? val
// bracket = '[' (param %0 ',') ']'
// params = bracket
// segment = name params?
// path = (segment %1 '.') (## '(' val ')')?
// lit = num-lit
//     | str-lit
//     | '[' (val %0 ',') ']'
//     | '{' (((name | str-lit) ':' val) %0 ',') '}'
// val0 = path
//      | lit
//      | '...'
//      | '(' val ')'
// valN = val0
//      | valN (';'* | ';' ## num-lit | ';*') params? valN
// val = valN
//     | valN ('->' | '→' | '~' | '~>') params? valN
// assign-op = '=' | ':=' | '+='
// mod = '{' decls '}'
//     | 'import' lit
// decl-unit = path+ (':' val)? (assign-op (val | mod))?
// decl-main = decl-unit | mod
// clause = ('with' | 'where') mod
// decorator = bracket
// decl = decorator+ | decorator* decl-main clause*
// decls = decl*
// program = decls eoi

struct Tracker<'a> {
    tokens: &'a [Token<'a>],
    pos: usize,
    indent: usize,
    depth: usize,
    decl_head: bool,
    errors: Vec<Error>,
}

#[derive(Clone)]
struct NodeBase {
    start: usize,
    indent: usize,
    decl_head: bool,
}

impl<'a> Tracker<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> Self {
        Self {
            tokens,
            pos: 0,
            indent: 0,
            depth: 0,
            decl_head: true,
            errors: vec![],
        }
    }

    pub fn begin(&self) -> NodeBase {
        NodeBase {
            start: self.pos,
            indent: self.indent,
            decl_head: self.decl_head,
        }
    }

    pub fn add_error(&mut self, msg: &str) {
        let pos = if self.pos < self.tokens.len() {
            self.tokens[self.pos].pos.clone()
        } else if let Some(t) = self.tokens.last() {
            t.pos.clone()
        } else {
            TokenPos {
                line: 0,
                col: 0,
                len: 0,
            }
        };
        self.errors.push((pos, msg.to_string()));
    }

    fn add_error_after_prev(&mut self, msg: &str) {
        let pos = if self.pos > 0 {
            let t = &self.tokens[self.pos - 1];
            TokenPos {
                line: t.pos.line,
                col: t.pos.col + t.pos.len + 1,
                len: 0,
            }
        } else {
            TokenPos {
                line: 0,
                col: 0,
                len: 0,
            }
        };
        self.errors.push((pos, msg.to_string()));
    }

    fn expected(&mut self, what: &str) {
        self.add_error(&format!("expected {what}"));
    }
    fn expected_after(&mut self, what: &str, after: &str) {
        self.add_error_after_prev(&format!("expected {what} after {after}"));
    }
    fn expected_in(&mut self, what: &str, context: &str) {
        self.add_error_after_prev(&format!("expected {what} in {context}"));
    }
    fn expected_at_end_of(&mut self, what: &str, context: &str) {
        self.add_error_after_prev(&format!("expected {what} at end of {context}"));
    }
    fn unexpected(&mut self, s: &str) {
        self.add_error(&format!("unexpected '{s}'"));
    }

    pub fn end<T>(&self, base: NodeBase, t: T) -> A<T> {
        let start = base.start;
        let end = self.pos;
        let span = TokenSpan { start, end };
        A::Accepted(t, span)
    }

    pub fn rollback(&mut self, base: NodeBase) {
        self.pos = base.start;
        self.indent = base.indent;
        self.decl_head = base.decl_head;
    }

    pub fn peek(&self) -> Option<&'a Token<'a>> {
        let t = self.tokens.get(self.pos)?;
        if self.decl_head || self.indent < t.pos.col {
            return Some(t);
        }
        None
    }
    pub fn next(&mut self) {
        assert!(self.pos < self.tokens.len());
        self.pos += 1;
        self.decl_head = false;
    }

    pub fn eat(&mut self, pred: impl Fn(&Token<'a>) -> bool) -> Option<&'a Token<'a>> {
        let t = self.peek()?;
        if pred(t) {
            self.next();
            Some(t)
        } else {
            None
        }
    }
    pub fn eat_close(&mut self, pred: impl Fn(&Token<'a>) -> bool) -> Option<&'a Token<'a>> {
        let t = self.tokens.get(self.pos)?;
        if self.decl_head || self.indent <= t.pos.col {
            if pred(t) {
                self.next();
                return Some(t);
            }
        }
        None
    }

    pub fn eoi(&self) -> bool {
        self.tokens.get(self.pos).is_none()
    }

    pub fn last_indent(&self) -> usize {
        assert!(self.pos > 0 && self.pos <= self.tokens.len());
        self.tokens[self.pos - 1].indent
    }

    pub fn symbol(&mut self, s: &'static str) -> Option<A<Symbol>> {
        let u = self.begin();
        self.eat(|t| t.ty == TokenTy::Symbol && t.str == s)?;
        Some(self.end(u, Symbol(s)))
    }
    pub fn symbol_connected(&mut self, s: &'static str) -> Option<A<Symbol>> {
        let u = self.begin();
        self.eat(|t| t.ty == TokenTy::Symbol && t.connected && t.str == s)?;
        Some(self.end(u, Symbol(s)))
    }
    pub fn symbol_close(&mut self, s: &'static str) -> Option<A<Symbol>> {
        let u = self.begin();
        self.eat_close(|t| t.ty == TokenTy::Symbol && t.str == s)?;
        Some(self.end(u, Symbol(s)))
    }
    pub fn operator(&mut self, s: &'static str) -> Option<&'a Token<'a>> {
        self.eat(|t| t.ty == TokenTy::Operator && t.str == s)
    }
    pub fn keyword(&mut self, s: &'static str) -> Option<&'a Token<'a>> {
        self.eat(|t| t.ty == TokenTy::Keyword && t.str == s)
    }

    fn separated<T>(
        &mut self,
        name: &str,
        open: &'static str,
        close: &'static str,
        mut parse_item: impl FnMut(&mut Self) -> Option<T>,
    ) -> Option<(A<Symbol>, Vec<T>, A<Symbol>)> {
        let open = self.symbol(open)?;
        if let Some(close) = self.symbol_close(close) {
            return Some((open, vec![], close));
        }
        let mut items = vec![];
        let close = loop {
            let Some(item) = parse_item(self) else {
                self.expected(name);
                break A::Error();
            };
            items.push(item);
            if self.symbol(",").is_some() {
                if let Some(close) = self.symbol_close(close) {
                    break close;
                }
                continue;
            } else if let Some(close) = self.symbol_close(close) {
                break close;
            } else {
                self.expected(&format!("',' or '{close}'"));
                // error recovery: skip garbage until closing bracket
                let recovered = loop {
                    if let Some(c) = self.symbol_close(close) {
                        break Some(c);
                    } else if self.peek().is_some() {
                        self.next();
                    } else {
                        break None;
                    }
                };
                break recovered.unwrap_or_else(A::Error);
            }
        };
        Some((open, items, close))
    }

    pub fn name(&mut self) -> Option<A<Name>> {
        let u = self.begin();
        let t = self.peek()?;
        if t.ty == TokenTy::Name {
            let name = Name(t.str.to_string());
            self.next();
            return Some(self.end(u, name));
        }
        None
    }
    pub fn param(&mut self) -> Option<A<Param>> {
        let u = self.begin();

        let u2 = self.begin();
        let mut names = vec![];
        while let Some(n) = self.name() {
            names.push(n);
        }
        let mut ty = None;
        if self.symbol(":").is_some() {
            ty = Some(ParamTy::Decl);
        } else if self.operator("=").is_some() {
            ty = Some(ParamTy::Named);
        }
        let val = match &ty {
            Some(ty) => self.val().unwrap_or_else(|| {
                let after = match ty {
                    ParamTy::Decl => "':'",
                    ParamTy::Named => "'='",
                };
                self.expected_after("value", after);
                A::Error()
            }),
            None => {
                self.rollback(u2);
                names.clear();

                self.val()?
            }
        };
        let param = Param { names, ty, val };
        Some(self.end(u, param))
    }

    fn bracket(&mut self) -> Option<(A<Symbol>, Vec<A<Param>>, A<Symbol>)> {
        let (open, items, close) = self.separated("parameter", "[", "]", |s| s.param())?;
        Some((open, items, close))
    }

    pub fn params(&mut self) -> Option<A<Params>> {
        let u = self.begin();
        let (open, items, close) = self.bracket()?;
        Some(self.end(u, Params(open, items, close)))
    }

    pub fn decorator(&mut self) -> Option<A<Decorator>> {
        let u = self.begin();
        let (open, items, close) = self.bracket()?;
        Some(self.end(u, Decorator(open, items, close)))
    }

    pub fn segment(&mut self) -> Option<A<Segment>> {
        let u = self.begin();

        let name = self.name()?;
        let args = self.params();

        let app_name = Segment(name, args);
        Some(self.end(u, app_name))
    }

    pub fn path(&mut self) -> Option<A<Path>> {
        let u = self.begin();

        let mut names = vec![];
        names.push(self.segment()?);
        while self.operator(".").is_some() {
            let n = self.segment().unwrap_or_else(|| {
                self.expected_after("name", "'.'");
                A::Error()
            });
            names.push(n);
        }

        let val = if self.symbol_connected("(").is_some() {
            let v = self.val().unwrap_or_else(|| {
                self.expected_in("value", "functor application");
                A::Error()
            });
            if self.symbol_close(")").is_some() {
                // pass
            } else {
                self.expected_at_end_of("')'", "functor application");
            }
            Some(v)
        } else {
            None
        };

        let path = Path(names, val);
        Some(self.end(u, path))
    }

    pub fn key(&mut self) -> Option<A<Key>> {
        let u = self.begin();
        if let Some(name) = self.name() {
            Some(self.end(u, Key::Name(name)))
        } else {
            let u2 = self.begin();
            let t = self.eat(|t| t.ty == TokenTy::String)?;
            let s = self.end(u2, t.str.to_string());
            Some(self.end(u, Key::String(s)))
        }
    }

    pub fn key_val(&mut self) -> Option<(A<Key>, A<Val>)> {
        let key = self.key()?;
        let val = if self.symbol(":").is_some() {
            self.val().unwrap_or_else(|| {
                self.expected_after("value", "':'");
                A::Error()
            })
        } else {
            self.expected_after("':'", "key");
            A::Error()
        };
        Some((key, val))
    }

    pub fn lit(&mut self) -> Option<A<Lit>> {
        let u = self.begin();
        let lit = if let Some(t) = self.eat(|t| t.ty == TokenTy::Number) {
            Lit::Number(t.str.to_string())
        } else if let Some(t) = self.eat(|t| t.ty == TokenTy::String) {
            Lit::String(t.str.to_string())
        } else if self.peek().is_some_and(|t| t.str == "[") {
            // array
            let vs = self.separated("value", "[", "]", |s| s.val());
            Lit::Array(vs.unwrap().1)
        } else if self.peek().is_some_and(|t| t.str == "{") {
            // object
            let ps = self.separated("key-value pair", "{", "}", |s| s.key_val());
            Lit::Object(ps.unwrap().1)
        } else {
            return None;
        };
        Some(self.end(u, lit))
    }

    pub fn op(&mut self) -> Option<A<Op>> {
        let u = self.begin();
        let t = self.eat(|t| t.ty == TokenTy::Operator)?;
        let op = if t.str == "->" || t.str == "→" {
            Op::Arrow(ArrowTy::To)
        } else if t.str == "~" {
            Op::Arrow(ArrowTy::Eq)
        } else if t.str == "~>" {
            Op::Arrow(ArrowTy::Functor)
        } else if t.str == ";*" {
            Op::CompStar
        } else if t.str.starts_with(";;") {
            Op::CompRep(t.str.len() as u32)
        } else if t.str == ";" {
            if let Some(n) = self.eat(|t| t.ty == TokenTy::Number && t.connected) {
                let n = n.str.parse::<u32>().unwrap_or_else(|_| {
                    self.add_error("invalid number literal in comp level");
                    0
                });
                Op::CompLit(n)
            } else {
                Op::CompRep(1)
            }
        } else {
            self.rollback(u);
            return None;
        };
        Some(self.end(u, op))
    }

    pub fn val0(&mut self) -> Option<A<Val0>> {
        let u = self.begin();

        let v = if let Some(lit) = self.lit() {
            Val0::Lit(lit)
        } else if let Some(rn) = self.path() {
            Val0::Path(Box::new(rn))
        } else if self.symbol("(").is_some() {
            let inner = self.val().unwrap_or_else(|| {
                self.expected_in("value", "parentheses");
                A::Error()
            });
            if self.symbol_close(")").is_some() {
                // pass
            } else {
                self.expected_at_end_of("')'", "parentheses");
            }
            Val0::Paren(Box::new(inner))
        } else if self
            .eat(|t| t.ty == TokenTy::Keyword && t.str.starts_with(".."))
            .is_some()
        {
            Val0::Dots
        } else {
            return None;
        };

        Some(self.end(u, v))
    }

    pub fn val(&mut self) -> Option<A<Val>> {
        let u = self.begin();
        let v0 = self.val0()?;
        let mut val = Val {
            vs: vec![v0],
            ops: vec![],
        };
        loop {
            let u2 = self.begin();
            let op = if let Some(op) = self.op() {
                let pp = self.params();
                (op, pp)
            } else {
                let op = self.end(u2.clone(), Op::CompRep(0));
                (op, None)
            };
            let v = if let Some(v) = self.val0() {
                v
            } else {
                self.rollback(u2);
                break;
            };

            val.vs.push(v);
            val.ops.push(op);
        }

        Some(self.end(u, val))
    }

    pub fn module(&mut self) -> Option<A<Module>> {
        let u = self.begin();

        let m = if self.symbol("{").is_some() {
            let p = self.decls(Some(self.last_indent()));
            if self.symbol_close("}").is_some() {
                // pass
            } else {
                self.expected_at_end_of("'}'", "module block");
            }
            Module::Block(p)
        } else if self.keyword("import").is_some() {
            // import
            let s = self.lit().unwrap_or_else(|| {
                self.expected_after("literal", "'import'");
                A::Error()
            });
            Module::Import(s)
        } else {
            return None;
        };

        Some(self.end(u, m))
    }

    pub fn assign_op(&mut self) -> Option<A<AssignOp>> {
        let u = self.begin();
        let t = self.eat(|t| t.ty == TokenTy::Operator && matches!(t.str, "=" | ":=" | "+="))?;
        let op = match t.str {
            "=" => AssignOp::Alias,
            ":=" => AssignOp::Def,
            "+=" => AssignOp::Add,
            _ => unreachable!(),
        };
        Some(self.end(u, op))
    }

    pub fn decl_unit(&mut self) -> Option<A<DeclUnit>> {
        let u = self.begin();

        let mut names = vec![];
        names.push(self.path()?);

        while self.peek().is_some_and(|t| t.ty == TokenTy::Name) {
            let n = self.path().unwrap_or_else(|| {
                self.expected("name");
                A::Error()
            });
            names.push(n);
        }

        let ty = if self.symbol(":").is_some() {
            let val = self.val().unwrap_or_else(|| {
                self.expected_after("type", "':'");
                A::Error()
            });
            Some(val)
        } else {
            None
        };

        let assign = if let Some(op) = self.assign_op() {
            let vm = if let Some(m) = self.module() {
                ValMod::Mod(m)
            } else {
                let v = self.val().unwrap_or_else(|| {
                    self.expected_after("value or module", "assignment operator");
                    A::Error()
                });
                ValMod::Val(v)
            };
            Some((op, vm))
        } else {
            None
        };

        let decl = DeclUnit { names, ty, assign };
        Some(self.end(u, decl))
    }

    pub fn clause_ty(&mut self) -> Option<A<ClauseTy>> {
        let u = self.begin();
        let ty = if self.keyword("with").is_some() {
            ClauseTy::With
        } else if self.keyword("where").is_some() {
            ClauseTy::Where
        } else {
            return None;
        };
        Some(self.end(u, ty))
    }

    pub fn clause(&mut self) -> Option<A<Clause>> {
        let u = self.begin();

        let ct = self.clause_ty()?;
        let m = self.module().unwrap_or_else(|| {
            self.expected_after("module", "clause keyword");
            A::Error()
        });

        let clause = Clause(ct, m);
        Some(self.end(u, clause))
    }

    pub fn consume_indent(&mut self, last_indent: usize) -> bool {
        fn is_close(t: &Token) -> bool {
            t.ty == TokenTy::Symbol && matches!(t.str, ")" | "}" | "]")
        }
        let mut consumed = false;
        while let Some(t) = self.peek() {
            // Leave close brackets for the outer parser only if they are on a line
            // whose indent is <= last_indent (i.e. at the right nesting level).
            // Close brackets on deeper-indented lines are garbage and should be
            // consumed here, per the principle that a decl owns all tokens at its
            // own indent level or deeper.
            if is_close(t) && self.depth > 0 && t.indent <= last_indent {
                break;
            }
            if !consumed {
                self.unexpected(t.str);
            }
            self.next();
            consumed = true;
        }
        self.indent = last_indent;
        consumed
    }

    pub fn decl(&mut self) -> Option<A<Decl>> {
        let u = self.begin();

        self.decl_head = true;
        let last_indent = self.indent;
        let next_indent = self.peek()?.indent;
        if next_indent < last_indent {
            self.rollback(u);
            return None;
        }
        self.indent = next_indent;

        let mut decos = vec![];
        while let Some(d) = self.decorator() {
            decos.push(d);
        }

        let main = if let Some(m) = self.module() {
            Some(DeclMain::Mod(m))
        } else if self
            .eat(|t| t.ty == TokenTy::Keyword && t.str.starts_with(".."))
            .is_some()
        {
            Some(DeclMain::Dots)
        } else {
            match self.decl_unit() {
                Some(du) => Some(DeclMain::Unit(du)),
                None => {
                    if decos.is_empty() {
                        // hmmmm
                        if self.consume_indent(last_indent) {
                            // garbage
                            return Some(A::Error());
                        } else {
                            // no consumption, simple rollback
                            return None;
                        }
                    } else {
                        // decorator-only block
                        None
                    }
                }
            }
        };

        let mut clauses = vec![];
        while let Some(c) = self.clause() {
            clauses.push(c);
        }

        let decl = Decl {
            decos,
            main,
            clauses,
        };

        if self.symbol(",").is_some() {
            self.indent = last_indent; // トークン消費なしで indent をブロック基底に戻す
        } else {
            self.consume_indent(last_indent);
        }
        Some(self.end(u, decl))
    }

    pub fn decls(&mut self, new_indent: Option<usize>) -> Vec<A<Decl>> {
        let saved_indent = self.indent;
        if let Some(indent) = new_indent {
            self.indent = indent;
            self.depth += 1;
        }
        let mut decls = vec![];
        while let Some(d) = self.decl() {
            decls.push(d);
        }
        if new_indent.is_some() {
            self.depth -= 1;
            self.indent = saved_indent;
        }
        decls
    }

    pub fn program(&mut self) -> A<Program> {
        let u = self.begin();
        let decls = self.decls(None);
        if !self.eoi() {
            self.add_error("expected end of input");
        }
        self.end(u, Program(decls))
    }
}

pub fn parse<'a>(tokens: &'a [Token<'a>]) -> (A<Program>, Vec<Error>) {
    let mut tracker = Tracker::new(tokens);
    let result = tracker.program();
    (result, tracker.errors)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pretty::pretty;
    use crate::tokenize::tokenize;

    fn p(code: &str) -> String {
        let (tokens, _, errors) = tokenize(code.trim());
        assert!(errors.is_empty(), "tokenize errors: {errors:?}");
        let (result, errors) = parse(&tokens);
        assert!(errors.is_empty(), "parse errors: {errors:?}");
        pretty(&result)
    }

    fn parse_result(code: &str) -> (String, Vec<String>) {
        let (tokens, _, _) = tokenize(code.trim());
        let (result, errors) = parse(&tokens);
        (
            pretty(&result),
            errors.into_iter().map(|(_, msg)| msg).collect(),
        )
    }

    fn errs(code: &str) -> Vec<String> {
        let (tokens, _, _) = tokenize(code.trim());
        let (_, errors) = parse(&tokens);
        errors.into_iter().map(|(_, msg)| msg).collect()
    }

    #[test]
    fn decl_dots() {
        assert_eq!(p("..."), "...\n");
        assert_eq!(p("x = {\n    ...\n}"), "x = {\n    ...\n}\n");
        assert_eq!(p("[A] ..."), "[A] ...\n");
        assert_eq!(p("...\nx = y"), "...\nx = y\n");
    }

    #[test]
    fn decl_forms() {
        assert_eq!(p("x"), "x\n");
        assert_eq!(p("x: A"), "x: A\n");
        assert_eq!(p("x = y"), "x = y\n");
        assert_eq!(p("x: A = y"), "x: A = y\n");
        assert_eq!(p("x: A := y"), "x: A := y\n");
        assert_eq!(p("x += y"), "x += y\n");
        assert_eq!(p("x += { a = b }"), "x += {\n    a = b\n}\n");
        assert_eq!(p("x y z: A = b"), "x y z: A = b\n");
        assert_eq!(p("a b c: T"), "a b c: T\n");
        assert_eq!(p("x.y = z"), "x.y = z\n");
    }

    #[test]
    fn value_forms() {
        assert_eq!(p("x = a b c"), "x = a b c\n");
        assert_eq!(p("x = a ; b"), "x = a; b\n");
        assert_eq!(p("x = a ;; b"), "x = a;; b\n");
        assert_eq!(p("x = a ;;; b"), "x = a;;; b\n");
        assert_eq!(p("x = a ;* b"), "x = a;* b\n");
        // assert_eq!(p("x = a ;2 b"), "x = a;2 b\n");
        assert_eq!(p("x: A → B"), "x: A → B\n");
        assert_eq!(p("x: A -> B"), "x: A → B\n"); // ASCII -> is the same as Unicode →
        assert_eq!(p("x: A → B → C"), "x: A → B → C\n");
        assert_eq!(p("x: A ~ B"), "x: A ~ B\n");
        assert_eq!(p("x: A ~> B"), "x: A ~> B\n");
        assert_eq!(p("x = (a ; b)"), "x = (a; b)\n");
        assert_eq!(p("x = ..."), "x = ...\n");
    }

    #[test]
    fn path_and_params() {
        assert_eq!(p("x = a.b.c"), "x = a.b.c\n");
        assert_eq!(p("x = a.b.c.d"), "x = a.b.c.d\n");
        assert_eq!(p("x = f[a]"), "x = f[a]\n");
        assert_eq!(p("x = f[a, b: c, d = e]"), "x = f[a, b: c, d = e]\n");
        assert_eq!(p("x = f[a].g[b]"), "x = f[a].g[b]\n");
        assert_eq!(p("x = f(a)"), "x = f(a)\n"); // connected → functor application
        assert_eq!(p("x = f (a)"), "x = f (a)\n"); // space → juxtaposition
        assert_eq!(p("x = a.b(c)"), "x = a.b(c)\n");
    }

    #[test]
    fn literals() {
        assert_eq!(p(r#"x = "hello""#), "x = \"hello\"\n");
        assert_eq!(p("x = 42"), "x = 42\n");
        assert_eq!(p("x = [a, b]"), "x = [a, b]\n");
        assert_eq!(p("x = {a: b}"), "x = {\n    a: b\n}\n");
    }

    #[test]
    fn module_forms() {
        assert_eq!(p("x = {}"), "x = {\n}\n");
        assert_eq!(p("x = { a = b }"), "x = {\n    a = b\n}\n");
        assert_eq!(
            p("x = {\n    a = b\n    c = d\n}"),
            "x = {\n    a = b\n    c = d\n}\n"
        );
        // anonymous module inside a block is parsed as its own decl
        assert_eq!(
            p("x = {\n    {}\n    a = b\n}"),
            "x = {\n    {\n    }\n    a = b\n}\n"
        );
        assert_eq!(p(r#"import "foo""#), "import \"foo\"\n");
        assert_eq!(p(r#"import "foo/bar/baz""#), "import \"foo/bar/baz\"\n");
    }

    #[test]
    fn clauses() {
        // inline (all at indent=0): works
        assert_eq!(p("x = y with { a = b }"), "x = y with {\n    a = b\n}\n");
        assert_eq!(p("x = y where { a = b }"), "x = y where {\n    a = b\n}\n");
        // multi-line with: see indent_with_clause_* tests below
    }

    #[test]
    fn decorators() {
        assert_eq!(p("[A] x = y"), "[A] x = y\n");
        assert_eq!(p("[A, B] x = y"), "[A, B] x = y\n");
        assert_eq!(p("[style[...]] y: *"), "[style[...]] y: *\n");
        assert_eq!(p("[f[a, b]] x = y"), "[f[a, b]] x = y\n");
        assert_eq!(p("[a: T, b = v] x: U"), "[a: T, b = v] x: U\n");
        // decorator alone on its own line → decorator-only decl; next line is independent
        assert_eq!(p("[A]\nx = y"), "[A]\nx = y\n");
        assert_eq!(p("[A]\n[B]\n[C]"), "[A]\n[B]\n[C]\n");
    }

    #[test]
    fn multiple_decls() {
        assert_eq!(p("x = y\nz = w"), "x = y\nz = w\n");
        assert_eq!(p("x = y\n[A]\nz = w"), "x = y\n[A]\nz = w\n");
    }

    // ================================================================
    // カンマ区切り宣言のエッジケース
    // ================================================================

    #[test]
    fn comma_trailing_eoi() {
        // 末尾カンマで EOI → エラーなしで x だけ
        assert_eq!(p("x,"), "x\n");
    }

    #[test]
    fn comma_then_anon_module() {
        // カンマの後に匿名モジュール
        assert_eq!(p("x, { a = b }"), "x\n{\n    a = b\n}\n");
    }

    #[test]
    fn comma_after_module_rhs() {
        // モジュール代入の後にカンマ
        assert_eq!(p("x = { a = b }, y"), "x = {\n    a = b\n}\ny\n");
    }

    #[test]
    fn comma_after_with_clause() {
        // with/where 節の後にカンマ
        assert_eq!(
            p("x = y with { a = b }, z"),
            "x = y with {\n    a = b\n}\nz\n"
        );
        assert_eq!(
            p("x = y where { a = b }, z"),
            "x = y where {\n    a = b\n}\nz\n"
        );
    }

    #[test]
    fn comma_trailing_in_block() {
        // ブロック内末尾カンマ
        assert_eq!(p("x = { a, }"), "x = {\n    a\n}\n");
    }

    #[test]
    fn comma_decorated_decls() {
        // デコレータ付き宣言のカンマ区切り
        assert_eq!(p("[A] x, [B] y"), "[A] x\n[B] y\n");
    }

    #[test]
    fn comma_block_multiline_same_indent() {
        // ブロック内カンマ区切り、続きが同じ indent の次行
        assert_eq!(
            p("x = {\n  a = 1,\n  b = 2\n}"),
            "x = {\n    a = 1\n    b = 2\n}\n"
        );
    }

    #[test]
    fn comma_block_shallower_indent_after_comma() {
        // ブロック内でカンマの後、b が a より浅い indent になっている
        // a (indent=4) → カンマ → b (indent=2): self.indent が 4 のまま残り
        // b の next_indent=2 < last_indent=4 となってブロック内に入れなくなる
        let (result, errors) = parse_result("x = {\n    a,\n  b\n}");
        assert!(errors.is_empty(), "errors: {errors:?}");
        assert_eq!(result, "x = {\n    a\n    b\n}\n");
    }

    #[test]
    fn comma_separated_decls() {
        // 1行に複数の宣言をカンマで区切って並べられる
        assert_eq!(p("x, y"), "x\ny\n");
        assert_eq!(p("x: A, y: B"), "x: A\ny: B\n");
        assert_eq!(p("x, y, z"), "x\ny\nz\n");
        // カンマの後に改行しても続く
        assert_eq!(p("x,\ny"), "x\ny\n");
        // ブロック内でもカンマ区切りが使える
        assert_eq!(p("x = { a, b }"), "x = {\n    a\n    b\n}\n");
        // カンマ区切りと通常の改行区切りの混在
        assert_eq!(p("a, b\nc, d"), "a\nb\nc\nd\n");
    }

    #[test]
    fn error_unexpected_at_eoi() {
        // `.` is consumed by consume_indent now; "unexpected" is more informative
        assert_eq!(errs("."), vec!["unexpected '.'"]);
    }

    #[test]
    fn error_unclosed_bracket() {
        assert!(errs("[x").iter().any(|e| e.contains("']'")));
    }

    /*
    // Old tests (kept for reference)

    fn test(code: &str) {
        let (tokens, _, errors) = tokenize(code);
        assert!(errors.is_empty());
        let (result, errors) = parse(&tokens);
        eprintln!("{}", pretty(&result));
        if !errors.is_empty() { eprintln!("Errors: {:#?}", errors); }
        assert!(errors.is_empty());
    }

    // parse_test0: decorator + decl on separate lines (same indent → two separate decls)
    // parse_test1: with-clause where `with` is at same indent as decl (broken, see bug)
    // parse_test3/4: complex indent/clause cases — assert!(false), WIP
    // parse_test5: nested inline module `{ a = { p = q }}`
    // parse_test6: path-as-name `x.y = z`
    // parse_test7: error recovery at indent=0, `[(.\n[(.` (bug)
    */

    // ================================================================
    // Indent behavior tests
    // Principles:
    //   1. Errors are isolated per decl — next decl is not affected
    //   2. A decl sees only tokens at strictly deeper indent (col > indent)
    //      OR closing brackets at same-or-deeper indent (col >= indent)
    // ================================================================

    /// Values can span indented continuation lines.
    #[test]
    fn indent_value_continues_deeper() {
        assert_eq!(p("x = y\n  z"), "x = y z\n"); // z at col=2 > indent=0 → juxtaposition
        assert_eq!(p("x = y\nz"), "x = y\nz\n"); // z at col=0 = indent=0 → new decl
    }

    /// Closing brackets are visible at same indent via eat_close (col >= indent).
    #[test]
    fn indent_close_bracket_same_level() {
        assert_eq!(p("[a\n]"), "[a]\n"); // ] at col=0, same as [ at col=0
        assert_eq!(p("[a,\n  b\n]"), "[a, b]\n"); // b at col=2, ] at col=0
        assert_eq!(p("x = {\n  a = b\n}"), "x = {\n    a = b\n}\n"); // } at col=0
    }

    /// Nested blocks at proper indentation levels.
    #[test]
    fn indent_nested_blocks() {
        assert_eq!(
            p("x = {\n    a = b\n    c = d\n}"),
            "x = {\n    a = b\n    c = d\n}\n"
        );
        assert_eq!(
            p("x = {\n    a = {\n        b = c\n    }\n}"),
            "x = {\n    a = {\n        b = c\n    }\n}\n"
        );
    }

    #[test]
    fn indent_with_clause_inline() {
        assert_eq!(p("x = y with { a = b }"), "x = y with {\n    a = b\n}\n");
        // `with` at deeper indent — consume_indent now stops before closing brackets
        assert_eq!(p("x = y\n  with { a = b }"), "x = y with {\n    a = b\n}\n");
    }

    #[test]
    fn indent_with_clause_multiline_body() {
        // } on its own line — consume_indent now stops before closing brackets
        assert_eq!(
            p("x = y\n  with {\n    a = b\n  }"),
            "x = y with {\n    a = b\n}\n"
        );
    }

    #[test]
    fn indent_with_clause_same_indent() {
        // `with` at same indent as decl is NOT a clause — only deeper tokens continue a decl
        // (closing brackets are an exception: they can appear at the same indent)
        let (result, errors) = parse_result("x = y\nwith { a = b }");
        assert!(result.contains("x = y"));
        assert!(!errors.is_empty()); // `with` not recognized as clause at col=0
    }

    /// Unrecognized token inside a block is consumed, and subsequent decls still parse.
    /// This works because consume_indent fires (self.indent=4 != last_indent=0) and
    /// peek() sees `.` via decl_head=true (nothing called next() yet), so it's consumed.
    #[test]
    fn error_recovery_garbage_in_block() {
        let (result, errors) = parse_result("x = {\n    .\n    a = b\n}");
        assert_eq!(errors, vec!["unexpected '.'"]);
        assert!(result.contains("a = b")); // a = b IS parsed — recovery works here
    }

    #[test]
    fn error_recovery_garbage_at_indent0() {
        // consume_indent now fires even at indent=0 (peek() sees non-closing token)
        let (result, errors) = parse_result(".\nx = y");
        assert_eq!(errors, vec!["unexpected '.'"]);
        assert!(result.contains("x = y")); // x = y IS parsed after recovery
    }

    #[test]
    fn error_recovery_value_error_at_indent0() {
        // leftover `.` after failed val is consumed by trailing consume_indent
        let (result, errors) = parse_result("x = .\ny = z");
        assert!(!errors.is_empty());
        assert!(result.contains("y = z")); // y = z IS parsed after recovery
    }

    /// Integration test: a program using many grammar features in an n-category context.
    #[test]
    fn integration_n_category() {
        // --- main program (no errors) ---
        let code = r#"import "prelude"

*: *
Nat Bool: *
1: *

zero: 1 → Nat
succ: Nat → Nat
add: Nat Nat → Nat

[n: Nat → Nat] fixed: n → n

k[c: *, f g: c → c, a: f → g]: f → g = id[a]

F: Nat ~> Nat

bool: 1 → Bool
  with {
    true: 1 → bool
    false: 1 → bool
  }

bool += {
  not: bool → bool
}

comp: Nat → Nat = succ succ
  where {
    succ: Nat → Nat
  }"#;

        assert_eq!(
            p(code),
            r#"import "prelude"
*: *
Nat Bool: *
1: *
zero: 1 → Nat
succ: Nat → Nat
add: Nat Nat → Nat
[n: Nat → Nat] fixed: n → n
k[c: *, f g: c → c, a: f → g]: f → g = id[a]
F: Nat ~> Nat
bool: 1 → Bool with {
    true: 1 → bool
    false: 1 → bool
}
bool += {
    not: bool → bool
}
comp: Nat → Nat = succ succ where {
    succ: Nat → Nat
}
"#
        );

        // --- error recovery: garbage token between valid declarations ---
        let (result, errors) = parse_result("succ: Nat → Nat\n.\nzero: 1 → Nat");
        assert_eq!(errors, vec!["unexpected '.'"]);
        assert!(result.contains("succ: Nat → Nat"));
        assert!(result.contains("zero: 1 → Nat"));
    }

    #[test]
    fn error_recovery_nested_bracket_fail_at_indent0() {
        // `[(.]`: separator recovery in separated() consumes `.` and closes `]`
        // both lines are parsed as decorator-only decls
        let (result, errors) = parse_result("[(.]\n[(.]");
        assert_eq!(result.matches("[(?)]").count(), 2); // both lines parsed
        assert!(!errors.iter().any(|e| e.contains("end of input")));
        assert!(!errors.is_empty()); // paren/bracket errors still present
    }

    #[test]
    fn error_recovery_module_literal_after_name() {
        // `a {}`: `a` completes as a decl, then `{}` is consumed as garbage by
        // consume_indent (depth==0 so close brackets don't stop the loop).
        // `[.]` on the second line is reached.
        let (result, errors) = parse_result("a {}\n[.]");
        assert_eq!(result, "a\n[]\n");
        assert!(
            errors.iter().any(|e| e == "unexpected '{'"),
            "errors: {errors:?}"
        );
        assert!(
            !errors.iter().any(|e| e.contains("end of input")),
            "got eoi: {errors:?}"
        );
        assert!(result.contains("[]")); // second line IS reached
    }

    #[test]
    fn error_recovery_unclosed_module() {
        // `a {` with no closing `}` — consume_indent consumes `{` and hits EOI.
        let (_, errors) = parse_result("a {\nb = c");
        assert!(!errors.is_empty());
        assert!(
            !errors.iter().any(|e| e.contains("end of input")),
            "got eoi: {errors:?}"
        );
    }

    #[test]
    fn error_recovery_unclosed_bracket() {
        // `[x` with no closing `]`
        let (_, errors) = parse_result("[x\ny = z");
        assert!(!errors.is_empty());
        assert!(
            !errors.iter().any(|e| e.contains("end of input")),
            "got eoi: {errors:?}"
        );
    }
}
