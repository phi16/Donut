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

    fn expected(&mut self, what: &str) {
        self.add_error(&format!("expected {what}"));
    }
    fn expected_after(&mut self, what: &str, after: &str) {
        self.add_error(&format!("expected {what} after '{after}'"));
    }
    fn expected_in(&mut self, what: &str, context: &str) {
        self.add_error(&format!("expected {what} in {context}"));
    }
    fn expected_at_end_of(&mut self, what: &str, context: &str) {
        self.add_error(&format!("expected {what} at end of {context}"));
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
    pub fn peek_is(&self, ty: TokenTy) -> Option<&'a str> {
        if let Some(t) = self.peek() {
            return if t.ty == ty { Some(t.str) } else { None };
        }
        None
    }

    pub fn next(&mut self) {
        assert!(self.pos < self.tokens.len());
        self.pos += 1;
        self.decl_head = false;
    }

    pub fn eoi(&self) -> bool {
        self.tokens.get(self.pos).is_none()
    }

    fn separated<T>(
        &mut self,
        close: &'static str,
        err: &str,
        mut parse_item: impl FnMut(&mut Self) -> Option<T>,
    ) -> Vec<T> {
        if self.symbol_close(close).is_some() {
            return vec![];
        }
        let mut items = vec![];
        loop {
            let Some(item) = parse_item(self) else {
                self.expected(err);
                break;
            };
            items.push(item);
            if self.symbol(",").is_some() {
                if self.symbol_close(close).is_some() {
                    break;
                }
                continue;
            } else if self.symbol_close(close).is_some() {
                break;
            } else {
                self.expected(&format!("',' or '{close}'"));
                break;
            }
        }
        items
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
    pub fn symbol(&mut self, s: &'static str) -> Option<()> {
        let t = self.peek()?;
        if t.ty == TokenTy::Symbol && t.str == s {
            self.next();
            return Some(());
        }
        None
    }
    pub fn symbol_connected(&mut self, s: &'static str) -> Option<()> {
        let t = self.peek()?;
        if t.ty == TokenTy::Symbol && t.connected && t.str == s {
            self.next();
            return Some(());
        }
        None
    }
    pub fn symbol_close(&mut self, s: &'static str) -> Option<()> {
        let t = self.tokens.get(self.pos)?;
        if self.decl_head || self.indent <= t.pos.col {
            if t.ty == TokenTy::Symbol && t.str == s {
                self.next();
                return Some(());
            }
        }
        None
    }
    pub fn operator(&mut self, s: &'static str) -> Option<()> {
        let t = self.peek()?;
        if t.ty == TokenTy::Operator && t.str == s {
            self.next();
            return Some(());
        }
        None
    }
    pub fn keyword(&mut self, s: &'static str) -> Option<()> {
        let t = self.peek()?;
        if t.ty == TokenTy::Keyword && t.str == s {
            self.next();
            return Some(());
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
                    ParamTy::Decl => ":",
                    ParamTy::Named => "=",
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

    fn bracket(&mut self) -> Option<Vec<A<Param>>> {
        self.symbol("[")?;
        Some(self.separated("]", "parameter", |s| s.param()))
    }

    pub fn params(&mut self) -> Option<A<Params>> {
        let u = self.begin();
        let params = self.bracket()?;
        Some(self.end(u, Params(params)))
    }

    pub fn decorator(&mut self) -> Option<A<Decorator>> {
        let u = self.begin();
        let params = self.bracket()?;
        Some(self.end(u, Decorator(params)))
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
        while self.symbol(".").is_some() {
            let n = self.segment().unwrap_or_else(|| {
                self.expected_after("name", ".");
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
        } else if let Some(s) = self.peek_is(TokenTy::String) {
            let u2 = self.begin();
            self.next();
            let s = self.end(u2, s.to_string());
            Some(self.end(u, Key::String(s)))
        } else {
            None
        }
    }

    pub fn key_val(&mut self) -> Option<(A<Key>, A<Val>)> {
        let key = self.key()?;
        let val = if self.symbol(":").is_some() {
            self.val().unwrap_or_else(|| {
                self.expected_after("value", ":");
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
        let lit = if let Some(str) = self.peek_is(TokenTy::Number) {
            // number
            self.next();
            Lit::Number(str.to_string())
        } else if let Some(str) = self.peek_is(TokenTy::String) {
            // string
            self.next();
            Lit::String(str.to_string())
        } else if self.symbol("[").is_some() {
            // array
            let vs = self.separated("]", "value", |s| s.val());
            Lit::Array(vs)
        } else if self.symbol("{").is_some() {
            // object
            let ps = self.separated("}", "key-value pair", |s| s.key_val());
            Lit::Object(ps)
        } else {
            return None;
        };
        Some(self.end(u, lit))
    }

    pub fn op(&mut self) -> Option<A<Op>> {
        let u = self.begin();
        let s = self.peek_is(TokenTy::Operator)?;
        self.next();
        let op = if s == "->" || s == "→" {
            Op::Arrow(ArrowTy::To)
        } else if s == "~" {
            Op::Arrow(ArrowTy::Eq)
        } else if s == "~>" {
            Op::Arrow(ArrowTy::Functor)
        } else if s == ";*" {
            Op::CompStar
        } else if s.starts_with(";;") {
            Op::CompRep(s.len() as u32)
        } else if s == ";" {
            if let Some(t) = self.peek()
                && t.ty == TokenTy::Number
                && t.connected
            {
                self.next();
                let n = t.str;
                let n = n.parse::<u32>().unwrap_or_else(|_| {
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
            Val0::Ref(Box::new(rn))
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
            .peek_is(TokenTy::Keyword)
            .filter(|s| s.starts_with(".."))
            .is_some()
        {
            self.next();
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
            // block
            let p = self.decls();
            if self.symbol_close("}").is_some() {
                // pass
            } else {
                self.expected_at_end_of("'}'", "module block");
            }
            Module::Block(p)
        } else if self.keyword("import").is_some() {
            // import
            let s = self.lit().unwrap_or_else(|| {
                self.expected_after("literal", "import");
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
        if let Some(s) = self.peek_is(TokenTy::Operator) {
            self.next();
            let op = if s == "=" {
                AssignOp::Alias
            } else if s == ":=" {
                AssignOp::Def
            } else if s == "+=" {
                AssignOp::Add
            } else {
                self.rollback(u);
                return None;
            };
            Some(self.end(u, op))
        } else {
            None
        }
    }

    pub fn decl_unit(&mut self) -> Option<A<DeclUnit>> {
        let u = self.begin();

        let mut names = vec![];
        names.push(self.path()?);

        while self.peek_is(TokenTy::Name).is_some() {
            let n = self.path().unwrap_or_else(|| {
                self.expected("name");
                A::Error()
            });
            names.push(n);
        }

        let ty = if self.symbol(":").is_some() {
            let val = self.val().unwrap_or_else(|| {
                self.expected_after("type", ":");
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
        let mut consumed = false;
        if self.indent != last_indent {
            // decl should read everything in this indent level
            if let Some(t) = self.peek() {
                // TODO: or, close symbol?

                // TODO: if there's already an error, don't add this one
                self.unexpected(t.str);

                // skip tokens
                loop {
                    if self.peek().is_some() {
                        self.next();
                        consumed = true;
                        continue;
                    } else if self.symbol_close(")").is_some()
                        || self.symbol_close("}").is_some()
                        || self.symbol_close("]").is_some()
                    {
                        consumed = true;
                        continue;
                    }
                    break;
                }
            }
            self.indent = last_indent;
        }
        consumed
    }

    pub fn decl(&mut self) -> Option<A<Decl>> {
        let u = self.begin();

        self.decl_head = true;
        let last_indent = self.indent;
        self.indent = self.peek()?.indent;

        let mut decos = vec![];
        while let Some(d) = self.decorator() {
            decos.push(d);
        }

        let main = if let Some(m) = self.module() {
            Some(DeclMain::Mod(m))
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
            // pass
        }
        self.consume_indent(last_indent);
        Some(self.end(u, decl))
    }

    pub fn decls(&mut self) -> Vec<A<Decl>> {
        let mut decls = vec![];
        while let Some(d) = self.decl() {
            decls.push(d);
        }
        decls
    }

    pub fn program(&mut self) -> A<Program> {
        let u = self.begin();
        let p = self.decls();
        if !self.eoi() {
            self.add_error("expected end of input");
        }
        self.end(u, Program(p))
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

    fn test(code: &str) {
        let (tokens, _, errors) = tokenize(code);
        assert!(errors.is_empty());

        let (result, errors) = parse(&tokens);
        eprintln!("{}", pretty(&result));
        if !errors.is_empty() {
            eprintln!("Errors: {:#?}", errors);
        }
        assert!(errors.is_empty());
    }

    #[test]
    fn parse_test0() {
        test(
            r#"
            [a[x = c]]
            x: y = z
            "#,
        );
    }

    #[test]
    fn parse_test1() {
        test(
            r#"
[x[x = x]]
x: y = z {
    } with {
}
            "#,
        );
        test(
            r#"
            [x[x = x]]
            x: y = z {
                }
            with {
            }
            "#,
        );
    }

    #[test]
    fn parse_test() {
        test("a += a");
        test("a = \"x\"");
        test("a: A = \"x\"");
        test("x: A");
        test("x: A = B");
        test("x = B");
        test("x: A → B");
        test(
            r#"
        x: A → B = f where {
            f = g
        }"#,
        );
        test("a b c: d = e ; f");
        test("a b c: d = e;f ;;; g");
        test("a b c: d = e f g");
        test("a b c: d = e ; f ;;; g;h;;i");
        test("a b c: d = e;;f ;;; g;h;;i");
        test("a b c: d = e;;;;;f g h ;* i");
        test("a b c: d = e;;;;;f (g h ;* i)");
        test("import \"module.donut\"");
        test("a = \"a\"");
        test("x = 1");
        test("[style[...]] y: *");
    }

    #[test]
    fn parse_test2() {
        test(
            r#"
        x = y
        [A[...], C]
        [B[.....]]
        z = w
        "#,
        );
        test(
            r#"
        x = y
        z = w
        "#,
        );
        test(
            r#"
        x = y
        [style[...]]
        z = w
        "#,
        );
        assert!(false);
    }

    #[test]
    fn parse_test3() {
        test(
            r#"
        [style]
        x = y with {
            a = b
        } where {
            c = d where { z = z }
            c = d where { z = z, z = z }
            v = v,
            v = v
            v = {
                a = a
            }
        }
        [a, b(
            c = d
        )]
        [c]
        [c(
            d
        )]
        [c(
            d)
        ]
        a = q
        "#,
        );
        assert!(false);
    }

    #[test]
    fn parse_test4() {
        test(
            r#"
        x = y with {
            a = b
        }
        x = y with
        {
            a = b
        }
        x = y
        with {
            a = b
        }
        x = y
        with { a = b }
        x = y
        with
        { a = b }
        x
          = y with {
            a = b
        }
        x = y with {
            a = b
            }
        x = y with {
        }
        "#,
        );
        assert!(false);
    }

    #[test]
    fn parse_test5() {
        test(
            r#"
        x = { a = {
            p = q
        }}
        "#,
        );
        assert!(false);
    }
}
