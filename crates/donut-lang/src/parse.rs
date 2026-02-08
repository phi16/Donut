use crate::types::token::*;
use crate::types::tree::*;

// num-lit = ...
// str-lit = ...
// name = ...
// param = (name+ (':' | '='))? val
// param-pack = '[' (param %0 ',') ']'
// app-name = name param-pack?
// ref-name = (app-name %1 '.') (## '(' val ')')?
// lit = num-lit
//     | str-lit
//     | '[' (val %0 ',') ']'
//     | '{' (((name | str-lit) ':' val) %0 ',') '}'
// val = ref-name
//     | lit
//     | val ('→' | '~' | '~>') val
//     | val (';'* | ';' param-pack) val
//     | '(' val ')'
// assign-op = '=' | ':=' | '+='
// mod = '{' decl* '}'
//     | 'import' str-lit
// unit-decl = ref-name+ (':' val)? (assign-op (val | mod))?
// clause = ('with' | 'where') mod
// decl = param-pack* (unit-decl | mod) clause*
// program = decl* eoi

struct Tracker<'a> {
    tokens: &'a [Token<'a>],
    pos: usize,
    errors: Vec<Error>,
}

struct NodeBase {
    start: usize,
}

impl<'a> Tracker<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> Self {
        Self {
            tokens,
            pos: 0,
            errors: vec![],
        }
    }

    pub fn begin(&self) -> NodeBase {
        NodeBase { start: self.pos }
    }

    pub fn add_error(&mut self, msg: &'static str) {
        // TODO: refine pos
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

    pub fn end<T>(&self, base: NodeBase, t: T) -> A<T> {
        let start = base.start;
        let end = self.pos;
        let span = TokenSpan { start, end };
        A::Accepted(t, span)
    }

    pub fn rollback(&mut self, base: NodeBase) {
        self.pos = base.start;
    }

    pub fn peek(&self) -> Option<&'a Token<'a>> {
        self.tokens.get(self.pos)
    }
    pub fn peek_n(&self, n: usize) -> Option<&'a Token<'a>> {
        self.tokens.get(self.pos + n)
    }

    pub fn skip_until(&mut self, s: &'static str) {
        /* while let Some(t) = self.peek() {
            if t.str == s {
                return;
            }
            self.next();
        } */
        // TODO
    }

    pub fn next(&mut self) {
        assert!(self.pos < self.tokens.len());
        self.pos += 1;
    }

    pub fn next_is(&self, ty: TokenTy) -> bool {
        if let Some(t) = self.peek() {
            if t.ty == ty {
                return true;
            }
        }
        false
    }

    pub fn skip_whitespaces(&mut self) {
        while self.next_is(TokenTy::Whitespace) {
            self.next();
        }
    }

    pub fn peek_is(&self, ty: TokenTy) -> bool {
        let mut p = self.pos;
        while let Some(t) = self.tokens.get(p) {
            if t.ty == TokenTy::Whitespace {
                p += 1;
                continue;
            }
            return t.ty == ty;
        }
        false
    }

    pub fn eoi(&mut self) -> Option<()> {
        let u = self.begin();
        self.skip_whitespaces();
        if self.peek().is_some() {
            self.rollback(u);
            return None;
        }
        Some(())
    }

    pub fn name(&mut self) -> Option<A<Name>> {
        let u = self.begin();
        self.skip_whitespaces();
        if let Some(t) = self.peek() {
            if t.ty == TokenTy::Name {
                let name = Name(t.str.to_string());
                self.next();
                return Some(self.end(u, name));
            }
        }
        self.rollback(u);
        None
    }

    pub fn symbol(&mut self, s: &'static str) -> Option<()> {
        let u = self.begin();
        self.skip_whitespaces();
        if self.symbol_no_skip(s).is_some() {
            Some(())
        } else {
            self.rollback(u);
            None
        }
    }

    pub fn symbol_no_skip(&mut self, s: &'static str) -> Option<()> {
        let u = self.begin();
        let mut s = s;
        while !s.is_empty() {
            if let Some(t) = self.peek() {
                if t.ty == TokenTy::Name || t.ty == TokenTy::Symbol {
                    if s.starts_with(t.str) {
                        self.next();
                        s = &s[t.str.len()..];
                        continue;
                    }
                }
            }
            self.rollback(u);
            return None;
        }
        Some(())
    }

    pub fn keyword(&mut self, s: &'static str) -> Option<()> {
        let u = self.begin();
        self.skip_whitespaces();
        if let Some(t) = self.peek() {
            if t.ty == TokenTy::Keyword && t.str == s {
                self.next();
                return Some(());
            }
        }
        self.rollback(u);
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
        } else if self.symbol("=").is_some() {
            ty = Some(ParamTy::Named);
        }
        let val = match &ty {
            Some(ty) => self.val().unwrap_or_else(|| {
                self.add_error(match ty {
                    ParamTy::Decl => "expected value after ':'",
                    ParamTy::Named => "expected value after '='",
                });
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

    pub fn param_pack(&mut self) -> Option<A<ParamPack>> {
        let u = self.begin();

        self.symbol("[")?;
        let mut params = vec![];
        if self.symbol(",").is_some() {
            self.add_error("unexpected ',' at start of parameter pack");
            while self.symbol(",").is_some() {
                // skip extra commas
            }
        }
        if self.symbol("]").is_some() {
            // pass
        } else {
            loop {
                let p = self.param().unwrap_or_else(|| {
                    self.add_error("expected parameter in parameter pack");
                    A::Error()
                });
                params.push(p);
                if self.symbol(",").is_some() {
                    if self.symbol("]").is_some() {
                        // trailing comma
                        break;
                    }
                    continue;
                } else if self.symbol("]").is_some() {
                    break;
                } else {
                    self.add_error("expected ',' or ']' in parameter pack");
                    self.skip_until("]");
                    break;
                }
            }
        }
        Some(self.end(u, ParamPack(params)))
    }

    pub fn app_name(&mut self) -> Option<A<AppName>> {
        let u = self.begin();

        let name = self.name()?;
        let param_pack = self.param_pack();

        let app_name = AppName(name, param_pack);
        Some(self.end(u, app_name))
    }

    pub fn ref_name(&mut self) -> Option<A<RefName>> {
        let u = self.begin();

        let mut names = vec![];
        names.push(self.app_name()?);
        while self.symbol(".").is_some() {
            let n = self.app_name().unwrap_or_else(|| {
                self.add_error("expected applicand name after '.' in name reference");
                A::Error()
            });
            names.push(n);
        }

        let val = if self.symbol_no_skip("(").is_some() {
            let v = self.val().unwrap_or_else(|| {
                self.add_error("expected value in functor application");
                A::Error()
            });
            if self.symbol(")").is_some() {
                // pass
            } else {
                self.add_error("expected ')' at end of functor application");
                self.skip_until(")");
            }
            Some(v)
        } else {
            None
        };

        let ref_name = RefName(names, val);
        Some(self.end(u, ref_name))
    }

    pub fn lit(&mut self) -> Option<A<Lit>> {
        let u = self.begin();
        let lit = if self.peek_is(TokenTy::Number) {
            // number
            let t = self.peek().unwrap();
            let num_str = t.str.to_string();
            self.next();
            Lit::Number(num_str)
        } else if self.peek_is(TokenTy::String) {
            // string
            let t = self.peek().unwrap();
            let str_lit = t.str.to_string();
            self.next();
            Lit::String(str_lit)
        } else if self.symbol("[").is_some() {
            // array
            unimplemented!()
        } else if self.symbol("{").is_some() {
            // object
            unimplemented!()
        } else {
            return None;
        };
        Some(self.end(u, lit))
    }

    pub fn val(&mut self) -> Option<A<Val>> {
        let u = self.begin();

        let v = if let Some(lit) = self.lit() {
            Val::Lit(lit)
        } else if let Some(rn) = self.ref_name() {
            Val::Ref(Box::new(rn))
        } else if self.symbol("(").is_some() {
            let inner = self.val().unwrap_or_else(|| {
                self.add_error("expected value in parentheses");
                A::Error()
            });
            if self.symbol(")").is_some() {
                // pass
            } else {
                self.add_error("expected ')' at end of parentheses");
                self.skip_until(")");
            }
            Val::Paren(Box::new(inner))
        } else {
            // TODO!
            return None;
        };

        Some(self.end(u, v))
    }

    pub fn module(&mut self) -> Option<A<Module>> {
        let u = self.begin();

        let m = if self.symbol("{").is_some() {
            // block
            let p = self.program();
            if self.symbol("}").is_some() {
                // pass
            } else {
                self.add_error("expected '}' at end of module block");
                self.skip_until("}");
            }
            Module::Block(p)
        } else if self.keyword("import").is_some() {
            // import
            let s = self.lit().unwrap_or_else(|| {
                self.add_error("expected literal after 'import'");
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
        let op = if self.symbol("=").is_some() {
            AssignOp::Alias
        } else if self.symbol(":=").is_some() {
            AssignOp::Def
        } else if self.symbol("+=").is_some() {
            AssignOp::Add
        } else {
            return None;
        };
        Some(self.end(u, op))
    }

    pub fn unit_decl(&mut self) -> Option<A<UnitDecl>> {
        let u = self.begin();

        let mut names = vec![];
        names.push(self.ref_name()?);

        while self.peek_is(TokenTy::Name) {
            let n = self.ref_name().unwrap_or_else(|| {
                self.add_error("expected name reference in declaration");
                A::Error()
            });
            names.push(n);
        }

        let ty = if self.symbol(":").is_some() {
            let val = self.val().unwrap_or_else(|| {
                self.add_error("expected type after ':' in declaration");
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
                    self.add_error("expected value or module after assignment operator");
                    A::Error()
                });
                ValMod::Val(v)
            };
            Some((op, vm))
        } else {
            None
        };

        let decl = UnitDecl { names, ty, assign };
        Some(self.end(u, decl))
    }

    pub fn clause_ty(&mut self) -> Option<A<ClauseTy>> {
        let u = self.begin();
        if self.keyword("with").is_some() {
            Some(self.end(u, ClauseTy::With))
        } else if self.keyword("where").is_some() {
            Some(self.end(u, ClauseTy::Where))
        } else {
            None
        }
    }

    pub fn clause(&mut self) -> Option<A<Clause>> {
        let u = self.begin();

        let ct = self.clause_ty()?;
        let m = self.module().unwrap_or_else(|| {
            self.add_error("expected module after clause type");
            A::Error()
        });

        let clause = Clause(ct, m);
        Some(self.end(u, clause))
    }

    pub fn decl(&mut self) -> Option<A<Decl>> {
        let u = self.begin();

        let mut decos = vec![];
        while let Some(d) = self.param_pack() {
            decos.push(d);
        }

        let unit = if let Some(m) = self.module() {
            Unit::Mod(m)
        } else {
            let ud = match self.unit_decl() {
                Some(ud) => ud,
                None => {
                    if decos.is_empty() {
                        return None;
                    } else {
                        self.add_error("expected unit declaration or module in declaration");
                        A::Error()
                    }
                }
            };
            Unit::Decl(ud)
        };

        let mut clauses = vec![];
        while let Some(c) = self.clause() {
            clauses.push(c);
        }

        let decl = Decl {
            decos,
            unit,
            clauses,
        };
        Some(self.end(u, decl))
    }

    pub fn program(&mut self) -> A<Program> {
        let u = self.begin();

        let mut decls = vec![];
        while let Some(d) = self.decl() {
            decls.push(d);
        }

        self.end(u, Program(decls))
    }

    pub fn parse_full(&mut self) -> A<Program> {
        let u = self.begin();
        let p = self.program();
        if self.eoi().is_none() {
            self.add_error("expected end of input"); // how
        }
        p
    }
}

pub fn parse<'a>(tokens: &'a [Token<'a>]) -> (A<Program>, Vec<Error>) {
    let mut tracker = Tracker::new(tokens);
    let result = tracker.parse_full();
    (result, tracker.errors)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pretty::pretty;
    use crate::tokenize::tokenize;

    #[test]
    fn parse_simple() {
        let code = "x: *";
        let (tokens, _, tok_errors) = tokenize(code);
        assert!(tok_errors.is_empty());

        let (result, errors) = parse(&tokens);
        eprintln!("{}", pretty(&result));
        eprintln!("Errors: {:#?}", errors);
        assert!(false);
    }

    #[test]
    fn parse_arrow() {
        let code = "x: A → B";
        let (tokens, _, tok_errors) = tokenize(code);
        assert!(tok_errors.is_empty());

        let (result, errors) = parse(&tokens);
        eprintln!("{}", pretty(&result));
        eprintln!("Errors: {:#?}", errors);
        assert!(false);
    }

    #[test]
    fn parse_with_clause() {
        let code = r#"x: A → B = f where {
            f = g
        }"#;
        let (tokens, _, tok_errors) = tokenize(code);
        assert!(tok_errors.is_empty());

        let (result, errors) = parse(&tokens);
        eprintln!("{}", pretty(&result));
        eprintln!("Errors: {:#?}", errors);
        assert!(false);
    }

    #[test]
    fn parse_with_multiple_decl() {
        let code = r#"a b c: d = e"#;
        let (tokens, _, tok_errors) = tokenize(code);
        assert!(tok_errors.is_empty());

        let (result, errors) = parse(&tokens);
        eprintln!("{}", pretty(&result));
        eprintln!("Errors: {:#?}", errors);
        assert!(false);
    }

    #[test]
    fn parse_attribute() {
        let code = "[style[...]] y: *";
        let (tokens, _, tok_errors) = tokenize(code);
        assert!(tok_errors.is_empty());

        let (result, errors) = parse(&tokens);
        eprintln!("{}", pretty(&result));
        eprintln!("Errors: {:#?}", errors);
        assert!(false);
    }
}
