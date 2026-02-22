use crate::types::token::*;
use crate::types::tree::*;

// num-lit = ...
// str-lit = ...
// name = ...
// head-of-line = ...
// param = (name+ (':' | '='))? val
// param-pack = '[' (param %0 ',') ']'
// local-ref = name param-pack?
// ref = (local-ref %1 '.') (## '(' val ')')?
// lit = num-lit
//     | str-lit
//     | '[' (val %0 ',') ']'
//     | '{' (((name | str-lit) ':' val) %0 ',') '}'
// val0 = ref
//      | lit
//      | '...'
//      | '(' val ')'
// valN = val0
//      | valN (';'* | ';' ## num-lit | ';*' | '+') param-pack? valN
// val = valN
//     | valN ('->' | '→' | '~' | '~>') param-pack? valN
// assign-op = '=' | ':=' | '+='
// mod = '{' decls '}'
//     | 'import' lit
// decl-unit = ref+ (':' val)? (assign-op (val | mod))?
// decorator = param-pack
// decl-main = decl-unit | mod
// clause = ('with' | 'where') mod
// decl = head-of-line decorator* decl-main clause*
// decls = decl*
// program = decls eoi

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

    pub fn peek_is(&self, ty: TokenTy) -> Option<&'a str> {
        if let Some(t) = self.peek() {
            return if t.ty == ty { Some(t.str) } else { None };
        }
        None
    }

    pub fn eoi(&self) -> Option<()> {
        if self.peek().is_some() {
            return None;
        }
        Some(())
    }

    pub fn name(&mut self) -> Option<A<Name>> {
        let u = self.begin();
        if let Some(t) = self.peek() {
            if t.ty == TokenTy::Name {
                let name = Name(t.str.to_string());
                self.next();
                return Some(self.end(u, name));
            }
        }
        None
    }
    pub fn symbol(&mut self, s: &'static str) -> Option<()> {
        if let Some(t) = self.peek() {
            if t.ty == TokenTy::Symbol && t.str == s {
                self.next();
                return Some(());
            }
        }
        None
    }
    pub fn keyword(&mut self, s: &'static str) -> Option<()> {
        if let Some(t) = self.peek() {
            if t.ty == TokenTy::Keyword && t.str == s {
                self.next();
                return Some(());
            }
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

    pub fn local_ref_name(&mut self) -> Option<A<LocalRefName>> {
        let u = self.begin();

        let name = self.name()?;
        let param_pack = self.param_pack();

        let app_name = LocalRefName(name, param_pack);
        Some(self.end(u, app_name))
    }

    pub fn ref_name(&mut self) -> Option<A<RefName>> {
        let u = self.begin();

        let mut names = vec![];
        names.push(self.local_ref_name()?);
        while self.symbol(".").is_some() {
            let n = self.local_ref_name().unwrap_or_else(|| {
                self.add_error("expected applicand name after '.' in name reference");
                A::Error()
            });
            names.push(n);
        }

        let val = if self.symbol("(").is_some() {
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
            unimplemented!()
        } else if self.symbol("{").is_some() {
            // object
            unimplemented!()
        } else {
            return None;
        };
        Some(self.end(u, lit))
    }

    pub fn val0(&mut self) -> Option<A<Val0>> {
        let u = self.begin();

        // TODO?

        let v = if let Some(lit) = self.lit() {
            Val0::Lit(lit)
        } else if let Some(rn) = self.ref_name() {
            Val0::Ref(Box::new(rn))
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
            Val0::Paren(Box::new(inner))
        } else {
            // TODO!
            return None;
        };

        Some(self.end(u, v))
    }
    pub fn val(&mut self) -> Option<A<Val>> {
        let u = self.begin();

        let v = unimplemented!();

        Some(self.end(u, v))
    }

    pub fn module(&mut self) -> Option<A<Module>> {
        let u = self.begin();

        let m = if self.symbol("{").is_some() {
            // block
            let p = self.decls();
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

    pub fn decl_unit(&mut self) -> Option<A<DeclUnit>> {
        let u = self.begin();

        let mut names = vec![];
        names.push(self.ref_name()?);

        while self.peek_is(TokenTy::Name).is_some() {
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

        let decl = DeclUnit { names, ty, assign };
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

        let main = if let Some(m) = self.module() {
            DeclMain::Mod(m)
        } else {
            let du = match self.decl_unit() {
                Some(du) => du,
                None => {
                    if decos.is_empty() {
                        return None;
                    } else {
                        self.add_error("expected unit declaration or module in declaration");
                        A::Error()
                    }
                }
            };
            DeclMain::Unit(du)
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
        Some(self.end(u, decl))
    }

    pub fn decls(&mut self) -> Vec<A<Decl>> {
        let mut decls = vec![];
        // TODO: head?
        while let Some(d) = self.decl() {
            decls.push(d);
        }
        decls
    }

    pub fn program(&mut self) -> A<Program> {
        let u = self.begin();
        let p = self.decls();
        if self.eoi().is_none() {
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
        // eprintln!("Errors: {:#?}", errors);
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
        test("x: A → B = f where { f = g }");
        test(
            r#"
        x: A → B = f where {
            f = g
        }"#,
        );
        test("a b c: d = e");
        test("import \"module.donut\"");
        test("a = \"a\"");
        test("x = 1");
        test("[style[...]] y: *");
        assert!(false);
    }
}
