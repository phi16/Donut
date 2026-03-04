use crate::types::common::*;
use crate::types::semtree;
use crate::types::syntree;
use crate::types::token::Token;
use std::rc::Rc;

trait Convert {
    type Output;
    fn convert(self, c: &mut Converter<'_>) -> Self::Output;
}

impl<T: Convert> Convert for Vec<T> {
    type Output = Vec<T::Output>;
    fn convert(self, c: &mut Converter<'_>) -> Vec<T::Output> {
        self.into_iter().map(|t| t.convert(c)).collect()
    }
}
impl<T: Convert> Convert for Option<T> {
    type Output = Option<T::Output>;
    fn convert(self, c: &mut Converter<'_>) -> Option<T::Output> {
        self.map(|t| t.convert(c))
    }
}
impl<T: Convert> Convert for Box<T> {
    type Output = Box<T::Output>;
    fn convert(self, c: &mut Converter<'_>) -> Box<T::Output> {
        Box::new((*self).convert(c))
    }
}

// --- Convert impls ---

impl Convert for syntree::Name {
    type Output = semtree::Name;
    fn convert(self, _c: &mut Converter<'_>) -> semtree::Name {
        semtree::Name(self.0)
    }
}
impl Convert for syntree::AssignOp {
    type Output = semtree::AssignOp;
    fn convert(self, _c: &mut Converter<'_>) -> semtree::AssignOp {
        match self {
            syntree::AssignOp::Alias => semtree::AssignOp::Alias,
            syntree::AssignOp::Def => semtree::AssignOp::Def,
            syntree::AssignOp::Add => semtree::AssignOp::Add,
        }
    }
}
// Key conversion is handled inline in convert_lit since it needs to handle A<T> unwrapping
impl Convert for syntree::ArrowTy {
    type Output = semtree::ArrowTy;
    fn convert(self, _c: &mut Converter<'_>) -> semtree::ArrowTy {
        match self {
            syntree::ArrowTy::To => semtree::ArrowTy::To,
            syntree::ArrowTy::Eq => semtree::ArrowTy::Eq,
            syntree::ArrowTy::Functor => semtree::ArrowTy::Functor,
        }
    }
}
impl Convert for syntree::Op {
    type Output = semtree::Op;
    fn convert(self, c: &mut Converter<'_>) -> semtree::Op {
        match self {
            syntree::Op::CompRep(i) | syntree::Op::CompLit(i) => semtree::Op::Comp(i),
            syntree::Op::CompStar => semtree::Op::CompStar,
            syntree::Op::Arrow(ty) => semtree::Op::Arrow(ty.convert(c)),
        }
    }
}

// --- Converter ---

struct Converter<'a> {
    tokens: &'a [Token<'a>],
    errors: Vec<Error>,
}

impl<'a> Converter<'a> {
    fn new(tokens: &'a [Token<'a>]) -> Self {
        Self {
            tokens,
            errors: Vec::new(),
        }
    }

    fn error_at(&mut self, span: &TokenSpan, msg: impl Into<String>) {
        if let Some(token) = self.tokens.get(span.start) {
            self.errors.push((token.pos.clone(), msg.into()));
        }
    }

    // --- Val conversion ---

    /// Convert A<syntree::Val> → Option<S<semtree::Val>>. None on error.
    fn convert_val_a(&mut self, val_a: A<syntree::Val>) -> Option<S<semtree::Val>> {
        match val_a {
            A::Accepted(val, _) => self.convert_val(val),
            A::Error(_) => None,
        }
    }

    /// Convert A<syntree::Val> → S<semtree::Val>, using Val::Any for errors.
    fn convert_val_a_or_any(&mut self, val_a: A<syntree::Val>) -> S<semtree::Val> {
        match val_a {
            A::Accepted(val, span) => self.convert_val(val)
                .unwrap_or_else(|| S(semtree::Val::Any, span)),
            A::Error(span) => S(semtree::Val::Any, span),
        }
    }

    fn convert_val(&mut self, val: syntree::Val) -> Option<S<semtree::Val>> {
        self.build_val_tree(val.vs, val.ops)
    }

    fn convert_val0(&mut self, val0: syntree::Val0, span: &TokenSpan) -> Option<semtree::Val> {
        match val0 {
            syntree::Val0::Path(path_a) => match *path_a {
                A::Accepted(path, path_span) => {
                    if let Some(num_str) = try_as_number_literal(&path) {
                        Some(semtree::Val::Lit(S(
                            semtree::Lit::Number(num_str),
                            path_span,
                        )))
                    } else {
                        let converted = self.convert_path_val(path);
                        Some(semtree::Val::Path(Box::new(S(converted, path_span))))
                    }
                }
                A::Error(_) => None,
            }
            syntree::Val0::Lit(lit_a) => match lit_a {
                A::Accepted(lit, lit_span) => {
                    let converted = self.convert_lit(lit);
                    Some(semtree::Val::Lit(S(converted, lit_span)))
                }
                A::Error(_) => None,
            },
            syntree::Val0::Paren(val_a) => match *val_a {
                A::Accepted(inner, _) => self.convert_val(inner).map(|s| s.0),
                A::Error(_) => None,
            },
            syntree::Val0::Dots => {
                self.error_at(span, "found `...` in value");
                None
            }
        }
    }

    fn convert_lit(&mut self, lit: syntree::Lit) -> semtree::Lit {
        match lit {
            syntree::Lit::Number(s) => semtree::Lit::Number(s),
            syntree::Lit::String(s) => semtree::Lit::String(s),
            syntree::Lit::Array(vs) => {
                semtree::Lit::Array(vs.into_iter().map(|v| self.convert_val_a_or_any(v)).collect())
            }
            syntree::Lit::Object(kvs) => semtree::Lit::Object(
                kvs.into_iter()
                    .filter_map(|(k, v)| {
                        let key = match k {
                            A::Accepted(k, span) => {
                                let sem_key = match k {
                                    syntree::Key::Name(n) => match n {
                                        A::Accepted(n, _) => semtree::Key::Name(semtree::Name(n.0)),
                                        A::Error(_) => return None,
                                    },
                                    syntree::Key::String(s) => match s {
                                        A::Accepted(s, _) => semtree::Key::String(s),
                                        A::Error(_) => return None,
                                    },
                                };
                                S(sem_key, span)
                            }
                            A::Error(_) => return None,
                        };
                        let val = self.convert_val_a_or_any(v);
                        Some((key, val))
                    })
                    .collect(),
            ),
        }
    }

    fn build_val_tree(
        &mut self,
        mut vs: Vec<A<syntree::Val0>>,
        mut ops: Vec<(A<syntree::Op>, Option<A<syntree::Params>>)>,
    ) -> Option<S<semtree::Val>> {
        if ops.is_empty() {
            let v0_a = vs.into_iter().next()?;
            return match v0_a {
                A::Accepted(v0, span) => {
                    let val = self.convert_val0(v0, &span)?;
                    Some(S(val, span))
                }
                A::Error(_) => None,
            };
        }

        let split = self.find_weakest_op(&ops)?;

        let right_vs = vs.split_off(split + 1);
        let right_ops = ops.split_off(split + 1);
        let (op_a, params_a) = ops.pop().unwrap();

        let left = self.build_val_tree(vs, ops)?;
        let right = self.build_val_tree(right_vs, right_ops)?;

        let op = match op_a {
            A::Accepted(op, span) => S(op.convert(self), span),
            A::Error(_) => return None,
        };
        let params = params_a.and_then(|p| match p {
            A::Accepted(params, _) => Some(self.convert_params_val(params)),
            A::Error(_) => None,
        });

        let span = TokenSpan {
            start: left.1.start,
            end: right.1.end,
        };
        Some(S(
            semtree::Val::Op(Box::new(left), op, params, Box::new(right)),
            span,
        ))
    }

    /// Find the index of the weakest (lowest-precedence) operator.
    /// For left-associativity, picks the rightmost among ties.
    fn find_weakest_op(
        &mut self,
        ops: &[(A<syntree::Op>, Option<A<syntree::Params>>)],
    ) -> Option<usize> {
        // Reject if any op is A::Error
        if ops.iter().any(|(op_a, _)| matches!(op_a, A::Error(_))) {
            return None;
        }

        // CompStar/Arrow are the weakest; at most one is allowed
        let weakest: Vec<usize> = ops
            .iter()
            .enumerate()
            .filter(|(_, (op_a, _))| {
                matches!(
                    op_a,
                    A::Accepted(syntree::Op::CompStar | syntree::Op::Arrow(_), _)
                )
            })
            .map(|(i, _)| i)
            .collect();

        if weakest.len() > 1 {
            if let A::Accepted(_, span) = &ops[weakest[1]].0 {
                self.error_at(span, "cannot chain `*` or arrow operators");
            }
            return None;
        }
        if weakest.len() == 1 {
            return Some(weakest[0]);
        }

        // All ops are Comp(i). Find rightmost with highest weakness (left-associative).
        let mut max_w = 0u32;
        let mut max_i = 0;
        for (i, (op_a, _)) in ops.iter().enumerate() {
            if let A::Accepted(op, _) = op_a {
                let w = match op {
                    syntree::Op::CompRep(w) | syntree::Op::CompLit(w) => *w,
                    _ => unreachable!(),
                };
                if w >= max_w {
                    max_w = w;
                    max_i = i;
                }
            }
        }
        Some(max_i)
    }

    // --- Decls (main==None: forward decorators to next Decl) ---

    fn convert_decls(&mut self, decls: Vec<A<syntree::Decl>>) -> Vec<semtree::Decl> {
        let mut result: Vec<semtree::Decl> = Vec::new();
        let mut pending_decos: Vec<semtree::Decorator> = Vec::new();

        for decl_a in decls {
            match decl_a {
                A::Accepted(decl, span) => {
                    if decl.main.is_none() {
                        if !decl.clauses.is_empty() {
                            self.error_at(
                                &span,
                                "clause is not allowed on a declaration without a body",
                            );
                        }
                        pending_decos.extend(self.convert_decorators(decl.decos));
                    } else {
                        match self.convert_decl(decl, span) {
                            Some(mut sem_decl) => {
                                if !pending_decos.is_empty() {
                                    let mut carried = std::mem::take(&mut pending_decos);
                                    carried.append(&mut sem_decl.decos);
                                    sem_decl.decos = carried;
                                }
                                result.push(sem_decl);
                            }
                            None => {
                                pending_decos.clear();
                            }
                        }
                    }
                }
                A::Error(_) => {}
            }
        }

        if !pending_decos.is_empty() {
            // Try to find a span from the last decorator
            let span = pending_decos.iter().rev().find_map(|d| match d {
                semtree::Decorator::Param(_, v) => Some(v.1.clone()),
                semtree::Decorator::Deco(v) => Some(v.1.clone()),
            });
            if let Some(span) = span {
                self.error_at(&span, "decorator has no following declaration");
            }
        }

        result
    }

    // --- Decl ---

    fn convert_decl(&mut self, decl: syntree::Decl, span: TokenSpan) -> Option<semtree::Decl> {
        let decos = self.convert_decorators(decl.decos);

        let main = match decl.main {
            Some(m) => m,
            None => {
                self.error_at(&span, "missing declaration body");
                return None;
            }
        };

        let main = match self.convert_decl_main(main, &span) {
            Some(m) => m,
            None => return None,
        };

        let (with_clauses, where_clauses) = self.convert_clauses(decl.clauses);

        Some(semtree::Decl {
            decos,
            main,
            with_clauses,
            where_clauses,
        })
    }

    fn convert_decl_main(
        &mut self,
        main: syntree::DeclMain,
        parent_span: &TokenSpan,
    ) -> Option<semtree::DeclMain> {
        match main {
            syntree::DeclMain::Unit(unit_a) => match unit_a {
                A::Accepted(unit, span) => self.convert_decl_unit(unit, &span)
                    .map(semtree::DeclMain::Unit),
                A::Error(_) => None,
            },
            syntree::DeclMain::Mod(mod_a) => match mod_a {
                A::Accepted(m, span) => {
                    let converted = m.convert(self);
                    Some(semtree::DeclMain::Mod(S(converted, span)))
                }
                A::Error(_) => None,
            },
            syntree::DeclMain::Dots => {
                self.error_at(parent_span, "found `...` in declaration");
                None
            }
        }
    }

    fn convert_decl_unit(
        &mut self,
        unit: syntree::DeclUnit,
        span: &TokenSpan,
    ) -> Option<semtree::DeclUnit> {
        let names: Vec<S<semtree::Path<semtree::ParamDecl>>> = unit
            .names
            .into_iter()
            .filter_map(|p| match p {
                A::Accepted(path, span) => Some(S(self.convert_path_decl(path), span)),
                A::Error(_) => None,
            })
            .collect();

        let ty = unit.ty.and_then(|v| self.convert_val_a(v));

        // Multiple names only allowed in declaration-only form (no assignment)
        if names.len() > 1 && unit.assign.is_some() {
            self.error_at(span, "multiple names are only allowed in declaration-only form");
        }

        match unit.assign {
            Some((op_a, val_mod)) => {
                let op = match op_a {
                    A::Accepted(op, span) => S(op.convert(self), span),
                    A::Error(_) => return None,
                };
                let body = Some(self.convert_valmod(val_mod));
                Some(semtree::DeclUnit {
                    names,
                    ty,
                    op,
                    body,
                })
            }
            None => {
                if ty.is_some() {
                    Some(semtree::DeclUnit {
                        names,
                        ty,
                        op: S(semtree::AssignOp::Decl, span.clone()),
                        body: None,
                    })
                } else {
                    self.error_at(span, "missing assignment in declaration");
                    None
                }
            }
        }
    }

    fn convert_valmod(&mut self, vm: syntree::ValMod) -> semtree::ValMod {
        match vm {
            syntree::ValMod::Val(v) => {
                semtree::ValMod::Val(self.convert_val_a_or_any(v))
            }
            syntree::ValMod::Mod(m) => match m {
                A::Accepted(m, span) => {
                    semtree::ValMod::Mod(S(m.convert(self), span))
                }
                A::Error(span) => {
                    semtree::ValMod::Mod(S(
                        semtree::Module::Block(vec![]),
                        span,
                    ))
                }
            },
        }
    }

    // --- Clauses ---

    fn convert_clauses(
        &mut self,
        clauses: Vec<A<syntree::Clause>>,
    ) -> (Vec<S<semtree::Module>>, Vec<S<semtree::Module>>) {
        let mut with_clauses = Vec::new();
        let mut where_clauses = Vec::new();
        let mut seen_where = false;

        for clause_a in clauses {
            match clause_a {
                A::Accepted(syntree::Clause(ty_a, mod_a), _) => {
                    let converted = match mod_a {
                        A::Accepted(m, span) => Some(S(m.convert(self), span)),
                        A::Error(_) => None,
                    };
                    if let Some(converted) = converted {
                        match ty_a {
                            A::Accepted(syntree::ClauseTy::With, ty_span) => {
                                if seen_where {
                                    self.error_at(
                                        &ty_span,
                                        "`with` clause must come before `where` clause",
                                    );
                                }
                                with_clauses.push(converted);
                            }
                            A::Accepted(syntree::ClauseTy::Where, _) => {
                                seen_where = true;
                                self.check_where_alias_only(&converted);
                                where_clauses.push(converted);
                            }
                            A::Error(_) => {}
                        }
                    }
                }
                A::Error(_) => {}
            }
        }

        (with_clauses, where_clauses)
    }

    fn check_where_alias_only(&mut self, module: &S<semtree::Module>) {
        let S(semtree::Module::Block(decls), _) = module else {
            return;
        };
        for decl in decls {
            match &decl.main {
                semtree::DeclMain::Unit(unit) => {
                    let S(ref op, ref span) = unit.op;
                    let is_alias_with_body = matches!(op, semtree::AssignOp::Alias) && unit.body.is_some();
                    if !is_alias_with_body {
                        self.error_at(
                            span,
                            "only `=` (alias) is allowed in `where` clause",
                        );
                    }
                    if let Some(semtree::ValMod::Mod(m)) = &unit.body {
                        self.check_where_alias_only(m);
                    }
                }
                semtree::DeclMain::Mod(m) => {
                    self.check_where_alias_only(m);
                }
            }
        }
    }

    // --- Decorators ---

    fn convert_decorators(
        &mut self,
        decos: Vec<A<syntree::Decorator>>,
    ) -> Vec<semtree::Decorator> {
        let mut result = Vec::new();
        for deco_a in decos {
            match deco_a {
                A::Accepted(syntree::Decorator(_open, params, _close), _) => {
                    for param_a in params {
                        match param_a {
                            A::Accepted(param, param_span) => {
                                self.convert_decorator_param(param, param_span, &mut result);
                            }
                            A::Error(_) => {}
                        }
                    }
                }
                A::Error(_) => {}
            }
        }
        result
    }

    fn convert_decorator_param(
        &mut self,
        param: syntree::Param,
        span: TokenSpan,
        result: &mut Vec<semtree::Decorator>,
    ) {
        match param.ty {
            Some(syntree::ParamTy::Decl) => {
                let val = Rc::new(self.convert_val_a_or_any(param.val));
                for name_a in param.names {
                    match name_a {
                        A::Accepted(name, _) => {
                            result.push(semtree::Decorator::Param(
                                name.convert(self),
                                Rc::clone(&val),
                            ));
                        }
                        A::Error(_) => {}
                    }
                }
            }
            Some(syntree::ParamTy::Named) => {
                self.error_at(&span, "named parameter is not allowed in decorator");
            }
            None => {
                let val = self.convert_val_a_or_any(param.val);
                result.push(semtree::Decorator::Deco(val));
            }
        }
    }

    // --- Path/Segment (generic) ---

    fn convert_path<P>(
        &mut self,
        path: syntree::Path,
        mut convert_seg: impl FnMut(&mut Self, syntree::Segment) -> Option<semtree::Segment<P>>,
    ) -> semtree::Path<P> {
        let segments = path
            .0
            .into_iter()
            .filter_map(|s| match s {
                A::Accepted(seg, span) => convert_seg(self, seg).map(|sd| S(sd, span)),
                A::Error(_) => None,
            })
            .collect();
        let val = path.1.and_then(|v| self.convert_val_a(v));
        semtree::Path(segments, val)
    }

    fn convert_segment<P>(
        &mut self,
        seg: syntree::Segment,
        convert_params: impl FnOnce(&mut Self, syntree::Params) -> semtree::Params<P>,
        default_params: impl FnOnce() -> semtree::Params<P>,
    ) -> Option<semtree::Segment<P>> {
        let name = match seg.0 {
            A::Accepted(name, _) => name.convert(self),
            A::Error(_) => return None,
        };
        let params = match seg.1 {
            Some(params_a) => match params_a {
                A::Accepted(params, _) => convert_params(self, params),
                A::Error(_) => default_params(),
            },
            None => default_params(),
        };
        Some(semtree::Segment(name, params))
    }

    fn convert_path_decl(&mut self, path: syntree::Path) -> semtree::Path<semtree::ParamDecl> {
        self.convert_path(path, |c, seg| {
            c.convert_segment(
                seg,
                |c, p| c.convert_params_decl(p),
                || semtree::Params(vec![]),
            )
        })
    }

    fn convert_path_val(&mut self, path: syntree::Path) -> semtree::Path<semtree::ParamVal> {
        self.convert_path(path, |c, seg| {
            c.convert_segment(
                seg,
                |c, p| c.convert_params_val(p),
                || semtree::Params(vec![]),
            )
        })
    }

    fn convert_params_decl(&mut self, params: syntree::Params) -> semtree::Params<semtree::ParamDecl> {
        let syntree::Params(_open, param_list, _close) = params;
        let mut result = Vec::new();
        for param_a in param_list {
            match param_a {
                A::Accepted(param, span) => match param.ty {
                    Some(syntree::ParamTy::Decl) => {
                        let ty = Rc::new(self.convert_val_a_or_any(param.val));
                        for name_a in param.names {
                            match name_a {
                                A::Accepted(name, _) => {
                                    result.push(semtree::ParamDecl {
                                        name: name.convert(self),
                                        ty: Rc::clone(&ty),
                                    });
                                }
                                A::Error(_) => {}
                            }
                        }
                    }
                    Some(syntree::ParamTy::Named) => {
                        self.error_at(
                            &span,
                            "named parameter is not allowed in declaration context",
                        );
                    }
                    None => {
                        self.error_at(
                            &span,
                            "type annotation is required for declaration parameter",
                        );
                    }
                },
                A::Error(_) => {}
            }
        }
        semtree::Params(result)
    }

    fn convert_params_val(&mut self, params: syntree::Params) -> semtree::Params<semtree::ParamVal> {
        let syntree::Params(_open, param_list, _close) = params;
        let mut result = Vec::new();
        for param_a in param_list {
            match param_a {
                A::Accepted(param, span) => match param.ty {
                    Some(syntree::ParamTy::Named) => {
                        if param.names.len() != 1 {
                            self.error_at(
                                &span,
                                "named parameter must have exactly one name",
                            );
                        }
                        let name = param
                            .names
                            .into_iter()
                            .next()
                            .and_then(|n| match n {
                                A::Accepted(n, _) => Some(n.convert(self)),
                                A::Error(_) => None,
                            });
                        let val = self.convert_val_a_or_any(param.val);
                        result.push(semtree::ParamVal { name, val });
                    }
                    None => {
                        let val = self.convert_val_a_or_any(param.val);
                        result.push(semtree::ParamVal { name: None, val });
                    }
                    Some(syntree::ParamTy::Decl) => {
                        self.error_at(
                            &span,
                            "declaration parameter is not allowed in value context",
                        );
                    }
                },
                A::Error(_) => {}
            }
        }
        semtree::Params(result)
    }
}

impl Convert for syntree::Module {
    type Output = semtree::Module;
    fn convert(self, c: &mut Converter<'_>) -> semtree::Module {
        match self {
            syntree::Module::Block(decls) => semtree::Module::Block(c.convert_decls(decls)),
            syntree::Module::Import(lit) => match lit {
                A::Accepted(lit, span) => {
                    semtree::Module::Import(S(c.convert_lit(lit), span))
                }
                A::Error(_) => semtree::Module::Block(vec![]),
            },
        }
    }
}

pub fn is_number_str(s: &str) -> bool {
    let s = s.strip_prefix('-').unwrap_or(s);
    !s.is_empty() && s.chars().all(|c| c.is_ascii_digit())
}

fn try_as_number_literal(path: &syntree::Path) -> Option<String> {
    // Single segment, no params, no suffix
    if path.0.len() != 1 || path.1.is_some() {
        return None;
    }
    let seg = match &path.0[0] {
        A::Accepted(seg, _) => seg,
        A::Error(_) => return None,
    };
    if seg.1.is_some() {
        return None;
    }
    let name = match &seg.0 {
        A::Accepted(name, _) => &name.0,
        A::Error(_) => return None,
    };
    if is_number_str(name) {
        Some(name.clone())
    } else {
        None
    }
}

pub fn convert<'a>(
    program: A<syntree::Program>,
    tokens: &[Token<'a>],
) -> (semtree::Program, Vec<Error>) {
    let mut converter = Converter::new(tokens);
    let result = match program {
        A::Accepted(prog, _) => {
            semtree::Program(converter.convert_decls(prog.0))
        }
        A::Error(_) => semtree::Program(vec![]),
    };
    (result, converter.errors)
}
