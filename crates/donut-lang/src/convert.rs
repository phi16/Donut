use crate::types::common::*;
use crate::types::semtree;
use crate::types::syntree;
use crate::types::token::Token;
use std::rc::Rc;

fn map_a<T, U>(a: A<T>, f: impl FnOnce(T) -> U) -> A<U> {
    match a {
        A::Accepted(v, span) => A::Accepted(f(v), span),
        A::Error() => A::Error(),
    }
}

trait Convert {
    type Output;
    fn convert(self, c: &mut Converter<'_>) -> Self::Output;
}

impl<T: Convert> Convert for A<T> {
    type Output = A<T::Output>;
    fn convert(self, c: &mut Converter<'_>) -> A<T::Output> {
        match self {
            A::Accepted(v, span) => A::Accepted(v.convert(c), span),
            A::Error() => A::Error(),
        }
    }
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
impl Convert for syntree::Key {
    type Output = semtree::Key;
    fn convert(self, c: &mut Converter<'_>) -> semtree::Key {
        match self {
            syntree::Key::Name(n) => semtree::Key::Name(n.convert(c)),
            syntree::Key::String(s) => semtree::Key::String(s),
        }
    }
}
impl Convert for syntree::Lit {
    type Output = semtree::Lit;
    fn convert(self, c: &mut Converter<'_>) -> semtree::Lit {
        match self {
            syntree::Lit::Number(s) => semtree::Lit::Number(s),
            syntree::Lit::String(s) => semtree::Lit::String(s),
            syntree::Lit::Array(vs) => {
                semtree::Lit::Array(vs.into_iter().map(|v| c.convert_val_a(v)).collect())
            }
            syntree::Lit::Object(kvs) => semtree::Lit::Object(
                kvs.into_iter()
                    .map(|(k, v)| (k.convert(c), c.convert_val_a(v)))
                    .collect(),
            ),
        }
    }
}
impl Convert for syntree::Module {
    type Output = semtree::Module;
    fn convert(self, c: &mut Converter<'_>) -> semtree::Module {
        match self {
            syntree::Module::Block(decls) => semtree::Module::Block(c.convert_decls(decls)),
            syntree::Module::Import(lit) => semtree::Module::Import(lit.convert(c)),
        }
    }
}
impl Convert for syntree::ValMod {
    type Output = semtree::ValMod;
    fn convert(self, c: &mut Converter<'_>) -> semtree::ValMod {
        match self {
            syntree::ValMod::Val(v) => semtree::ValMod::Val(c.convert_val_a(v)),
            syntree::ValMod::Mod(m) => semtree::ValMod::Mod(m.convert(c)),
        }
    }
}
impl Convert for syntree::Program {
    type Output = semtree::Program;
    fn convert(self, c: &mut Converter<'_>) -> semtree::Program {
        semtree::Program(c.convert_decls(self.0))
    }
}
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

    // --- Decls (main==None: forward decorators to next Decl) ---

    fn convert_decls(&mut self, decls: Vec<A<syntree::Decl>>) -> Vec<A<semtree::Decl>> {
        let mut result: Vec<A<semtree::Decl>> = Vec::new();
        let mut pending_decos: Vec<A<semtree::Decorator>> = Vec::new();

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
                        let mut sem_decl = self.convert_decl(decl, span);
                        if !pending_decos.is_empty() {
                            if let A::Accepted(ref mut d, _) = sem_decl {
                                let mut carried = std::mem::take(&mut pending_decos);
                                carried.append(&mut d.decos);
                                d.decos = carried;
                            } else {
                                pending_decos.clear();
                            }
                        }
                        result.push(sem_decl);
                    }
                }
                A::Error() => {
                    result.push(A::Error());
                }
            }
        }

        if !pending_decos.is_empty() {
            let span = pending_decos.iter().rev().find_map(|d| match d {
                A::Accepted(_, s) => Some(s.clone()),
                A::Error() => None,
            });
            if let Some(span) = span {
                self.error_at(&span, "decorator has no following declaration");
            }
        }

        result
    }

    // --- Decl ---

    fn convert_decl(&mut self, decl: syntree::Decl, span: TokenSpan) -> A<semtree::Decl> {
        let decos = self.convert_decorators(decl.decos);

        let main = match decl.main {
            Some(m) => m,
            None => {
                self.error_at(&span, "missing declaration body");
                return A::Error();
            }
        };

        let main = match self.convert_decl_main(main, &span) {
            Some(m) => m,
            None => return A::Error(),
        };

        let (with_clauses, where_clauses) = self.convert_clauses(decl.clauses);

        A::Accepted(
            semtree::Decl {
                decos,
                main,
                with_clauses,
                where_clauses,
            },
            span,
        )
    }

    fn convert_decl_main(
        &mut self,
        main: syntree::DeclMain,
        parent_span: &TokenSpan,
    ) -> Option<semtree::DeclMain> {
        match main {
            syntree::DeclMain::Unit(unit_a) => {
                let converted = match unit_a {
                    A::Accepted(unit, span) => match self.convert_decl_unit(unit, &span) {
                        Some(u) => A::Accepted(u, span),
                        None => A::Error(),
                    },
                    A::Error() => A::Error(),
                };
                Some(semtree::DeclMain::Unit(converted))
            }
            syntree::DeclMain::Mod(mod_a) => Some(semtree::DeclMain::Mod(mod_a.convert(self))),
            syntree::DeclMain::Dots => {
                self.error_at(parent_span, "`...` cannot be used as a declaration");
                None
            }
        }
    }

    fn convert_decl_unit(
        &mut self,
        unit: syntree::DeclUnit,
        span: &TokenSpan,
    ) -> Option<semtree::DeclUnit> {
        let names = unit
            .names
            .into_iter()
            .map(|p| map_a(p, |path| self.convert_path_decl(path)))
            .collect();

        let ty = unit.ty.map(|v| self.convert_val_a(v));

        match unit.assign {
            Some((op_a, val_mod)) => {
                let op = op_a.convert(self);
                let body = val_mod.convert(self);
                Some(semtree::DeclUnit {
                    names,
                    ty,
                    op,
                    body,
                })
            }
            None => {
                self.error_at(span, "missing assignment in declaration");
                None
            }
        }
    }

    // --- Clauses ---

    fn convert_clauses(
        &mut self,
        clauses: Vec<A<syntree::Clause>>,
    ) -> (Vec<A<semtree::Module>>, Vec<A<semtree::Module>>) {
        let mut with_clauses = Vec::new();
        let mut where_clauses = Vec::new();
        let mut seen_where = false;

        for clause_a in clauses {
            match clause_a {
                A::Accepted(syntree::Clause(ty_a, mod_a), _) => {
                    let converted = mod_a.convert(self);
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
                            where_clauses.push(converted);
                        }
                        A::Error() => {}
                    }
                }
                A::Error() => {}
            }
        }

        (with_clauses, where_clauses)
    }

    // --- Decorators ---

    fn convert_decorators(
        &mut self,
        decos: Vec<A<syntree::Decorator>>,
    ) -> Vec<A<semtree::Decorator>> {
        let mut result = Vec::new();
        for deco_a in decos {
            match deco_a {
                A::Accepted(syntree::Decorator(_open, params, _close), _) => {
                    for param_a in params {
                        match param_a {
                            A::Accepted(param, param_span) => {
                                self.convert_decorator_param(param, param_span, &mut result);
                            }
                            A::Error() => result.push(A::Error()),
                        }
                    }
                }
                A::Error() => result.push(A::Error()),
            }
        }
        result
    }

    fn convert_decorator_param(
        &mut self,
        param: syntree::Param,
        span: TokenSpan,
        result: &mut Vec<A<semtree::Decorator>>,
    ) {
        match param.ty {
            Some(syntree::ParamTy::Decl) => {
                let val = Rc::new(self.convert_val_a(param.val));
                for name_a in param.names {
                    let name_a = name_a.convert(self);
                    result.push(A::Accepted(
                        semtree::Decorator::Param(name_a, Rc::clone(&val)),
                        span.clone(),
                    ));
                }
            }
            Some(syntree::ParamTy::Named) => {
                self.error_at(&span, "named parameter is not allowed in decorator");
            }
            None => {
                let val = self.convert_val_a(param.val);
                result.push(A::Accepted(semtree::Decorator::Deco(val), span));
            }
        }
    }

    // --- Path (declaration context) ---

    fn convert_path_decl(&mut self, path: syntree::Path) -> semtree::PathDecl {
        let segments = path
            .0
            .into_iter()
            .map(|s| map_a(s, |seg| self.convert_segment_decl(seg)))
            .collect();
        let val = path.1.map(|v| self.convert_val_a(v));
        semtree::PathDecl(segments, val)
    }

    fn convert_segment_decl(&mut self, seg: syntree::Segment) -> semtree::SegmentDecl {
        let name = seg.0.convert(self);
        let params = match seg.1 {
            Some(params_a) => match params_a {
                A::Accepted(params, _) => self.convert_params_decl(params),
                A::Error() => semtree::ParamsDecl(vec![]),
            },
            None => semtree::ParamsDecl(vec![]),
        };
        semtree::SegmentDecl(name, params)
    }

    fn convert_params_decl(&mut self, params: syntree::Params) -> semtree::ParamsDecl {
        let syntree::Params(_open, param_list, _close) = params;
        let mut result = Vec::new();
        for param_a in param_list {
            match param_a {
                A::Accepted(param, span) => match param.ty {
                    Some(syntree::ParamTy::Decl) => {
                        let ty = Rc::new(self.convert_val_a(param.val));
                        for name_a in param.names {
                            let name_a = name_a.convert(self);
                            result.push(A::Accepted(
                                semtree::ParamDecl {
                                    name: name_a,
                                    ty: Rc::clone(&ty),
                                },
                                span.clone(),
                            ));
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
                A::Error() => result.push(A::Error()),
            }
        }
        semtree::ParamsDecl(result)
    }

    // --- Path (value context) ---

    fn convert_path_val(&mut self, path: syntree::Path) -> semtree::Path {
        let segments = path
            .0
            .into_iter()
            .map(|s| map_a(s, |seg| self.convert_segment_val(seg)))
            .collect();
        let val = path.1.map(|v| self.convert_val_a(v));
        semtree::Path(segments, val)
    }

    fn convert_segment_val(&mut self, seg: syntree::Segment) -> semtree::Segment {
        let name = seg.0.convert(self);
        let params = match seg.1 {
            Some(params_a) => match params_a {
                A::Accepted(params, _) => self.convert_params_val(params),
                A::Error() => semtree::ParamsVal(vec![]),
            },
            None => semtree::ParamsVal(vec![]),
        };
        semtree::Segment(name, params)
    }

    fn convert_params_val(&mut self, params: syntree::Params) -> semtree::ParamsVal {
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
                            .map(|n| n.convert(self));
                        let val = self.convert_val_a(param.val);
                        result.push(A::Accepted(semtree::ParamVal { name, val }, span));
                    }
                    None => {
                        let val = self.convert_val_a(param.val);
                        result.push(A::Accepted(
                            semtree::ParamVal { name: None, val },
                            span,
                        ));
                    }
                    Some(syntree::ParamTy::Decl) => {
                        self.error_at(
                            &span,
                            "declaration parameter is not allowed in value context",
                        );
                    }
                },
                A::Error() => result.push(A::Error()),
            }
        }
        semtree::ParamsVal(result)
    }

    // --- Val ---

    fn convert_val_a(&mut self, val_a: A<syntree::Val>) -> A<semtree::Val> {
        match val_a {
            A::Accepted(val, span) => match self.convert_val(val) {
                Some(v) => A::Accepted(v, span),
                None => A::Error(),
            },
            A::Error() => A::Error(),
        }
    }

    fn convert_val(&mut self, val: syntree::Val) -> Option<semtree::Val> {
        self.build_val_tree(val.vs, val.ops)
    }

    fn convert_val0(&mut self, val0: syntree::Val0, span: &TokenSpan) -> Option<semtree::Val> {
        match val0 {
            syntree::Val0::Path(path_a) => {
                let path_a = map_a(*path_a, |p| self.convert_path_val(p));
                Some(semtree::Val::Path(Box::new(path_a)))
            }
            syntree::Val0::Lit(lit_a) => Some(semtree::Val::Lit(lit_a.convert(self))),
            syntree::Val0::Paren(val_a) => match *val_a {
                A::Accepted(inner, _) => self.convert_val(inner),
                A::Error() => None,
            },
            syntree::Val0::Dots => {
                self.error_at(span, "`...` cannot be used as a value");
                None
            }
        }
    }

    fn build_val_tree(
        &mut self,
        mut vs: Vec<A<syntree::Val0>>,
        mut ops: Vec<(A<syntree::Op>, Option<A<syntree::Params>>)>,
    ) -> Option<semtree::Val> {
        if ops.is_empty() {
            let v0_a = vs.into_iter().next()?;
            return match v0_a {
                A::Accepted(v0, span) => self.convert_val0(v0, &span),
                A::Error() => None,
            };
        }

        let split = self.find_weakest_op(&ops)?;

        let right_vs = vs.split_off(split + 1);
        let right_ops = ops.split_off(split + 1);
        let (op_a, params_a) = ops.pop().unwrap();

        let left = self.build_val_tree(vs, ops)?;
        let right = self.build_val_tree(right_vs, right_ops)?;

        let op = op_a.convert(self);
        let params = params_a.map(|p| map_a(p, |params| self.convert_params_val(params)));

        Some(semtree::Val::Op(Box::new(left), op, params, Box::new(right)))
    }

    /// Find the index of the weakest (lowest-precedence) operator.
    /// For left-associativity, picks the rightmost among ties.
    fn find_weakest_op(
        &mut self,
        ops: &[(A<syntree::Op>, Option<A<syntree::Params>>)],
    ) -> Option<usize> {
        // Reject if any op is A::Error
        if ops.iter().any(|(op_a, _)| matches!(op_a, A::Error())) {
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
}

pub fn convert<'a>(
    program: A<syntree::Program>,
    tokens: &[Token<'a>],
) -> (A<semtree::Program>, Vec<Error>) {
    let mut converter = Converter::new(tokens);
    let result = program.convert(&mut converter);
    (result, converter.errors)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::parse;
    use crate::tokenize::tokenize;

    // --- Minimal pretty printer for semtree ---

    struct PP(String);
    impl PP {
        fn s(&mut self, s: &str) { self.0.push_str(s); }
    }

    trait Fmt { fn fmt(&self, p: &mut PP); }

    impl<T: Fmt> Fmt for A<T> {
        fn fmt(&self, p: &mut PP) {
            match self { A::Accepted(v, _) => v.fmt(p), A::Error() => p.s("?") }
        }
    }
    impl Fmt for semtree::Name {
        fn fmt(&self, p: &mut PP) { p.s(&self.0); }
    }
    impl Fmt for String {
        fn fmt(&self, p: &mut PP) { p.s(self); }
    }
    impl Fmt for semtree::Op {
        fn fmt(&self, p: &mut PP) {
            match self {
                semtree::Op::Comp(0) => p.s(" "),
                semtree::Op::Comp(n) => { p.s(" "); p.s(&";".repeat(*n as usize)); p.s(" "); }
                semtree::Op::CompStar => p.s(" ;* "),
                semtree::Op::Arrow(semtree::ArrowTy::To) => p.s(" → "),
                semtree::Op::Arrow(semtree::ArrowTy::Eq) => p.s(" ~ "),
                semtree::Op::Arrow(semtree::ArrowTy::Functor) => p.s(" ~> "),
            }
        }
    }
    impl Fmt for semtree::ParamDecl {
        fn fmt(&self, p: &mut PP) { self.name.fmt(p); p.s(": "); self.ty.fmt(p); }
    }
    impl Fmt for semtree::ParamVal {
        fn fmt(&self, p: &mut PP) {
            if let Some(n) = &self.name { n.fmt(p); p.s(" = "); }
            self.val.fmt(p);
        }
    }
    fn fmt_params_decl(params: &semtree::ParamsDecl, p: &mut PP) {
        if params.0.is_empty() { return; }
        p.s("[");
        for (i, x) in params.0.iter().enumerate() { if i > 0 { p.s(", "); } x.fmt(p); }
        p.s("]");
    }
    fn fmt_params_val(params: &semtree::ParamsVal, p: &mut PP) {
        if params.0.is_empty() { return; }
        p.s("[");
        for (i, x) in params.0.iter().enumerate() { if i > 0 { p.s(", "); } x.fmt(p); }
        p.s("]");
    }
    impl Fmt for semtree::SegmentDecl {
        fn fmt(&self, p: &mut PP) { self.0.fmt(p); fmt_params_decl(&self.1, p); }
    }
    impl Fmt for semtree::Segment {
        fn fmt(&self, p: &mut PP) { self.0.fmt(p); fmt_params_val(&self.1, p); }
    }
    impl Fmt for semtree::PathDecl {
        fn fmt(&self, p: &mut PP) {
            for (i, s) in self.0.iter().enumerate() { if i > 0 { p.s("."); } s.fmt(p); }
            if let Some(v) = &self.1 { p.s("("); v.fmt(p); p.s(")"); }
        }
    }
    impl Fmt for semtree::Path {
        fn fmt(&self, p: &mut PP) {
            for (i, s) in self.0.iter().enumerate() { if i > 0 { p.s("."); } s.fmt(p); }
            if let Some(v) = &self.1 { p.s("("); v.fmt(p); p.s(")"); }
        }
    }
    impl Fmt for semtree::Key {
        fn fmt(&self, p: &mut PP) {
            match self { semtree::Key::Name(n) => n.fmt(p), semtree::Key::String(s) => s.fmt(p) }
        }
    }
    impl Fmt for semtree::Lit {
        fn fmt(&self, p: &mut PP) {
            match self {
                semtree::Lit::Number(n) => p.s(n),
                semtree::Lit::String(s) => p.s(s),
                semtree::Lit::Array(vs) => {
                    p.s("[");
                    for (i, v) in vs.iter().enumerate() { if i > 0 { p.s(", "); } v.fmt(p); }
                    p.s("]");
                }
                semtree::Lit::Object(kvs) => {
                    p.s("{");
                    for (i, (k, v)) in kvs.iter().enumerate() {
                        if i > 0 { p.s(", "); }
                        k.fmt(p); p.s(": "); v.fmt(p);
                    }
                    p.s("}");
                }
            }
        }
    }
    impl Fmt for semtree::Val {
        fn fmt(&self, p: &mut PP) {
            match self {
                semtree::Val::Path(path) => path.fmt(p),
                semtree::Val::Lit(lit) => lit.fmt(p),
                semtree::Val::Op(l, op, params, r) => {
                    p.s("(");
                    l.fmt(p);
                    op.fmt(p);
                    if let Some(pv) = params { pv.fmt(p); }
                    r.fmt(p);
                    p.s(")");
                }
            }
        }
    }
    // A<ParamsVal> in Val::Op
    impl Fmt for semtree::ParamsVal {
        fn fmt(&self, p: &mut PP) { fmt_params_val(self, p); }
    }
    impl Fmt for semtree::AssignOp {
        fn fmt(&self, p: &mut PP) {
            match self {
                semtree::AssignOp::Alias => p.s(" = "),
                semtree::AssignOp::Def => p.s(" := "),
                semtree::AssignOp::Add => p.s(" += "),
            }
        }
    }
    impl Fmt for semtree::Module {
        fn fmt(&self, p: &mut PP) {
            match self {
                semtree::Module::Block(ds) => {
                    p.s("{ ");
                    for (i, d) in ds.iter().enumerate() { if i > 0 { p.s("; "); } d.fmt(p); }
                    p.s(" }");
                }
                semtree::Module::Import(l) => { p.s("import "); l.fmt(p); }
            }
        }
    }
    impl Fmt for semtree::Decorator {
        fn fmt(&self, p: &mut PP) {
            match self {
                semtree::Decorator::Param(n, v) => {
                    p.s("["); n.fmt(p); p.s(": "); v.fmt(p); p.s("]");
                }
                semtree::Decorator::Deco(v) => {
                    p.s("["); v.fmt(p); p.s("]");
                }
            }
        }
    }
    impl Fmt for semtree::DeclUnit {
        fn fmt(&self, p: &mut PP) {
            for (i, n) in self.names.iter().enumerate() { if i > 0 { p.s(" "); } n.fmt(p); }
            if let Some(ty) = &self.ty { p.s(": "); ty.fmt(p); }
            self.op.fmt(p);
            match &self.body {
                semtree::ValMod::Val(v) => v.fmt(p),
                semtree::ValMod::Mod(m) => m.fmt(p),
            }
        }
    }
    impl Fmt for semtree::Decl {
        fn fmt(&self, p: &mut PP) {
            for d in &self.decos { d.fmt(p); p.s(" "); }
            match &self.main {
                semtree::DeclMain::Unit(u) => u.fmt(p),
                semtree::DeclMain::Mod(m) => m.fmt(p),
            }
            for w in &self.with_clauses { p.s(" with "); w.fmt(p); }
            for w in &self.where_clauses { p.s(" where "); w.fmt(p); }
        }
    }
    impl Fmt for semtree::Program {
        fn fmt(&self, p: &mut PP) {
            for d in &self.0 { d.fmt(p); p.s("\n"); }
        }
    }

    fn pretty_sem(prog: &A<semtree::Program>) -> String {
        let mut p = PP(String::new());
        prog.fmt(&mut p);
        p.0
    }

    // --- Test helpers ---

    fn c(code: &str) -> String {
        let (tokens, _, tok_errors) = tokenize(code.trim());
        assert!(tok_errors.is_empty(), "tokenize errors: {tok_errors:?}");
        let (program, parse_errors) = parse(&tokens);
        assert!(parse_errors.is_empty(), "parse errors: {parse_errors:?}");
        let (result, conv_errors) = convert(program, &tokens);
        assert!(conv_errors.is_empty(), "convert errors: {conv_errors:?}");
        pretty_sem(&result)
    }

    fn conv_errs(code: &str) -> Vec<String> {
        let (tokens, _, _) = tokenize(code.trim());
        let (program, _) = parse(&tokens);
        let (_, errors) = convert(program, &tokens);
        errors.into_iter().map(|(_, msg)| msg).collect()
    }

    // --- Tests ---

    #[test]
    fn basic_decl() {
        assert_eq!(c("x = a"), "x = a\n");
        assert_eq!(c("x: T = a"), "x: T = a\n");
        assert_eq!(c("x := a"), "x := a\n");
        assert_eq!(c("x += a"), "x += a\n");
    }

    #[test]
    fn val_single_op() {
        assert_eq!(c("x = a ; b"), "x = (a ; b)\n");
        assert_eq!(c("x = a b"), "x = (a b)\n");
    }

    #[test]
    fn val_precedence() {
        // Comp(1) is stronger than Comp(2)
        assert_eq!(c("x = a ; b ;; c"), "x = ((a ; b) ;; c)\n");
        // Comp(0) is strongest
        assert_eq!(c("x = a b ;; c"), "x = ((a b) ;; c)\n");
        // Stronger op on right side
        assert_eq!(c("x = a ;; b ; c"), "x = (a ;; (b ; c))\n");
    }

    #[test]
    fn val_left_assoc() {
        assert_eq!(c("x = a ; b ; c"), "x = ((a ; b) ; c)\n");
        assert_eq!(c("x = a b c"), "x = ((a b) c)\n");
        assert_eq!(c("x = a ;; b ;; c"), "x = ((a ;; b) ;; c)\n");
    }

    #[test]
    fn val_arrow() {
        assert_eq!(c("x = a → b"), "x = (a → b)\n");
        assert_eq!(c("x = a ~ b"), "x = (a ~ b)\n");
        assert_eq!(c("x = a ~> b"), "x = (a ~> b)\n");
        // Arrow is weakest
        assert_eq!(c("x = a ; b → c"), "x = ((a ; b) → c)\n");
        assert_eq!(c("x = a → b ; c"), "x = (a → (b ; c))\n");
    }

    #[test]
    fn val_comp_star() {
        assert_eq!(c("x = a ;* b"), "x = (a ;* b)\n");
        assert_eq!(c("x = a ; b ;* c"), "x = ((a ; b) ;* c)\n");
    }

    #[test]
    fn val_paren() {
        // Parens override precedence
        assert_eq!(c("x = (a ;; b) ; c"), "x = ((a ;; b) ; c)\n");
        assert_eq!(c("x = a ; (b ;; c)"), "x = (a ; (b ;; c))\n");
    }

    #[test]
    fn val_literals() {
        assert_eq!(c("x = 42"), "x = 42\n");
        assert_eq!(c("x = \"hello\""), "x = \"hello\"\n");
        assert_eq!(c("x = [1, 2, 3]"), "x = [1, 2, 3]\n");
        // {a: 1} is parsed as a module block, not an object literal
    }

    #[test]
    fn path_with_val_params() {
        assert_eq!(c("x = f[a]"), "x = f[a]\n");
        assert_eq!(c("x = f[a, b]"), "x = f[a, b]\n");
    }

    #[test]
    fn path_with_named_params() {
        assert_eq!(c("x = f[n = a]"), "x = f[n = a]\n");
    }

    #[test]
    fn decl_params() {
        assert_eq!(c("f[x: T] = a"), "f[x: T] = a\n");
        assert_eq!(c("f[x: T, y: U] = a"), "f[x: T, y: U] = a\n");
    }

    #[test]
    fn decl_params_fan_out() {
        // Multiple names with same type get fanned out
        assert_eq!(c("f[x y: T] = a"), "f[x: T, y: T] = a\n");
    }

    #[test]
    fn dotted_path() {
        assert_eq!(c("x = a.b.c"), "x = a.b.c\n");
        assert_eq!(c("a.b = c"), "a.b = c\n");
    }

    #[test]
    fn decorators() {
        assert_eq!(c("[v] x = a"), "[v] x = a\n");
        assert_eq!(c("[n: T] x = a"), "[n: T] x = a\n");
    }

    #[test]
    fn module_block() {
        assert_eq!(c("x = {\n    a = b\n}"), "x = { a = b }\n");
    }

    #[test]
    fn clauses() {
        assert_eq!(
            c("x = a\n  with {\n    y = b\n  }"),
            "x = a with { y = b }\n"
        );
        assert_eq!(
            c("x = a\n  where {\n    y = b\n  }"),
            "x = a where { y = b }\n"
        );
        assert_eq!(
            c("x = a\n  with {\n    y = b\n  }\n  where {\n    z = c\n  }"),
            "x = a with { y = b } where { z = c }\n"
        );
    }

    // --- Error cases ---

    #[test]
    fn error_chained_arrows() {
        let errs = conv_errs("x = a → b → c");
        assert_eq!(errs, vec!["cannot chain `*` or arrow operators"]);
    }

    #[test]
    fn error_chained_star_arrow() {
        let errs = conv_errs("x = a ;* b → c");
        assert_eq!(errs, vec!["cannot chain `*` or arrow operators"]);
    }

    #[test]
    fn error_named_in_decorator() {
        // `=` in params produces ParamTy::Named, which is invalid in decorator context
        let errs = conv_errs("[n = v] x = a");
        assert!(errs.iter().any(|e| e.contains("named")), "expected named param error, got: {errs:?}");
    }

    #[test]
    fn error_where_after_with() {
        let errs = conv_errs(
            "x = a\n  where {\n    y = b\n  }\n  with {\n    z = c\n  }"
        );
        assert_eq!(errs, vec!["`with` clause must come before `where` clause"]);
    }
}
