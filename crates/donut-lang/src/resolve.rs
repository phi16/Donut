use crate::types::common::*;
use crate::types::item::*;
use crate::types::semtree;
use crate::types::token::Token;

// --- Resolve trait ---

trait Resolve {
    type Output;
    fn resolve(self, ctx: &mut Checker) -> Self::Output;
}

// --- Helper functions ---

fn is_functor_type(val: &Val) -> bool {
    matches!(val, Val::Arrow(ArrowKind::Functor, _, _))
}

fn seg_name<P>(seg_s: &S<semtree::Segment<P>>) -> (&str, &TokenSpan) {
    let S(seg, span) = seg_s;
    (&seg.0 .0, span)
}

fn decl_item_kind(unit: &semtree::DeclUnit) -> Option<ItemKind> {
    let S(ref op, _) = unit.op;
    let kind = match &unit.body {
        Some(vm) => match vm {
            semtree::ValMod::Val(_) => match op {
                semtree::AssignOp::Alias => ItemKind::Alias,
                semtree::AssignOp::Def => ItemKind::Def,
                semtree::AssignOp::Decl => ItemKind::Decl,
                semtree::AssignOp::Add => return None,
            },
            semtree::ValMod::Mod(_) => return None,
        },
        None => match op {
            semtree::AssignOp::Decl => ItemKind::Decl,
            _ => return None,
        },
    };
    Some(kind)
}

fn resolve_arrow_kind(arrow_ty: &semtree::ArrowTy) -> ArrowKind {
    match arrow_ty {
        semtree::ArrowTy::To => ArrowKind::To,
        semtree::ArrowTy::Eq => ArrowKind::Eq,
        semtree::ArrowTy::Functor => ArrowKind::Functor,
    }
}

// --- Resolve trait implementations ---

impl Resolve for S<semtree::Val> {
    type Output = S<Val>;
    fn resolve(self, ctx: &mut Checker) -> S<Val> {
        let S(v, span) = self;
        let val = v.resolve(ctx);
        S(val, span)
    }
}

impl Resolve for semtree::Val {
    type Output = Val;
    fn resolve(self, ctx: &mut Checker) -> Val {
        match self {
            semtree::Val::Path(path_s) => {
                let path = (*path_s).resolve(ctx);
                Val::Path(path)
            }
            semtree::Val::Lit(lit_s) => {
                let lit = lit_s.resolve(ctx);
                Val::Lit(lit)
            }
            semtree::Val::Op(l, op_s, _params, r) => {
                let S(op, _) = op_s;
                match op {
                    semtree::Op::Comp(axis) => {
                        let mut children = Vec::new();
                        ctx.resolve_comp_flat(*l, axis, &mut children);
                        ctx.resolve_comp_flat(*r, axis, &mut children);
                        Val::Comp(axis, children)
                    }
                    semtree::Op::CompStar => {
                        let mut children = Vec::new();
                        ctx.resolve_comp_star_flat(*l, &mut children);
                        ctx.resolve_comp_star_flat(*r, &mut children);
                        Val::CompStar(children)
                    }
                    semtree::Op::Arrow(arrow_ty) => {
                        let kind = resolve_arrow_kind(&arrow_ty);
                        let l_s = (*l).resolve(ctx);
                        let r_s = (*r).resolve(ctx);
                        Val::Arrow(kind, Box::new(l_s), Box::new(r_s))
                    }
                }
            }
            semtree::Val::Any => Val::Hole(Hole::Any),
        }
    }
}

impl Resolve for S<semtree::Path<semtree::ParamVal>> {
    type Output = Path;
    fn resolve(self, ctx: &mut Checker) -> Path {
        let S(path, _) = self;

        // Name resolution
        let name_spans: Vec<_> = path.0.iter().map(seg_name).collect();
        ctx.resolve_segments(&name_spans);

        // Convert segments (resolving param vals recursively)
        let segments: Vec<S<Segment>> = path
            .0
            .into_iter()
            .map(|seg_s| seg_s.resolve(ctx))
            .collect();

        let applicand = path.1.map(|v| Box::new(v.resolve(ctx)));

        Path {
            segments,
            applicand,
        }
    }
}

impl Resolve for S<semtree::Segment<semtree::ParamVal>> {
    type Output = S<Segment>;
    fn resolve(self, ctx: &mut Checker) -> S<Segment> {
        let S(seg, span) = self;
        let params: Vec<ParamVal> = seg
            .1
             .0
            .into_iter()
            .map(|pv| pv.resolve(ctx))
            .collect();
        S(
            Segment {
                name: seg.0 .0,
                params,
            },
            span,
        )
    }
}

impl Resolve for semtree::ParamVal {
    type Output = ParamVal;
    fn resolve(self, ctx: &mut Checker) -> ParamVal {
        let name = self.name.map(|n| n.0);
        let val = self.val.resolve(ctx);
        ParamVal { name, val }
    }
}

impl Resolve for S<semtree::Lit> {
    type Output = Lit;
    fn resolve(self, ctx: &mut Checker) -> Lit {
        let S(lit, _) = self;
        match lit {
            semtree::Lit::Number(s) => Lit::Number(s),
            semtree::Lit::String(s) => Lit::String(s),
            semtree::Lit::Array(vs) => {
                let items: Vec<S<Val>> =
                    vs.into_iter().map(|v| v.resolve(ctx)).collect();
                Lit::Array(items)
            }
            semtree::Lit::Object(kvs) => {
                let items: Vec<(String, S<Val>)> = kvs
                    .into_iter()
                    .map(|(k, v)| {
                        let S(key, _) = k;
                        let key_str = match key {
                            semtree::Key::Name(n) => n.0,
                            semtree::Key::String(s) => s,
                        };
                        let val = v.resolve(ctx);
                        (key_str, val)
                    })
                    .collect();
                Lit::Object(items)
            }
        }
    }
}

impl Resolve for S<semtree::Module> {
    type Output = Module;
    fn resolve(self, ctx: &mut Checker) -> Self::Output {
        let S(module, _) = self;
        match module {
            semtree::Module::Block(decls) => {
                ctx.push_scope();
                for d in decls {
                    d.resolve(ctx);
                }
                ctx.scopes.pop().unwrap()
            }
            semtree::Module::Import(_) => Module::new(), // TODO
        }
    }
}

impl Resolve for semtree::Decl {
    type Output = ();
    fn resolve(self, ctx: &mut Checker) {
        let semtree::Decl {
            decos,
            main,
            with_clauses,
            where_clauses,
        } = self;

        let (deco_param_defs, deco_vals) = ctx.resolve_decorators(&decos);

        match main {
            semtree::DeclMain::Unit(unit) => {
                ctx.process_unit_decl(unit, deco_param_defs, deco_vals, with_clauses, where_clauses);
            }
            semtree::DeclMain::Mod(mod_s) => {
                ctx.process_mod_decl(mod_s, deco_param_defs, with_clauses, where_clauses);
            }
        }
    }
}

// --- Checker ---

struct Checker<'a> {
    tokens: &'a [Token<'a>],
    scopes: Vec<Module>,
    errors: Vec<Error>,
}

impl<'a> Checker<'a> {
    fn new(tokens: &'a [Token<'a>]) -> Self {
        Checker {
            tokens,
            scopes: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn error_at(&mut self, span: &TokenSpan, msg: impl Into<String>) {
        if let Some(token) = self.tokens.get(span.start) {
            self.errors.push((token.pos.clone(), msg.into()));
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(Module::new());
    }
    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define(&mut self, name: String, item: Item) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.define(name, item);
        }
    }

    fn lookup(&self, name: &str) -> Option<&Item> {
        for scope in self.scopes.iter().rev() {
            if let Some(item) = scope.get(name) {
                return Some(item);
            }
        }
        None
    }

    fn lookup_mut(&mut self, name: &str) -> Option<&mut Item> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(item) = scope.get_mut(name) {
                return Some(item);
            }
        }
        None
    }

    // --- Resolve helpers (borrowing, used in outer scope phases) ---

    fn resolve_s_val(&mut self, sv: &S<semtree::Val>) -> S<Val> {
        let S(v, span) = sv;
        let val = self.resolve_val(v);
        S(val, span.clone())
    }

    fn resolve_val(&mut self, val: &semtree::Val) -> Val {
        match val {
            semtree::Val::Path(path_s) => {
                let path = self.resolve_path(path_s);
                Val::Path(path)
            }
            semtree::Val::Lit(lit_s) => {
                let lit = self.resolve_lit(lit_s);
                Val::Lit(lit)
            }
            semtree::Val::Op(l, op_s, _params, r) => {
                let S(op, _) = op_s;
                match op {
                    semtree::Op::Comp(axis) => {
                        let mut children = Vec::new();
                        self.resolve_comp_flat_ref(l, *axis, &mut children);
                        self.resolve_comp_flat_ref(r, *axis, &mut children);
                        Val::Comp(*axis, children)
                    }
                    semtree::Op::CompStar => {
                        let mut children = Vec::new();
                        self.resolve_comp_star_flat_ref(l, &mut children);
                        self.resolve_comp_star_flat_ref(r, &mut children);
                        Val::CompStar(children)
                    }
                    semtree::Op::Arrow(arrow_ty) => {
                        let kind = resolve_arrow_kind(arrow_ty);
                        let l_s = self.resolve_s_val(l);
                        let r_s = self.resolve_s_val(r);
                        Val::Arrow(kind, Box::new(l_s), Box::new(r_s))
                    }
                }
            }
            semtree::Val::Any => Val::Hole(Hole::Any),
        }
    }

    fn resolve_path(&mut self, path_s: &S<semtree::Path<semtree::ParamVal>>) -> Path {
        let S(path, _) = path_s;

        // Name resolution
        let name_spans: Vec<_> = path.0.iter().map(seg_name).collect();
        self.resolve_segments(&name_spans);

        // Convert segments (resolving param vals recursively)
        let segments: Vec<S<Segment>> = path
            .0
            .iter()
            .map(|seg_s| self.resolve_segment(seg_s))
            .collect();

        let applicand = path.1.as_ref().map(|v| Box::new(self.resolve_s_val(v)));

        Path {
            segments,
            applicand,
        }
    }

    fn resolve_segment(&mut self, seg_s: &S<semtree::Segment<semtree::ParamVal>>) -> S<Segment> {
        let S(seg, span) = seg_s;
        let params: Vec<ParamVal> = seg
            .1
             .0
            .iter()
            .map(|pv| self.resolve_param_val(pv))
            .collect();
        S(
            Segment {
                name: seg.0 .0.clone(),
                params,
            },
            span.clone(),
        )
    }

    fn resolve_param_val(&mut self, pv: &semtree::ParamVal) -> ParamVal {
        let name = pv.name.as_ref().map(|n| n.0.clone());
        let val = self.resolve_s_val(&pv.val);
        ParamVal { name, val }
    }

    fn resolve_lit(&mut self, lit_s: &S<semtree::Lit>) -> Lit {
        let S(lit, _) = lit_s;
        match lit {
            semtree::Lit::Number(s) => Lit::Number(s.clone()),
            semtree::Lit::String(s) => Lit::String(s.clone()),
            semtree::Lit::Array(vs) => {
                let items: Vec<S<Val>> =
                    vs.iter().map(|v| self.resolve_s_val(v)).collect();
                Lit::Array(items)
            }
            semtree::Lit::Object(kvs) => {
                let items: Vec<(String, S<Val>)> = kvs
                    .iter()
                    .map(|(k, v)| {
                        let S(key, _) = k;
                        let key_str = match key {
                            semtree::Key::Name(n) => n.0.clone(),
                            semtree::Key::String(s) => s.clone(),
                        };
                        let val = self.resolve_s_val(v);
                        (key_str, val)
                    })
                    .collect();
                Lit::Object(items)
            }
        }
    }

    // --- Comp flat helpers ---

    fn resolve_comp_flat(&mut self, val_s: S<semtree::Val>, axis: u32, out: &mut Vec<S<Val>>) {
        match val_s {
            S(semtree::Val::Op(l, op_s, _, r), _)
                if matches!(&op_s.0, semtree::Op::Comp(n) if *n == axis) =>
            {
                self.resolve_comp_flat(*l, axis, out);
                self.resolve_comp_flat(*r, axis, out);
            }
            other => {
                out.push(other.resolve(self));
            }
        }
    }

    fn resolve_comp_star_flat(&mut self, val_s: S<semtree::Val>, out: &mut Vec<S<Val>>) {
        match val_s {
            S(semtree::Val::Op(l, op_s, _, r), _)
                if matches!(&op_s.0, semtree::Op::CompStar) =>
            {
                self.resolve_comp_star_flat(*l, out);
                self.resolve_comp_star_flat(*r, out);
            }
            other => {
                out.push(other.resolve(self));
            }
        }
    }

    fn resolve_comp_flat_ref(&mut self, val_s: &S<semtree::Val>, axis: u32, out: &mut Vec<S<Val>>) {
        if let S(semtree::Val::Op(l, op_s, _, r), _) = val_s {
            if matches!(&op_s.0, semtree::Op::Comp(n) if *n == axis) {
                self.resolve_comp_flat_ref(l, axis, out);
                self.resolve_comp_flat_ref(r, axis, out);
                return;
            }
        }
        out.push(self.resolve_s_val(val_s));
    }

    fn resolve_comp_star_flat_ref(&mut self, val_s: &S<semtree::Val>, out: &mut Vec<S<Val>>) {
        if let S(semtree::Val::Op(l, op_s, _, r), _) = val_s {
            if matches!(&op_s.0, semtree::Op::CompStar) {
                self.resolve_comp_star_flat_ref(l, out);
                self.resolve_comp_star_flat_ref(r, out);
                return;
            }
        }
        out.push(self.resolve_s_val(val_s));
    }

    // --- Scope resolution ---

    fn resolve_segments(&mut self, segments: &[(&str, &TokenSpan)]) -> Option<Item> {
        let mut current: Option<Item> = None;
        for (i, &(name, span)) in segments.iter().enumerate() {
            if i == 0 {
                match self.lookup(name) {
                    Some(item) => current = Some(item.clone()),
                    None => {
                        if name != "*" && !crate::convert::is_number_str(name) {
                            self.error_at(span, format!("undefined name `{}`", name));
                        }
                        return None;
                    }
                }
            } else {
                let next = current
                    .take()
                    .and_then(|item| item.members().and_then(|m| m.get(name).cloned()));
                if next.is_none() {
                    self.error_at(span, format!("undefined member `{}`", name));
                    return None;
                }
                current = next;
            }
        }
        current
    }

    fn resolve_val_as_members(&self, val_s: &S<Val>) -> Option<Module> {
        let path = match &val_s.0 {
            Val::Path(path) => path,
            _ => return None,
        };
        let mut current: Option<&Item> = None;
        for (i, seg_s) in path.segments.iter().enumerate() {
            let name = &seg_s.0.name;
            if i == 0 {
                current = self.lookup(name);
            } else {
                current =
                    current.and_then(|item| item.members().and_then(|m| m.get(name)));
            }
        }
        Some(current?.members()?.clone())
    }

    // --- Declaration checking + resolution ---

    fn resolve_decorators(
        &mut self,
        decos: &[semtree::Decorator],
    ) -> (Vec<(String, S<Val>)>, Vec<S<Val>>) {
        let mut param_defs = Vec::new();
        let mut deco_vals = Vec::new();
        for deco in decos {
            match deco {
                semtree::Decorator::Param(name, ty) => {
                    param_defs.push((name.0.clone(), self.resolve_s_val(ty)));
                }
                semtree::Decorator::Deco(v) => {
                    deco_vals.push(self.resolve_s_val(v));
                }
            }
        }
        (param_defs, deco_vals)
    }

    fn check_decl_unit(
        &mut self,
        unit: &semtree::DeclUnit,
    ) -> (Option<S<Val>>, Vec<Param>) {
        let mut params = Vec::new();

        for (i, name_s) in unit.names.iter().enumerate() {
            let S(pd, _) = name_s;
            let segs = &pd.0;

            // Resolve prefix (for dotted declarations like m.x)
            if segs.len() > 1 {
                let prefix: Vec<_> = segs[..segs.len() - 1]
                    .iter()
                    .map(seg_name)
                    .collect();
                self.resolve_segments(&prefix);
            }

            // Resolve param types and collect from first name
            for seg_s in segs {
                let S(seg, _) = seg_s;
                for param in &seg.1 .0 {
                    let ty = self.resolve_s_val(&param.ty);
                    if i == 0 {
                        params.push(Param {
                            name: param.name.0.clone(),
                            ty,
                        });
                    }
                }
            }

            // Resolve applicand (for functor application)
            if let Some(v) = &pd.1 {
                let _ = self.resolve_s_val(v);
            }
        }

        let ty = unit.ty.as_ref().map(|ty| self.resolve_s_val(ty));

        // += pre-check
        if matches!(&unit.op.0, semtree::AssignOp::Add) {
            for name_s in &unit.names {
                let S(pd, _) = name_s;
                if pd.0.len() == 1 {
                    let (name, span) = seg_name(&pd.0[0]);
                    if self.lookup(name).is_none() {
                        self.error_at(
                            span,
                            format!("`{}` must be declared before `+=`", name),
                        );
                    }
                }
            }
        }

        (ty, params)
    }

    // --- Registration ---

    fn register_unit_results(
        &mut self,
        names: &[S<semtree::Path<semtree::ParamDecl>>],
        op: &S<semtree::AssignOp>,
        item: Item,
    ) {
        if matches!(&op.0, semtree::AssignOp::Add) {
            self.register_add(names, item);
            return;
        }
        for name_s in names {
            let S(pd, _) = name_s;
            self.register_path_decl(pd, item.clone());
        }
    }

    fn register_path_decl(&mut self, pd: &semtree::Path<semtree::ParamDecl>, item: Item) {
        let segs = &pd.0;
        match segs.len() {
            0 => {}
            1 => {
                let (name, _) = seg_name(&segs[0]);
                self.define(name.to_owned(), item);
            }
            _ => self.insert_into_dotted(segs, item),
        }
    }

    fn insert_into_dotted(&mut self, segs: &[S<semtree::Segment<semtree::ParamDecl>>], item: Item) {
        if segs.len() < 2 {
            return;
        }
        let (first_name, _) = seg_name(&segs[0]);
        let (last_name, last_span) = seg_name(segs.last().unwrap());

        let mut conflict = false;
        if let Some(root) = self.lookup_mut(first_name) {
            let mut current = root;
            for seg_s in &segs[1..segs.len() - 1] {
                let (name, _) = seg_name(seg_s);
                let Some(members) = current.members_mut() else {
                    return;
                };
                if !members.contains_key(name) {
                    return;
                }
                current = members.get_mut(name).unwrap();
            }
            if let Some(members) = current.members_mut() {
                if members.contains_key(last_name) {
                    conflict = true;
                } else {
                    members.define(last_name.to_owned(), item);
                }
            }
        }

        if conflict {
            self.error_at(last_span, format!("duplicate member `{}`", last_name));
        }
    }

    fn register_add(&mut self, names: &[S<semtree::Path<semtree::ParamDecl>>], item: Item) {
        let members_to_merge = match item.body {
            ItemBody::Value { val, members } => {
                if let Some(ref val_s) = val {
                    self.resolve_val_as_members(val_s)
                        .unwrap_or_else(Module::new)
                } else {
                    members
                }
            }
            ItemBody::Functor { .. } => Module::new(),
        };

        for name_s in names {
            let S(pd, _) = name_s;
            self.merge_into_path(pd, members_to_merge.clone());
        }
    }

    fn merge_into_path(&mut self, pd: &semtree::Path<semtree::ParamDecl>, new_members: Module) {
        if new_members.entries.is_empty() {
            return;
        }
        let segs = &pd.0;
        if segs.is_empty() {
            return;
        }

        if segs.len() == 1 {
            let (name, span) = seg_name(&segs[0]);
            let conflicts = if let Some(existing) = self.lookup_mut(name) {
                if let Some(members) = existing.members_mut() {
                    members.merge(new_members)
                } else {
                    Vec::new()
                }
            } else {
                Vec::new()
            };
            for key in conflicts {
                self.error_at(span, format!("duplicate member `{}`", key));
            }
        }
        // TODO: multi-segment += / with paths
    }

    // --- Unit locals ---

    fn register_unit_locals(
        &mut self,
        unit: &semtree::DeclUnit,
        resolved_params: &[Param],
    ) {
        let is_add = matches!(&unit.op.0, semtree::AssignOp::Add);
        let item_kind = decl_item_kind(unit);

        // Define params (already resolved in outer scope)
        for param in resolved_params {
            self.define(param.name.clone(), Item::param(param.ty.clone()));
        }

        // Define declared names (forward references)
        for name_s in &unit.names {
            let S(pd, _) = name_s;
            if !is_add && pd.0.len() == 1 {
                let (name, _) = seg_name(&pd.0[0]);
                self.define(name.to_owned(), Item::new(item_kind));
            }
        }
    }

    // --- With clause merging ---

    fn merge_with_clauses(
        &mut self,
        mut result: Module,
        with_clauses: Vec<S<semtree::Module>>,
    ) -> Module {
        for with_mod in with_clauses {
            let span = with_mod.1.clone();
            let with_members = with_mod.resolve(self);
            let conflicts = result.merge(with_members);
            for key in conflicts {
                self.error_at(&span, format!("duplicate member `{}`", key));
            }
        }
        result
    }

    // --- Process declarations ---

    fn process_unit_decl(
        &mut self,
        unit: semtree::DeclUnit,
        deco_param_defs: Vec<(String, S<Val>)>,
        deco_vals: Vec<S<Val>>,
        with_clauses: Vec<S<semtree::Module>>,
        where_clauses: Vec<S<semtree::Module>>,
    ) {
        // 1. Outer scope: check decl unit (borrowing phase)
        let (ty_resolved, params_resolved) = self.check_decl_unit(&unit);

        // 2. Inner scope setup (borrowing phase)
        self.push_scope();

        // Define deco params
        for (name, ty) in &deco_param_defs {
            self.define(name.clone(), Item::param(ty.clone()));
        }

        // Register unit locals
        self.register_unit_locals(&unit, &params_resolved);

        // 3. Where clauses (consume, reverse order)
        for wm in where_clauses.into_iter().rev() {
            let S(module, _) = wm;
            if let semtree::Module::Block(decls) = module {
                for d in decls {
                    d.resolve(self);
                }
            }
        }

        // 4. Resolve body val in inner scope (borrowing)
        let body_val_resolved = if let Some(semtree::ValMod::Val(ref v)) = unit.body {
            Some(self.resolve_s_val(v))
        } else {
            None
        };

        // 5. Consume main
        let kind = decl_item_kind(&unit);
        let semtree::DeclUnit {
            names,
            ty: _,
            op,
            body,
        } = unit;

        // Functor application constraints
        let has_functor_app =
            names.iter().any(|n| { let S(pd, _) = n; pd.1.is_some() });
        if has_functor_app {
            if !matches!(&op.0, semtree::AssignOp::Alias) {
                self.error_at(&op.1, "functor application only allows `=`");
            }
            if let Some(semtree::ValMod::Mod(ref m)) = body {
                self.error_at(
                    &m.1,
                    "functor application cannot have a module body",
                );
            }
            for wc in &with_clauses {
                self.error_at(
                    &wc.1,
                    "functor application cannot have `with` clauses",
                );
            }
        }

        let body_members = match body {
            Some(semtree::ValMod::Val(_)) => Module::new(),
            Some(semtree::ValMod::Mod(m)) => m.resolve(self),
            None => Module::new(),
        };

        // With clauses + pop scope
        let result = self.merge_with_clauses(body_members, with_clauses);
        self.pop_scope();

        // Register
        if has_functor_app {
            for name_s in names {
                let S(pd, _) = name_s;
                let semtree::Path(segs, applicand) = pd;
                if let Some(applicand) = applicand {
                    let (fname, fspan) = seg_name(&segs[0]);
                    let fname = fname.to_owned();
                    let fspan = fspan.clone();
                    let app_resolved = self.resolve_s_val(&applicand);
                    if let Some(existing) = self.lookup_mut(&fname) {
                        if let ItemBody::Functor { mappings } =
                            &mut existing.body
                        {
                            if let Some(v) = &body_val_resolved {
                                mappings.push(FunctorMapping {
                                    applicand: app_resolved,
                                    val: v.clone(),
                                });
                            }
                        } else {
                            self.error_at(
                                &fspan,
                                format!("`{}` is not a functor", fname),
                            );
                        }
                    } else {
                        self.error_at(
                            &fspan,
                            format!("undefined functor `{}`", fname),
                        );
                    }
                }
            }
        } else {
            let is_functor = ty_resolved
                .as_ref()
                .map_or(false, |t| is_functor_type(&t.0));
            let body = if is_functor {
                ItemBody::Functor {
                    mappings: Vec::new(),
                }
            } else {
                ItemBody::Value {
                    val: body_val_resolved,
                    members: result,
                }
            };
            let item = Item {
                kind,
                ty: ty_resolved,
                params: params_resolved,
                body,
                decos: deco_vals,
            };
            self.register_unit_results(&names, &op, item);
        }
    }

    fn process_mod_decl(
        &mut self,
        mod_s: S<semtree::Module>,
        deco_param_defs: Vec<(String, S<Val>)>,
        with_clauses: Vec<S<semtree::Module>>,
        where_clauses: Vec<S<semtree::Module>>,
    ) {
        let span = mod_s.1.clone();

        self.push_scope();

        // Define deco params
        for (name, ty) in &deco_param_defs {
            self.define(name.clone(), Item::param(ty.clone()));
        }

        // Where clauses (consume, reverse order)
        for wm in where_clauses.into_iter().rev() {
            let S(module, _) = wm;
            if let semtree::Module::Block(decls) = module {
                for d in decls {
                    d.resolve(self);
                }
            }
        }

        let result = mod_s.resolve(self);
        let result = self.merge_with_clauses(result, with_clauses);

        self.pop_scope();

        // Mod → promote to current scope
        if let Some(scope) = self.scopes.last_mut() {
            let conflicts = scope.merge(result);
            for key in conflicts {
                self.error_at(&span, format!("duplicate member `{}`", key));
            }
        }
    }
}

pub fn resolve(program: semtree::Program, tokens: &[Token]) -> (Module, Vec<Error>) {
    let mut checker = Checker::new(tokens);
    checker.push_scope();
    for d in program.0 {
        d.resolve(&mut checker);
    }
    let module = checker.scopes.pop().unwrap();
    (module, checker.errors)
}
