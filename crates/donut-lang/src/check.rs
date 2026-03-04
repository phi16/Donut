use crate::types::common::*;
use crate::types::semtree;
use crate::types::token::Token;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, Copy)]
pub enum ItemKind {
    Decl,  // x: T
    Alias, // x = ...
    Def,   // x := ...
    Param, // [x: T]
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Rc<A<semtree::Val>>,
}

#[derive(Debug, Clone)]
pub struct FunctorMapping {
    pub applicand: Rc<A<semtree::Val>>,
    pub val: Rc<A<semtree::Val>>,
}

#[derive(Debug, Clone)]
pub enum ItemBody {
    Value {
        val: Option<Rc<A<semtree::Val>>>,
        members: Module,
    },
    Functor {
        mappings: Vec<FunctorMapping>,
    },
}

#[derive(Debug, Clone)]
pub struct Item {
    pub kind: Option<ItemKind>,
    pub ty: Option<Rc<A<semtree::Val>>>,
    pub params: Vec<Param>,
    pub body: ItemBody,
    pub decos: Vec<Rc<A<semtree::Val>>>,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub entries: Vec<(String, Item)>,
    index: HashMap<String, usize>,
}

impl Module {
    fn new() -> Self {
        Module {
            entries: Vec::new(),
            index: HashMap::new(),
        }
    }

    fn define(&mut self, name: String, item: Item) {
        if let Some(&idx) = self.index.get(&name) {
            self.entries[idx].1 = item;
        } else {
            let idx = self.entries.len();
            self.index.insert(name.clone(), idx);
            self.entries.push((name, item));
        }
    }

    pub fn get(&self, name: &str) -> Option<&Item> {
        let &idx = self.index.get(name)?;
        Some(&self.entries[idx].1)
    }

    fn get_mut(&mut self, name: &str) -> Option<&mut Item> {
        let &idx = self.index.get(name)?;
        Some(&mut self.entries[idx].1)
    }

    fn contains_key(&self, name: &str) -> bool {
        self.index.contains_key(name)
    }

    /// Merge source entries into self. Returns conflicting key names.
    fn merge(&mut self, source: Module) -> Vec<String> {
        let mut conflicts = Vec::new();
        for (key, item) in source.entries {
            if self.contains_key(&key) {
                conflicts.push(key);
            } else {
                self.define(key, item);
            }
        }
        conflicts
    }
}

impl Item {
    fn new(kind: Option<ItemKind>) -> Self {
        Item {
            kind,
            ty: None,
            params: Vec::new(),
            body: ItemBody::Value {
                val: None,
                members: Module::new(),
            },
            decos: Vec::new(),
        }
    }

    fn param(ty: Rc<A<semtree::Val>>) -> Self {
        Item {
            kind: Some(ItemKind::Param),
            ty: Some(ty),
            params: Vec::new(),
            body: ItemBody::Value {
                val: None,
                members: Module::new(),
            },
            decos: Vec::new(),
        }
    }

    pub fn members(&self) -> Option<&Module> {
        match &self.body {
            ItemBody::Value { members, .. } => Some(members),
            _ => None,
        }
    }

    pub fn members_mut(&mut self) -> Option<&mut Module> {
        match &mut self.body {
            ItemBody::Value { members, .. } => Some(members),
            _ => None,
        }
    }

    pub fn val(&self) -> Option<&Rc<A<semtree::Val>>> {
        match &self.body {
            ItemBody::Value { val, .. } => val.as_ref(),
            _ => None,
        }
    }
}

fn is_functor_type(val: &A<semtree::Val>) -> bool {
    if let Some(semtree::Val::Op(_, op_a, _, _)) = val.inner() {
        matches!(
            op_a.inner(),
            Some(semtree::Op::Arrow(semtree::ArrowTy::Functor))
        )
    } else {
        false
    }
}

fn seg_decl_name(seg_a: &A<semtree::SegmentDecl>) -> Option<(&str, &TokenSpan)> {
    let (seg, span) = seg_a.accepted()?;
    let (name, _) = seg.0.accepted()?;
    Some((&name.0, span))
}

fn seg_val_name(seg_a: &A<semtree::Segment>) -> Option<(&str, &TokenSpan)> {
    let (seg, span) = seg_a.accepted()?;
    let (name, _) = seg.0.accepted()?;
    Some((&name.0, span))
}

fn decl_item_kind(unit: &semtree::DeclUnit) -> Option<ItemKind> {
    let kind = match &unit.body {
        Some(vm) => match vm {
            semtree::ValMod::Val(_) => match unit.op.inner()? {
                semtree::AssignOp::Alias => ItemKind::Alias,
                semtree::AssignOp::Def => ItemKind::Def,
                semtree::AssignOp::Decl => ItemKind::Decl,
                semtree::AssignOp::Add => return None,
            },
            semtree::ValMod::Mod(_) => return None,
        },
        None => match unit.op.inner()? {
            semtree::AssignOp::Decl => ItemKind::Decl,
            _ => return None,
        },
    };
    Some(kind)
}

trait Check {
    fn check(&self, c: &mut Checker<'_>);
}

impl<T: Check> Check for A<T> {
    fn check(&self, c: &mut Checker<'_>) {
        if let Some(v) = self.inner() {
            v.check(c);
        }
    }
}

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

    /// Resolve a sequence of (name, span) through scope lookup and member chain.
    /// Reports errors for undefined names/members. Returns the final resolved item.
    fn resolve_segments(&mut self, segments: &[(&str, &TokenSpan)]) -> Option<Item> {
        let mut current: Option<Item> = None;
        for (i, &(name, span)) in segments.iter().enumerate() {
            if i == 0 {
                match self.lookup(name) {
                    Some(item) => current = Some(item.clone()),
                    None => {
                        // TODO: temporary suppression — `*` and numeric names are
                        // not yet resolvable in the checker; remove once proper
                        // built-in name handling is implemented.
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

    // --- Module ---

    fn collect_module_members_owned(&mut self, module: A<semtree::Module>) -> Module {
        match module {
            A::Accepted(semtree::Module::Block(decls), _) => {
                self.push_scope();
                for d in decls {
                    self.process_decl(d);
                }
                self.scopes.pop().unwrap()
            }
            _ => Module::new(), // Import: TODO
        }
    }

    // --- Registration ---

    fn register_unit_results(
        &mut self,
        names: &[A<semtree::PathDecl>],
        op: &A<semtree::AssignOp>,
        item: Item,
    ) {
        // For +=, merge into existing item
        if matches!(op.inner(), Some(semtree::AssignOp::Add)) {
            self.register_add(names, item);
            return;
        }

        // Register each declared name
        for name_a in names {
            if let Some((pd, _)) = name_a.accepted() {
                self.register_path_decl(pd, item.clone());
            }
        }
    }

    fn register_path_decl(&mut self, pd: &semtree::PathDecl, item: Item) {
        let segs = &pd.0;
        match segs.len() {
            0 => {}
            1 => {
                if let Some((name, _)) = segs.first().and_then(seg_decl_name) {
                    self.define(name.to_owned(), item);
                }
            }
            _ => self.insert_into_dotted(segs, item),
        }
    }

    fn insert_into_dotted(&mut self, segs: &[A<semtree::SegmentDecl>], item: Item) {
        if segs.len() < 2 {
            return;
        }
        let Some((first_name, _)) = seg_decl_name(&segs[0]) else {
            return;
        };
        let Some((last_name, last_span)) = seg_decl_name(segs.last().unwrap()) else {
            return;
        };

        let mut conflict = false;
        if let Some(root) = self.lookup_mut(first_name) {
            let mut current = root;
            for seg_a in &segs[1..segs.len() - 1] {
                if let Some((name, _)) = seg_decl_name(seg_a) {
                    let Some(members) = current.members_mut() else {
                        return;
                    };
                    if !members.contains_key(name) {
                        return; // Error already reported in check phase
                    }
                    current = members.get_mut(name).unwrap();
                }
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

    fn register_add(
        &mut self,
        names: &[A<semtree::PathDecl>],
        item: Item,
    ) {
        let members_to_merge = match item.body {
            ItemBody::Value { val, members } => {
                if let Some(val_rc) = &val {
                    // Body was Val - resolve the val path to get members
                    self.resolve_val_as_members(val_rc)
                        .unwrap_or_else(Module::new)
                } else {
                    // Body was Mod or None - use the collected members
                    members
                }
            }
            ItemBody::Functor { .. } => Module::new(),
        };

        for name_a in names {
            if let A::Accepted(pd, _) = name_a {
                self.merge_into_path(pd, members_to_merge.clone());
            }
        }
    }

    fn resolve_val_as_members(&self, val_a: &A<semtree::Val>) -> Option<Module> {
        let path = match val_a.inner() {
            Some(semtree::Val::Path(path_a)) => path_a.inner()?,
            _ => return None,
        };
        let mut current: Option<&Item> = None;
        for (i, seg_a) in path.0.iter().enumerate() {
            if let Some((name, _)) = seg_val_name(seg_a) {
                if i == 0 {
                    current = self.lookup(name);
                } else {
                    current =
                        current.and_then(|item| item.members().and_then(|m| m.get(name)));
                }
            }
        }
        Some(current?.members()?.clone())
    }

    fn merge_into_path(&mut self, pd: &semtree::PathDecl, new_members: Module) {
        if new_members.entries.is_empty() {
            return;
        }
        let segs = &pd.0;
        if segs.is_empty() {
            return;
        }

        if segs.len() == 1 {
            if let Some((name, span)) = segs.first().and_then(seg_decl_name) {
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
        }
        // TODO: multi-segment += / with paths
    }

    // --- Unit locals ---

    fn register_unit_locals(&mut self, unit: &semtree::DeclUnit) {
        let is_add = matches!(unit.op.inner(), Some(semtree::AssignOp::Add));
        let item_kind = decl_item_kind(unit);
        for name_a in &unit.names {
            if let A::Accepted(pd, _) = name_a {
                for seg_a in &pd.0 {
                    if let A::Accepted(seg, _) = seg_a {
                        for param_a in &seg.1.0 {
                            if let A::Accepted(param, _) = param_a {
                                if let A::Accepted(name, _) = &param.name {
                                    self.define(
                                        name.0.clone(),
                                        Item::param(Rc::clone(&param.ty)),
                                    );
                                }
                            }
                        }
                    }
                }
                if !is_add && pd.0.len() == 1 {
                    if let Some((name, _)) = pd.0.first().and_then(seg_decl_name) {
                        self.define(name.to_owned(), Item::new(item_kind));
                    }
                }
            }
        }
    }
}

// --- Process (owned) ---

impl<'a> Checker<'a> {
    fn process_program(&mut self, program: A<semtree::Program>) {
        if let A::Accepted(prog, _) = program {
            for d in prog.0 {
                self.process_decl(d);
            }
        }
    }

    fn check_decl_unit(unit: &semtree::DeclUnit, c: &mut Checker<'_>) {
        for name_a in &unit.names {
            if let Some((pd, _)) = name_a.accepted() {
                pd.check(c);
            }
        }
        if let Some(ty) = &unit.ty {
            ty.check(c);
        }
        if matches!(unit.op.inner(), Some(semtree::AssignOp::Add)) {
            for name_a in &unit.names {
                if let Some((pd, _)) = name_a.accepted() {
                    if pd.0.len() == 1 {
                        if let Some((name, span)) = pd.0.first().and_then(seg_decl_name) {
                            if c.lookup(name).is_none() {
                                c.error_at(
                                    span,
                                    format!("`{}` must be declared before `+=`", name),
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    fn extract_params(names: &[A<semtree::PathDecl>]) -> Vec<Param> {
        let mut params = Vec::new();
        if let Some(first_name) = names.first() {
            if let Some(pd) = first_name.inner() {
                for seg_a in &pd.0 {
                    if let A::Accepted(seg, _) = seg_a {
                        for param_a in &seg.1.0 {
                            if let A::Accepted(param, _) = param_a {
                                if let A::Accepted(name, _) = &param.name {
                                    params.push(Param {
                                        name: name.0.clone(),
                                        ty: Rc::clone(&param.ty),
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }
        params
    }

    fn process_decl(&mut self, decl_a: A<semtree::Decl>) {
        let A::Accepted(decl, _) = decl_a else {
            return;
        };
        let semtree::Decl {
            decos,
            main,
            with_clauses,
            where_clauses,
        } = decl;

        // 1. Check decorators (borrow)
        for deco in &decos {
            deco.check(self);
        }

        // 2. Unit pre-checks in outer scope (borrow)
        if let semtree::DeclMain::Unit(ref u) = main {
            if let Some(unit) = u.inner() {
                Self::check_decl_unit(unit, self);
            }
        }

        // 3. Push local scope
        self.push_scope();

        // Deco params → define
        for deco_a in &decos {
            if let A::Accepted(semtree::Decorator::Param(name_a, ty), _) = deco_a {
                if let A::Accepted(name, _) = name_a {
                    self.define(name.0.clone(), Item::param(Rc::clone(ty)));
                }
            }
        }

        // Unit locals → register (borrow main via ref)
        if let semtree::DeclMain::Unit(ref u) = main {
            if let Some(unit) = u.inner() {
                self.register_unit_locals(unit);
            }
        }

        // 4. Where clauses (consume)
        for wm in where_clauses.into_iter().rev() {
            if let A::Accepted(semtree::Module::Block(decls), _) = wm {
                for d in decls {
                    self.process_decl(d);
                }
            }
        }

        // 5. Body val check (borrow main via ref)
        if let semtree::DeclMain::Unit(ref u) = main {
            if let Some(unit) = u.inner() {
                if let Some(semtree::ValMod::Val(v)) = &unit.body {
                    v.check(self);
                }
            }
        }

        // 6. Extract deco values
        let deco_vals: Vec<Rc<A<semtree::Val>>> = decos
            .into_iter()
            .filter_map(|d| match d {
                A::Accepted(semtree::Decorator::Deco(val), _) => Some(Rc::new(val)),
                _ => None,
            })
            .collect();

        // 7. Consume main
        match main {
            semtree::DeclMain::Unit(unit_a) => {
                if let A::Accepted(unit, _) = unit_a {
                    let kind = decl_item_kind(&unit);
                    let semtree::DeclUnit {
                        names,
                        ty,
                        op,
                        body,
                    } = unit;

                    // Functor application constraints
                    let has_functor_app = names.iter().any(|n| {
                        n.inner().map_or(false, |pd| pd.1.is_some())
                    });
                    if has_functor_app {
                        if !matches!(op.inner(), Some(semtree::AssignOp::Alias)) {
                            if let Some((_, span)) = op.accepted() {
                                self.error_at(
                                    span,
                                    "functor application only allows `=`",
                                );
                            }
                        }
                        if let Some(semtree::ValMod::Mod(m)) = &body {
                            if let Some((_, span)) = m.accepted() {
                                self.error_at(
                                    span,
                                    "functor application cannot have a module body",
                                );
                            }
                        }
                        for wc in &with_clauses {
                            if let Some((_, span)) = wc.accepted() {
                                self.error_at(
                                    span,
                                    "functor application cannot have `with` clauses",
                                );
                            }
                        }
                    }

                    let params = Self::extract_params(&names);

                    let ty_rc = ty.map(|t| Rc::new(t));

                    let (val, body_members) = match body {
                        Some(semtree::ValMod::Val(v)) => (Some(Rc::new(v)), Module::new()),
                        Some(semtree::ValMod::Mod(m)) => {
                            (None, self.collect_module_members_owned(m))
                        }
                        None => (None, Module::new()),
                    };

                    // 7. With clauses (consume)
                    let mut result = body_members;
                    for with_mod in with_clauses {
                        let span = match &with_mod {
                            A::Accepted(_, s) => Some(s.clone()),
                            _ => None,
                        };
                        let with_members = self.collect_module_members_owned(with_mod);
                        let conflicts = result.merge(with_members);
                        if let Some(span) = &span {
                            for key in conflicts {
                                self.error_at(span, format!("duplicate member `{}`", key));
                            }
                        }
                    }

                    // 8. Pop scope
                    self.pop_scope();

                    // 10. Register
                    if has_functor_app {
                        for name_a in names {
                            if let A::Accepted(pd, _) = name_a {
                                let semtree::PathDecl(segs, applicand) = pd;
                                if let Some(applicand) = applicand {
                                    if let Some((fname, fspan)) =
                                        segs.first().and_then(|s| seg_decl_name(s))
                                    {
                                        let fname = fname.to_owned();
                                        let fspan = fspan.clone();
                                        if let Some(existing) = self.lookup_mut(&fname) {
                                            if let ItemBody::Functor { mappings } =
                                                &mut existing.body
                                            {
                                                if let Some(v) = &val {
                                                    mappings.push(FunctorMapping {
                                                        applicand: Rc::new(applicand),
                                                        val: Rc::clone(v),
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
                            }
                        }
                    } else {
                        let is_functor = ty_rc
                            .as_ref()
                            .map_or(false, |t| is_functor_type(t));
                        let body = if is_functor {
                            ItemBody::Functor {
                                mappings: Vec::new(),
                            }
                        } else {
                            ItemBody::Value {
                                val,
                                members: result,
                            }
                        };
                        let item = Item {
                            kind,
                            ty: ty_rc,
                            params,
                            body,
                            decos: deco_vals,
                        };
                        self.register_unit_results(&names, &op, item);
                    }
                } else {
                    // unit_a is Error
                    self.pop_scope();
                }
            }
            semtree::DeclMain::Mod(mod_a) => {
                let span = match &mod_a {
                    A::Accepted(_, s) => Some(s.clone()),
                    _ => None,
                };

                // With clauses (consume)
                let mut result = self.collect_module_members_owned(mod_a);
                for with_mod in with_clauses {
                    let with_span = match &with_mod {
                        A::Accepted(_, s) => Some(s.clone()),
                        _ => None,
                    };
                    let with_members = self.collect_module_members_owned(with_mod);
                    let conflicts = result.merge(with_members);
                    if let Some(span) = &with_span {
                        for key in conflicts {
                            self.error_at(span, format!("duplicate member `{}`", key));
                        }
                    }
                }

                self.pop_scope();

                // Mod → promote to current scope
                if let Some(scope) = self.scopes.last_mut() {
                    let conflicts = scope.merge(result);
                    if let Some(span) = &span {
                        for key in conflicts {
                            self.error_at(span, format!("duplicate member `{}`", key));
                        }
                    }
                }
            }
        }
    }
}

// --- Check impls ---

impl Check for semtree::Decorator {
    fn check(&self, c: &mut Checker<'_>) {
        match self {
            semtree::Decorator::Param(_, ty) => ty.check(c),
            semtree::Decorator::Deco(v) => v.check(c),
        }
    }
}

impl Check for semtree::Val {
    fn check(&self, c: &mut Checker<'_>) {
        match self {
            semtree::Val::Path(path_a) => path_a.check(c),
            semtree::Val::Lit(lit_a) => lit_a.check(c),
            semtree::Val::Op(l, _, params, r) => {
                l.check(c);
                if let Some(p) = params {
                    p.check(c);
                }
                r.check(c);
            }
        }
    }
}

impl Check for semtree::Path {
    fn check(&self, c: &mut Checker<'_>) {
        let name_spans: Vec<_> = self.0.iter().filter_map(seg_val_name).collect();
        c.resolve_segments(&name_spans);

        for seg_a in &self.0 {
            if let A::Accepted(seg, _) = seg_a {
                seg.1.check(c);
            }
        }
        if let Some(v) = &self.1 {
            v.check(c);
        }
    }
}

impl Check for semtree::ParamsVal {
    fn check(&self, c: &mut Checker<'_>) {
        for param_a in &self.0 {
            if let A::Accepted(param, _) = param_a {
                param.val.check(c);
            }
        }
    }
}

impl Check for semtree::Lit {
    fn check(&self, c: &mut Checker<'_>) {
        match self {
            semtree::Lit::Array(vs) => {
                for v in vs {
                    v.check(c);
                }
            }
            semtree::Lit::Object(kvs) => {
                for (_, v) in kvs {
                    v.check(c);
                }
            }
            _ => {}
        }
    }
}

impl Check for semtree::PathDecl {
    fn check(&self, c: &mut Checker<'_>) {
        let segs = &self.0;

        if segs.len() > 1 {
            let prefix: Vec<_> = segs[..segs.len() - 1]
                .iter()
                .filter_map(seg_decl_name)
                .collect();
            c.resolve_segments(&prefix);
        }

        for seg_a in segs {
            if let A::Accepted(seg, _) = seg_a {
                for param_a in &seg.1.0 {
                    if let A::Accepted(param, _) = param_a {
                        param.ty.check(c);
                    }
                }
            }
        }

        if let Some(v) = &self.1 {
            v.check(c);
        }
    }
}

pub fn check(program: A<semtree::Program>, tokens: &[Token]) -> (Module, Vec<Error>) {
    let mut checker = Checker::new(tokens);
    checker.push_scope();
    checker.process_program(program);
    let module = checker.scopes.pop().unwrap();
    (module, checker.errors)
}
