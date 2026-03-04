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
pub struct Item {
    pub kind: Option<ItemKind>,
    pub ty: Option<Rc<A<semtree::Val>>>,
    pub params: Vec<Param>,
    pub val: Option<Rc<A<semtree::Val>>>,
    pub members: Module,
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

    fn get(&self, name: &str) -> Option<&Item> {
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
            val: None,
            members: Module::new(),
        }
    }

    fn param(ty: Rc<A<semtree::Val>>) -> Self {
        Item {
            kind: Some(ItemKind::Param),
            ty: Some(ty),
            params: Vec::new(),
            val: None,
            members: Module::new(),
        }
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
                        self.error_at(span, format!("undefined name `{}`", name));
                        return None;
                    }
                }
            } else {
                let next = current
                    .take()
                    .and_then(|item| item.members.get(name).cloned());
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
                    if !current.members.contains_key(name) {
                        return; // Error already reported in check phase
                    }
                    current = current.members.get_mut(name).unwrap();
                }
            }
            if current.members.contains_key(last_name) {
                conflict = true;
            } else {
                current.members.define(last_name.to_owned(), item);
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
        let members_to_merge = if let Some(val_rc) = &item.val {
            // Body was Val - resolve the val path to get members
            self.resolve_val_as_members(val_rc)
                .unwrap_or_else(Module::new)
        } else {
            // Body was Mod or None - use the collected members
            item.members
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
                    current = current.and_then(|item| item.members.get(name));
                }
            }
        }
        Some(current?.members.clone())
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
                    existing.members.merge(new_members)
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

        // 6. Consume main
        match main {
            semtree::DeclMain::Unit(unit_a) => {
                if let A::Accepted(unit, _) = unit_a {
                    let semtree::DeclUnit {
                        names,
                        ty,
                        op,
                        body,
                    } = unit;

                    let kind = {
                        let kind = match &body {
                            Some(vm) => match vm {
                                semtree::ValMod::Val(_) => match op.inner() {
                                    Some(semtree::AssignOp::Alias) => Some(ItemKind::Alias),
                                    Some(semtree::AssignOp::Def) => Some(ItemKind::Def),
                                    Some(semtree::AssignOp::Decl) => Some(ItemKind::Decl),
                                    _ => None,
                                },
                                semtree::ValMod::Mod(_) => None,
                            },
                            None => match op.inner() {
                                Some(semtree::AssignOp::Decl) => Some(ItemKind::Decl),
                                _ => None,
                            },
                        };
                        kind
                    };

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

                    // 9. Register
                    let item = Item {
                        kind,
                        ty: ty_rc,
                        params,
                        val,
                        members: result,
                    };

                    self.register_unit_results(&names, &op, item);
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::convert::convert;
    use crate::parse::parse;
    use crate::tokenize::tokenize;

    fn check_errs(code: &str) -> Vec<String> {
        let (tokens, _, _) = tokenize(code.trim());
        let (program, _) = parse(&tokens);
        let (sem_prog, conv_errors) = convert(program, &tokens);
        assert!(
            conv_errors.is_empty(),
            "unexpected convert errors: {conv_errors:?}"
        );
        let (_module, errors) = check(sem_prog, &tokens);
        errors.into_iter().map(|(_, msg)| msg).collect()
    }

    fn check_ok(code: &str) {
        let errs = check_errs(code);
        assert!(errs.is_empty(), "unexpected check errors: {errs:?}");
    }

    // --- Basic resolution ---
    // Note: in Donut, numbers (1, 42, ...) are tokenized as Names, not literals.
    // Use string literals ("...") for leaf values that don't require name resolution.

    #[test]
    fn basic_resolution() {
        check_ok("x = \"a\"\ny = x");
    }

    #[test]
    fn undefined_name() {
        let errs = check_errs("x = y");
        assert!(
            errs.iter()
                .any(|e| e.contains("undefined") && e.contains("y")),
            "{errs:?}"
        );
    }

    #[test]
    fn scope_ordering() {
        // y is not yet defined when x is checked
        let errs = check_errs("x = y\ny = \"a\"");
        assert!(
            errs.iter()
                .any(|e| e.contains("undefined") && e.contains("y")),
            "{errs:?}"
        );
    }

    #[test]
    fn string_literal_no_resolution() {
        check_ok("x = \"hello\"");
    }

    #[test]
    fn number_literal_no_resolution() {
        // Numbers are converted to literals, no name resolution needed
        check_ok("x = 42");
        check_ok("x = -1");
        check_ok("x = 0");
    }

    // --- Params ---

    #[test]
    fn param_in_scope() {
        check_ok("T = \"t\"\nf[x: T] = x");
    }

    #[test]
    fn param_not_visible_outside() {
        let errs = check_errs("T = \"t\"\nf[x: T] = \"a\"\ny = x");
        assert!(
            errs.iter()
                .any(|e| e.contains("undefined") && e.contains("x")),
            "{errs:?}"
        );
    }

    #[test]
    fn decorator_param_in_scope() {
        check_ok("T = \"t\"\n[p: T] f: T = p");
    }

    // --- Where ---

    #[test]
    fn where_scope() {
        check_ok("x = g\n  where {\n    g = \"a\"\n  }");
    }

    #[test]
    fn where_closed() {
        // g is defined in where, not visible outside
        let errs = check_errs("x = \"a\"\n  where {\n    g = \"b\"\n  }\ny = g");
        assert!(
            errs.iter()
                .any(|e| e.contains("undefined") && e.contains("g")),
            "{errs:?}"
        );
    }

    #[test]
    fn where_ordering() {
        // within where, top-to-bottom
        let errs = check_errs("x = g\n  where {\n    g = h\n    h = \"a\"\n  }");
        assert!(
            errs.iter()
                .any(|e| e.contains("undefined") && e.contains("h")),
            "{errs:?}"
        );
    }

    #[test]
    fn where_reverse_order() {
        // With reverse processing, the second where's binding is registered first,
        // making it visible in the first where clause
        check_ok(
            r#"
x = "val"
  where {
    a = b
  }
  where {
    b = "inner"
  }
"#,
        );
    }

    // --- Module ---

    #[test]
    fn module_scope() {
        check_ok("m = {\n    x = \"a\"\n    y = x\n}");
    }

    #[test]
    fn module_ref_outer() {
        check_ok("a = \"x\"\nm = {\n    x = a\n}");
    }

    #[test]
    fn module_inner_not_visible() {
        // x is defined inside m, not visible outside
        let errs = check_errs("m = {\n    x = \"a\"\n}\ny = x");
        assert!(
            errs.iter()
                .any(|e| e.contains("undefined") && e.contains("x")),
            "{errs:?}"
        );
    }

    // --- Module member resolution ---

    #[test]
    fn dotted_ref_member() {
        check_ok("m = {\n    x = \"a\"\n}\ny = m.x");
    }

    #[test]
    fn dotted_ref_undefined_member() {
        let errs = check_errs("m = {\n    x = \"a\"\n}\ny = m.z");
        assert!(
            errs.iter()
                .any(|e| e.contains("undefined member") && e.contains("z")),
            "{errs:?}"
        );
    }

    #[test]
    fn dotted_ref_deep() {
        check_ok("a = {\n    b = {\n        c = \"x\"\n    }\n}\ny = a.b.c");
    }

    #[test]
    fn dotted_ref_deep_undefined() {
        let errs = check_errs("a = {\n    b = \"x\"\n}\ny = a.b.c");
        assert!(
            errs.iter()
                .any(|e| e.contains("undefined member") && e.contains("c")),
            "{errs:?}"
        );
    }

    // --- Dotted declarations ---

    #[test]
    fn dotted_decl_basic() {
        // Any declared name can have members added (namespace separation)
        check_ok("m = \"a\"\nm.x = \"b\"");
    }

    #[test]
    fn dotted_decl_module() {
        check_ok("m = {\n    dummy = \"a\"\n}\nm.x = \"b\"");
    }

    #[test]
    fn dotted_decl_undefined_prefix() {
        let errs = check_errs("m.x = \"a\"");
        assert!(
            errs.iter()
                .any(|e| e.contains("undefined") && e.contains("m")),
            "{errs:?}"
        );
    }

    #[test]
    fn dotted_decl_ref_after() {
        check_ok("m = \"a\"\nm.x = \"b\"\ny = m.x");
    }

    // --- With clauses ---

    #[test]
    fn with_basic() {
        check_ok("x = \"a\"\n  with {\n    y = \"b\"\n  }\nz = x.y");
    }

    #[test]
    fn with_self_ref() {
        // x is visible inside its own with block
        check_ok("x = \"a\"\n  with {\n    y = x\n  }");
    }

    #[test]
    fn with_duplicate_key() {
        let errs =
            check_errs("x = \"a\"\n  with {\n    y = \"b\"\n  }\n  with {\n    y = \"c\"\n  }");
        assert!(
            errs.iter()
                .any(|e| e.contains("duplicate member") && e.contains("y")),
            "{errs:?}"
        );
    }

    #[test]
    fn with_body_and_with_conflict() {
        // Module body member conflicts with with clause member
        let errs = check_errs("x = {\n    a = \"1\"\n}\n  with {\n    a = \"2\"\n  }");
        assert!(
            errs.iter()
                .any(|e| e.contains("duplicate member") && e.contains("a")),
            "{errs:?}"
        );
    }

    // --- += ---

    #[test]
    fn add_basic() {
        check_ok("x = {\n    a = \"1\"\n}\nx += {\n    b = \"2\"\n}\ny = x.b");
    }

    #[test]
    fn add_undefined_lhs() {
        let errs = check_errs("x += {\n    a = \"1\"\n}");
        assert!(
            errs.iter()
                .any(|e| e.contains("must be declared before `+=`")),
            "{errs:?}"
        );
    }

    #[test]
    fn add_duplicate_key() {
        let errs = check_errs("x = {\n    a = \"1\"\n}\nx += {\n    a = \"2\"\n}");
        assert!(
            errs.iter()
                .any(|e| e.contains("duplicate member") && e.contains("a")),
            "{errs:?}"
        );
    }

    #[test]
    fn add_from_ref() {
        check_ok("m = {\n    a = \"1\"\n}\nx = {\n    b = \"2\"\n}\nx += m\ny = x.a");
    }

    // --- Value and members coexistence ---

    #[test]
    fn value_and_members() {
        // x has both a value and members
        check_ok("x = \"a\"\nx += {\n    y = \"b\"\n}\nz = x\nw = x.y");
    }

    // --- Operators ---

    #[test]
    fn op_both_sides_checked() {
        let errs = check_errs("x = a ; b");
        assert!(
            errs.len() >= 2,
            "expected errors for both a and b: {errs:?}"
        );
    }

    #[test]
    fn op_with_defined_names() {
        check_ok("a = \"x\"\nb = \"y\"\nx = a ; b");
    }

    // --- Compound tests ---

    #[test]
    fn compound_nested_module_with_params() {
        check_ok(
            r#"
T = "t"
m[x: T] = {
    inner = x
}
"#,
        );
    }

    #[test]
    fn compound_deep_dotted_ref_through_modules() {
        check_ok(
            r#"
a = {
    b = {
        c = {
            val = "leaf"
        }
    }
}
r = a.b.c.val
"#,
        );
    }

    #[test]
    fn compound_dotted_decl_with_params() {
        check_ok(
            r#"
T = "t"
m = "base"
m.f[x: T] = x
"#,
        );
    }

    #[test]
    fn compound_dotted_decl_then_ref() {
        check_ok(
            r#"
m = "root"
m.a = "1"
m.b = "2"
x = m.a
y = m.b
"#,
        );
    }

    #[test]
    fn compound_with_where_combined() {
        // with must come before where in Donut syntax
        check_ok(
            r#"
x = g
  with {
    a = "member"
  }
  where {
    g = "val"
  }
r = x.a
"#,
        );
    }

    #[test]
    fn compound_where_module_in_body() {
        check_ok(
            r#"
x = helper.val
  where {
    helper = {
        val = "x"
    }
  }
"#,
        );
    }

    #[test]
    fn compound_with_nested_with() {
        check_ok(
            r#"
x = "val"
  with {
    a = "inner"
      with {
        b = "nested"
      }
  }
r1 = x.a
r2 = x.a.b
"#,
        );
    }

    #[test]
    fn compound_add_with_and_dotted_combined() {
        // Module body + with + dotted decl + += all contributing members
        check_ok(
            r#"
m = {
    a = "1"
}
  with {
    b = "2"
  }
m.c = "3"
m += {
    d = "4"
}
r1 = m.a
r2 = m.b
r3 = m.c
r4 = m.d
"#,
        );
    }

    #[test]
    fn compound_add_transfers_deep_members() {
        check_ok(
            r#"
src = {
    inner = {
        val = "deep"
    }
}
dst = {
    other = "x"
}
dst += src
r = dst.inner.val
"#,
        );
    }

    #[test]
    fn compound_decorator_param_with_module() {
        check_ok(
            r#"
T = "t"
[p: T] m = {
    x = p
}
"#,
        );
    }

    #[test]
    fn compound_param_visible_in_with() {
        // Params are visible in with clauses (with is part of the RHS)
        check_ok(
            r#"
T = "t"
f[x: T] = "a"
  with {
    y = x
  }
"#,
        );
    }

    #[test]
    fn compound_name_visible_in_where() {
        // Declared name is visible in its own where clause
        check_ok(
            r#"
f = g
  where {
    g = f
  }
"#,
        );
    }

    #[test]
    fn compound_dotted_decl_duplicate_member() {
        let errs = check_errs(
            r#"
m = {
    x = "1"
}
m.x = "2"
"#,
        );
        assert!(
            errs.iter()
                .any(|e| e.contains("duplicate member") && e.contains("x")),
            "{errs:?}"
        );
    }

    #[test]
    fn compound_with_on_both_decls() {
        check_ok(
            r#"
a = "v1"
  with {
    x = "m1"
  }
b = "v2"
  with {
    y = a.x
  }
r1 = a.x
r2 = b.y
"#,
        );
    }

    #[test]
    fn compound_multi_name_decl_only() {
        // Multiple names allowed in declaration-only form
        check_ok(
            r#"
T = "t"
a b: T
r1 = a
r2 = b
"#,
        );
    }

    #[test]
    fn compound_kitchen_sink() {
        check_ok(
            r#"
T = "type"
U = "type2"

base = {
    core = {
        v = "x"
    }
    util = "u"
}

[p: T] ext[q: U] = base.core.v
  with {
    sub = "s"
  }
  where {
    helper = base.util
  }

ext.extra = "e"

r1 = ext.sub
r2 = ext.extra
r3 = base.core.v
"#,
        );
    }

    #[test]
    fn compound_error_deep_undefined_member() {
        let errs = check_errs(
            r#"
a = {
    b = {
        c = "x"
    }
}
r = a.b.missing.c
"#,
        );
        assert!(
            errs.iter()
                .any(|e| e.contains("undefined member") && e.contains("missing")),
            "{errs:?}"
        );
    }

    #[test]
    fn compound_error_add_then_duplicate_via_dotted() {
        let errs = check_errs(
            r#"
m = "base"
m += {
    x = "1"
}
m.x = "2"
"#,
        );
        assert!(
            errs.iter()
                .any(|e| e.contains("duplicate member") && e.contains("x")),
            "{errs:?}"
        );
    }

    // --- Anonymous block tests ---

    #[test]
    fn anon_block_names_visible_outside() {
        check_ok(
            r#"
{
    x = "a"
    y = "b"
}
r1 = x
r2 = y
"#,
        );
    }

    #[test]
    fn anon_block_with_decorator() {
        check_ok(
            r#"
T = "t"
[T] {
    x = "a"
    y = "b"
}
r = x
"#,
        );
    }

    #[test]
    fn anon_block_where() {
        check_ok(
            r#"
{
    x = g
}
  where {
    g = "val"
  }
r = x
"#,
        );
    }

    #[test]
    fn anon_block_deco_param_visible() {
        check_ok(
            r#"
T = "t"
[p: T] {
    x = p
}
r = x
"#,
        );
    }

    #[test]
    fn anon_block_deco_param_not_leaking() {
        let errs = check_errs(
            r#"
T = "t"
[p: T] {
    x = p
}
r = p
"#,
        );
        assert!(
            errs.iter()
                .any(|e| e.contains("undefined") && e.contains("p")),
            "{errs:?}"
        );
    }

    #[test]
    fn anon_block_where_not_leaking() {
        let errs = check_errs(
            r#"
{
    x = g
}
  where {
    g = "val"
  }
r = g
"#,
        );
        assert!(
            errs.iter()
                .any(|e| e.contains("undefined") && e.contains("g")),
            "{errs:?}"
        );
    }

    #[test]
    fn anon_block_with() {
        check_ok(
            r#"
{
    x = "a"
}
  with {
    y = "b"
  }
r1 = x
r2 = y
"#,
        );
    }

    #[test]
    fn anon_block_ordering() {
        // Top-to-bottom within the block
        let errs = check_errs(
            r#"
{
    x = y
    y = "a"
}
"#,
        );
        assert!(
            errs.iter()
                .any(|e| e.contains("undefined") && e.contains("y")),
            "{errs:?}"
        );
    }
}
