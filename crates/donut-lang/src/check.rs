use crate::types::common::*;
use crate::types::semtree;
use crate::types::token::Token;
use std::collections::HashMap;

#[derive(Clone, Copy)]
enum ItemType {
    Decl,  // x: T
    Alias, // x = ...
    Def,   // x := ...
    Param, // [x: T]
}

#[derive(Clone)]
struct Item {
    ty: Option<ItemType>,
    members: HashMap<String, Item>,
}

impl Item {
    fn new(ty: Option<ItemType>) -> Self {
        Item {
            ty,
            members: HashMap::new(),
        }
    }

    fn with_members(ty: Option<ItemType>, members: HashMap<String, Item>) -> Self {
        Item { ty, members }
    }
}

struct Checker<'a> {
    tokens: &'a [Token<'a>],
    scopes: Vec<HashMap<String, Item>>,
    errors: Vec<Error>,
}

impl<'a> Checker<'a> {
    fn new(tokens: &'a [Token<'a>]) -> Self {
        Checker {
            tokens,
            scopes: vec![HashMap::new()],
            errors: Vec::new(),
        }
    }

    fn error_at(&mut self, span: &TokenSpan, msg: impl Into<String>) {
        if let Some(token) = self.tokens.get(span.start) {
            self.errors.push((token.pos.clone(), msg.into()));
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define(&mut self, name: String, item: Item) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, item);
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

    /// Merge source members into target. Returns conflicting key names.
    fn merge_members(
        target: &mut HashMap<String, Item>,
        source: HashMap<String, Item>,
    ) -> Vec<String> {
        let mut conflicts = Vec::new();
        for (key, value) in source {
            if target.contains_key(&key) {
                conflicts.push(key);
            } else {
                target.insert(key, value);
            }
        }
        conflicts
    }

    // --- Program & Module ---

    fn check_program(&mut self, program: &A<semtree::Program>) {
        if let A::Accepted(prog, _) = program {
            self.check_decls(&prog.0);
        }
    }

    fn check_decls(&mut self, decls: &[A<semtree::Decl>]) {
        for decl_a in decls {
            if let A::Accepted(decl, _) = decl_a {
                // Phase 1: Check body (declared name NOT yet in scope)
                let body_members = self.check_decl_core(decl);
                // Phase 2: Register the declaration
                self.register_decl(decl, body_members);
            }
        }
    }

    /// Process a module and return its member map.
    fn collect_module_members(&mut self, module: &A<semtree::Module>) -> HashMap<String, Item> {
        if let A::Accepted(m, _) = module {
            match m {
                semtree::Module::Block(decls) => {
                    self.push_scope();
                    self.check_decls(decls);
                    let members = self.scopes.last().cloned().unwrap_or_default();
                    self.pop_scope();
                    return members;
                }
                semtree::Module::Import(_) => {} // TODO
            }
        }
        HashMap::new()
    }

    // --- Registration ---

    fn register_decl(&mut self, decl: &semtree::Decl, body_members: HashMap<String, Item>) {
        if let semtree::DeclMain::Unit(unit_a) = &decl.main {
            if let A::Accepted(unit, _) = unit_a {
                // For +=, merge into existing item
                if matches!(unit.op.inner(), Some(semtree::AssignOp::Add)) {
                    self.register_add(unit, body_members);
                    return;
                }

                // Build the item
                let item_type = Self::decl_item_type(unit);
                let item = Item::with_members(item_type, body_members);

                // Register each declared name
                for name_a in &unit.names {
                    if let A::Accepted(pd, _) = name_a {
                        self.register_path_decl(pd, item.clone());
                    }
                }
            }
        }
        // DeclMain::Mod is anonymous, nothing to register
    }

    fn register_path_decl(&mut self, pd: &semtree::PathDecl, item: Item) {
        let segs = &pd.0;
        if segs.is_empty() {
            return;
        }

        if segs.len() == 1 {
            if let Some(A::Accepted(seg, _)) = segs.first() {
                if let A::Accepted(name, _) = &seg.0 {
                    self.define(name.0.clone(), item);
                }
            }
        } else {
            self.insert_into_dotted(segs, item);
        }
    }

    fn insert_into_dotted(&mut self, segs: &[A<semtree::SegmentDecl>], item: Item) {
        if segs.len() < 2 {
            return;
        }

        let first_name = match &segs[0] {
            A::Accepted(seg, _) => match &seg.0 {
                A::Accepted(name, _) => name.0.clone(),
                A::Error() => return,
            },
            A::Error() => return,
        };
        let last_name = match &segs[segs.len() - 1] {
            A::Accepted(seg, _) => match &seg.0 {
                A::Accepted(name, _) => name.0.clone(),
                A::Error() => return,
            },
            A::Error() => return,
        };
        let last_span = match &segs[segs.len() - 1] {
            A::Accepted(_, span) => span.clone(),
            A::Error() => return,
        };

        let mut conflict = false;
        if let Some(root) = self.lookup_mut(&first_name) {
            let mut current = root;
            for seg_a in &segs[1..segs.len() - 1] {
                if let A::Accepted(seg, _) = seg_a {
                    if let A::Accepted(name, _) = &seg.0 {
                        if !current.members.contains_key(&name.0) {
                            return; // Error already reported in check phase
                        }
                        current = current.members.get_mut(&name.0).unwrap();
                    }
                }
            }
            if current.members.contains_key(&last_name) {
                conflict = true;
            } else {
                current.members.insert(last_name.clone(), item);
            }
        }

        if conflict {
            self.error_at(
                &last_span,
                format!("duplicate member `{}`", last_name),
            );
        }
    }

    fn register_add(&mut self, unit: &semtree::DeclUnit, body_members: HashMap<String, Item>) {
        // For += with value reference, try to resolve the reference's members
        let members_to_merge = if body_members.is_empty() {
            if let Some(semtree::ValMod::Val(val_a)) = &unit.body {
                self.resolve_val_as_members(val_a)
            } else {
                HashMap::new()
            }
        } else {
            body_members
        };

        for name_a in &unit.names {
            if let A::Accepted(pd, _) = name_a {
                self.merge_into_path(pd, members_to_merge.clone());
            }
        }
    }

    fn resolve_val_as_members(&self, val_a: &A<semtree::Val>) -> HashMap<String, Item> {
        if let A::Accepted(val, _) = val_a {
            if let semtree::Val::Path(path_a) = val {
                if let A::Accepted(path, _) = path_a.as_ref() {
                    let segments = &path.0;
                    let mut current: Option<&Item> = None;
                    for (i, seg_a) in segments.iter().enumerate() {
                        if let A::Accepted(seg, _) = seg_a {
                            if let A::Accepted(name, _) = &seg.0 {
                                if i == 0 {
                                    current = self.lookup(&name.0);
                                } else {
                                    current =
                                        current.and_then(|item| item.members.get(&name.0));
                                }
                            }
                        }
                    }
                    if let Some(item) = current {
                        return item.members.clone();
                    }
                }
            }
        }
        HashMap::new()
    }

    fn merge_into_path(&mut self, pd: &semtree::PathDecl, new_members: HashMap<String, Item>) {
        if new_members.is_empty() {
            return;
        }
        let segs = &pd.0;
        if segs.is_empty() {
            return;
        }

        if segs.len() == 1 {
            if let Some(A::Accepted(seg, span)) = segs.first() {
                if let A::Accepted(name, _) = &seg.0 {
                    let conflicts = if let Some(existing) = self.lookup_mut(&name.0) {
                        Self::merge_members(&mut existing.members, new_members)
                    } else {
                        Vec::new()
                    };
                    for key in conflicts {
                        self.error_at(span, format!("duplicate member `{}`", key));
                    }
                }
            }
        }
        // TODO: multi-segment += / with paths
    }

    fn decl_item_type(unit: &semtree::DeclUnit) -> Option<ItemType> {
        match &unit.body {
            Some(semtree::ValMod::Mod(_)) => None,
            Some(semtree::ValMod::Val(_)) => match unit.op.inner() {
                Some(semtree::AssignOp::Alias) => Some(ItemType::Alias),
                Some(semtree::AssignOp::Def) => Some(ItemType::Def),
                Some(semtree::AssignOp::Decl) => Some(ItemType::Decl),
                Some(semtree::AssignOp::Add) => None,
                None => None,
            },
            None => match unit.op.inner() {
                Some(semtree::AssignOp::Decl) => Some(ItemType::Decl),
                _ => None,
            },
        }
    }

    // --- Decl checking ---

    fn check_decl_core(&mut self, decl: &semtree::Decl) -> HashMap<String, Item> {
        // Check decorators in current scope
        for deco in &decl.decos {
            self.check_decorator(deco);
        }

        match &decl.main {
            semtree::DeclMain::Unit(unit_a) => {
                if let A::Accepted(unit, _) = unit_a {
                    return self.check_decl_unit(
                        unit,
                        &decl.decos,
                        &decl.with_clauses,
                        &decl.where_clauses,
                    );
                }
            }
            semtree::DeclMain::Mod(mod_a) => {
                self.collect_module_members(mod_a);
            }
        }
        HashMap::new()
    }

    fn check_decl_unit(
        &mut self,
        unit: &semtree::DeclUnit,
        decos: &[A<semtree::Decorator>],
        with_clauses: &[A<semtree::Module>],
        where_clauses: &[A<semtree::Module>],
    ) -> HashMap<String, Item> {
        // 1. Check path declarations and type annotation in current (outer) scope
        for name_a in &unit.names {
            if let A::Accepted(pd, _) = name_a {
                self.check_path_decl_refs(pd);
            }
        }
        if let Some(ty) = &unit.ty {
            self.check_val_a(ty);
        }

        // For +=, check that LHS already exists
        if matches!(unit.op.inner(), Some(semtree::AssignOp::Add)) {
            for name_a in &unit.names {
                if let A::Accepted(pd, _) = name_a {
                    if pd.0.len() == 1 {
                        if let Some(A::Accepted(seg, span)) = pd.0.first() {
                            if let A::Accepted(name, _) = &seg.0 {
                                if self.lookup(&name.0).is_none() {
                                    self.error_at(
                                        span,
                                        format!(
                                            "`{}` must be declared before `+=`",
                                            name.0
                                        ),
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }

        // 2. Push inner scope for params + where
        self.push_scope();

        // Register decorator params
        for deco_a in decos {
            if let A::Accepted(semtree::Decorator::Param(name_a, _), _) = deco_a {
                if let A::Accepted(name, _) = name_a {
                    self.define(name.0.clone(), Item::new(Some(ItemType::Param)));
                }
            }
        }

        // Register declaration params (from all PathDecls)
        for name_a in &unit.names {
            if let A::Accepted(pd, _) = name_a {
                for seg_a in &pd.0 {
                    if let A::Accepted(seg, _) = seg_a {
                        for param_a in &seg.1 .0 {
                            if let A::Accepted(param, _) = param_a {
                                if let A::Accepted(name, _) = &param.name {
                                    self.define(
                                        name.0.clone(),
                                        Item::new(Some(ItemType::Param)),
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }

        // Temporarily register declared name(s) for self-reference in where/body/with
        if !matches!(unit.op.inner(), Some(semtree::AssignOp::Add)) {
            let item_type = Self::decl_item_type(unit);
            for name_a in &unit.names {
                if let A::Accepted(pd, _) = name_a {
                    if pd.0.len() == 1 {
                        if let Some(A::Accepted(seg, _)) = pd.0.first() {
                            if let A::Accepted(name, _) = &seg.0 {
                                self.define(name.0.clone(), Item::new(item_type));
                            }
                        }
                    }
                }
            }
        }

        // Check and register where clause names (visible in body)
        for wm in where_clauses {
            self.check_and_register_where(wm);
        }

        // 3. Check body in inner scope and extract members
        let body_members = match &unit.body {
            Some(semtree::ValMod::Val(v)) => {
                self.check_val_a(v);
                HashMap::new()
            }
            Some(semtree::ValMod::Mod(m)) => self.collect_module_members(m),
            None => HashMap::new(),
        };

        // 4. Process with clauses in inner scope (params + declared name visible)
        let mut all_members = body_members;
        for with_mod in with_clauses {
            let with_members = self.collect_module_members(with_mod);
            let conflicts = Self::merge_members(&mut all_members, with_members);
            if let A::Accepted(_, span) = with_mod {
                for key in conflicts {
                    self.error_at(span, format!("duplicate member `{}`", key));
                }
            }
        }

        self.pop_scope();
        all_members
    }

    fn check_path_decl_refs(&mut self, pd: &semtree::PathDecl) {
        let segs = &pd.0;

        // For multi-segment paths (a.b.c), check all but last exist
        if segs.len() > 1 {
            let mut current_item: Option<Item> = None;

            for (i, seg_a) in segs[..segs.len() - 1].iter().enumerate() {
                if let A::Accepted(seg, span) = seg_a {
                    if let A::Accepted(name, _) = &seg.0 {
                        if i == 0 {
                            match self.lookup(&name.0) {
                                None => {
                                    self.error_at(
                                        span,
                                        format!("undefined name `{}`", name.0),
                                    );
                                    break;
                                }
                                Some(item) => {
                                    current_item = Some(item.clone());
                                }
                            }
                        } else {
                            let next = current_item
                                .take()
                                .and_then(|item| item.members.get(&name.0).cloned());
                            if next.is_none() {
                                self.error_at(
                                    span,
                                    format!("undefined member `{}`", name.0),
                                );
                                break;
                            }
                            current_item = next;
                        }
                    }
                }
            }
        }

        // Check param types on all segments
        for seg_a in segs {
            if let A::Accepted(seg, _) = seg_a {
                for param_a in &seg.1 .0 {
                    if let A::Accepted(param, _) = param_a {
                        self.check_val_a(&param.ty);
                    }
                }
            }
        }

        // Check optional val suffix
        if let Some(v) = &pd.1 {
            self.check_val_a(v);
        }
    }

    fn check_and_register_where(&mut self, module: &A<semtree::Module>) {
        if let A::Accepted(semtree::Module::Block(decls), _) = module {
            self.check_decls(decls);
        }
    }

    // --- Decorator ---

    fn check_decorator(&mut self, deco: &A<semtree::Decorator>) {
        if let A::Accepted(d, _) = deco {
            match d {
                semtree::Decorator::Param(_, ty) => self.check_val_a(ty),
                semtree::Decorator::Deco(v) => self.check_val_a(v),
            }
        }
    }

    // --- Val ---

    fn check_val_a(&mut self, val: &A<semtree::Val>) {
        if let A::Accepted(v, _) = val {
            self.check_val(v);
        }
    }

    fn check_val(&mut self, val: &semtree::Val) {
        match val {
            semtree::Val::Path(path_a) => {
                if let A::Accepted(path, _) = path_a.as_ref() {
                    self.check_path_ref(path);
                }
            }
            semtree::Val::Lit(lit_a) => {
                if let A::Accepted(lit, _) = lit_a {
                    self.check_lit(lit);
                }
            }
            semtree::Val::Op(l, _, params, r) => {
                self.check_val(l);
                if let Some(pv_a) = params {
                    if let A::Accepted(pv, _) = pv_a {
                        self.check_params_val(pv);
                    }
                }
                self.check_val(r);
            }
        }
    }

    fn check_path_ref(&mut self, path: &semtree::Path) {
        let segments = &path.0;
        let mut current_item: Option<Item> = None;
        let mut failed = false;

        for (i, seg_a) in segments.iter().enumerate() {
            if let A::Accepted(seg, span) = seg_a {
                if !failed {
                    if let A::Accepted(name, _) = &seg.0 {
                        if i == 0 {
                            match self.lookup(&name.0) {
                                None => {
                                    self.error_at(
                                        span,
                                        format!("undefined name `{}`", name.0),
                                    );
                                    failed = true;
                                }
                                Some(item) => {
                                    current_item = Some(item.clone());
                                }
                            }
                        } else {
                            let next = current_item
                                .take()
                                .and_then(|item| item.members.get(&name.0).cloned());
                            if next.is_none() {
                                self.error_at(
                                    span,
                                    format!("undefined member `{}`", name.0),
                                );
                                failed = true;
                            }
                            current_item = next;
                        }
                    }
                }
                self.check_params_val(&seg.1);
            }
        }

        if let Some(v) = &path.1 {
            self.check_val_a(v);
        }
    }

    fn check_params_val(&mut self, params: &semtree::ParamsVal) {
        for param_a in &params.0 {
            if let A::Accepted(param, _) = param_a {
                self.check_val_a(&param.val);
            }
        }
    }

    fn check_lit(&mut self, lit: &semtree::Lit) {
        match lit {
            semtree::Lit::Array(vs) => {
                for v in vs {
                    self.check_val_a(v);
                }
            }
            semtree::Lit::Object(kvs) => {
                for (_, v) in kvs {
                    self.check_val_a(v);
                }
            }
            _ => {}
        }
    }
}

pub fn check(program: &A<semtree::Program>, tokens: &[Token]) -> Vec<Error> {
    let mut checker = Checker::new(tokens);
    checker.check_program(program);
    checker.errors
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
        let errors = check(&sem_prog, &tokens);
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
            errs.iter().any(|e| e.contains("undefined") && e.contains("y")),
            "{errs:?}"
        );
    }

    #[test]
    fn scope_ordering() {
        // y is not yet defined when x is checked
        let errs = check_errs("x = y\ny = \"a\"");
        assert!(
            errs.iter().any(|e| e.contains("undefined") && e.contains("y")),
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
            errs.iter().any(|e| e.contains("undefined") && e.contains("x")),
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
            errs.iter().any(|e| e.contains("undefined") && e.contains("g")),
            "{errs:?}"
        );
    }

    #[test]
    fn where_ordering() {
        // within where, top-to-bottom
        let errs = check_errs("x = g\n  where {\n    g = h\n    h = \"a\"\n  }");
        assert!(
            errs.iter().any(|e| e.contains("undefined") && e.contains("h")),
            "{errs:?}"
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
            errs.iter().any(|e| e.contains("undefined") && e.contains("x")),
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
        assert!(errs.len() >= 2, "expected errors for both a and b: {errs:?}");
    }

    #[test]
    fn op_with_defined_names() {
        check_ok("a = \"x\"\nb = \"y\"\nx = a ; b");
    }

    // --- Compound tests ---

    #[test]
    fn compound_nested_module_with_params() {
        check_ok(r#"
T = "t"
m[x: T] = {
    inner = x
}
"#);
    }

    #[test]
    fn compound_deep_dotted_ref_through_modules() {
        check_ok(r#"
a = {
    b = {
        c = {
            val = "leaf"
        }
    }
}
r = a.b.c.val
"#);
    }

    #[test]
    fn compound_dotted_decl_with_params() {
        check_ok(r#"
T = "t"
m = "base"
m.f[x: T] = x
"#);
    }

    #[test]
    fn compound_dotted_decl_then_ref() {
        check_ok(r#"
m = "root"
m.a = "1"
m.b = "2"
x = m.a
y = m.b
"#);
    }

    #[test]
    fn compound_with_where_combined() {
        // with must come before where in Donut syntax
        check_ok(r#"
x = g
  with {
    a = "member"
  }
  where {
    g = "val"
  }
r = x.a
"#);
    }

    #[test]
    fn compound_where_module_in_body() {
        check_ok(r#"
x = helper.val
  where {
    helper = {
        val = "x"
    }
  }
"#);
    }

    #[test]
    fn compound_with_nested_with() {
        check_ok(r#"
x = "val"
  with {
    a = "inner"
      with {
        b = "nested"
      }
  }
r1 = x.a
r2 = x.a.b
"#);
    }

    #[test]
    fn compound_add_with_and_dotted_combined() {
        // Module body + with + dotted decl + += all contributing members
        check_ok(r#"
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
"#);
    }

    #[test]
    fn compound_add_transfers_deep_members() {
        check_ok(r#"
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
"#);
    }

    #[test]
    fn compound_decorator_param_with_module() {
        check_ok(r#"
T = "t"
[p: T] m = {
    x = p
}
"#);
    }

    #[test]
    fn compound_param_visible_in_with() {
        // Params are visible in with clauses (with is part of the RHS)
        check_ok(r#"
T = "t"
f[x: T] = "a"
  with {
    y = x
  }
"#);
    }

    #[test]
    fn compound_name_visible_in_where() {
        // Declared name is visible in its own where clause
        check_ok(r#"
f = g
  where {
    g = f
  }
"#);
    }

    #[test]
    fn compound_dotted_decl_duplicate_member() {
        let errs = check_errs(r#"
m = {
    x = "1"
}
m.x = "2"
"#);
        assert!(
            errs.iter()
                .any(|e| e.contains("duplicate member") && e.contains("x")),
            "{errs:?}"
        );
    }

    #[test]
    fn compound_with_on_both_decls() {
        check_ok(r#"
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
"#);
    }

    #[test]
    fn compound_multi_name_decl_only() {
        // Multiple names allowed in declaration-only form
        check_ok(r#"
T = "t"
a b: T
r1 = a
r2 = b
"#);
    }

    #[test]
    fn compound_kitchen_sink() {
        check_ok(r#"
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
"#);
    }

    #[test]
    fn compound_error_deep_undefined_member() {
        let errs = check_errs(r#"
a = {
    b = {
        c = "x"
    }
}
r = a.b.missing.c
"#);
        assert!(
            errs.iter()
                .any(|e| e.contains("undefined member") && e.contains("missing")),
            "{errs:?}"
        );
    }

    #[test]
    fn compound_error_add_then_duplicate_via_dotted() {
        let errs = check_errs(r#"
m = "base"
m += {
    x = "1"
}
m.x = "2"
"#);
        assert!(
            errs.iter()
                .any(|e| e.contains("duplicate member") && e.contains("x")),
            "{errs:?}"
        );
    }
}
