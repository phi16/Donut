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
    #[allow(dead_code)]
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

    fn collect_module_members(&mut self, module: &A<semtree::Module>) -> HashMap<String, Item> {
        match module.inner() {
            Some(semtree::Module::Block(decls)) => {
                self.push_scope();
                for d in decls {
                    d.check(self);
                }
                let members = self.scopes.last().cloned().unwrap_or_default();
                self.pop_scope();
                members
            }
            _ => HashMap::new(), // Import: TODO
        }
    }

    // --- Registration ---

    fn register_unit_results(&mut self, unit: &semtree::DeclUnit, body_members: HashMap<String, Item>) {
        // For +=, merge into existing item
        if matches!(unit.op.inner(), Some(semtree::AssignOp::Add)) {
            self.register_add(unit, body_members);
            return;
        }

        // Build the item and register each declared name
        let item_type = Self::decl_item_type(unit);
        let item = Item::with_members(item_type, body_members);
        for name_a in &unit.names {
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
                current.members.insert(last_name.to_owned(), item);
            }
        }

        if conflict {
            self.error_at(last_span, format!("duplicate member `{}`", last_name));
        }
    }

    fn register_add(&mut self, unit: &semtree::DeclUnit, body_members: HashMap<String, Item>) {
        let members_to_merge = match &unit.body {
            Some(semtree::ValMod::Mod(_)) => body_members,
            Some(semtree::ValMod::Val(val_a)) => self.resolve_val_as_members(val_a),
            None => HashMap::new(),
        };

        for name_a in &unit.names {
            if let A::Accepted(pd, _) = name_a {
                self.merge_into_path(pd, members_to_merge.clone());
            }
        }
    }

    fn resolve_val_as_members(&self, val_a: &A<semtree::Val>) -> HashMap<String, Item> {
        let path = match val_a.inner() {
            Some(semtree::Val::Path(path_a)) => match path_a.inner() {
                Some(path) => path,
                None => return HashMap::new(),
            },
            _ => return HashMap::new(),
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
        current
            .map(|item| item.members.clone())
            .unwrap_or_default()
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
            if let Some((name, span)) = segs.first().and_then(seg_decl_name) {
                let conflicts = if let Some(existing) = self.lookup_mut(name) {
                    Self::merge_members(&mut existing.members, new_members)
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

    // --- Unit locals ---

    fn register_unit_locals(&mut self, unit: &semtree::DeclUnit) {
        let is_add = matches!(unit.op.inner(), Some(semtree::AssignOp::Add));
        let item_type = Self::decl_item_type(unit);
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
                if !is_add && pd.0.len() == 1 {
                    if let Some((name, _)) = pd.0.first().and_then(seg_decl_name) {
                        self.define(name.to_owned(), Item::new(item_type));
                    }
                }
            }
        }
    }

}

// --- Check impls ---

impl Check for semtree::Program {
    fn check(&self, c: &mut Checker<'_>) {
        for d in &self.0 {
            d.check(c);
        }
    }
}

impl Check for semtree::Decl {
    fn check(&self, c: &mut Checker<'_>) {
        for deco in &self.decos {
            deco.check(c);
        }

        let unit = match &self.main {
            semtree::DeclMain::Unit(u) => u.inner(),
            semtree::DeclMain::Mod(_) => None,
        };

        // Unit: pre-checks in outer scope
        if let Some(unit) = unit {
            unit.check(c);
        }

        // 1. Local scope (deco params + where bindings)
        c.push_scope();
        for deco_a in &self.decos {
            if let A::Accepted(semtree::Decorator::Param(name_a, _), _) = deco_a {
                if let A::Accepted(name, _) = name_a {
                    c.define(name.0.clone(), Item::new(Some(ItemType::Param)));
                }
            }
        }
        if let Some(unit) = unit {
            c.register_unit_locals(unit);
        }
        for wm in self.where_clauses.iter().rev() {
            if let A::Accepted(semtree::Module::Block(decls), _) = wm {
                for d in decls {
                    d.check(c);
                }
            }
        }

        // 2. Result = body + with
        let body_members = match &self.main {
            semtree::DeclMain::Unit(_) => match unit {
                Some(unit) => match &unit.body {
                    Some(semtree::ValMod::Val(v)) => {
                        v.check(c);
                        HashMap::new()
                    }
                    Some(semtree::ValMod::Mod(m)) => c.collect_module_members(m),
                    None => HashMap::new(),
                },
                None => HashMap::new(),
            },
            semtree::DeclMain::Mod(mod_a) => c.collect_module_members(mod_a),
        };
        let mut result = body_members;
        for with_mod in &self.with_clauses {
            let with_members = c.collect_module_members(with_mod);
            let conflicts = Checker::merge_members(&mut result, with_members);
            if let A::Accepted(_, span) = with_mod {
                for key in conflicts {
                    c.error_at(span, format!("duplicate member `{}`", key));
                }
            }
        }

        c.pop_scope();

        // 3. Unit → register / 4. Mod → promote
        match &self.main {
            semtree::DeclMain::Unit(_) => {
                if let Some(unit) = unit {
                    c.register_unit_results(unit, result);
                }
            }
            semtree::DeclMain::Mod(mod_a) => {
                if let Some(scope) = c.scopes.last_mut() {
                    let conflicts = Checker::merge_members(scope, result);
                    if let A::Accepted(_, span) = mod_a {
                        for key in conflicts {
                            c.error_at(span, format!("duplicate member `{}`", key));
                        }
                    }
                }
            }
        }
    }
}

impl Check for semtree::DeclUnit {
    fn check(&self, c: &mut Checker<'_>) {
        for name_a in &self.names {
            if let Some((pd, _)) = name_a.accepted() {
                pd.check(c);
            }
        }
        if let Some(ty) = &self.ty {
            ty.check(c);
        }
        if matches!(self.op.inner(), Some(semtree::AssignOp::Add)) {
            for name_a in &self.names {
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
}

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
                for param_a in &seg.1 .0 {
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

pub fn check(program: &A<semtree::Program>, tokens: &[Token]) -> Vec<Error> {
    let mut checker = Checker::new(tokens);
    program.check(&mut checker);
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

    #[test]
    fn where_reverse_order() {
        // With reverse processing, the second where's binding is registered first,
        // making it visible in the first where clause
        check_ok(r#"
x = "val"
  where {
    a = b
  }
  where {
    b = "inner"
  }
"#);
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

    // --- Anonymous block tests ---

    #[test]
    fn anon_block_names_visible_outside() {
        check_ok(r#"
{
    x = "a"
    y = "b"
}
r1 = x
r2 = y
"#);
    }

    #[test]
    fn anon_block_with_decorator() {
        check_ok(r#"
T = "t"
[T] {
    x = "a"
    y = "b"
}
r = x
"#);
    }

    #[test]
    fn anon_block_where() {
        check_ok(r#"
{
    x = g
}
  where {
    g = "val"
  }
r = x
"#);
    }

    #[test]
    fn anon_block_deco_param_visible() {
        check_ok(r#"
T = "t"
[p: T] {
    x = p
}
r = x
"#);
    }

    #[test]
    fn anon_block_deco_param_not_leaking() {
        let errs = check_errs(r#"
T = "t"
[p: T] {
    x = p
}
r = p
"#);
        assert!(
            errs.iter().any(|e| e.contains("undefined") && e.contains("p")),
            "{errs:?}"
        );
    }

    #[test]
    fn anon_block_where_not_leaking() {
        let errs = check_errs(r#"
{
    x = g
}
  where {
    g = "val"
  }
r = g
"#);
        assert!(
            errs.iter().any(|e| e.contains("undefined") && e.contains("g")),
            "{errs:?}"
        );
    }

    #[test]
    fn anon_block_with() {
        check_ok(r#"
{
    x = "a"
}
  with {
    y = "b"
  }
r1 = x
r2 = y
"#);
    }

    #[test]
    fn anon_block_ordering() {
        // Top-to-bottom within the block
        let errs = check_errs(r#"
{
    x = y
    y = "a"
}
"#);
        assert!(
            errs.iter().any(|e| e.contains("undefined") && e.contains("y")),
            "{errs:?}"
        );
    }
}
