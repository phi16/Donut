use std::collections::HashMap;

use crate::types::token::Token;
use crate::types::common::{Error, TokenSpan};
use crate::types::env::{self, ArrowTy, Meta, Ty};
use crate::types::item::*;
use donut_core::common::{ExtId, PrimId, PureVal};

struct Checker<'a> {
    program: &'a Program,
    tokens: &'a [Token<'a>],

    // Output
    env_items: Vec<Option<env::Item>>,
    env_defs: Vec<Option<env::Def>>,
    modules: HashMap<DefId, env::Module>,
    prim_item: HashMap<PrimId, ItemId>,

    // State
    next_prim: u64,
    prefixes: Vec<String>,
    // Ref → PureVal for checked items/defs
    checked: HashMap<Ref, PureVal>,

    errors: Vec<Error>,
}

impl<'a> Checker<'a> {
    fn new(program: &'a Program, tokens: &'a [Token<'a>]) -> Self {
        let n_items = program.items.len();
        let n_defs = program.defs.len();
        let mut checker = Checker {
            program,
            tokens,
            env_items: (0..n_items).map(|_| None).collect(),
            env_defs: (0..n_defs).map(|_| None).collect(),
            modules: HashMap::new(),
            prim_item: HashMap::new(),
            next_prim: 1, // 0 reserved for meta
            prefixes: Vec::new(),
            checked: HashMap::new(),
            errors: Vec::new(),
        };
        checker.register_builtins();
        checker
    }

    fn register_builtins(&mut self) {
        let builtins: &[(&str, Ty)] = &[("meta", Ty::Meta), ("*", Ty::Star)];
        for (i, item) in self.program.items.iter().enumerate() {
            if let Some((_, ty)) = builtins.iter().find(|(name, _)| *name == item.lname) {
                let item_id = ItemId(i);
                let pv: PureVal = Meta::Ty(ty.clone()).into();
                self.checked.insert(Ref::Item(item_id), pv);
                self.env_items[i] = Some(env::Item {
                    cname: item.cname.clone(),
                    lname: item.lname.clone(),
                    kind: item.kind,
                    ty: Ty::Meta,
                    prim_id: None,
                    params: vec![],
                });
            }
        }
    }

    fn error_at(&mut self, span: &TokenSpan, msg: impl Into<String>) {
        if let Some(token) = self.tokens.get(span.start) {
            self.errors.push((token.pos.clone(), msg.into()));
        }
    }

    fn fresh_prim_id(&mut self) -> PrimId {
        let id = PrimId(self.next_prim);
        self.next_prim += 1;
        id
    }

    fn current_prefix(&self) -> String {
        self.prefixes.join(".")
    }

    // --- DefTree traversal ---

    fn process_trees(&mut self, trees: &[DefTree]) {
        for tree in trees {
            match tree {
                DefTree::Def(def_id) => self.process_def(*def_id),
                DefTree::Scope { def_id, children } => self.process_scope(*def_id, children),
            }
        }
    }

    fn process_def(&mut self, def_id: DefId) {
        if self.env_defs[def_id.0].is_some() {
            return; // already checked
        }
        let def = self.program.def(def_id);
        let span = def.span.clone();
        let lname = def.lname.clone();
        let qname = def.qname.clone();
        let origin = def.origin.clone();
        let param_counts = def.param_counts.clone();

        // Evaluate type
        let ty = if let Some(ty_val) = def.ty {
            self.eval_ty(ty_val)
        } else {
            Ty::Star // default; will be refined
        };

        // Evaluate body
        let (item, val) = match &self.program.def(def_id).body {
            DefBody::Decl { item: item_id } => {
                let item_id = *item_id;
                self.check_item(item_id, &ty);
                let pv = self.checked[&Ref::Item(item_id)].clone();
                (Some(item_id), pv)
            }
            DefBody::Alias { val: val_id } => {
                let val_id = *val_id;
                let pv = self.eval_val(val_id);
                (None, pv)
            }
            DefBody::Functor { .. } => {
                // TODO: functor checking
                (None, PureVal::App(ExtId(0), vec![]))
            }
            DefBody::None => {
                (None, PureVal::App(ExtId(0), vec![]))
            }
        };

        // Evaluate decorators
        let decos: Vec<PureVal> = self.program.def(def_id).decos
            .iter()
            .map(|&d| self.eval_val(d))
            .collect();

        // Evaluate params → ItemIds
        let params: Vec<ItemId> = self.program.def(def_id).params
            .iter()
            .filter_map(|p| {
                // Find the ItemId for this param by name
                // Params should have been checked when entering scope
                None // TODO
            })
            .collect();

        let env_def = env::Def {
            qname,
            lname,
            span,
            item,
            ty: ty.clone(),
            params,
            val,
            decos,
            origin,
            param_counts,
        };
        self.checked.insert(Ref::Def(def_id), env_def.val.clone());
        self.env_defs[def_id.0] = Some(env_def);
    }

    fn process_scope(&mut self, def_id: DefId, children: &[DefTree]) {
        let def = self.program.def(def_id);
        let lname = def.lname.clone();

        self.prefixes.push(lname);

        // Push params into scope
        let params = self.program.def(def_id).params.clone();
        for param in &params {
            // Each param references an Item via the scope
            // We need to check param items here
        }

        // Process children
        self.process_trees(children);

        // Build/update module for this def
        let module = self.modules.entry(def_id).or_insert_with(|| env::Module {
            this: Some(def_id),
            lookup: HashMap::new(),
        });
        // Add children defs to module lookup
        for tree in children {
            if let DefTree::Def(child_id) = tree {
                let child_def = self.program.def(*child_id);
                module.lookup.insert(child_def.lname.clone(), *child_id);
            }
        }

        self.prefixes.pop();
    }

    // --- Type evaluation ---

    fn eval_ty(&mut self, val_id: ValId) -> Ty {
        let pv = self.eval_val(val_id);
        self.extract_ty(&pv, val_id)
    }

    fn extract_ty(&mut self, pv: &PureVal, val_id: ValId) -> Ty {
        if let Some(Meta::Ty(ty)) = env::as_meta(pv) {
            ty.clone()
        } else {
            let span = self.program.val_span(val_id);
            self.error_at(span, "expected type expression");
            Ty::Star
        }
    }

    // --- Value evaluation ---

    fn eval_val(&mut self, val_id: ValId) -> PureVal {
        let val = self.program.val(val_id);
        match val {
            Val::Path(path) => self.eval_val_path(path, val_id),
            Val::Lit(lit) => self.eval_lit(lit, val_id),
            Val::Comp(axis, children) => self.eval_comp(*axis, children, val_id),
            Val::CompStar(children) => self.eval_comp_star(children, val_id),
            Val::Arrow(kind, l, r) => {
                let l_val = self.eval_val(*l);
                let r_val = self.eval_val(*r);
                let ty = match kind {
                    ArrowKind::To => {
                        Ty::Arrow(1, ArrowTy::To, l_val, r_val) // TODO: infer level
                    }
                    ArrowKind::Eq => {
                        Ty::Arrow(1, ArrowTy::Eq, l_val, r_val) // TODO: infer level
                    }
                    ArrowKind::Functor => {
                        Ty::Functor(l_val, r_val)
                    }
                };
                Meta::Ty(ty).into()
            }
            Val::Hole(_) => PureVal::App(ExtId(0), vec![]), // TODO
        }
    }

    fn eval_val_path(&mut self, path: &Path, val_id: ValId) -> PureVal {
        if let Some(pv) = self.resolve_ref(path.target) {
            pv
        } else {
            let span = self.program.val_span(val_id);
            self.error_at(span, "unresolved path");
            PureVal::App(ExtId(0), vec![])
        }
    }

    fn eval_lit(&mut self, lit: &Lit, val_id: ValId) -> PureVal {
        match lit {
            Lit::Number(s) => {
                // Try as nat
                if let Ok(n) = s.parse::<u64>() {
                    Meta::Nat(n).into()
                } else if let Ok(f) = s.parse::<f64>() {
                    Meta::Rat(f).into()
                } else {
                    let span = self.program.val_span(val_id);
                    self.error_at(span, format!("invalid number `{}`", s));
                    Meta::Nat(0).into()
                }
            }
            Lit::String(_s) => {
                // String literals → used for import paths etc.
                PureVal::App(ExtId(0), vec![])
            }
            Lit::Array(_) | Lit::Object(_) => {
                // TODO
                PureVal::App(ExtId(0), vec![])
            }
        }
    }

    fn eval_comp(&mut self, _axis: u8, _children: &[ValId], _val_id: ValId) -> PureVal {
        // TODO: evaluate composition
        PureVal::App(ExtId(0), vec![])
    }

    fn eval_comp_star(&mut self, _children: &[ValId], _val_id: ValId) -> PureVal {
        // TODO
        PureVal::App(ExtId(0), vec![])
    }

    // --- Path resolution ---

    fn resolve_ref(&self, target: Ref) -> Option<PureVal> {
        self.checked.get(&target).cloned()
    }

    // --- Item checking ---

    fn check_item(&mut self, item_id: ItemId, ty: &Ty) {
        if self.checked.contains_key(&Ref::Item(item_id)) {
            return;
        }
        let item = self.program.item(item_id);
        let prim_id = self.fresh_prim_id();
        let ext_id = ExtId(item_id.0 as u64);

        let pv = PureVal::App(ext_id, vec![]);

        self.checked.insert(Ref::Item(item_id), pv);
        self.prim_item.insert(prim_id, item_id);

        let env_item = env::Item {
            cname: item.cname.clone(),
            lname: item.lname.clone(),
            kind: item.kind,
            ty: ty.clone(),
            prim_id: Some(prim_id),
            params: vec![], // TODO
        };
        self.env_items[item_id.0] = Some(env_item);
    }

    // --- Finalize ---

    fn into_result(self) -> (env::Env, Vec<Error>) {
        // Build root module
        let mut root_lookup = HashMap::new();
        for &def_id in &self.program.root.entries {
            let def = self.program.def(def_id);
            root_lookup.insert(def.lname.clone(), def_id);
        }
        let root = env::Module {
            this: None,
            lookup: root_lookup,
        };

        // Fill unchecked entries with defaults
        let items: Vec<env::Item> = self.env_items.into_iter().enumerate().map(|(i, o)| {
            o.unwrap_or_else(|| {
                let item = self.program.item(ItemId(i));
                env::Item {
                    cname: item.cname.clone(),
                    lname: item.lname.clone(),
                    kind: item.kind,
                    ty: Ty::Star,
                    prim_id: None,
                    params: vec![],
                }
            })
        }).collect();
        let defs: Vec<env::Def> = self.env_defs.into_iter().enumerate().map(|(i, o)| {
            o.unwrap_or_else(|| {
                let def = self.program.def(DefId(i));
                env::Def {
                    qname: def.qname.clone(),
                    lname: def.lname.clone(),
                    span: def.span.clone(),
                    item: None,
                    ty: Ty::Star,
                    params: vec![],
                    val: PureVal::App(ExtId(0), vec![]),
                    decos: vec![],
                    origin: def.origin.clone(),
                    param_counts: def.param_counts.clone(),
                }
            })
        }).collect();

        let env = env::Env {
            items,
            defs,
            prim_item: self.prim_item,
            root,
        };
        (env, self.errors)
    }
}

// --- Public API ---

pub fn check(program: &Program, tokens: &[Token]) -> (env::Env, Vec<Error>) {
    let mut checker = Checker::new(program, tokens);
    checker.process_trees(&program.def_order);
    checker.into_result()
}
