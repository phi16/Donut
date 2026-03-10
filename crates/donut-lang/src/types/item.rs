use crate::types::common::{S, TokenSpan};
use donut_core::common::Axis;
use std::collections::HashMap;

// --- Index types ---

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ValId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ItemId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(pub usize);

/// A scope entry: either a direct Item (e.g. parameter) or a Def (named definition).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Ref {
    Item(ItemId),
    Def(DefId),
}

// --- Val types ---

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ArrowKind {
    To,
    Eq,
    Functor,
}

#[derive(Debug)]
pub struct Path {
    pub target: Ref,
    pub args: Vec<ValId>,
    pub applicand: Option<ValId>,
}

#[derive(Debug)]
pub enum Lit {
    Number(String),
    String(String),
    Array(Vec<ValId>),
    Object(Vec<(String, ValId)>),
}

#[derive(Debug, Clone)]
pub enum Hole {
    Any,
    Named(String),
}

#[derive(Debug)]
pub enum Val {
    Path(Path),
    Lit(Lit),
    Comp(Axis, Vec<ValId>),
    CompStar(Vec<ValId>),
    Arrow(ArrowKind, ValId, ValId),
    Hole(Hole),
    Subst(ValId, HashMap<ItemId, ValId>),
}

// --- Param ---

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: ValId,
    pub item: ItemId,
}

// --- Item ---
// The actual entity. Identified by cname. ItemId ≈ ExtId.

#[derive(Debug, Clone, Copy)]
pub enum ItemKind {
    Decl,
    DeclDef, // TODO
    Param,
}

#[derive(Debug)]
pub struct Item {
    pub cname: String,
    pub lname: String,
    pub kind: ItemKind,
    pub ty: ValId,
    pub params: Vec<Param>,
}

// --- Def ---
// Named definition. Has body, members, decorators.

/// The name used for the auto-generated equivalence member of a DeclDef.
pub const DEF_MEMBER_NAME: &str = "def";

#[derive(Debug)]
pub struct FunctorMapping {
    pub params: Vec<Param>,
    pub applicand: ValId,
    pub val: ValId,
}

#[derive(Debug)]
pub enum DefBody {
    None,
    Decl {
        item: ItemId,
        /// For DeclDef (`:=`): the body val `y` in `x: T := y`.
        def_val: Option<ValId>,
    },
    Alias { val: ValId },
    Functor { mappings: Vec<FunctorMapping> },
}

#[derive(Debug)]
pub struct Def {
    pub qname: String,
    pub lname: String,
    pub span: TokenSpan,
    pub ty: Option<ValId>,
    pub params: Vec<Param>,
    pub body: DefBody,
    pub members: Module,
    pub decos: Vec<ValId>,
    pub origin: Option<String>,
    pub param_counts: Vec<usize>,
}

// --- Module ---

#[derive(Debug, Clone)]
pub struct Module {
    pub entries: Vec<DefId>,
    index: HashMap<String, usize>,
}

// --- DefTree ---

#[derive(Debug, Clone)]
pub struct DefTree {
    pub def_id: DefId,
    pub children: Vec<DefTree>,
}

// --- Program ---

pub struct Program {
    pub root: Module,
    pub items: Vec<Item>,
    pub defs: Vec<Def>,
    pub vals: Vec<S<Val>>,
    pub def_order: Vec<DefTree>,
}

impl Program {
    pub fn val(&self, id: ValId) -> &Val {
        &self.vals[id.0].0
    }
    pub fn val_span(&self, id: ValId) -> &TokenSpan {
        &self.vals[id.0].1
    }
    pub fn item(&self, id: ItemId) -> &Item {
        &self.items[id.0]
    }
    pub fn def(&self, id: DefId) -> &Def {
        &self.defs[id.0]
    }
}

impl Module {
    pub fn new() -> Self {
        Module {
            entries: Vec::new(),
            index: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, def: DefId) -> Option<DefId> {
        if let Some(&idx) = self.index.get(&name) {
            Some(self.entries[idx])
        } else {
            let idx = self.entries.len();
            self.index.insert(name, idx);
            self.entries.push(def);
            None
        }
    }

    pub fn get(&self, name: &str) -> Option<DefId> {
        let &idx = self.index.get(name)?;
        Some(self.entries[idx])
    }

    pub fn contains_key(&self, name: &str) -> bool {
        self.index.contains_key(name)
    }

    /// Merge entries from another Module. Returns conflicting lnames.
    pub fn merge_from(&mut self, source: &Module, defs: &[Def]) -> Vec<String> {
        let mut conflicts = Vec::new();
        for &def_id in &source.entries {
            let lname = defs[def_id.0].lname.clone();
            if self.define(lname.clone(), def_id).is_some() {
                conflicts.push(lname);
            }
        }
        conflicts
    }
}

impl Def {
    pub fn val(&self) -> Option<ValId> {
        match &self.body {
            DefBody::Alias { val } => Some(*val),
            _ => None,
        }
    }
}

// --- Display ---

impl Program {
    pub fn display_all(&self) -> String {
        let mut s = String::new();

        s.push_str("=== Vals ===\n");
        for (i, sv) in self.vals.iter().enumerate() {
            s.push_str(&format!("  Val({}) = {}\n", i, self.display_val(ValId(i))));
            let _ = &sv.1; // span, skip
        }

        s.push_str("\n=== Items ===\n");
        for (i, item) in self.items.iter().enumerate() {
            s.push_str(&format!(
                "  Item({}) cname={} lname={} kind={:?} ty=Val({}) params=[{}]\n",
                i,
                item.cname,
                item.lname,
                item.kind,
                item.ty.0,
                item.params
                    .iter()
                    .map(|p| format!("{}:Val({})=Item({})", p.name, p.ty.0, p.item.0))
                    .collect::<Vec<_>>()
                    .join(", "),
            ));
        }

        s.push_str("\n=== Defs ===\n");
        for (i, def) in self.defs.iter().enumerate() {
            s.push_str(&format!(
                "  Def({}) qname={} lname={}",
                i, def.qname, def.lname
            ));
            if let Some(origin) = &def.origin {
                s.push_str(&format!(" origin={}", origin));
            }
            s.push('\n');
            if !def.params.is_empty() {
                s.push_str(&format!(
                    "    params: [{}]\n",
                    def.params
                        .iter()
                        .map(|p| format!("{}:Val({})=Item({})", p.name, p.ty.0, p.item.0))
                        .collect::<Vec<_>>()
                        .join(", "),
                ));
            }
            if let Some(ty) = def.ty {
                s.push_str(&format!("    ty: {}\n", self.display_val(ty)));
            }
            match &def.body {
                DefBody::None => s.push_str("    body: none\n"),
                DefBody::Decl { item, def_val } => {
                    s.push_str(&format!("    body: decl Item({})", item.0));
                    if let Some(dv) = def_val {
                        s.push_str(&format!(" := {}", self.display_val(*dv)));
                    }
                    s.push('\n');
                }
                DefBody::Alias { val } => {
                    s.push_str(&format!("    body: alias {}\n", self.display_val(*val)))
                }
                DefBody::Functor { mappings } => {
                    s.push_str("    body: functor\n");
                    for m in mappings {
                        s.push_str(&format!(
                            "      ({}) = {}\n",
                            self.display_val(m.applicand),
                            self.display_val(m.val),
                        ));
                    }
                }
            }
            if !def.decos.is_empty() {
                s.push_str(&format!(
                    "    decos: [{}]\n",
                    def.decos
                        .iter()
                        .map(|&d| self.display_val(d))
                        .collect::<Vec<_>>()
                        .join(", "),
                ));
            }
            if !def.members.entries.is_empty() {
                s.push_str(&format!(
                    "    members: [{}]\n",
                    def.members
                        .entries
                        .iter()
                        .map(|d| format!("Def({})", d.0))
                        .collect::<Vec<_>>()
                        .join(", "),
                ));
            }
        }

        s.push_str("\n=== DefTree ===\n");
        for tree in &self.def_order {
            self.display_tree(tree, 1, &mut s);
        }

        s.push_str("\n=== Root Module ===\n");
        self.display_module(&self.root, 1, &mut s);

        s
    }

    pub fn display_val(&self, id: ValId) -> String {
        let val = self.val(id);
        match val {
            Val::Path(path) => {
                let target = match path.target {
                    Ref::Item(item_id) => format!("Item({})", item_id.0),
                    Ref::Def(def_id) => format!("Def({})", def_id.0),
                };
                let mut s = target;
                if !path.args.is_empty() {
                    s.push_str(&format!(
                        "[{}]",
                        path.args
                            .iter()
                            .map(|&a| self.display_val(a))
                            .collect::<Vec<_>>()
                            .join(", "),
                    ));
                }
                if let Some(app) = path.applicand {
                    s.push_str(&format!("({})", self.display_val(app)));
                }
                s
            }
            Val::Lit(lit) => match lit {
                Lit::Number(n) => n.clone(),
                Lit::String(s) => format!("\"{}\"", s),
                Lit::Array(vs) => format!(
                    "[{}]",
                    vs.iter()
                        .map(|&v| self.display_val(v))
                        .collect::<Vec<_>>()
                        .join(", "),
                ),
                Lit::Object(kvs) => format!(
                    "{{{}}}",
                    kvs.iter()
                        .map(|(k, v)| format!("{}: {}", k, self.display_val(*v)))
                        .collect::<Vec<_>>()
                        .join(", "),
                ),
            },
            Val::Comp(axis, children) => {
                let sep = match axis {
                    0 => " ",
                    1 => "; ",
                    n => {
                        return format!(
                            "comp({}, [{}])",
                            n,
                            children
                                .iter()
                                .map(|&c| self.display_val(c))
                                .collect::<Vec<_>>()
                                .join(", "),
                        );
                    }
                };
                children
                    .iter()
                    .map(|&c| self.display_val(c))
                    .collect::<Vec<_>>()
                    .join(sep)
            }
            Val::CompStar(children) => format!(
                ";* [{}]",
                children
                    .iter()
                    .map(|&c| self.display_val(c))
                    .collect::<Vec<_>>()
                    .join(", "),
            ),
            Val::Arrow(kind, l, r) => {
                let op = match kind {
                    ArrowKind::To => " → ",
                    ArrowKind::Eq => " ~ ",
                    ArrowKind::Functor => " ~> ",
                };
                format!("{}{}{}", self.display_val(*l), op, self.display_val(*r))
            }
            Val::Hole(Hole::Any) => "_".to_string(),
            Val::Hole(Hole::Named(n)) => format!("?{}", n),
            Val::Subst(inner, mapping) => {
                let substs: Vec<String> = mapping
                    .iter()
                    .map(|(item_id, &val_id)| {
                        format!("Item({})={}", item_id.0, self.display_val(val_id))
                    })
                    .collect();
                format!("{}[{}]", self.display_val(*inner), substs.join(", "))
            }
        }
    }

    fn display_tree(&self, tree: &DefTree, indent: usize, s: &mut String) {
        let def = self.def(tree.def_id);
        s.push_str(&format!(
            "{}Def({}) {}\n",
            "  ".repeat(indent),
            tree.def_id.0,
            def.qname,
        ));
        for child in &tree.children {
            self.display_tree(child, indent + 1, s);
        }
    }

    fn display_module(&self, module: &Module, indent: usize, s: &mut String) {
        for &def_id in &module.entries {
            let def = self.def(def_id);
            s.push_str(&format!(
                "{}Def({}) {}\n",
                "  ".repeat(indent),
                def_id.0,
                def.qname,
            ));
        }
    }
}
