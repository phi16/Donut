use donut_core::cell::Globular;
use donut_core::common::{Level, Prim, PrimId};
use donut_core::free_cell::FreeCell;
use donut_core::pure_cell::PureCell;
use std::collections::HashMap;

// --- Meta type ---

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MetaType(pub PrimId, pub Vec<MetaType>);

#[derive(Debug, Clone)]
pub struct MetaSig {
    pub params: Vec<MetaType>,
    pub ret: MetaType,
}

// --- Param kind ---

#[derive(Debug, Clone, PartialEq)]
pub enum ParamKind {
    Cell,
    Nat,
    Rat,
    Meta(MetaType),
}

pub type ParamInfo = (String, PrimId, ParamKind);

// --- Entry ---

#[derive(Debug, Clone)]
pub(crate) enum EntryBody {
    Cell(FreeCell),
    Meta(Prim),
    Type(u8, Ty),
}

#[derive(Debug, Clone)]
pub(crate) enum Ty {
    Zero,
    Succ(FreeCell, FreeCell),
    Meta(MetaType),
}

#[derive(Debug, Clone)]
pub struct Entry {
    pub name: String,
    pub color: (u8, u8, u8),
    pub(crate) body: EntryBody,
    pub param_counts: Vec<usize>,
}

impl Entry {
    pub fn as_cell(&self) -> Option<&FreeCell> {
        match &self.body {
            EntryBody::Cell(c) => Some(c),
            _ => None,
        }
    }

    pub fn kind_description(&self) -> String {
        match &self.body {
            EntryBody::Cell(cell) => {
                let dim = cell.pure.dim().in_space;
                format!("{}-cell", dim)
            }
            EntryBody::Meta(_) => "meta".to_string(),
            EntryBody::Type(_, _) => "type".to_string(),
        }
    }

    pub fn type_display(&self, env: &Env) -> Option<String> {
        match &self.body {
            EntryBody::Cell(cell) => {
                let dim = cell.pure.dim().in_space;
                if dim == 0 {
                    Some("*".to_string())
                } else {
                    let s = display_pure_cell(&cell.pure.s(), &env.prim_decls);
                    let t = display_pure_cell(&cell.pure.t(), &env.prim_decls);
                    Some(format!("{} → {}", s, t))
                }
            }
            EntryBody::Meta(prim) => {
                let sig = env.meta_sigs.get(&prim.id)?;
                Some(env.display_meta_type(&sig.ret))
            }
            EntryBody::Type(_, _) => None,
        }
    }
}

fn display_pure_cell(cell: &PureCell, prim_decls: &HashMap<PrimId, PrimDecl>) -> String {
    match cell {
        PureCell::Prim(prim, _, _) => prim_decls
            .get(&prim.id)
            .map(|d| d.name.clone())
            .unwrap_or_else(|| format!("?{}", prim.id)),
        PureCell::Comp(axis, children, _) => {
            let parts: Vec<_> = children
                .iter()
                .map(|c| {
                    let s = display_pure_cell(c, prim_decls);
                    if let PureCell::Comp(child_axis, _, _) = c {
                        if *child_axis > *axis {
                            return format!("({})", s);
                        }
                    }
                    s
                })
                .collect();
            let sep = if *axis == 0 {
                " "
            } else {
                "; "
            };
            parts.join(sep)
        }
    }
}

// --- PrimDecl ---

#[derive(Debug, Clone)]
pub struct PrimDecl {
    pub name: String,
    pub level: Level,
    pub color: (u8, u8, u8),
    pub param_counts: Vec<usize>,
}

// --- Env ---

#[derive(Debug, Clone)]
pub struct Env {
    pub entries: Vec<Entry>,
    pub lookup: HashMap<String, usize>,
    pub prim_decls: HashMap<PrimId, PrimDecl>,
    pub entry_params: HashMap<usize, Vec<ParamInfo>>,
    pub module_params: HashMap<String, Vec<ParamInfo>>,
    pub meta_sigs: HashMap<PrimId, MetaSig>,
    pub meta_prim_names: HashMap<PrimId, String>,
}

impl Env {
    pub fn param_display(&self, entry_idx: usize) -> String {
        match self.entry_params.get(&entry_idx) {
            Some(ps) if !ps.is_empty() => self.format_params(ps),
            _ => String::new(),
        }
    }

    pub fn module_param_display(&self, qname: &str) -> String {
        match self.module_params.get(qname) {
            Some(ps) if !ps.is_empty() => self.format_params(ps),
            _ => String::new(),
        }
    }

    fn format_params(&self, params: &[ParamInfo]) -> String {
        let parts: Vec<_> = params
            .iter()
            .map(|(name, _, kind)| match kind {
                ParamKind::Nat => format!("{}: nat", name),
                ParamKind::Rat => format!("{}: rat", name),
                ParamKind::Meta(mt) => format!("{}: {}", name, self.display_meta_type(mt)),
                ParamKind::Cell => {
                    if let Some(&idx) = self.lookup.get(name) {
                        if let Some(ty) = self.entries[idx].type_display(self) {
                            return format!("{}: {}", name, ty);
                        }
                    }
                    name.clone()
                }
            })
            .collect();
        format!("[{}]", parts.join(", "))
    }

    pub fn display_meta_type(&self, mt: &MetaType) -> String {
        let name = self.meta_prim_names.get(&mt.0)
            .cloned()
            .unwrap_or_else(|| format!("?{}", mt.0));
        if mt.1.is_empty() {
            name
        } else {
            let args: Vec<_> = mt.1.iter().map(|a| self.display_meta_type(a)).collect();
            format!("{}[{}]", name, args.join(", "))
        }
    }
}
