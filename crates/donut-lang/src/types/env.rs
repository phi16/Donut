use donut_core::cell::Globular;
use donut_core::common::{Level, Prim, PrimArg, PrimId};
use donut_core::free_cell::FreeCell;
use donut_core::pure_cell::PureCell;
use std::collections::HashMap;

// --- Meta type ---

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MetaType(pub PrimId, pub Vec<MetaType>);

// --- Param kind ---

#[derive(Debug, Clone, PartialEq)]
pub enum ParamKind {
    Cell,
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

    pub fn display_kind(&self) -> String {
        match &self.body {
            EntryBody::Cell(cell) => {
                let dim = cell.pure.dim().in_space;
                format!("{}-cell", dim)
            }
            EntryBody::Meta(_) => "meta".to_string(),
            EntryBody::Type(_, _) => "type".to_string(),
        }
    }

    pub fn display_type(&self, env: &Env) -> Option<String> {
        match &self.body {
            EntryBody::Cell(cell) => Some(display_cell_type(&cell.pure, &env.prim_decls)),
            EntryBody::Meta(prim) => {
                let ret = env.meta_ret_types.get(&prim.id)?;
                Some(env.display_meta_type(ret))
            }
            EntryBody::Type(_, _) => None,
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

// --- Prim display (free functions) ---

/// Display a Prim with name resolution via prim_decls.
pub fn display_prim(prim: &Prim, prim_decls: &HashMap<PrimId, PrimDecl>) -> String {
    match prim_decls.get(&prim.id) {
        Some(decl) => format_name_with_args(&decl.name, &decl.param_counts, &prim.args, prim_decls),
        None => format!("?{}", prim.id),
    }
}

/// Display a PureCell with name resolution via prim_decls.
pub fn display_pure_cell(cell: &PureCell, prim_decls: &HashMap<PrimId, PrimDecl>) -> String {
    match cell {
        PureCell::Prim(prim, _, _) => display_prim(prim, prim_decls),
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
            let sep = format!("{} ", ";".repeat(*axis as usize));
            parts.join(&sep)
        }
    }
}

/// Display a cell type (e.g. "*", "C → D", "f g → h k").
pub fn display_cell_type(pure: &PureCell, prim_decls: &HashMap<PrimId, PrimDecl>) -> String {
    let dim = pure.dim().in_space;
    if dim == 0 {
        "*".to_string()
    } else {
        let src = display_pure_cell(&pure.s(), prim_decls);
        let tgt = display_pure_cell(&pure.t(), prim_decls);
        format!("{} → {}", src, tgt)
    }
}

fn display_prim_arg(arg: &PrimArg, prim_decls: &HashMap<PrimId, PrimDecl>) -> String {
    match arg {
        PrimArg::Cell(pc) => display_pure_cell(pc, prim_decls),
        PrimArg::Nat(n) => n.to_string(),
        PrimArg::Rat(v) => format!("{}", v),
        PrimArg::App(id, args) => match prim_decls.get(id) {
            Some(decl) => format_name_with_args(&decl.name, &decl.param_counts, args, prim_decls),
            None => format!("?{}", id),
        },
    }
}

/// Format name with args distributed by param_counts.
/// Canonical name uses "::" as origin separator, displayed as ".".
/// e.g. name="u.x", counts=[1,1], args=[20,32] → "u[20].x[32]"
/// e.g. name="sys::u32.lit", counts=[0,0,1], args=[1] → "sys.u32.lit[1]"
fn format_name_with_args(
    name: &str,
    param_counts: &[usize],
    args: &[PrimArg],
    prim_decls: &HashMap<PrimId, PrimDecl>,
) -> String {
    let expected: usize = param_counts.iter().sum();
    debug_assert!(
        args.len() == expected,
        "display: args count mismatch for {}: expected {}, got {}",
        name, expected, args.len(),
    );
    if args.is_empty() {
        return name.to_string();
    }
    let (prefix, local) = match name.split_once("::") {
        Some((p, l)) => (Some(p), l),
        None => (None, name),
    };
    let mut result = String::new();
    if let Some(p) = prefix {
        result.push_str(p);
        result.push_str("::");
    }
    let mut arg_idx = 0;
    for (i, seg) in local.split('.').enumerate() {
        if i > 0 {
            result.push('.');
        }
        result.push_str(seg);
        let count = param_counts.get(i).copied().unwrap_or(0);
        if count > 0 && arg_idx < args.len() {
            let end = (arg_idx + count).min(args.len());
            let formatted: Vec<_> = args[arg_idx..end]
                .iter()
                .map(|a| display_prim_arg(a, prim_decls))
                .collect();
            result.push_str(&format!("[{}]", formatted.join(", ")));
            arg_idx = end;
        }
    }
    result
}

// --- Env ---

#[derive(Debug, Clone)]
pub struct Env {
    pub entries: Vec<Entry>,
    pub lookup: HashMap<String, usize>,
    pub prim_decls: HashMap<PrimId, PrimDecl>,
    pub entry_params: HashMap<usize, Vec<ParamInfo>>,
    pub module_params: HashMap<String, Vec<ParamInfo>>,
    pub meta_ret_types: HashMap<PrimId, MetaType>,
    pub meta_prim_names: HashMap<PrimId, String>,
}

impl Env {
    // --- Param display ---

    pub fn display_params(&self, entry_idx: usize) -> String {
        match self.entry_params.get(&entry_idx) {
            Some(ps) if !ps.is_empty() => self.format_params(ps),
            _ => String::new(),
        }
    }

    pub fn display_module_params(&self, qname: &str) -> String {
        match self.module_params.get(qname) {
            Some(ps) if !ps.is_empty() => self.format_params(ps),
            _ => String::new(),
        }
    }

    /// Collect all params for an entry, including parent module params.
    pub fn all_params(&self, entry_idx: usize) -> Vec<ParamInfo> {
        let name = &self.entries[entry_idx].name;
        let local = name.split_once("::").map_or(name.as_str(), |(_, l)| l);
        let segments: Vec<&str> = local.split('.').collect();
        let mut result = Vec::new();
        for i in 0..segments.len() - 1 {
            let module_name = segments[..=i].join(".");
            if let Some(ps) = self.module_params.get(&module_name) {
                result.extend(ps.iter().cloned());
            }
        }
        if let Some(ps) = self.entry_params.get(&entry_idx) {
            result.extend(ps.iter().cloned());
        }
        result
    }

    pub fn display_all_params(&self, entry_idx: usize) -> String {
        let ps = self.all_params(entry_idx);
        if ps.is_empty() {
            String::new()
        } else {
            self.format_params(&ps)
        }
    }

    fn format_params(&self, params: &[ParamInfo]) -> String {
        let parts: Vec<_> = params
            .iter()
            .map(|(name, _, kind)| match kind {
                ParamKind::Meta(mt) => format!("{}: {}", name, self.display_meta_type(mt)),
                ParamKind::Cell => {
                    if let Some(&idx) = self.lookup.get(name) {
                        if let Some(ty) = self.entries[idx].display_type(self) {
                            return format!("{}: {}", name, ty);
                        }
                    }
                    name.clone()
                }
            })
            .collect();
        format!("[{}]", parts.join(", "))
    }

    // --- Meta type display ---

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
