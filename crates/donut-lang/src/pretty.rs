use crate::types::tree::*;

#[derive(Debug)]
struct Pretty {
    indent: usize,
    strs: Vec<String>,
    head: bool,
}

trait Prettyable {
    fn pretty(&self, pp: &mut Pretty);
}

impl Pretty {
    fn new() -> Self {
        Pretty {
            indent: 0,
            strs: vec![],
            head: true,
        }
    }

    fn extract(self) -> String {
        self.strs.join("")
    }

    fn str(&mut self, s: &str) {
        if self.head {
            self.strs.push(" ".repeat(self.indent));
            self.head = false;
        }
        self.strs.push(s.to_string());
    }

    fn indent(&mut self) {
        self.indent += 4;
    }

    fn dedent(&mut self) {
        self.indent -= 4;
    }

    fn ln(&mut self) {
        self.str("\n");
        self.head = true;
    }

    fn error(&mut self) {
        self.str("?");
    }

    fn sep<T: Prettyable>(&mut self, items: &Vec<T>, sep: &str) {
        for (i, item) in items.iter().enumerate() {
            if i > 0 {
                self.str(sep);
            }
            item.pretty(self);
        }
    }
}

impl<T: Prettyable> Prettyable for A<T> {
    fn pretty(&self, pp: &mut Pretty) {
        match self.inner() {
            Some(v) => v.pretty(pp),
            None => pp.error(),
        }
    }
}

impl Prettyable for String {
    fn pretty(&self, pp: &mut Pretty) {
        pp.str(self);
    }
}

impl Prettyable for Symbol {
    fn pretty(&self, pp: &mut Pretty) {
        pp.str(self.0);
    }
}

impl Prettyable for Name {
    fn pretty(&self, pp: &mut Pretty) {
        pp.str(&self.0);
    }
}

impl Prettyable for Param {
    fn pretty(&self, pp: &mut Pretty) {
        pp.sep(&self.names, " ");
        if let Some(ty) = &self.ty {
            match ty {
                ParamTy::Decl => pp.str(": "),
                ParamTy::Named => pp.str(" = "),
            }
        }
        self.val.pretty(pp);
    }
}

impl Prettyable for ParamPack {
    fn pretty(&self, pp: &mut Pretty) {
        pp.str("[");
        pp.sep(&self.0, ", ");
        pp.str("]");
    }
}

impl Prettyable for AppName {
    fn pretty(&self, pp: &mut Pretty) {
        self.0.pretty(pp);
        if let Some(p) = &self.1 {
            p.pretty(pp);
        }
    }
}

impl Prettyable for RefName {
    fn pretty(&self, pp: &mut Pretty) {
        pp.sep(&self.0, ".");
        if let Some(v) = &self.1 {
            pp.str("(");
            v.pretty(pp);
            pp.str(")");
        }
    }
}

impl Prettyable for Key {
    fn pretty(&self, pp: &mut Pretty) {
        match self {
            Key::Name(n) => n.pretty(pp),
            Key::String(s) => s.pretty(pp),
        }
    }
}

impl Prettyable for (A<Key>, A<Val>) {
    fn pretty(&self, pp: &mut Pretty) {
        self.0.pretty(pp);
        pp.str(": ");
        self.1.pretty(pp);
    }
}

impl Prettyable for Lit {
    fn pretty(&self, pp: &mut Pretty) {
        match self {
            Lit::Number(n) => pp.str(n),
            Lit::String(s) => pp.str(s),
            Lit::Array(arr) => {
                pp.str("[");
                pp.sep(arr, ", ");
                pp.str("]");
            }
            Lit::Object(obj) => {
                pp.str("{");
                pp.sep(obj, ", ");
                pp.str("}");
            }
        }
    }
}

impl Prettyable for Op {
    fn pretty(&self, pp: &mut Pretty) {
        match self {
            Op::Comp(0) => pp.str(" "),
            Op::Comp(n) => pp.str(";".repeat(*n as usize).as_str()),
            Op::CompParam(p) => {
                pp.str(";[");
                p.pretty(pp);
                pp.str("]");
            }
            Op::Arrow(s) => pp.str(s),
        }
    }
}

impl Prettyable for Val {
    fn pretty(&self, pp: &mut Pretty) {
        match self {
            Val::Ref(r) => r.pretty(pp),
            Val::Lit(l) => l.pretty(pp),
            Val::Op(l, o, r) => {
                l.pretty(pp);
                o.pretty(pp);
                r.pretty(pp);
            }
            Val::Paren(v) => {
                pp.str("(");
                v.pretty(pp);
                pp.str(")");
            }
        }
    }
}

impl Prettyable for Module {
    fn pretty(&self, pp: &mut Pretty) {
        match self {
            Module::Block(p) => {
                pp.str("{");
                pp.indent();
                pp.ln();
                p.pretty(pp);
                pp.dedent();
                pp.str("}");
            }
            Module::Import(s) => {
                pp.str("import ");
                s.pretty(pp);
            }
        }
    }
}

impl Prettyable for AssignOp {
    fn pretty(&self, pp: &mut Pretty) {
        match self {
            AssignOp::Alias => pp.str(" = "),
            AssignOp::Def => pp.str(" := "),
            AssignOp::Add => pp.str(" += "),
        }
    }
}

impl Prettyable for UnitDecl {
    fn pretty(&self, pp: &mut Pretty) {
        pp.sep(&self.names, " ");
        if let Some(ty) = &self.ty {
            pp.str(": ");
            ty.pretty(pp);
        }
        if let Some((assign_op, val_mod)) = &self.assign {
            assign_op.pretty(pp);
            match val_mod {
                ValMod::Val(v) => v.pretty(pp),
                ValMod::Mod(m) => m.pretty(pp),
            }
        }
    }
}

impl Prettyable for ClauseTy {
    fn pretty(&self, pp: &mut Pretty) {
        match self {
            ClauseTy::With => pp.str("with"),
            ClauseTy::Where => pp.str("where"),
        }
    }
}

impl Prettyable for Clause {
    fn pretty(&self, pp: &mut Pretty) {
        self.0.pretty(pp);
        pp.str(" ");
        self.1.pretty(pp);
    }
}

impl Prettyable for Decl {
    fn pretty(&self, pp: &mut Pretty) {
        for deco in &self.decos {
            deco.pretty(pp);
            pp.ln();
        }
        match &self.unit {
            Unit::Decl(ud) => {
                ud.pretty(pp);
            }
            Unit::Mod(m) => {
                m.pretty(pp);
            }
        }
        for clause in &self.clauses {
            pp.str(" ");
            clause.pretty(pp);
        }
        pp.ln();
    }
}

impl Prettyable for Program {
    fn pretty(&self, pp: &mut Pretty) {
        for d in &self.0 {
            d.pretty(pp);
        }
    }
}

pub fn pretty(p: &A<Program>) -> String {
    let mut pp = Pretty::new();
    p.pretty(&mut pp);
    pp.extract()
}
