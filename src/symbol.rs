use std::{
    collections::HashMap,
    fmt::{self, Debug},
    rc::Rc,
};

use index_vec::IndexVec;

use crate::{ast::AstNode, ty::FnType, utils::FmtEnumerated};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FnId(usize);
impl Debug for FnId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "fn{}", self.0)
    }
}

impl index_vec::Idx for FnId {
    fn from_usize(idx: usize) -> Self {
        Self(idx)
    }
    fn index(self) -> usize {
        self.0
    }
}

#[derive(Clone, Default)]
pub struct GlobalSymbols {
    /// Map from `FnId`s to their signatures (`FnType`).
    functions: IndexVec<FnId, FnType>,
    /// Map from function names to their `FnId`s.
    function_ids: HashMap<Rc<str>, FnId>,
}

impl Debug for GlobalSymbols {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("GlobalSymbols")
            .field("functions", &FmtEnumerated(&self.functions))
            .field("function_ids", &self.function_ids)
            .finish()
    }
}
impl GlobalSymbols {
    pub fn add_fn(&mut self, name: Rc<str>, sig: FnType) -> FnId {
        let idx = self.functions.push(sig);
        self.function_ids.insert(name, idx);
        idx
    }
    pub fn fn_id(&self, name: &str) -> Option<FnId> {
        self.function_ids.get(name).copied()
    }
    pub fn sig(&self, fn_id: FnId) -> Option<&FnType> {
        self.functions.get(fn_id)
    }
    pub fn sig_for_name<'a>(&'a self, name: &str) -> Option<&'a FnType> {
        self.sig(self.fn_id(name)?)
    }
}

#[derive(Debug, Clone)]
pub struct GlobalSymbolBuilder<P: Iterator<Item = AstNode>> {
    buildee: GlobalSymbols,
    parser: P,
}

impl<P: Iterator<Item = AstNode>> GlobalSymbolBuilder<P> {
    pub fn new(parser: P) -> Self {
        Self {
            buildee: GlobalSymbols::default(),
            parser,
        }
    }
    pub fn finish(self) -> GlobalSymbols {
        self.buildee
    }
}

impl<P: Iterator<Item = AstNode>> Iterator for GlobalSymbolBuilder<P> {
    type Item = AstNode;

    fn next(&mut self) -> Option<Self::Item> {
        let node = self.parser.next()?;
        match &node {
            AstNode::FnDef {
                name,
                arg_vars: _,
                sig,
                body: _,
            }
            | AstNode::ExternFn(name, sig) => {
                self.buildee.add_fn(Rc::clone(&name), sig.clone());
            }
            AstNode::InvalidVariable(name) => panic!("Unexpected `{name}` at top level"),
            _ => panic!("expression not allowed at top level"),
        }
        Some(node)
    }
}
