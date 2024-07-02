#![allow(dead_code)]

pub mod build;

use std::fmt::{self, Debug};

use crate::{define_idx_type, span::Spanned, token::tokens::Ident, utils::FmtEnumeratedTrait};

use derive_more::From;
use index_vec::IndexVec;

#[derive(Clone, Default)]
pub struct HirProgram {
    adts: IndexVec<AdtId, Spanned<Adt>>,
    funcs: IndexVec<FuncId, Spanned<Func>>,
}

impl Debug for HirProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("HirProgram")
            .field("adts", &self.adts.fmt_enumerated())
            .field("funcs", &self.funcs.fmt_enumerated())
            .finish()
    }
}

define_idx_type! {
    /// Globally unique ID for ADTs.
    name: AdtId,
    debug_fmt: "adt{}",
}

define_idx_type! {
    /// Globally unique ID for functions.
    name: FuncId,
    debug_fmt: "func{}",
}

define_idx_type! {
    /// Per-ADT unique ID for a variant.
    name: VariantId,
    debug_fmt: "variant{}",
}

define_idx_type! {
    /// Per-function unique ID for a variable.
    name: VarId,
    debug_fmt: "var{}",
}

define_idx_type! {
    /// Per-function unique ID for a type variable.
    name: TyVarId,
    debug_fmt: "tyvar{}",
}

#[derive(Clone, PartialEq, Eq)]
pub enum Ty {
    Adt(AdtId),
    #[allow(clippy::enum_variant_names)]
    TyVar(TyVarId),
    Apply(Box<Spanned<Ty>>, Box<Spanned<Ty>>),
    Func(Box<Spanned<Ty>>, Box<Spanned<Ty>>),
    Tuple(Vec<Spanned<Ty>>),
}

impl Debug for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ty::Adt(x) => Debug::fmt(x, f),
            Ty::TyVar(x) => Debug::fmt(x, f),
            Ty::Apply(x, y) => write!(f, "({x:?} {y:?})"),
            Ty::Func(x, y) => write!(f, "({x:?} -> {y:?})"),
            Ty::Tuple(xs) => match &xs[..] {
                [] => write!(f, "{{}}"),
                [xs @ .., y] => {
                    write!(f, "{{")?;
                    xs.iter()
                        .map(|t| write!(f, "{t:?},"))
                        .collect::<fmt::Result>()?;
                    write!(f, "{y:?}}}")
                }
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    Var(VarId),
    Apply(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Tuple(Vec<Spanned<Expr>>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Pat {
    Binding(VarId),
    Apply(Box<Spanned<Pat>>, Box<Spanned<Pat>>),
    Tuple(Vec<Spanned<Pat>>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Equation {
    pub lhs: Vec<Spanned<Pat>>,
    pub rhs: Spanned<Expr>,
}

#[derive(Clone, PartialEq, Eq, From)]
pub enum Func {
    User(UserFunc),
    Constructor(ConsturctorFunc),
}

impl Debug for Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Func::User(x) => Debug::fmt(x, f),
            Func::Constructor(x) => Debug::fmt(x, f),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UserFunc {
    /// For debugging and error reporting.
    pub name: Ident,
    pub tyvars: Vec<TyVarId>,
    pub ty: Spanned<Ty>,
    pub equations: Vec<Spanned<Equation>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConsturctorFunc {
    pub name: Ident,
    pub tyvars: Vec<TyVarId>,
    pub ty: Spanned<Ty>,
    pub adt: AdtId,
    pub variant: VariantId,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Adt {
    /// For debugging and error reporting.
    pub name: Ident,
    pub tyvars: Vec<TyVarId>,
    pub constructors: Vec<FuncId>,
}
