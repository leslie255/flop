#![allow(dead_code)]

pub mod build;
pub mod tycheck;

use std::fmt::{self, Debug};

use crate::{define_idx_type, span::Spanned, token::tokens::Ident, utils::FmtEnumeratedTrait};

use derive_more::From;
use index_vec::IndexVec;

#[derive(Clone, Default)]
pub struct HirProgram {
    pub adts: IndexVec<AdtId, Spanned<Adt>>,
    pub user_funcs: IndexVec<UserFuncId, Spanned<UserFunc>>,
    pub constructor_funcs: IndexVec<ConstructorFuncId, Spanned<ConstructorFunc>>,
}

impl Debug for HirProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("HirProgram")
            .field("adts", &self.adts.fmt_enumerated())
            .field("user_funcs", &self.user_funcs.fmt_enumerated())
            .field(
                "constructor_funcs",
                &self.constructor_funcs.fmt_enumerated(),
            )
            .finish()
    }
}

define_idx_type! {
    /// Globally unique ID for ADTs.
    name: AdtId,
    debug_fmt: "adt{}",
}

define_idx_type! {
    /// Globally unique ID for user-defined functions.
    name: UserFuncId,
    debug_fmt: "user_func{}",
}

define_idx_type! {
    /// Globally unique ID for constructor functions.
    name: ConstructorFuncId,
    debug_fmt: "constructor_func{}",
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, From)]
pub enum FuncId {
    UserFunc(UserFuncId),
    ConstructorFunc(ConstructorFuncId),
}

impl Debug for FuncId {
    #[allow(clippy::only_used_in_recursion)]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            x @ FuncId::UserFunc(..) => Debug::fmt(x, f),
            x @ FuncId::ConstructorFunc(..) => Debug::fmt(x, f),
        }
    }
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
    Apply(Box<Ty>, Box<Ty>),
    Func(Box<Ty>, Box<Ty>),
    Tuple(Vec<Ty>),
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
                    xs.iter().try_for_each(|t| write!(f, "{t:?},"))?;
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
    Apply(Spanned<AdtId>, Box<Spanned<Pat>>),
    Tuple(Vec<Spanned<Pat>>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Equation {
    pub lhs: Vec<Spanned<Pat>>,
    pub rhs: Spanned<Expr>,
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
pub struct ConstructorFunc {
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
    pub constructors: Vec<ConstructorFuncId>,
}
