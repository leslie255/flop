pub mod parse;

#[cfg(test)]
mod parsing_tests;

use std::fmt::{self, Debug};

use crate::{
    span::Spanned,
    token::{tokens::*, Token},
};

use derive_more::From;

#[derive(Clone, PartialEq)]
pub struct Punctuated<T, P> {
    pub pairs: Vec<(Spanned<T>, Spanned<P>)>,
    pub last: Option<Box<Spanned<T>>>,
}

pub type InParens<T> = (Spanned<ParenL>, Spanned<T>, Spanned<ParenR>);
pub type InBraces<T> = (Spanned<BraceL>, Spanned<T>, Spanned<BraceR>);

#[derive(Clone, PartialEq, From, Debug)]
pub enum Item {
    Decl(Decl),
    TyDecl(TyDecl),
}

#[derive(Clone, PartialEq, From, Debug)]
pub struct Decl {
    pub name: Spanned<Ident>,
    pub colon: Spanned<Token![:]>,
    pub quan: Option<(Spanned<Quan>, Spanned<Token![.]>)>,
    pub ty: Spanned<TyExpr>,
    pub body: Spanned<DeclBody>,
}

#[derive(Clone, PartialEq, From, Debug)]
pub enum DeclBody {
    Inline(Spanned<Token![=]>, Spanned<Expr>, Spanned<Token![;]>),
    #[allow(clippy::type_complexity)]
    Equations(Vec<Spanned<(Spanned<Token![|]>, Spanned<Equation>, Spanned<Token![;]>)>>),
}

#[derive(Clone, PartialEq, From, Debug)]
pub struct Equation {
    pub lhs: Spanned<Vec<Spanned<Pat_>>>,
    pub eq: Spanned<Token![=]>,
    pub rhs: Spanned<Expr>,
}

#[derive(Clone, PartialEq, From, Debug)]
pub struct Quan {
    pub forall: Spanned<Token![forall]>,
    pub vars: Spanned<QuanVars>,
}

#[allow(clippy::large_enum_variant)] // idc
#[derive(Clone, PartialEq, From, Debug)]
pub enum QuanVars {
    Single(Ident),
    Multi(InParens<Punctuated<Ident, Token![,]>>),
}

/// As oppose to `hir::Ty`, this is just verbatim repr of a type expression.
#[derive(Clone, PartialEq, From, Debug)]
pub enum TyExpr {
    Typename(Ident),
    Apply(Box<Spanned<TyExpr>>, Box<Spanned<TyExpr>>),
    Func(
        Box<Spanned<TyExpr>>,
        Spanned<Token![->]>,
        Box<Spanned<TyExpr>>,
    ),
    InParens(Box<InParens<TyExpr>>),
    Tuple(InBraces<Punctuated<TyExpr, Token![,]>>),
}

#[derive(Clone, PartialEq, From, Debug)]
pub enum Pat {
    Binding(Ident),
    Apply(Spanned<Ident>, Box<Spanned<Pat>>),
    InParens(Box<InParens<Pat>>),
    Tuple(InBraces<Punctuated<Pat, Token![,]>>),
}

#[derive(Clone, PartialEq, From, Debug)]
pub enum Pat_ {
    Binding(Ident),
    InParens(Box<InParens<Pat>>),
    Tuple(InBraces<Punctuated<Pat, Token![,]>>),
}

impl From<Pat_> for Pat {
    fn from(value: Pat_) -> Self {
        match value {
            Pat_::Binding(x) => Self::Binding(x),
            Pat_::InParens(x) => Self::InParens(x),
            Pat_::Tuple(x) => Self::Tuple(x),
        }
    }
}

#[derive(Clone, PartialEq, From, Debug)]
pub struct TyDecl {
    pub type_: Spanned<Token![type]>,
    pub quan: Option<(Spanned<Quan>, Spanned<Token![.]>)>,
    pub name: Spanned<Ident>,
    pub eq: Spanned<Token![=]>,
    #[allow(clippy::type_complexity)]
    pub body: Spanned<Punctuated<(Spanned<Ident>, Spanned<Option<TyExpr>>), Token![|]>>,
    pub semicolon: Spanned<Token![;]>,
}

#[derive(Clone, PartialEq, From, Debug)]
pub enum Expr {
    Var(Ident),
    Apply(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    InParens(Box<InParens<Expr>>),
    Tuple(InBraces<Punctuated<Expr, Token![,]>>),
}

impl<T, P> Debug for Punctuated<T, P>
where
    T: Debug,
    P: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut list = f.debug_list();
        for (x, p) in &self.pairs {
            list.entry(x);
            list.entry(p);
        }
        self.last.as_ref().inspect(|x| {
            list.entry(x);
        });
        list.finish()
    }
}

impl<T, P> Punctuated<T, P> {
    pub fn iter(&self) -> impl Iterator<Item = &Spanned<T>> {
        self.into_iter()
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Spanned<T>> {
        self.into_iter()
    }
    pub fn len(&self) -> usize {
        self.pairs.len() + usize::from(self.last.is_some())
    }
}

impl<'a, T, P> IntoIterator for &'a Punctuated<T, P> {
    type Item = &'a Spanned<T>;

    type IntoIter = impl Iterator<Item = &'a Spanned<T>> + 'a;

    fn into_iter(self) -> Self::IntoIter {
        self.pairs
            .iter()
            .map(|(x, _)| x)
            .chain(self.last.iter().map(|x| x.as_ref()))
    }
}

impl<'a, T, P> IntoIterator for &'a mut Punctuated<T, P> {
    type Item = &'a mut Spanned<T>;

    type IntoIter = impl Iterator<Item = &'a mut Spanned<T>> + 'a;

    fn into_iter(self) -> Self::IntoIter {
        self.pairs
            .iter_mut()
            .map(|(x, _)| x)
            .chain(self.last.iter_mut().map(|x| x.as_mut()))
    }
}

impl<T, P> IntoIterator for Punctuated<T, P> {
    type Item = Spanned<T>;

    type IntoIter = impl Iterator<Item = Spanned<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.pairs
            .into_iter()
            .map(|(x, _)| x)
            .chain(self.last.into_iter().map(|x| *x))
    }
}

pub enum QuanVarsIter {
    Empty,
    Single(Spanned<Ident>),
    Multi(<Punctuated<Ident, Token![,]> as IntoIterator>::IntoIter),
}

impl Iterator for QuanVarsIter {
    type Item = Spanned<Ident>;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Empty => None,
            Self::Single(x) => {
                let x = x.clone();
                *self = Self::Empty;
                Some(x)
            }
            Self::Multi(xs) => xs.next(),
        }
    }
}

impl IntoIterator for Spanned<QuanVars> {
    type Item = Spanned<Ident>;

    type IntoIter = QuanVarsIter;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Spanned(QuanVars::Single(x), span) => QuanVarsIter::Single(Spanned(x, span)),
            Spanned(QuanVars::Multi((_, Spanned(xs, _), _)), _) => {
                QuanVarsIter::Multi(xs.into_iter())
            }
        }
    }
}
