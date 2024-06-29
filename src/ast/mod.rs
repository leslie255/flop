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
    pub ty: Spanned<Ty>,
    pub body: Spanned<DeclBody>,
}

#[derive(Clone, PartialEq, From, Debug)]
pub enum DeclBody {
    Inline(Spanned<Token![=]>, Spanned<Expr>, Spanned<Token![;]>),
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

#[derive(Clone, PartialEq, From, Debug)]
pub enum Ty {
    Typename(Ident),
    Apply(Box<Spanned<Ty>>, Box<Spanned<Ty>>),
    Func(Box<Spanned<Ty>>, Spanned<Token![->]>, Box<Spanned<Ty>>),
    InParens(Box<InParens<Ty>>),
}

#[derive(Clone, PartialEq, From, Debug)]
pub enum Pat {
    Binding(Ident),
    Apply(Box<Spanned<Pat>>, Box<Spanned<Pat>>),
    InParens(Box<InParens<Pat>>),
}

#[derive(Clone, PartialEq, From, Debug)]
pub enum Pat_ {
    Binding(Ident),
    InParens(Box<InParens<Pat>>),
}

impl Pat_ {
    pub fn into_pat(self) -> Pat {
        match self {
            Pat_::Binding(x) => Pat::Binding(x),
            Pat_::InParens(x) => Pat::InParens(x),
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
    pub body: Spanned<Punctuated<(Spanned<Ident>, Spanned<Ty>), Token![|]>>,
    pub semicolon: Spanned<Token![;]>,
}

#[derive(Clone, PartialEq, From, Debug)]
pub enum Expr {
    Var(Ident),
    Apply(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    InParens(Box<InParens<Expr>>),
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
