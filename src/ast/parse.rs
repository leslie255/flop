use std::{iter::Peekable, ops::Range, rc::Rc, slice};

use super::*;

use crate::{
    source_str::SourceIndex,
    span::{span, spanned_into, Span, Spanned, ToSpanned},
    token::{
        tokens::{self, Ident},
        Token, TokenTrait,
    },
};

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    UnexpectedEof,
    ExpectToken(Token),
    ExpectPat,
    ExpectTy,
    ExpectExpr,
    ExpectQuanVars,
    ExpectDeclBody,
    ExpectItem,
}

#[derive(Debug, Clone)]
pub struct ParserState<'a> {
    tokens: Peekable<slice::Iter<'a, Spanned<Token>>>,
    path: Rc<str>,
    prev_span: Span,
}

impl<'a> ParserState<'a> {
    /// `tokens` must have at least one item.
    pub fn new(tokens: &'a [Spanned<Token>], path: Rc<str>) -> Self {
        let mut tokens = tokens.iter().peekable();
        let prev_span = tokens
            .peek()
            .expect("Argument `tokens` must have at least one item")
            .span();
        Self {
            tokens,
            path,
            prev_span,
        }
    }
    fn peek(&mut self) -> Option<&Spanned<Token>> {
        let &t = self.tokens.peek()?;
        self.prev_span = t.span();
        Some(t)
    }
    fn next(&mut self) -> Option<&Spanned<Token>> {
        let t = self.tokens.next()?;
        self.prev_span = t.span();
        Some(t)
    }
    // fn peek_or_eof_error(&mut self) -> Result<&Spanned<Token>, Spanned<ParseError>> {
    //     let &t = self
    //         .tokens
    //         .peek()
    //         .ok_or_else(|| ParseError::UnexpectedEof.to_spanned(self.prev_span.clone()))?;
    //     self.prev_span = t.span();
    //     Ok(t)
    // }
    // fn next_or_eof_error(&mut self) -> Result<&Spanned<Token>, Spanned<ParseError>> {
    //     let t = self
    //         .tokens
    //         .next()
    //         .ok_or_else(|| ParseError::UnexpectedEof.to_spanned(self.prev_span.clone()))?;
    //     self.prev_span = t.span();
    //     Ok(t)
    // }
    fn prev_span(&self) -> Span {
        self.prev_span.clone()
    }
    fn path(&self) -> Rc<str> {
        self.path.clone()
    }
}

pub trait Parse: Sized {
    fn peek(state: &mut ParserState) -> bool;
    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>>;
}

impl<L, R> Parse for (Spanned<L>, Spanned<R>)
where
    L: Parse,
    R: Parse,
{
    fn peek(state: &mut ParserState) -> bool {
        L::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        let left = L::parse(state)?;
        let right = R::parse(state)?;
        let range = join_range(find_span_start!(left, right), find_span_end!(right, left));
        let span = Span::new(Some(state.path()), range);
        Ok((left, right).to_spanned(span))
    }
}

impl<L, M, R> Parse for (Spanned<L>, Spanned<M>, Spanned<R>)
where
    L: Parse,
    M: Parse,
    R: Parse,
{
    fn peek(state: &mut ParserState) -> bool {
        L::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        let left = L::parse(state)?;
        let center = M::parse(state)?;
        let right = R::parse(state)?;
        let range = join_range(
            find_span_start!(left, center, right),
            find_span_end!(right, center, left),
        );
        let span = Span::new(Some(state.path()), range);
        Ok((left, center, right).to_spanned(span))
    }
}

impl<T, P> Parse for Punctuated<T, P>
where
    T: Parse,
    P: Parse,
{
    fn peek(state: &mut ParserState) -> bool {
        T::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        let mut self_ = Punctuated::<T, P> {
            pairs: Vec::new(),
            last: None,
        };
        let mut start = None;
        let mut end = start;
        loop {
            if !T::peek(state) {
                break;
            }
            let item = T::parse(state)?;
            item.span().range.inspect(|i| {
                start.get_or_insert(i.start);
                end = Some(i.end);
            });
            if P::peek(state) {
                let punct = P::parse(state)?;
                self_.pairs.push((item, punct));
            } else {
                self_.last = Some(Box::new(item));
                break;
            }
        }
        let span = Span::new(Some(state.path()), join_range(start, end));
        Ok(self_.to_spanned(span))
    }
}

impl<T: Parse> Parse for Vec<Spanned<T>> {
    fn peek(state: &mut ParserState) -> bool {
        T::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        let mut xs = <Vec<Spanned<T>>>::new();
        while T::peek(state) {
            xs.push(T::parse(state)?);
        }
        let start = xs.first().and_then(|x| x.span().range.map(|x| x.start));
        let end = xs.last().and_then(|x| x.span().range.map(|x| x.end));
        let span = Span::new(Some(state.path.clone()), join_range(start, end));
        Ok(xs.to_spanned(span))
    }
}

impl<T> Parse for Option<T>
where
    T: Parse,
{
    fn peek(state: &mut ParserState) -> bool {
        T::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        if T::peek(state) {
            T::parse(state).map(spanned_into)
        } else {
            Ok(None.to_spanned(span!(None, None)))
        }
    }
}

impl<T> Parse for Box<T>
where
    T: Parse,
{
    fn peek(state: &mut ParserState) -> bool {
        T::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        T::parse(state).map(Spanned::into_boxed)
    }
}

impl Parse for Item {
    fn peek(state: &mut ParserState) -> bool {
        TyDecl::peek(state) | Decl::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        if TyDecl::peek(state) {
            TyDecl::parse(state).map(spanned_into)
        } else if Decl::peek(state) {
            Decl::parse(state).map(spanned_into)
        } else {
            Err(ParseError::ExpectItem.to_spanned(state.prev_span()))
        }
    }
}

impl Parse for TyDecl {
    fn peek(state: &mut ParserState) -> bool {
        <Token![type]>::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        let type_ = <Token![type]>::parse(state)?;
        let quan = <Option<(Spanned<Quan>, Spanned<Token![.]>)>>::parse(state)?;
        let name = Ident::parse(state)?;
        let eq = <Token![=]>::parse(state)?;
        let body = <Punctuated<(Spanned<Ident>, Spanned<Ty>), Token![|]>>::parse(state)?;
        let semicolon = <Token![;]>::parse(state)?;
        let span = Span::new(
            Some(state.path()),
            join_range(
                find_span_start!(type_, quan, name, eq, body, semicolon),
                find_span_start!(semicolon, body, eq, name, quan, type_),
            ),
        );
        Ok(TyDecl {
            type_,
            quan: quan.into_inner(),
            name,
            eq,
            body,
            semicolon,
        }
        .to_spanned(span))
    }
}

impl Parse for Decl {
    fn peek(state: &mut ParserState) -> bool {
        Ident::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        let name = Ident::parse(state)?;
        let colon = <Token![:]>::parse(state)?;
        let quan = <Option<(Spanned<Quan>, Spanned<Token![.]>)>>::parse(state)?;
        let ty = Ty::parse(state)?;
        let body = DeclBody::parse(state)?;
        let span = Span::new(
            Some(state.path()),
            join_range(
                find_span_start!(name, colon, quan, ty, body),
                find_span_start!(body, ty, quan, colon, name),
            ),
        );
        Ok(Decl {
            name,
            colon,
            quan: quan.into_inner(),
            ty,
            body,
        }
        .to_spanned(span))
    }
}

impl Parse for DeclBody {
    fn peek(state: &mut ParserState) -> bool {
        <Token![=]>::peek(state) | <Token![|]>::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        if <Token![=]>::peek(state) {
            let eq = <Token![=]>::parse(state)?;
            let expr = Expr::parse(state)?;
            let semicolon = <Token![;]>::parse(state)?;
            let span = Span::new(
                Some(state.path()),
                join_range(
                    find_span_start!(eq, expr, semicolon),
                    find_span_start!(semicolon, expr, eq),
                ),
            );
            Ok(DeclBody::Inline(eq, expr, semicolon).to_spanned(span))
        } else if <Token![|]>::peek(state) {
            <Vec<Spanned<(Spanned<Token![|]>, Spanned<Equation>, Spanned<Token![;]>)>>>::parse(
                state,
            )
            .map(spanned_into)
        } else {
            Err(ParseError::ExpectDeclBody.to_spanned(state.prev_span()))
        }
    }
}

impl Parse for Equation {
    fn peek(state: &mut ParserState) -> bool {
        Pat_::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        let lhs = <Vec<Spanned<Pat_>>>::parse(state)?;
        let eq = <Token![=]>::parse(state)?;
        let rhs = Expr::parse(state)?;
        let span = Span::new(
            Some(state.path()),
            join_range(find_span_start!(lhs, eq, rhs), find_span_end!(rhs, eq, lhs)),
        );
        Ok(Equation { lhs, eq, rhs }.to_spanned(span))
    }
}

impl Parse for Quan {
    fn peek(state: &mut ParserState) -> bool {
        <Token![forall]>::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        let forall = <Token![forall]>::parse(state)?;
        let vars = <QuanVars>::parse(state)?;
        let span = Span::new(
            Some(state.path()),
            join_range(find_span_start!(forall, vars), find_span_end!(vars, forall)),
        );
        Ok(Quan { forall, vars }.to_spanned(span))
    }
}

impl Parse for QuanVars {
    fn peek(state: &mut ParserState) -> bool {
        Ident::peek(state) | <InParens<Punctuated<Ident, Token![,]>>>::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        if Ident::peek(state) {
            Ident::parse(state).map(spanned_into)
        } else if <InParens<Punctuated<Ident, Token![,]>>>::peek(state) {
            <InParens<Punctuated<Ident, Token![,]>>>::parse(state).map(spanned_into)
        } else {
            Err(ParseError::ExpectQuanVars.to_spanned(state.prev_span()))
        }
    }
}

impl Parse for Ty {
    fn peek(state: &mut ParserState) -> bool {
        Ident::peek(state)
            | <InParens<Ty>>::peek(state)
            | <InBraces<Punctuated<Ty, Token![,]>>>::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        let mut ty: Spanned<Ty> = if Ident::peek(state) {
            Ident::parse(state).map(spanned_into)?
        } else if <InParens<Ty>>::peek(state) {
            <Box<InParens<Ty>>>::parse(state).map(spanned_into)?
        } else if <InBraces<Punctuated<Ty, Token![,]>>>::peek(state) {
            <InBraces<Punctuated<Ty, Token![,]>>>::parse(state).map(spanned_into)?
        } else {
            return Err(ParseError::ExpectTy.to_spanned(state.prev_span()));
        };
        loop {
            if <Token![->]>::peek(state) {
                let arrow = <Token![->]>::parse(state)?;
                let ty_ = Ty::parse(state)?;
                let span = Span::new(
                    Some(state.path()),
                    join_range(find_span_start!(ty, ty_), find_span_end!(ty_, ty)),
                );
                ty = Ty::Func(Box::new(ty), arrow, Box::new(ty_)).to_spanned(span);
            } else if Ty::peek(state) {
                let ty_ = Ty::parse(state)?;
                let span = Span::new(
                    Some(state.path()),
                    join_range(find_span_start!(ty, ty_), find_span_end!(ty_, ty)),
                );
                ty = Ty::Apply(Box::new(ty), Box::new(ty_)).to_spanned(span);
            } else {
                break Ok(ty);
            }
        }
    }
}

impl Parse for Pat_ {
    fn peek(state: &mut ParserState) -> bool {
        Ident::peek(state) | <InParens<Pat>>::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        if Ident::peek(state) {
            Ident::parse(state).map(spanned_into)
        } else if <InParens<Pat>>::peek(state) {
            <Box<InParens<Pat>>>::parse(state).map(spanned_into)
        } else if <InBraces<Punctuated<Pat, Token![,]>>>::peek(state) {
            <InBraces<Punctuated<Pat, Token![,]>>>::parse(state).map(spanned_into)
        } else {
            Err(ParseError::ExpectPat.to_spanned(state.prev_span()))
        }
    }
}

impl Parse for Pat {
    fn peek(state: &mut ParserState) -> bool {
        Ident::peek(state) | <InParens<Pat>>::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        if Ident::peek(state) {
            let mut pat: Spanned<Pat> = Ident::parse(state)?.map(Into::into);
            while Pat::peek(state) {
                let pat_ = Pat::parse(state)?;
                let span = Span::new(
                    Some(state.path()),
                    join_range(find_span_start!(pat, pat_), find_span_end!(pat_, pat)),
                );
                pat = Pat::Apply(Box::new(pat), Box::new(pat_)).to_spanned(span);
            }
            Ok(pat)
        } else if <InParens<Pat>>::peek(state) {
            <Box<InParens<Pat>>>::parse(state).map(spanned_into)
        } else if <InBraces<Punctuated<Pat, Token![,]>>>::peek(state) {
            <InBraces<Punctuated<Pat, Token![,]>>>::parse(state).map(spanned_into)
        } else {
            Err(ParseError::ExpectPat.to_spanned(state.prev_span()))
        }
    }
}

impl Parse for Expr {
    fn peek(state: &mut ParserState) -> bool {
        Ident::peek(state) | <InParens<Expr>>::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        let mut expr: Spanned<Expr> = if Ident::peek(state) {
            Ident::parse(state).map(spanned_into)?
        } else if <InParens<Expr>>::peek(state) {
            <Box<InParens<Expr>>>::parse(state).map(spanned_into)?
        } else if <InBraces<Punctuated<Expr, Token![,]>>>::peek(state) {
            <InBraces<Punctuated<Expr, Token![,]>>>::parse(state).map(spanned_into)?
        } else {
            return Err(ParseError::ExpectExpr.to_spanned(state.prev_span()));
        };
        loop {
            if Expr::peek(state) {
                let expr_ = Expr::parse(state)?;
                let span = Span::new(
                    Some(state.path()),
                    join_range(find_span_start!(expr, expr_), find_span_end!(expr_, expr)),
                );
                expr = Expr::Apply(Box::new(expr), Box::new(expr_)).to_spanned(span);
            } else {
                break Ok(expr);
            }
        }
    }
}

pub macro find_span_start {
    ($withspan:expr) => {
        if let Some(range) = $withspan.span().range.clone() {
            Some(range.start)
        } else {
            None
        }
    },
    ($withspan:expr , $($ts:tt)*) => {
        if let Some(range) = $withspan.span().range.clone() {
            Some(range.start)
        } else {
            find_span_start!($($ts)*)
        }
    },
    () => {None},
}

pub macro find_span_end {
    ($withspan:expr) => {
        if let Some(range) = $withspan.span().range.clone() {
            Some(range.end)
        } else {
            None
        }
    },
    ($withspan:expr , $($ts:tt)*) => {
        if let Some(range) = $withspan.span().range.clone() {
            Some(range.end)
        } else {
            find_span_start!($($ts)*)
        }
    },
    () => {None},
}

pub fn join_range(
    start: Option<SourceIndex>,
    end: Option<SourceIndex>,
) -> Option<Range<SourceIndex>> {
    match (start, end) {
        (None, None) => None,
        (None, Some(i)) => Some(i..i),
        (Some(i), None) => Some(i..i),
        (Some(start), Some(end)) => Some(start..end),
    }
}

macro impl_parse_for_token {
    ($name:ident) => {
        impl Parse for tokens::$name {
            fn peek(parser: &mut ParserState) -> bool {
                match parser.peek() {
                    Some(t) => match t.inner() {
                        Token::$name => true,
                        _ => false,
                    },
                    None => false,
                }
            }
            fn parse(parser: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
                match parser.peek() {
                    Some(t) => match t.inner() {
                        Token::$name => {
                            let span = t.span();
                            parser.next();
                            Ok(tokens::$name.to_spanned(span))
                        },
                        _ => Err(ParseError::ExpectToken(tokens::$name::default().to_token())
                            .to_spanned(parser.prev_span.clone())),
                    },
                    None => Err(ParseError::UnexpectedEof.to_spanned(parser.prev_span.clone())),
                }
            }
        }
    },
    ($name:ident(_)) => {
        impl Parse for tokens::$name {
            fn peek(parser: &mut ParserState) -> bool {
                match parser.peek() {
                    Some(t) => match &t.inner() {
                        Token::$name(_) => true,
                        _ => false,
                    },
                    None => false,
                }
            }
            fn parse(parser: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
                match parser.peek() {
                    Some(t) => match t.inner() {
                        Token::$name(x) => {
                            let span = t.span();
                            let x = x.clone();
                            parser.next();
                            Ok(tokens::$name(x).to_spanned(span))
                        },
                        _ => Err(ParseError::ExpectToken(tokens::$name::default().to_token())
                            .to_spanned(parser.prev_span.clone())),
                    },
                    None => Err(ParseError::UnexpectedEof.to_spanned(parser.prev_span.clone())),
                }
            }
        }
    },
    ($name:ident, $($ts:tt)*) => {
        impl_parse_for_token!($name);
        impl_parse_for_token!($($ts)*);
    },
    ($name:ident(_), $($ts:tt)*) => {
        impl_parse_for_token!($name(_));
        impl_parse_for_token!($($ts)*);
    },
    () => {},
}

impl_parse_for_token! {
    Ident(_),
    ParenL,
    ParenR,
    BracketL,
    BracketR,
    BraceL,
    BraceR,
    Eq,
    Verbar,
    Colon,
    Period,
    Comma,
    Semicolon,
    Arrow,
    Type,
    Forall,
    Punct(_),
}
