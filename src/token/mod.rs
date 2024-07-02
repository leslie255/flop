pub mod lex;

use std::{fmt, fmt::Debug, rc::Rc};

use crate::span::{Span, Spanned, ToSpanned};

pub trait TokenTrait: Sized + Default {
    fn to_token(self) -> Token;
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum Token {
    Ident(Rc<str>),
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
    /// `→`
    Arrow,
    Type,
    /// `∀`
    Forall,
    Punct(Rc<str>),
}

pub macro Token {
    [=] => { crate::token::tokens::Eq },
    [|] => { crate::token::tokens::Verbar },
    [:] => { crate::token::tokens::Colon },
    [.] => { crate::token::tokens::Period },
    [,] => { crate::token::tokens::Comma },
    [;] => { crate::token::tokens::Semicolon },
    [->] => { crate::token::tokens::Arrow },
    [type] => { crate::token::tokens::Type },
    [forall] => { crate::token::tokens::Forall },
}

pub mod tokens {
    use super::*;

    macro decl_token_type {
        { name: $name:ident, debug: $debug:literal $(,)? } => {
            #[derive(Clone, PartialEq, PartialOrd, Default)]
            pub struct $name;
            impl TokenTrait for $name {
                fn to_token(self) -> Token {
                    Token::$name
                }
            }
            impl FnOnce<(Span,)> for $name {
                type Output = Spanned<Token>;
                extern "rust-call" fn call_once(self, args: (Span,)) -> Self::Output {
                    self.to_token().to_spanned(args.0)
                }
            }
            impl Debug for $name {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    write!(f, $debug)
                }
            }
        },
    }

    #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
    pub struct Ident(pub Rc<str>);
    impl TokenTrait for Ident {
        fn to_token(self) -> Token {
            #[inline(always)]
            fn f(self_: Ident) -> Rc<str> {
                self_.0
            }
            Token::Ident(f(self))
        }
    }
    impl FnOnce<(Span,)> for Ident {
        type Output = Spanned<Token>;
        extern "rust-call" fn call_once(self, args: (Span,)) -> Self::Output {
            self.to_token().to_spanned(args.0)
        }
    }
    impl Debug for Ident {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            Debug::fmt(&self.0, f)
        }
    }
    impl std::ops::Deref for Ident {
        type Target = Rc<str>;
        fn deref(&self) -> &<Self as std::ops::Deref>::Target {
            #[inline(always)]
            fn f(self_: &Ident) -> &Rc<str> {
                &self_.0
            }
            f(self)
        }
    }
    impl std::ops::DerefMut for Ident {
        fn deref_mut(&mut self) -> &mut <Self as std::ops::Deref>::Target {
            #[inline(always)]
            fn f(self_: &mut Ident) -> &mut Rc<str> {
                &mut self_.0
            }
            f(self)
        }
    }
    impl AsRef<Rc<str>> for Ident {
        fn as_ref(&self) -> &Rc<str> {
            self
        }
    }
    impl AsMut<Rc<str>> for Ident {
        fn as_mut(&mut self) -> &mut Rc<str> {
            self
        }
    }

    #[derive(Clone, PartialEq, PartialOrd, Default)]
    pub struct Punct(pub Rc<str>);
    impl TokenTrait for Punct {
        fn to_token(self) -> Token {
            #[inline(always)]
            fn f(self_: Punct) -> Rc<str> {
                self_.0
            }
            Token::Punct(f(self))
        }
    }
    impl FnOnce<(Span,)> for Punct {
        type Output = Spanned<Token>;
        extern "rust-call" fn call_once(self, args: (Span,)) -> Self::Output {
            self.to_token().to_spanned(args.0)
        }
    }
    impl Debug for Punct {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            Debug::fmt(&self.0, f)
        }
    }
    impl std::ops::Deref for Punct {
        type Target = Rc<str>;
        fn deref(&self) -> &<Self as std::ops::Deref>::Target {
            #[inline(always)]
            fn f(self_: &Punct) -> &Rc<str> {
                &self_.0
            }
            f(self)
        }
    }
    impl std::ops::DerefMut for Punct {
        fn deref_mut(&mut self) -> &mut <Self as std::ops::Deref>::Target {
            #[inline(always)]
            fn f(self_: &mut Punct) -> &mut Rc<str> {
                &mut self_.0
            }
            f(self)
        }
    }
    impl AsRef<Rc<str>> for Punct {
        fn as_ref(&self) -> &Rc<str> {
            self
        }
    }
    impl AsMut<Rc<str>> for Punct {
        fn as_mut(&mut self) -> &mut Rc<str> {
            self
        }
    }

    decl_token_type! { name: ParenL, debug: "ParenL" }
    decl_token_type! { name: ParenR, debug: "ParenR" }
    decl_token_type! { name: BracketL, debug: "BracketL" }
    decl_token_type! { name: BracketR, debug: "BracketR" }
    decl_token_type! { name: BraceL, debug: "BraceL" }
    decl_token_type! { name: BraceR, debug: "BraceR" }
    decl_token_type! { name: Eq, debug: "Token![=]" }
    decl_token_type! { name: Verbar, debug: "Token![|]" }
    decl_token_type! { name: Colon, debug: "Token![:]" }
    decl_token_type! { name: Period, debug: "Token![.]" }
    decl_token_type! { name: Comma, debug: "Token![,]" }
    decl_token_type! { name: Semicolon, debug: "Token![;]" }
    decl_token_type! { name: Arrow, debug: "Token![->]" }
    decl_token_type! { name: Type, debug: "Token![type]" }
    decl_token_type! { name: Forall, debug: "Token![forall]" }
}
