#![cfg(test)]

use crate::{
    ast::{parse::*, *},
    span::Spanned,
    token::lex::lex,
};
use std::rc::Rc;

use super::parse::ParseError;

fn parse_a_thing<T: Parse>(path: &str, source: &str) -> Result<Spanned<T>, Spanned<ParseError>> {
    let path = <Rc<str>>::from(path);
    let tokens = lex(path.clone(), source.into()).unwrap();
    let mut parser_state = ParserState::new(&tokens, path.clone());
    T::parse(&mut parser_state)
}

macro item {
    {
        name: $name:ident,
        ty: $ty:ty,
        cases: {
            $($source:expr => $expect:expr),* $(,)?
        } $(,)?
    } => {
        #[test]
        fn $name() {
            let path = concat!(stringify!($name), ".flop");
            $(
                let source: &str = $source;
                let expect: &str = $expect;
                let x = match parse_a_thing::<$ty>(path, source) {
                    Ok(x) => x,
                    Err(e) => {
                        println!("error: {e:?} @ {:?}", e.span());
                        println!("while parsing: {source}");
                        panic!();
                    }
                };
                if format!("{x:?}") != expect {
                    println!("expect : {expect}");
                    println!("found  : {x:?}");
                    panic!();
                }
            )*
        }
    }
}

item! {
    name: decl,
    ty: Decl,
    cases: {
        "x : ∀ α . α = x ;" => r#"Decl { name: "x", colon: Token![:], quan: Some((Quan { forall: Token![forall], vars: Single("α") }, Token![.])), ty: Typename("α"), body: Inline(Token![=], Var("x"), Token![;]) }"#,
        "x : ∀ (α, β) . α → β → α | x _ = x ;" => r#"Decl { name: "x", colon: Token![:], quan: Some((Quan { forall: Token![forall], vars: Multi((ParenL, ["α", Token![,], "β"], ParenR)) }, Token![.])), ty: Func(Typename("α"), Token![->], Func(Typename("β"), Token![->], Typename("α"))), body: Equations([(Token![|], Equation { lhs: [Binding("x"), Binding("_")], eq: Token![=], rhs: Var("x") }, Token![;])]) }"#,
    },
}

item! {
    name: decl_body,
    ty: DeclBody,
    cases: {
        "= x ;" => r#"Inline(Token![=], Var("x"), Token![;])"#,
        "| x = x ;" => r#"Equations([(Token![|], Equation { lhs: [Binding("x")], eq: Token![=], rhs: Var("x") }, Token![;])])"#,
    },
}

item! {
    name: equation,
    ty: Equation,
    cases: {
        "x = x" => r#"Equation { lhs: [Binding("x")], eq: Token![=], rhs: Var("x") }"#,
        "x y = x" => r#"Equation { lhs: [Binding("x"), Binding("y")], eq: Token![=], rhs: Var("x") }"#,
    },
}

item! {
    name: ty,
    ty: Ty,
    cases: {
        "T" => r#"Typename("T")"#,
        "T a" => r#"Apply(Typename("T"), Typename("a"))"#,
        "T → U" => r#"Func(Typename("T"), Token![->], Typename("U"))"#,
        "(T)" => r#"InParens((ParenL, Typename("T"), ParenR))"#,
    },
}

item! {
    name: expr,
    ty: Expr,
    cases: {
        "x" => r#"Var("x")"#,
        "x x" => r#"Apply(Var("x"), Var("x"))"#,
        "(x)" => r#"InParens((ParenL, Var("x"), ParenR))"#,
    },
}

item! {
    name: pat_,
    ty: Pat_,
    cases: {
        "x" => r#"Binding("x")"#,
        "x y" => r#"Binding("x")"#,
        "(x y)" => r#"InParens((ParenL, Apply(Binding("x"), Binding("y")), ParenR))"#,
    },
}

item! {
    name: pat,
    ty: Pat,
    cases: {
        "x" => r#"Binding("x")"#,
        "x x" => r#"Apply(Binding("x"), Binding("x"))"#,
        "(x)" => r#"InParens((ParenL, Binding("x"), ParenR))"#,
    },
}

item! {
    name: quan_vars,
    ty: QuanVars,
    cases: {
        "x" => r#"Single("x")"#,
        "(x)" => r#"Multi((ParenL, ["x"], ParenR))"#,
        "(x,)" => r#"Multi((ParenL, ["x", Token![,]], ParenR))"#,
        "(x, y)" => r#"Multi((ParenL, ["x", Token![,], "y"], ParenR))"#,
        "(x, y,)" => r#"Multi((ParenL, ["x", Token![,], "y", Token![,]], ParenR))"#,
    },
}

item! {
    name: quan,
    ty: Quan,
    cases: {
        "∀ x" => r#"Quan { forall: Token![forall], vars: Single("x") }"#,
    },
}
