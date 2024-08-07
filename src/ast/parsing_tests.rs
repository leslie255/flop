use crate::{
    ast::{parse::*, *},
    span::Spanned,
    token::lex::lex,
};
use std::rc::Rc;

use super::parse::ParseError;

fn parse_a_thing<T: Parse>(source: &str) -> Result<Spanned<T>, Spanned<ParseError>> {
    let path = <Rc<str>>::from("source.flop");
    let tokens = lex(path.clone(), source).unwrap();
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
            $(
                let source: &str = $source;
                let expect: &str = $expect;
                let x = match parse_a_thing::<$ty>(source) {
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
    name: ty_decl,
    ty: TyDecl,
    cases: {
        "type T = T U;" => r#"TyDecl { type_: Token![type], quan: None, name: "T", eq: Token![=], body: [("T", Some(Typename("U")))], semicolon: Token![;] }"#,
        "type T = T;" => r#"TyDecl { type_: Token![type], quan: None, name: "T", eq: Token![=], body: [("T", None)], semicolon: Token![;] }"#,
    },
}

item! {
    name: decl,
    ty: Decl,
    cases: {
        "f : ∀ α . α = x ;" => r#"Decl { name: "f", colon: Token![:], quan: Some((Quan { forall: Token![forall], vars: Single("α") }, Token![.])), ty: Typename("α"), body: Inline(Token![=], Var("x"), Token![;]) }"#,
        "f : ∀ (α, β) . α → β → α | x _ = x ;" => r#"Decl { name: "f", colon: Token![:], quan: Some((Quan { forall: Token![forall], vars: Multi((ParenL, ["α", Token![,], "β"], ParenR)) }, Token![.])), ty: Func(Typename("α"), Token![->], Func(Typename("β"), Token![->], Typename("α"))), body: Equations([(Token![|], Equation { lhs: [Binding("x"), Binding("_")], eq: Token![=], rhs: Var("x") }, Token![;])]) }"#,
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
    name: ty_expr,
    ty: TyExpr,
    cases: {
        "T" => r#"Typename("T")"#,
        "T a" => r#"Apply(Typename("T"), Typename("a"))"#,
        "T → U" => r#"Func(Typename("T"), Token![->], Typename("U"))"#,
        "(T)" => r#"InParens((ParenL, Typename("T"), ParenR))"#,
        "{T}" => r#"Tuple((BraceL, [Typename("T")], BraceR))"#,
    },
}

item! {
    name: expr,
    ty: Expr,
    cases: {
        "x" => r#"Var("x")"#,
        "x x" => r#"Apply(Var("x"), Var("x"))"#,
        "{}" => r#"Tuple((BraceL, [], BraceR))"#,
        "x {}" => r#"Apply(Var("x"), Tuple((BraceL, [], BraceR)))"#,
        "(x)" => r#"InParens((ParenL, Var("x"), ParenR))"#,
        "{x}" => r#"Tuple((BraceL, [Var("x")], BraceR))"#,
    },
}

item! {
    name: pat_,
    ty: Pat_,
    cases: {
        "x" => r#"Binding("x")"#,
        "x y" => r#"Binding("x")"#,
        "(x y)" => r#"InParens((ParenL, Apply("x", Binding("y")), ParenR))"#,
        "{x}" => r#"Tuple((BraceL, [Binding("x")], BraceR))"#,
    },
}

item! {
    name: pat,
    ty: Pat,
    cases: {
        "x" => r#"Binding("x")"#,
        "x x" => r#"Apply("x", Binding("x"))"#,
        "(x)" => r#"InParens((ParenL, Binding("x"), ParenR))"#,
        "{x}" => r#"Tuple((BraceL, [Binding("x")], BraceR))"#,
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
