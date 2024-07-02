#![feature(
    box_patterns,
    decl_macro,
    unboxed_closures,
    fn_traits,
    impl_trait_in_assoc_type
)]

use std::{env, rc::Rc};

use ast::parse::{Parse, ParserState};
use hir::build::RawProgram;

mod ast;
mod hir;
mod source_str;
mod span;
mod token;
mod utils;

fn main() {
    let input_path: Rc<str> = env::args().nth(1).expect("Expect input path").into();
    assert!(
        env::args().count() == 2,
        "Must have only 1 argument as input path"
    );
    let source = std::fs::read_to_string(&input_path[..]).unwrap();
    let tokens = token::lex::lex(input_path.clone(), &source).unwrap();
    let raw_program = {
        let mut parser_state = ParserState::new(&tokens, input_path.clone());
        let mut p = RawProgram::default();
        while !parser_state.is_eof() {
            let item = ast::Item::parse(&mut parser_state).unwrap();
            p.add_item(item).unwrap();
        }
        p
    };
    let hir_program = hir::build::build_hir(raw_program).unwrap();
    dbg!(&hir_program);
}
