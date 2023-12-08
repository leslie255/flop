#![allow(dead_code)]

use ast::AstParser;
use symbol::GlobalSymbolBuilder;
use token::TokenStream;

use crate::ast::AstNode;

mod cooker;
mod symbol;
mod token;
mod ty;
mod utils;

mod ast;

fn main() {
    let source = "
    fn main() {
        let x: i32;
        let y = x;
    };
    ";
    let ast_parser = AstParser::new(TokenStream::new(source).peekable());
    let mut symbols_builder = GlobalSymbolBuilder::new(ast_parser);
    let mut ast = Vec::<AstNode>::new();
    while let Some(node) = symbols_builder.next() {
        ast.push(node);
    }
    let global_symbols = symbols_builder.finish();
    cooker::cook_ast(ast, &global_symbols);
}
