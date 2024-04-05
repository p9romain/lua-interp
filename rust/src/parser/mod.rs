use self::{ast::Block, lexer::Extras};
use lalrpop_util::lalrpop_mod;
use logos::Logos;

pub mod ast;
mod internalast;
mod lexer;
mod simplify;
lalrpop_mod!(pub parser, "/parser/parser.rs");

#[derive(Debug, PartialEq, Clone, Default)]
pub struct LexingError();

pub fn parse_and_simplify(input: &str) -> Result<Block, String> {
    let lexer = lexer::Token::lexer_with_extras(input, Extras::new())
        .spanned()
        .map(|(token, span)| token.map(|token| (span.start, token, span.end)));
    let parser = parser::BlockParser::new();
    let ast = parser
        .parse(lexer)
        .map_err(|err| format!("Parsing error : {:?}", err))?;
    simplify::simp_outerblock(ast).map_err(|s| {
        format!(
            "Input program uses a lua feature that mini-lua does not support: {}",
            s
        )
    })
}
