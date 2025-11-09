use crate::{
    Context, Node, Pattern, Term,
    error::{Error, Spanned},
    parser::lib::{Output, ParseContext, Parser},
};
use ariadne::Cache;

pub mod lexer;
pub mod lib;
pub mod syntax;

/// - On invalid syntax
/// # Panics
/// - If the source has not been interned in the context
#[allow(clippy::result_large_err)]
pub fn parse(ctx: &mut Context, source: &'static str) -> Result<Node<Term>, Vec<Error>> {
    /*let contents = ctx.fetch(source).expect("Source not found").text();
    let lex = lexer::lexer(source, contents);
    match term.parse(&mut ParseContext::new(lex)) {
        Output::Ok(t) if !t.contains_error() => Ok(t),
        Output::Ok(t) => Err(t.into_inner().collect_errors()),
        Output::Error(e) | Output::Fatal(e) => Err(vec![e]),
    }*/
    todo!()
}
