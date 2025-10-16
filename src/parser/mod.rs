use crate::{
    Context, Term, Types,
    error::{Error, extract::ExtractError},
    parser::{
        lib::{Output, ParseContext, Parser, Spanned},
        syntax::term,
    },
};
use ariadne::Cache;

pub mod lexer;
pub mod lib;
pub mod syntax;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Parsed;

impl Types for Parsed {
    type Identifier = Spanned<String>;
    type List<T> = Vec<Spanned<T>>;
    type Block<T> = Vec<Spanned<T>>;
    type TeRec<T> = Box<Spanned<T>>;
    type PaRec<P> = Box<Spanned<P>>;
}

/// # Errors
/// - On invalid syntax
/// # Panics
/// - If the source has not been interned in the context
#[allow(clippy::result_large_err)]
pub fn parse(ctx: &mut Context, source: &'static str) -> Result<Spanned<Term<Parsed>>, Vec<Error>> {
    let contents = ctx.fetch(source).expect("Source not found").text();
    let lex = lexer::lexer(source, contents);
    match term.parse(&mut ParseContext::new(lex)) {
        Output::Ok(t) if !t.contains_error() => Ok(t),
        Output::Ok(t) => Err(t.into_inner().collect_errors()),
        Output::Error(e) | Output::Fatal(e) => Err(vec![e]),
    }
}
