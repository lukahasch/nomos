use crate::{
    Term, Type,
    error::Spanned,
    parser::{
        Parsed,
        lexer::Token,
        lib::{Or, Output, ParseContext, Parser, identifier, just},
        syntax::term,
    },
};

pub fn r#type(px: &mut ParseContext) -> Output<Spanned<Term<Parsed>>> {
    Or((function, variable, term_type)).parse(px)
}

fn term_type(px: &mut ParseContext) -> Output<Spanned<Term<Parsed>>> {
    just(Token::At).ignore_and(term).parse(px)
}

fn variable(px: &mut ParseContext) -> Output<Spanned<Term<Parsed>>> {
    identifier
        .map(Type::Variable)
        .map(Term::Type)
        .spanned()
        .parse(px)
}

fn function(px: &mut ParseContext) -> Output<Spanned<Term<Parsed>>> {
    just(Token::Fn)
        .ignore_and(r#type.fatal())
        .and_ignore(just(Token::Arrow).fatal())
        .and_then(r#type.fatal())
        .map(|(param, result)| Type::Function {
            parameter: Box::new(param),
            result: Box::new(result),
        })
        .map(Term::Type)
        .spanned()
        .parse(px)
}
