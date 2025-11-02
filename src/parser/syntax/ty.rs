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
    atomic
        .repeated1()
        .map(|types| {
            types
                .into_iter()
                .reduce(|acc, item| {
                    let span = acc.span.clone() + item.span.clone();
                    Spanned {
                        item: Term::Type(Type::Application {
                            function: Box::new(acc),
                            argument: Box::new(item),
                        }),
                        span,
                    }
                })
                .unwrap()
        })
        .parse(px)
}

pub fn symbol(px: &mut ParseContext) -> Output<Spanned<Term<Parsed>>> {
    just(Token::Dollar)
        .ignore_and(identifier)
        .map(Term::Symbol)
        .spanned()
        .parse(px)
}

pub fn atomic(px: &mut ParseContext) -> Output<Spanned<Term<Parsed>>> {
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
