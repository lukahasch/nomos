use crate::{
    LangItem, Term,
    error::{Error, Expected, Spanned},
    parser::{
        Parsed,
        lexer::Token,
        lib::{
            Binary, Or, Output, ParseContext, Parser, error, identifier, just, skip_delimited,
            token,
        },
        syntax::pattern,
    },
};

pub fn term(px: &mut ParseContext<'_>) -> Output<Spanned<Term<Parsed>>> {
    atomic_term
        .operators(
            error(Error::Shrug),
            just(Token::Plus).map(|_| {
                Binary::new(
                    |lhs, rhs, span| Spanned {
                        item: Term::Application {
                            function: Box::new(Spanned {
                                item: Term::LangItem(LangItem::Add),
                                span: span.clone(),
                            }),
                            argument: Box::new(Spanned {
                                item: Term::List(vec![lhs, rhs]),
                                span: span.clone(),
                            }),
                        },
                        span,
                    },
                    1,
                )
            }),
            error(Error::Shrug),
        )
        .parse(px)
}

#[allow(clippy::missing_panics_doc)]
pub fn atomic_term(px: &mut ParseContext<'_>) -> Output<Spanned<Term<Parsed>>> {
    Or((
        function, list, tuple, block, integer, float, variable, r#if, r#let, r#match, define,
    ))
    .spanned()
    .repeated1()
    .map(|terms| {
        terms
            .into_iter()
            .reduce(|function, argument| {
                let span = function.span.clone() + argument.span.clone();
                Spanned {
                    item: Term::Application {
                        function: Box::new(function),
                        argument: Box::new(argument),
                    },
                    span,
                }
            })
            .expect("At least one term present")
    })
    .parse(px)
}

pub fn integer(px: &mut ParseContext<'_>) -> Output<Term<Parsed>> {
    match token(px, Expected::Integer)? {
        Token::Integer(i) => Output::Ok(Term::Integer(i)),
        t => Output::Error(Error::exp_found(Expected::Integer, t, px)),
    }
}

pub fn float(px: &mut ParseContext<'_>) -> Output<Term<Parsed>> {
    match token(px, Expected::Float)? {
        Token::Float(f) => Output::Ok(Term::Float(f)),
        t => Output::Error(Error::exp_found(Expected::Float, t, px)),
    }
}

pub fn list(px: &mut ParseContext) -> Output<Term<Parsed>> {
    term.fatal()
        .delimited_sequence(Token::OpenBracket, Token::Comma, Token::CloseBracket)
        .map(Term::List)
        .recover_with(skip_delimited(Token::OpenBracket, Token::CloseBracket))
        .parse(px)
}

pub fn tuple(px: &mut ParseContext) -> Output<Term<Parsed>> {
    term.fatal()
        .map(|Spanned { item, .. }| item)
        .delimited_by(Token::OpenParen, Token::CloseParen)
        .recover_with(skip_delimited(Token::OpenParen, Token::CloseParen))
        .parse(px)
}

pub fn block(px: &mut ParseContext) -> Output<Term<Parsed>> {
    term.fatal()
        .delimited_sequence(Token::OpenBrace, Token::Semicolon, Token::CloseBrace)
        .map(Term::Block)
        .recover_with(skip_delimited(Token::OpenBrace, Token::CloseBrace))
        .parse(px)
}

pub fn function(px: &mut ParseContext) -> Output<Term<Parsed>> {
    just(Token::Fn)
        .ignore_and(pattern.spanned().fatal())
        .and_ignore(just(Token::Arrow).fatal())
        .and_then(term.fatal())
        .map(|(pattern, body)| Term::Function {
            pattern: Box::new(pattern),
            body: Box::new(body),
        })
        .parse(px)
}

pub fn variable(px: &mut ParseContext) -> Output<Term<Parsed>> {
    identifier.map(Term::Variable).parse(px)
}

pub fn r#if(px: &mut ParseContext) -> Output<Term<Parsed>> {
    just(Token::If)
        .ignore_and(term.fatal())
        .and_ignore(just(Token::Then).fatal())
        .and_then(term.fatal())
        .and_ignore(just(Token::Else).fatal())
        .and_then(term.fatal())
        .map(|((condition, then), r#else)| Term::If {
            condition: Box::new(condition),
            then: Box::new(then),
            r#else: Box::new(r#else),
        })
        .parse(px)
}

pub fn r#match(px: &mut ParseContext) -> Output<Term<Parsed>> {
    just(Token::Match)
        .ignore_and(term.fatal())
        .and_ignore(just(Token::With).fatal())
        .and_ignore(just(Token::Pipe).optional())
        .and_then(
            pattern
                .spanned()
                .and_ignore(just(Token::Arrow).fatal())
                .and_then(term.fatal())
                .seperated_by(Token::Pipe),
        )
        .map(|(value, branches)| Term::Match {
            value: Box::new(value),
            branches: branches
                .into_iter()
                .map(|(pattern, body)| (Box::new(pattern), Box::new(body)))
                .collect(),
        })
        .parse(px)
}

pub fn r#let(px: &mut ParseContext) -> Output<Term<Parsed>> {
    just(Token::Let)
        .ignore_and(pattern.spanned().fatal())
        .and_ignore(just(Token::Equals).fatal())
        .and_then(term.fatal())
        .and_then(just(Token::In).ignore_and(term.fatal()).optional())
        .map(|((pattern, value), body)| Term::Let {
            pattern: Box::new(pattern),
            value: Box::new(value),
            body: body.map(Box::new),
        })
        .parse(px)
}

pub fn define(px: &mut ParseContext) -> Output<Term<Parsed>> {
    just(Token::Def)
        .ignore_and(pattern.spanned().fatal())
        .and_ignore(just(Token::Equals).fatal())
        .and_then(term.fatal())
        .and_then(just(Token::In).ignore_and(term.fatal()).optional())
        .map(|((pattern, value), body)| Term::Define {
            pattern: Box::new(pattern),
            value: Box::new(value),
            body: body.map(Box::new),
        })
        .parse(px)
}
