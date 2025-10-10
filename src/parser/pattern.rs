use crate::{
    Pattern,
    error::{Error, Expected},
    parser::{
        Parsed,
        lexer::Token,
        lib::{Or, Output, ParseContext, Parser, identifier, just, token},
        term,
    },
};

pub fn pattern(px: &mut ParseContext) -> Output<Pattern<Parsed>> {
    Or((
        atomic_pattern
            .spanned()
            .and_ignore(just(Token::If))
            .and_then(term)
            .map(|(pattern, condition)| Pattern::If {
                pattern: Box::new(pattern),
                condition: Box::new(condition),
            }),
        atomic_pattern,
    ))
    .parse(px)
}

pub fn atomic_pattern(px: &mut ParseContext) -> Output<Pattern<Parsed>> {
    Or((
        as_pattern,
        underscore_pattern,
        capture_pattern,
        rest_pattern,
        integer_pattern,
        float_pattern,
        list_pattern,
        tuple_pattern,
    ))
    .parse(px)
}

pub fn list_pattern(px: &mut ParseContext) -> Output<Pattern<Parsed>> {
    pattern
        .spanned()
        .delimited_sequence(Token::OpenBracket, Token::Comma, Token::CloseBracket)
        .map(|patterns| Pattern::List(patterns.into_iter().map(Box::new).collect()))
        .parse(px)
}

pub fn tuple_pattern(px: &mut ParseContext) -> Output<Pattern<Parsed>> {
    pattern
        .spanned()
        .delimited_sequence(Token::OpenParen, Token::Comma, Token::CloseParen)
        .map(|patterns| Pattern::Tuple(patterns.into_iter().map(Box::new).collect()))
        .parse(px)
}

pub fn underscore_pattern(px: &mut ParseContext) -> Output<Pattern<Parsed>> {
    just(Token::Underscore).map(|_| Pattern::Wildcard).parse(px)
}

pub fn capture_pattern(px: &mut ParseContext) -> Output<Pattern<Parsed>> {
    identifier.map(Pattern::Capture).parse(px)
}

pub fn rest_pattern(px: &mut ParseContext) -> Output<Pattern<Parsed>> {
    just(Token::Ellipsis).map(|_| Pattern::Rest).parse(px)
}

pub fn as_pattern(px: &mut ParseContext) -> Output<Pattern<Parsed>> {
    identifier
        .and_ignore(just(Token::At))
        .and_then(pattern.spanned())
        .map(|(name, pattern)| Pattern::As {
            pattern: Box::new(pattern),
            name,
        })
        .parse(px)
}

pub fn integer_pattern(px: &mut ParseContext) -> Output<Pattern<Parsed>> {
    match token(px, Expected::Integer)? {
        Token::Integer(i) => Output::Ok(Pattern::Integer(i)),
        t => Output::Error(Error::exp_found(Expected::Integer, t, px)),
    }
}

pub fn float_pattern(px: &mut ParseContext) -> Output<Pattern<Parsed>> {
    match token(px, Expected::Float)? {
        Token::Float(f) => Output::Ok(Pattern::Float(f)),
        t => Output::Error(Error::exp_found(Expected::Float, t, px)),
    }
}
