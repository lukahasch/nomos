use std::{fmt::Debug, ops::FromResidual};

use crate::{
    Context, Pattern, Span, Term, Types,
    error::{Error, Expected, Found},
    parser::lexer::Token,
};
use ariadne::Cache;
use logos::Lexer;

pub mod lexer;

pub enum Output<T> {
    Ok(T),
    Error(Error),
    Fatal(Error),
}

pub struct ParseContext<'a> {
    pub lexer: Lexer<'a, Token>,
    pub delimiters: Vec<(Span, Token, Token)>,
}

impl<'a> ParseContext<'a> {
    pub fn new(lexer: Lexer<'a, Token>) -> Self {
        Self {
            lexer,
            delimiters: Vec::new(),
        }
    }

    pub fn extras(&self) -> &'static str {
        self.lexer.extras
    }

    pub fn span(&self) -> std::ops::Range<usize> {
        self.lexer.span()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub item: T,
    pub span: Span,
}

pub struct Parsed;

impl Types for Parsed {
    type Term = Box<Spanned<Term<Self>>>;
    type Identifier = Spanned<String>;
    type List = Vec<Spanned<Term<Self>>>;
    type Tuple = Vec<Spanned<Term<Self>>>;
    type Pattern = Box<Spanned<Pattern<Self>>>;
}

pub trait Parser {
    type Output;
    fn parse<'b, 'a>(&self, px: &'b mut ParseContext<'a>) -> Output<Self::Output>;

    fn spanned(self) -> impl Parser<Output = Spanned<Self::Output>> + Sized
    where
        Self: Sized,
    {
        move |px: &mut ParseContext<'_>| {
            let start = px.span().start;
            let item = self.parse(px)?;
            let end = px.span().end;
            let span = Span::new(px.extras(), start..end);
            Output::Ok(Spanned { item, span })
        }
    }

    fn r#try(self) -> impl Parser<Output = Option<Self::Output>> + Sized
    where
        Self: Sized,
    {
        move |px: &mut ParseContext<'_>| {
            let clone = px.lexer.clone();
            let lexer = std::mem::replace(&mut px.lexer, clone);
            let clone = px.delimiters.clone();
            let delimiters = std::mem::replace(&mut px.delimiters, clone);
            match self.parse(px) {
                Output::Ok(t) => Output::Ok(Some(t)),
                Output::Error(_) => {
                    px.lexer = lexer;
                    px.delimiters = delimiters;
                    Output::Ok(None)
                }
                Output::Fatal(e) => {
                    px.lexer = lexer;
                    px.delimiters = delimiters;
                    Output::Fatal(e)
                }
            }
        }
    }

    fn r#backtrack(self) -> impl Parser<Output = Self::Output> + Sized
    where
        Self: Sized,
    {
        move |px: &mut ParseContext<'_>| {
            let clone = px.lexer.clone();
            let lexer = std::mem::replace(&mut px.lexer, clone);
            let clone = px.delimiters.clone();
            let delimiters = std::mem::replace(&mut px.delimiters, clone);
            match self.parse(px) {
                Output::Ok(t) => Output::Ok(t),
                Output::Error(e) => {
                    px.lexer = lexer;
                    px.delimiters = delimiters;
                    Output::Error(e)
                }
                Output::Fatal(e) => {
                    px.lexer = lexer;
                    px.delimiters = delimiters;
                    Output::Fatal(e)
                }
            }
        }
    }

    fn r#ref(&self) -> impl Parser<Output = Self::Output> + '_
    where
        Self: Sized,
    {
        move |px: &mut ParseContext<'_>| self.parse(px)
    }

    fn seperated_by(self, token: Token) -> impl Parser<Output = Vec<Self::Output>>
    where
        Self: Sized,
    {
        move |px: &mut ParseContext<'_>| {
            let mut items = Vec::new();
            match self.parse(px) {
                Output::Ok(t) => items.push(t),
                Output::Error(e) => return Output::Ok(items),
                Output::Fatal(e) => return Output::Fatal(e),
            }
            while let Output::Ok(_) = just(token.clone()).backtrack().parse(px) {
                match self.parse(px) {
                    Output::Ok(t) => items.push(t),
                    Output::Error(_) => return Output::Ok(items),
                    Output::Fatal(e) => return Output::Fatal(e),
                }
            }
            Output::Ok(items)
        }
    }

    fn delimited_by(self, open: Token, close: Token) -> impl Parser<Output = Self::Output> + Sized
    where
        Self: Sized,
    {
        let open_token = open.clone();
        let close_token = close.clone();
        let open = just(open).spanned();
        let close = just(close).spanned();
        move |px: &mut ParseContext<'_>| {
            let Spanned {
                span: open_span, ..
            } = open.parse(px)?;
            px.delimiters
                .push((open_span, open_token.clone(), close_token.clone()));
            let item = self.parse(px)?;
            let Spanned {
                span: close_span, ..
            } = close.parse(px)?;
            px.delimiters.pop();
            Output::Ok(item)
        }
    }

    fn delimited_sequence(
        self,
        open: Token,
        seperator: Token,
        close: Token,
    ) -> impl Parser<Output = Vec<Self::Output>> + Sized
    where
        Self: Sized,
    {
        self.seperated_by(seperator).delimited_by(open, close)
    }

    fn map<U, F>(self, f: F) -> impl Parser<Output = U> + Sized
    where
        Self: Sized,
        F: Fn(Self::Output) -> U,
    {
        move |px: &mut ParseContext<'_>| {
            let item = self.parse(px)?;
            Output::Ok(f(item))
        }
    }

    fn and_then<P>(self, p: P) -> impl Parser<Output = (Self::Output, P::Output)> + Sized
    where
        Self: Sized,
        P: Parser,
    {
        move |px: &mut ParseContext<'_>| {
            let first = self.parse(px)?;
            let second = p.parse(px)?;
            Output::Ok((first, second))
        }
    }

    fn and_ignore<P>(self, p: P) -> impl Parser<Output = Self::Output> + Sized
    where
        Self: Sized,
        P: Parser,
    {
        move |px: &mut ParseContext<'_>| {
            let first = self.parse(px)?;
            let _ = p.parse(px)?;
            Output::Ok(first)
        }
    }

    fn ignore_and<P>(self, p: P) -> impl Parser<Output = P::Output> + Sized
    where
        Self: Sized,
        P: Parser,
    {
        move |px: &mut ParseContext<'_>| {
            let _ = self.parse(px)?;
            let second = p.parse(px)?;
            Output::Ok(second)
        }
    }

    fn inspect(
        self,
        f: impl Fn(&Self::Output, &ParseContext),
    ) -> impl Parser<Output = Self::Output> + Sized
    where
        Self: Sized,
    {
        move |px: &mut ParseContext<'_>| {
            let item = self.parse(px)?;
            f(&item, px);
            Output::Ok(item)
        }
    }

    fn fatal(self) -> impl Parser<Output = Self::Output> + Sized
    where
        Self: Sized,
    {
        move |px: &mut ParseContext<'_>| match self.parse(px) {
            Output::Ok(t) => Output::Ok(t),
            Output::Error(e) => Output::Fatal(e),
            Output::Fatal(e) => Output::Fatal(e),
        }
    }
}

/// INVARIANT: The source must exist in the context's sources map
pub fn parse(ctx: &mut Context, source: &'static str) -> Result<Spanned<Term<Parsed>>, Error> {
    let contents = ctx.fetch(source).expect("Source not found").text();
    let lex = lexer::lexer(source, contents);
    match term.parse(&mut ParseContext::new(lex)) {
        Output::Ok(t) => Ok(t),
        Output::Error(e) => Err(e),
        Output::Fatal(e) => Err(e),
    }
}

pub fn term(px: &mut ParseContext<'_>) -> Output<Spanned<Term<Parsed>>> {
    Or((function, list, tuple, block, integer, float, variable, r#if))
        .spanned()
        .parse(px)
}

impl<F, O> Parser for F
where
    F: Fn(&mut ParseContext<'_>) -> Output<O>,
{
    type Output = O;
    fn parse<'b, 'a>(&self, px: &'b mut ParseContext<'a>) -> Output<Self::Output> {
        self(px)
    }
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
    term.delimited_sequence(Token::OpenBracket, Token::Comma, Token::CloseBracket)
        .map(Term::List)
        .parse(px)
}

pub fn tuple(px: &mut ParseContext) -> Output<Term<Parsed>> {
    term.delimited_sequence(Token::OpenParen, Token::Comma, Token::CloseParen)
        .map(Term::Tuple)
        .parse(px)
}

pub fn block(px: &mut ParseContext) -> Output<Term<Parsed>> {
    term.delimited_by(Token::OpenBrace, Token::CloseBrace)
        .map(|t| t.item)
        .parse(px)
}

pub fn function(px: &mut ParseContext) -> Output<Term<Parsed>> {
    just(Token::Fn)
        .ignore_and(pattern.spanned())
        .and_ignore(just(Token::Arrow))
        .and_then(term)
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
        .ignore_and(term)
        .and_ignore(just(Token::Then))
        .and_then(term)
        .and_ignore(just(Token::Else))
        .and_then(term)
        .map(|((condition, then), r#else)| Term::If {
            condition: Box::new(condition),
            then: Box::new(then),
            r#else: Box::new(r#else),
        })
        .parse(px)
}

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

pub fn identifier(px: &mut ParseContext) -> Output<Spanned<String>> {
    match token(px, Expected::Identifier)? {
        Token::Identifier(name) => Output::Ok(Spanned {
            item: name,
            span: Span::new(px.extras(), px.span()),
        }),
        t => Output::Error(Error::exp_found(Expected::Identifier, t, px)),
    }
}

pub fn token(px: &mut ParseContext<'_>, expected: Expected) -> Output<Token> {
    match px.lexer.next() {
        Some(Ok(t)) => Output::Ok(t),
        Some(Err(e)) => Output::Fatal(e),
        None => Output::Error(Error::ExpectedFound {
            expected,
            found: Found::Eof,
            span: Span::new(px.extras(), px.span()),
        }),
    }
}

pub fn just(token: Token) -> impl Parser<Output = Token> {
    move |px: &mut ParseContext<'_>| match px.lexer.next() {
        Some(Ok(t)) if t == token => Output::Ok(t),
        Some(Ok(t)) => Output::Error(Error::exp_found(Expected::from(&token), t, px)),
        Some(Err(e)) => Output::Fatal(e),
        None => Output::Error(Error::ExpectedFound {
            expected: Expected::from(&token),
            found: Found::Eof,
            span: Span::new(px.extras(), px.span()),
        }),
    }
}

impl<T> std::ops::Try for Output<T> {
    type Output = T;
    type Residual = Output<!>;

    fn from_output(output: Self::Output) -> Self {
        Output::Ok(output)
    }

    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self {
            Output::Ok(t) => std::ops::ControlFlow::Continue(t),
            Output::Error(e) => std::ops::ControlFlow::Break(Output::Error(e)),
            Output::Fatal(e) => std::ops::ControlFlow::Break(Output::Fatal(e)),
        }
    }
}

impl<T> FromResidual for Output<T> {
    fn from_residual(residual: <Self as std::ops::Try>::Residual) -> Self {
        match residual {
            Output::Error(e) => Output::Error(e),
            Output::Fatal(e) => Output::Fatal(e),
        }
    }
}

pub struct Or<T>(pub T);

#[macro_export]
macro_rules! impl_or {
    () => {};
    ($first:ident $($ident:ident)*) => {
        #[allow(non_snake_case)]
        impl<$first, $($ident,)*> Parser for Or<($first, $($ident,)*)>
        where
            $first: Parser,
            $($ident: Parser<Output = $first::Output>,)*
        {
            type Output = $first::Output;

            #[allow(non_snake_case)]
            fn parse<'b, 'a>(&self, px: &'b mut ParseContext<'a>) -> $crate::parser::Output<Self::Output> {
                let (ref $first, $(ref $ident,)*) = self.0;
                let mut err = Error::Shrug;

                match ($first).r#ref().backtrack().parse(px) {
                    Output::Ok(t) => return Output::Ok(t),
                    Output::Error(e) => err = err + e,
                    Output::Fatal(e) => return Output::Fatal(e),
                }

                $(
                    match ($ident).r#ref().backtrack().parse(px) {
                        Output::Ok(t) => return Output::Ok(t),
                        Output::Error(e) => err = err + e,
                        Output::Fatal(e) => return Output::Fatal(e),
                    }
                )*

                Output::Error(err)
            }
        }
        impl_or!($($ident)*);
    };
}

impl_or!(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z);

impl Debug for Term<Parsed> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Error(e) => f.debug_tuple("Error").field(e).finish(),
            Term::Integer(i) => f.debug_tuple("Integer").field(i).finish(),
            Term::Float(fl) => f.debug_tuple("Float").field(fl).finish(),
            Term::List(l) => f.debug_tuple("List").field(l).finish(),
            Term::Tuple(t) => f.debug_tuple("Tuple").field(t).finish(),
            Term::Application { function, argument } => f
                .debug_struct("Application")
                .field("function", function)
                .field("argument", argument)
                .finish(),
            Term::Function { pattern, body } => f
                .debug_struct("Function")
                .field("pattern", pattern)
                .field("body", body)
                .finish(),
            Term::Variable(v) => Debug::fmt(v, f),
            Term::Let {
                pattern,
                value,
                body,
            } => f
                .debug_struct("Let")
                .field("pattern", pattern)
                .field("value", value)
                .field("body", body)
                .finish(),
            Term::If {
                condition,
                then,
                r#else,
            } => f
                .debug_struct("If")
                .field("condition", condition)
                .field("then", then)
                .field("else", r#else)
                .finish(),
            Term::Match { value, branches } => f
                .debug_struct("Match")
                .field("value", value)
                .field("branches", branches)
                .finish(),
            Term::Inference(i) => Debug::fmt(i, f),
        }
    }
}

impl Debug for Pattern<Parsed> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Wildcard => write!(f, "_"),
            Pattern::Capture(id) => Debug::fmt(id, f),
            Pattern::Integer(i) => write!(f, "{}", i),
            Pattern::Float(fl) => write!(f, "{}", fl),
            Pattern::Rest => write!(f, "..."),
            Pattern::Tuple(t) => f.debug_tuple("Tuple").field(t).finish(),
            Pattern::List(l) => f.debug_list().entries(l.iter()).finish(),
            Pattern::As { pattern, name } => f
                .debug_struct("As")
                .field("pattern", pattern)
                .field("name", name)
                .finish(),
            Pattern::If { pattern, condition } => f
                .debug_struct("If")
                .field("pattern", pattern)
                .field("condition", condition)
                .finish(),
        }
    }
}
