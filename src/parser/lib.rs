use std::ops::{FromResidual, Try};

use logos::Lexer;

use crate::{Error, error::Span, parser::lexer::Token};

pub enum Output<T> {
    Ok(T),
    Error(Error),
    Fatal(Error),
}

pub struct ParseContext<'a> {
    pub lexer: Lexer<'a, Token>,
    pub source: &'static str,
    pub stack: Vec<Item>,
}

pub struct Item {
    pub opened: Span,
    pub opener: Token,
    pub closer: Token,
}

pub trait Parser: Sized {
    type Output;

    fn parse(&self, px: &mut ParseContext) -> Output<Self::Output>;

    fn post<O>(&self, output: Output<O>) -> Output<O> {
        output
    }

    fn map<O>(self, f: impl Fn(Self::Output) -> O) -> impl Parser<Output = O> {
        move |px: &mut ParseContext| match self.parse(px) {
            Output::Ok(t) => Output::Ok(f(t)),
            Output::Error(e) => Output::Error(e),
            Output::Fatal(e) => Output::Fatal(e),
        }
    }

    fn map_err(self, f: impl Fn(Error) -> Error) -> impl Parser<Output = Self::Output> {
        move |px: &mut ParseContext| match self.parse(px) {
            Output::Ok(t) => Output::Ok(t),
            Output::Error(e) => Output::Error(f(e)),
            Output::Fatal(e) => Output::Fatal(e),
        }
    }

    fn map_fatal(self, f: impl Fn(Error) -> Error) -> impl Parser<Output = Self::Output> {
        move |px: &mut ParseContext| match self.parse(px) {
            Output::Ok(t) => Output::Ok(t),
            Output::Error(e) => Output::Error(e),
            Output::Fatal(e) => Output::Fatal(f(e)),
        }
    }

    fn and<P>(self, parser: P) -> impl Parser<Output = (Self::Output, P::Output)>
    where
        P: Parser,
    {
        move |px: &mut ParseContext| match self.parse(px) {
            Output::Ok(t1) => match self.post(parser.parse(px)) {
                Output::Ok(t2) => Output::Ok((t1, t2)),
                Output::Error(e) => Output::Error(e),
                Output::Fatal(e) => Output::Fatal(e),
            },
            Output::Error(e) => Output::Error(e),
            Output::Fatal(e) => Output::Fatal(e),
        }
    }

    fn and_ignore<P>(self, parser: P) -> impl Parser<Output = Self::Output>
    where
        P: Parser,
    {
        move |px: &mut ParseContext| match self.parse(px) {
            Output::Ok(t1) => match self.post(parser.parse(px)) {
                Output::Ok(_) => Output::Ok(t1),
                Output::Error(e) => Output::Error(e),
                Output::Fatal(e) => Output::Fatal(e),
            },
            Output::Error(e) => Output::Error(e),
            Output::Fatal(e) => Output::Fatal(e),
        }
    }

    fn ignore_and<P>(self, parser: P) -> impl Parser<Output = P::Output>
    where
        P: Parser,
    {
        move |px: &mut ParseContext| match self.parse(px) {
            Output::Ok(_) => match self.post(parser.parse(px)) {
                Output::Ok(t2) => Output::Ok(t2),
                Output::Error(e) => Output::Error(e),
                Output::Fatal(e) => Output::Fatal(e),
            },
            Output::Error(e) => Output::Error(e),
            Output::Fatal(e) => Output::Fatal(e),
        }
    }

    fn fatal(self) -> impl Parser<Output = Self::Output> {
        move |px: &mut ParseContext| match self.parse(px) {
            Output::Ok(t) => Output::Ok(t),
            Output::Error(e) => Output::Fatal(e),
            Output::Fatal(e) => Output::Fatal(e),
        }
    }

    fn and_fatal(self) -> impl Parser<Output = Self::Output> {
        Fatal { parser: self }
    }
}

impl<F, O> Parser for F
where
    F: Fn(&mut ParseContext) -> Output<O>,
{
    type Output = O;

    fn parse(&self, px: &mut ParseContext) -> Output<Self::Output> {
        self(px)
    }
}

pub struct Fatal<P: Parser> {
    parser: P,
}

impl<P: Parser> Parser for Fatal<P> {
    type Output = P::Output;

    fn parse(&self, px: &mut ParseContext) -> Output<Self::Output> {
        self.parser.parse(px)
    }

    fn post<O>(&self, output: Output<O>) -> Output<O> {
        match self.parser.post(output) {
            Output::Ok(t) => Output::Ok(t),
            Output::Error(e) => Output::Fatal(e),
            Output::Fatal(e) => Output::Fatal(e),
        }
    }
}

impl<T> Try for Output<T> {
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

pub fn just(t: Token) -> impl Parser<Output = Token> {
    move |px: &mut ParseContext| todo!()
}
