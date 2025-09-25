use ariadne::*;
use colored::Colorize;
use logos::Lexer;
use std::{
    cmp::Ordering,
    fmt::Display,
    ops::{Deref, DerefMut, Range},
};
use thiserror::Error;

use crate::parser::{ParseContext, lexer::Token};

#[derive(Debug, Error, Clone, PartialEq, Default)]
pub enum Error {
    #[default]
    #[error("{}", rainbow(r"¯\_(ツ)_/¯"))]
    Shrug,
    #[error("Expected {expected} found {found}")]
    ExpectedFound {
        expected: Expected,
        found: Found,
        span: Span,
    },
    #[error("Expected one of {} found {}", expected.to_string().green(), found.to_string().red())]
    ExpectedOneOf {
        expected: OneOf<Expected>,
        found: Found,
        span: Span,
    },
    #[error("Unknown Character {}", format!("'{}'", .0).red())]
    UnknownCharacter(char, Span),
    #[error("Invalid Integer {}", format!("'{}'", .0).red())]
    InvalidInteger(String, Span),
    #[error("Invalid Float {}", format!("'{}'", .0).red())]
    InvalidFloat(String, Span),
}

pub fn rainbow(str: &str) -> String {
    let colors = ["red", "yellow", "green", "cyan", "blue", "magenta"];
    str.chars()
        .enumerate()
        .map(|(i, c)| {
            let color = colors[i % colors.len()];
            match color {
                "red" => c.to_string().red().to_string(),
                "yellow" => c.to_string().yellow().to_string(),
                "green" => c.to_string().green().to_string(),
                "cyan" => c.to_string().cyan().to_string(),
                "blue" => c.to_string().blue().to_string(),
                "magenta" => c.to_string().magenta().to_string(),
                _ => c.to_string(),
            }
        })
        .collect()
}

#[derive(Debug, Error, Clone, PartialEq, Hash, Eq, PartialOrd)]
pub enum Expected {
    #[error("integer")]
    Integer,
    #[error("float")]
    Float,
    #[error("'['")]
    OpenBracket,
    #[error("']'")]
    CloseBracket,
    #[error("'('")]
    OpenParen,
    #[error("')'")]
    CloseParen,
    #[error("'{{'")]
    OpenBrace,
    #[error("'}}'")]
    CloseBrace,
    #[error("','")]
    Comma,
    #[error("'if'")]
    If,
    #[error("'then'")]
    Then,
    #[error("'else'")]
    Else,
    #[error("'let'")]
    Let,
    #[error("'in'")]
    In,
    #[error("'match'")]
    Match,
    #[error("'with'")]
    With,
    #[error("'->'")]
    Arrow,
    #[error("'fn'")]
    Fn,
    #[error("'_'")]
    Underscore,
    #[error("'...'")]
    Ellipsis,
    #[error("'@'")]
    At,
    #[error("'='")]
    Equals,
    #[error("identifier")]
    Identifier,
    #[error("end of file")]
    Eof,
    #[error("pattern")]
    Pattern,
}

impl<'a> From<&'a Token> for Expected {
    fn from(token: &'a Token) -> Self {
        match token {
            Token::Integer(_) => Expected::Integer,
            Token::Float(_) => Expected::Float,
            Token::OpenBracket => Expected::OpenBracket,
            Token::CloseBracket => Expected::CloseBracket,
            Token::OpenParen => Expected::OpenParen,
            Token::CloseParen => Expected::CloseParen,
            Token::OpenBrace => Expected::OpenBrace,
            Token::CloseBrace => Expected::CloseBrace,
            Token::Comma => Expected::Comma,
            Token::If => Expected::If,
            Token::Then => Expected::Then,
            Token::Else => Expected::Else,
            Token::Let => Expected::Let,
            Token::In => Expected::In,
            Token::Match => Expected::Match,
            Token::With => Expected::With,
            Token::Arrow => Expected::Arrow,
            Token::Fn => Expected::Fn,
            Token::Underscore => Expected::Underscore,
            Token::Ellipsis => Expected::Ellipsis,
            Token::At => Expected::At,
            Token::Equals => Expected::Equals,
            Token::Identifier(_) => Expected::Identifier,
        }
    }
}

#[derive(Debug, Error, Clone, PartialEq)]
pub enum Found {
    #[error("end of file")]
    Eof,
    #[error("{0}")]
    Token(Token),
}

impl From<Token> for Found {
    fn from(token: Token) -> Self {
        Found::Token(token)
    }
}

impl Error {
    pub fn from_lexer(lex: &Lexer<Token>) -> Self {
        let span = Span::new(lex.extras, lex.span());
        match lex.slice().chars().next() {
            Some(c) => Error::UnknownCharacter(c, span),
            None => Error::Shrug,
        }
    }

    pub fn exp_found(expected: Expected, found: Token, px: &ParseContext) -> Self {
        Error::ExpectedFound {
            expected,
            found: Found::from(found),
            span: Span::new(px.extras(), px.span()),
        }
    }

    pub fn span(&self) -> Option<Span> {
        match self {
            Error::Shrug => None,
            Error::ExpectedFound { span, .. } => Some(span),
            Error::ExpectedOneOf { span, .. } => Some(span),
            Error::UnknownCharacter(_, span) => Some(span),
            Error::InvalidInteger(_, span) => Some(span),
            Error::InvalidFloat(_, span) => Some(span),
        }
        .cloned()
    }

    pub fn sources(&self) -> Option<Vec<Span>> {
        match self {
            Error::Shrug => None,
            Error::ExpectedFound { span, .. } => Some(vec![span.clone()]),
            Error::ExpectedOneOf { span, .. } => Some(vec![span.clone()]),
            Error::UnknownCharacter(_, span) => Some(vec![span.clone()]),
            Error::InvalidInteger(_, span) => Some(vec![span.clone()]),
            Error::InvalidFloat(_, span) => Some(vec![span.clone()]),
        }
    }

    pub fn report(&self) -> Report<'_, Span> {
        match self {
            Self::Shrug => Report::build(ReportKind::Error, Span::new("somewhere", 0..0))
                .with_message(self.to_string())
                .finish(),
            Self::ExpectedFound { span, .. } => Report::build(ReportKind::Error, span.clone())
                .with_message(self.to_string())
                .with_label(
                    Label::new(span.clone())
                        .with_message(self.to_string())
                        .with_color(Color::Red),
                )
                .finish(),
            Self::ExpectedOneOf { span, .. } => Report::build(ReportKind::Error, span.clone())
                .with_message(self.to_string())
                .with_label(
                    Label::new(span.clone())
                        .with_message(self.to_string())
                        .with_color(Color::Red),
                )
                .finish(),
            Self::UnknownCharacter(_, span) => Report::build(ReportKind::Error, span.clone())
                .with_message(self.to_string())
                .with_label(
                    Label::new(span.clone())
                        .with_message(self.to_string())
                        .with_color(Color::Red),
                )
                .finish(),
            Self::InvalidInteger(_, span) => Report::build(ReportKind::Error, span.clone())
                .with_message(self.to_string())
                .with_label(
                    Label::new(span.clone())
                        .with_message(self.to_string())
                        .with_color(Color::Red),
                )
                .finish(),
            Self::InvalidFloat(_, span) => Report::build(ReportKind::Error, span.clone())
                .with_message(self.to_string())
                .with_label(
                    Label::new(span.clone())
                        .with_message(self.to_string())
                        .with_color(Color::Red),
                )
                .finish(),
            _ => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct OneOf<T>(pub Vec<T>);

impl<T> Display for OneOf<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl<T> Deref for OneOf<T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for OneOf<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> IntoIterator for OneOf<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T: std::hash::Hash + PartialEq + Clone + Eq> OneOf<T> {
    pub fn dedup(self) -> Self {
        OneOf(
            std::collections::HashSet::<T>::from_iter(self.0)
                .into_iter()
                .collect::<Vec<_>>(),
        )
    }
}

impl std::ops::Add for Error {
    type Output = Error;

    #[rustfmt::skip]
    fn add(self, rhs: Self) -> Self::Output {
        if let Some(s1) = self.span()
            && let Some(s2) = rhs.span()
            && s1.source == s2.source
        {
            match s1.range.end.cmp(&s2.range.end) {
                Ordering::Less => rhs,
                Ordering::Greater => self,
                Ordering::Equal => match (self, rhs) {
                    (Error::ExpectedOneOf { mut expected, found, span }, Error::ExpectedOneOf { expected: expected2, .. }) => {
                        expected.extend(expected2);
                        Error::ExpectedOneOf { expected: expected.dedup(), found, span }
                    }
                    (Error::ExpectedOneOf { mut expected, found, span }, Error::ExpectedFound { expected: expected2, .. }) => {
                        expected.push(expected2);
                        Error::ExpectedOneOf { expected: expected.dedup(), found, span }
                    }
                    (Error::ExpectedFound { expected, found, span }, Error::ExpectedOneOf { expected: expected2, .. }) => {
                        let mut expected = vec![expected];
                        expected.extend(expected2);
                        Error::ExpectedOneOf { expected: OneOf(expected).dedup(), found, span }
                    }
                    (Error::ExpectedFound { expected: expected1, found, span }, Error::ExpectedFound { expected: expected2, .. }) => {
                        let expected = vec![expected1, expected2];
                        Error::ExpectedOneOf { expected: OneOf(expected).dedup(), found, span }
                    }
                    (i, _) => i,
                },
            }
        } else if self.span().is_some() {
            self
        } else {
            rhs
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Span {
    pub source: &'static str,
    pub range: Range<usize>,
}

impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.source != other.source {
            return None;
        }
        Some(
            self.range
                .end
                .cmp(&other.range.end)
                .then_with(|| self.range.start.cmp(&other.range.start)),
        )
    }
}

impl Span {
    pub fn new(source: &'static str, range: Range<usize>) -> Self {
        Span { source, range }
    }

    pub fn extend_to(&self, to: usize) -> Self {
        Span {
            source: self.source,
            range: self.range.start.min(to)..self.range.end.max(to),
        }
    }

    pub fn begin(self) -> Self {
        Span {
            source: self.source,
            range: self.range.start..self.range.start,
        }
    }
}

impl std::ops::Add<Span> for Span {
    type Output = Span;

    fn add(self, rhs: Span) -> Self::Output {
        assert_eq!(self.source, rhs.source);
        Span {
            source: self.source,
            range: self.range.start.min(rhs.range.start)..self.range.end.max(rhs.range.end),
        }
    }
}

impl<'a> From<&'a Lexer<'a, Token>> for Span {
    fn from(lex: &'a Lexer<Token>) -> Self {
        Span::new(lex.extras, lex.span())
    }
}

impl std::ops::Add<Span> for &Span {
    type Output = Span;

    fn add(self, rhs: Span) -> Self::Output {
        assert_eq!(self.source, rhs.source);
        Span {
            source: self.source,
            range: self.range.start.min(rhs.range.start)..self.range.end.max(rhs.range.end),
        }
    }
}

impl<T> std::ops::Add<T> for &Span
where
    T: AsRef<Span>,
{
    type Output = Span;

    fn add(self, rhs: T) -> Self::Output {
        assert_eq!(self.source, rhs.as_ref().source);
        Span {
            source: self.source,
            range: self.range.start.min(rhs.as_ref().range.start)
                ..self.range.end.max(rhs.as_ref().range.end),
        }
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}..{}",
            self.source, self.range.start, self.range.end
        )
    }
}

impl ariadne::Span for Span {
    type SourceId = &'static str;

    fn source(&self) -> &Self::SourceId {
        &self.source
    }

    fn end(&self) -> usize {
        self.range.end
    }

    fn start(&self) -> usize {
        self.range.start
    }
}
