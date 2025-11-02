use ariadne::{Label, Report, ReportKind};
use logos::Lexer;
use std::{
    cmp::Ordering,
    fmt::Display,
    ops::{Deref, DerefMut, Range},
};
use thiserror::Error;
use yansi::{Color, Paint};

pub mod extract;

use crate::parser::{lexer::Token, lib::ParseContext};

pub const CORRECT: Color = Color::Green;
pub const INFO: Color = Color::Yellow;
pub const WARNING: Color = Color::Magenta;
pub const ERROR: Color = Color::Red;

#[derive(Debug, Error, Clone, PartialEq, Default)]
pub enum Error {
    #[default]
    #[error("{}", rainbow(r"¯\_(ツ)_/¯"))]
    Shrug,
    #[error("Expected {} found {}", expected.to_string().paint(CORRECT), found.to_string().paint(ERROR))]
    ExpectedFound {
        expected: Expected,
        found: Found,
        span: Span,
    },
    #[error("Expected one of {} found {}", expected.to_string().paint(CORRECT), found.to_string().paint(ERROR))]
    ExpectedOneOf {
        expected: OneOf<Expected>,
        found: Found,
        span: Span,
    },
    #[error("Unknown Character {}", format!("'{}'", .0).paint(ERROR))]
    UnknownCharacter(char, Span),
    #[error("Invalid Integer {}", format!("'{}'", .0).paint(ERROR))]
    InvalidInteger(String, Span),
    #[error("Invalid Float {}", format!("'{}'", .0).paint(ERROR))]
    InvalidFloat(String, Span),
    #[error("Found {}, expected {}", format!("{}", .found).paint(ERROR), .expected.to_string().paint(CORRECT))]
    FoundExpected {
        expected: Expected,
        opened: Span,
        opened_token: Token,
        found: Token,
        found_span: Span,
    },
    #[error("Unclosed {}", format!("{}", .0).paint(ERROR))]
    Unclosed(Token, Span),
    #[error("Mismatched closing delimiter, expected {} found {}", expected.to_string().paint(CORRECT), found.to_string().paint(ERROR))]
    MismatchedClosing {
        expected: Expected,
        opened: Span,
        opened_token: Token,
        found: Token,
        found_span: Span,
        matched: Token,
        matched_span: Span,
    },
}

#[must_use = "Pure function, non use calls should be removed"]
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
    #[error("'|'")]
    Pipe,
    #[error("'def'")]
    Def,
    #[error("';'")]
    Semicolon,
    #[error("'+'")]
    Plus,
    #[error("':'")]
    Colon,
    #[error("'&'")]
    Anpersand,
    #[error("'$'")]
    Dollar,
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
            Token::Pipe => Expected::Pipe,
            Token::Def => Expected::Def,
            Token::Semicolon => Expected::Semicolon,
            Token::Plus => Expected::Plus,
            Token::Colon => Expected::Colon,
            Token::Anpersand => Expected::Anpersand,
            Token::Dollar => Expected::Dollar,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub item: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn into_inner(self) -> T {
        self.item
    }

    pub fn map<O>(self, f: impl FnOnce(T) -> O) -> Spanned<O> {
        Spanned {
            item: f(self.item),
            span: self.span,
        }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.item
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.item
    }
}

impl From<Token> for Found {
    fn from(token: Token) -> Self {
        Found::Token(token)
    }
}

impl Error {
    #[must_use = "Pure function, non use calls should be removed"]
    pub fn from_lexer(lex: &Lexer<Token>) -> Self {
        let span = Span::new(lex.extras, lex.span());
        match lex.slice().chars().next() {
            Some(c) => Error::UnknownCharacter(c, span),
            None => Error::Shrug,
        }
    }

    #[must_use = "Pure function, non use calls should be removed"]
    pub fn exp_found(expected: Expected, found: Token, px: &ParseContext) -> Self {
        Error::ExpectedFound {
            expected,
            found: Found::from(found),
            span: Span::new(px.extras(), px.span()),
        }
    }

    #[must_use = "Pure function, non use calls should be removed"]
    pub fn span(&self) -> Option<Span> {
        match self {
            Error::Shrug => None,
            Error::ExpectedFound { span, .. }
            | Error::ExpectedOneOf { span, .. }
            | Error::UnknownCharacter(_, span)
            | Error::InvalidInteger(_, span)
            | Error::InvalidFloat(_, span)
            | Error::FoundExpected {
                found_span: span, ..
            }
            | Error::Unclosed(_, span)
            | Error::MismatchedClosing {
                found_span: span, ..
            } => Some(span),
        }
        .cloned()
    }

    #[must_use = "Pure function, non use calls should be removed"]
    pub fn sources(&self) -> Option<Vec<Span>> {
        match self {
            Error::Shrug => None,
            Error::ExpectedFound { span, .. }
            | Error::ExpectedOneOf { span, .. }
            | Error::UnknownCharacter(_, span)
            | Error::InvalidInteger(_, span)
            | Error::InvalidFloat(_, span)
            | Error::Unclosed(_, span) => Some(vec![span.clone()]),
            Error::FoundExpected {
                opened, found_span, ..
            } => Some(vec![opened.clone(), found_span.clone()]),
            Error::MismatchedClosing {
                opened,
                found_span,
                matched_span,
                ..
            } => Some(vec![
                opened.clone(),
                found_span.clone(),
                matched_span.clone(),
            ]),
        }
    }

    #[must_use = "Pure function, non use calls should be removed"]
    pub fn report(&self) -> Report<'_, Span> {
        match self {
            Self::Shrug => Report::build(ReportKind::Error, Span::new("somewhere", 0..0))
                .with_message(self.to_string())
                .finish(),
            Self::ExpectedFound { span, .. }
            | Self::ExpectedOneOf { span, .. }
            | Self::UnknownCharacter(_, span)
            | Self::InvalidInteger(_, span)
            | Self::Unclosed(_, span)
            | Self::InvalidFloat(_, span) => Report::build(ReportKind::Error, span.clone())
                .with_message(self.to_string())
                .with_label(
                    Label::new(span.clone())
                        .with_message(self.to_string())
                        .with_color(ERROR),
                )
                .finish(),
            Self::FoundExpected {
                expected,
                opened,
                opened_token,
                found,
                found_span,
            } => Report::build(ReportKind::Error, found_span.clone())
                .with_message(self.to_string())
                .with_label(
                    Label::new(found_span.clone())
                        .with_message(format!(
                            "found {}, expected {}",
                            found.to_string().paint(ERROR),
                            expected.to_string().paint(CORRECT)
                        ))
                        .with_order(0)
                        .with_color(ERROR),
                )
                .with_label(
                    Label::new(opened.clone())
                        .with_message(format!(
                            "opened here with {}",
                            opened_token.to_string().paint(INFO)
                        ))
                        .with_order(1)
                        .with_color(INFO),
                )
                .finish(),
            Self::MismatchedClosing {
                expected,
                opened,
                opened_token,
                found,
                found_span,
                matched,
                matched_span,
            } => Report::build(ReportKind::Error, found_span.clone())
                .with_message(self.to_string())
                .with_label(
                    Label::new(found_span.clone())
                        .with_message(format!(
                            "found {}, expected {}",
                            found.to_string().paint(ERROR),
                            expected.to_string().paint(CORRECT)
                        ))
                        .with_order(0)
                        .with_color(ERROR),
                )
                .with_label(
                    Label::new(opened.clone())
                        .with_message(format!(
                            "opened here with {}",
                            opened_token.to_string().paint(INFO)
                        ))
                        .with_order(1)
                        .with_color(INFO),
                )
                .with_label(
                    Label::new(matched_span.clone())
                        .with_message(format!(
                            "but {} matches here",
                            matched.to_string().paint(WARNING)
                        ))
                        .with_order(2)
                        .with_color(WARNING),
                )
                .finish(),
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
                .map(T::to_string)
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
    #[must_use = "Takes self by value and returns a new deduplicated instance"]
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
    #[must_use = "Pure function, non use calls should be removed"]
    pub fn new(source: &'static str, range: Range<usize>) -> Self {
        Span { source, range }
    }

    #[must_use = "Returns a new Span"]
    pub fn extend_to(&self, to: usize) -> Self {
        Span {
            source: self.source,
            range: self.range.start.min(to)..self.range.end.max(to),
        }
    }

    #[must_use = "Returns a new Span"]
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
