use std::{fs::File, io::Read, panic::Location};

use ariadne::{Cache, Color, Label, Report, ReportKind};

use crate::{
    Span,
    lexer::{Keyword, Symbol, Token, to_opener},
};

#[derive(Debug, Clone, PartialEq)]
#[repr(u16)]
pub enum Error<'a> {
    UnknownStringEscapeSequence {
        location: Span,
        found: Found<'a>,
        known_sequences: &'static [(&'static str, char)],
    } = 0,
    UntrerminatedString(Span) = 1,
    InvalidNumber {
        location: Span,
        found: Found<'a>,
    } = 2,
    InvalidIdentifier {
        location: Span,
        found: Found<'a>,
    } = 3,
    UnknownSymbol {
        location: Span,
        found: Found<'a>,
        known_symbols: &'static [(&'static str, Symbol)],
    } = 4,
    UnclosedDelimiter {
        open: Span,
        kind: char,
    } = 5,
    UnopenedDelimiter {
        location: Span,
        closer: char,
    } = 6,
    MismatchedDelimiter {
        mismatch: Span,
        opened: Span,
        expected: char,
        found: char,
    } = 7,
    ExpectedFound {
        location: Span,
        expected: Expected,
        found: Found<'a>,
    } = 8,
    Internal(&'static Location<'static>) = 9,
    InternalComment(&'static Location<'static>, &'static str) = 10,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Found<'a> {
    Slice(&'a str),
    Eof,
    Token(Token),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expected {
    Eof,
    Literal,
    Tuple,
    List,
    Block,
    Identifier,
    Expression,
    Type,
    Symbol(Symbol),
    Keyword(Keyword),
}

impl<'a> Error<'a> {
    #[track_caller]
    pub fn internal() -> Error<'a> {
        Error::Internal(Location::caller())
    }

    pub fn report(self) -> Report<'a, Span> {
        let code = self.code();
        match self {
            Self::UntrerminatedString(span) => Self::report_unterminated_string(code, span),
            Self::UnknownSymbol {
                location,
                found,
                known_symbols,
            } => Self::report_unknown_symbol(code, location, found, known_symbols),
            Self::UnopenedDelimiter { location, closer } => {
                Self::report_unopened_delimiter(code, location, closer)
            }
            Self::MismatchedDelimiter {
                mismatch,
                opened,
                expected,
                found,
            } => Self::report_mismatched_delimiter(code, mismatch, opened, expected, found),
            Self::UnclosedDelimiter { open, kind } => {
                Self::report_unclosed_delimiter(code, open, kind)
            }
            Self::ExpectedFound {
                location,
                expected,
                found,
            } => Self::report_expected_found(code, location, expected, found),
            _ => todo!("{:?}", self),
        }
    }

    fn report_expected_found(
        code: u16,
        location: Span,
        expected: Expected,
        found: Found<'a>,
    ) -> Report<'a, Span> {
        Report::build(ReportKind::Error, location.clone())
            .with_message(format!("Expected {expected} but found {found}"))
            .with_code(code)
            .with_label(
                Label::new(location)
                    .with_color(Color::Red)
                    .with_message(format!("Expected {expected} but found {found}")),
            )
            .finish()
    }

    fn report_unclosed_delimiter(code: u16, open: Span, kind: char) -> Report<'a, Span> {
        Report::build(ReportKind::Error, open.extend_to(usize::MAX))
            .with_message(format!("Delimiter {kind:?} was not closed"))
            .with_code(code)
            .with_label(
                Label::new(open)
                    .with_message("Opened here")
                    .with_color(Color::Blue),
            )
            .finish()
    }

    fn report_mismatched_delimiter(
        code: u16,
        mismatch: Span,
        opened: Span,
        expected: char,
        found: char,
    ) -> Report<'a, Span> {
        Report::build(ReportKind::Error, mismatch.clone())
            .with_message(format!(
                "Expected closer {expected:?} but found closer {found:?}"
            ))
            .with_code(code)
            .with_label(
                Label::new(mismatch)
                    .with_message(format!("Expected {expected:?}"))
                    .with_color(Color::Red)
                    .with_order(0),
            )
            .with_label(
                Label::new(opened)
                    .with_message("Delimiter opened here")
                    .with_color(Color::Blue)
                    .with_order(1),
            )
            .with_help("Close the inner delimiter first")
            .finish()
    }

    fn report_unopened_delimiter(code: u16, location: Span, closer: char) -> Report<'a, Span> {
        Report::build(ReportKind::Error, location.clone())
            .with_message(format!(
                "Tried closing delimiter {:?} that was not previously opened",
                to_opener(closer)
            ))
            .with_label(
                Label::new(location)
                    .with_message("Unexpected Closer")
                    .with_color(Color::Red),
            )
            .with_help(format!("Remove the closing delimiter {closer:?}"))
            .with_code(code)
            .finish()
    }

    fn report_unknown_symbol(
        code: u16,
        location: Span,
        _: Found<'a>,
        _: &'static [(&'static str, Symbol)],
    ) -> Report<'a, Span> {
        Report::build(ReportKind::Error, location.clone())
            .with_message("Unknown Symbol")
            .with_label(
                Label::new(location)
                    .with_message("Unknown Symbol")
                    .with_color(Color::Red),
            )
            .with_code(code)
            .finish()
    }

    fn report_unterminated_string(code: u16, location: Span) -> Report<'a, Span> {
        Report::build(ReportKind::Error, location.clone())
            .with_label(
                Label::new(location)
                    .with_message("Unterminated String")
                    .with_color(Color::Red),
            )
            .with_message("Unterminated String")
            .with_code(code)
            .finish()
    }

    pub fn code(&self) -> u16 {
        // SAFETY: Because `Self` is marked `repr(u16)`, its layout is a `repr(C)` `union`
        // between `repr(C)` structs, each of which has the `u16` discriminant as its first
        // field, so we can read the discriminant without offsetting the pointer.
        unsafe { *<*const _>::from(self).cast::<u16>() }
    }

    pub fn sources(&self) -> Vec<&'static str> {
        match self {
            Self::UntrerminatedString(location)
            | Self::InvalidIdentifier { location, .. }
            | Self::InvalidNumber { location, .. }
            | Self::UnclosedDelimiter { open: location, .. }
            | Self::UnopenedDelimiter { location, .. }
            | Self::UnknownStringEscapeSequence { location, .. }
            | Self::UnknownSymbol { location, .. }
            | Self::ExpectedFound { location, .. } => vec![location.source],
            Self::MismatchedDelimiter {
                mismatch, opened, ..
            } => vec![mismatch.source, opened.source],
            Self::Internal(_) | Self::InternalComment(..) => Vec::new(),
        }
    }

    pub fn map_span(self, mut f: impl FnMut(Span) -> Span) -> Self {
        match self {
            Self::UnknownStringEscapeSequence {
                location,
                found,
                known_sequences,
            } => Self::UnknownStringEscapeSequence {
                location: f(location),
                found,
                known_sequences,
            },
            Self::UntrerminatedString(location) => Self::UntrerminatedString(f(location)),
            Self::InvalidNumber { location, found } => Self::InvalidNumber {
                location: f(location),
                found,
            },
            Self::InvalidIdentifier { location, found } => Self::InvalidIdentifier {
                location: f(location),
                found,
            },
            Self::UnknownSymbol {
                location,
                found,
                known_symbols,
            } => Self::UnknownSymbol {
                location: f(location),
                found,
                known_symbols,
            },
            Self::UnclosedDelimiter { open, kind } => Self::UnclosedDelimiter {
                open: f(open),
                kind,
            },
            Self::UnopenedDelimiter { location, closer } => Self::UnopenedDelimiter {
                location: f(location),
                closer,
            },
            Self::MismatchedDelimiter {
                mismatch,
                opened,
                expected,
                found,
            } => Self::MismatchedDelimiter {
                mismatch: f(mismatch),
                opened: f(opened),
                expected,
                found,
            },
            Self::ExpectedFound {
                location,
                expected,
                found,
            } => Self::ExpectedFound {
                location: f(location),
                expected,
                found,
            },
            Self::Internal(loc) => Self::Internal(loc),
            Self::InternalComment(loc, comment) => Self::InternalComment(loc, comment),
        }
    }

    pub fn cache(&self) -> impl Cache<&'static str> + use<> {
        ariadne::sources(self.sources().into_iter().map(|source| {
            let mut buf = Vec::new();
            File::open(source).unwrap().read_to_end(&mut buf).unwrap();
            (source, String::from_utf8(buf).unwrap())
        }))
    }
}

impl<'a> std::fmt::Display for Found<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eof => write!(f, "End of File"),
            Self::Slice(slice) => write!(f, "{slice:?}"),
            Self::Token(token) => write!(f, "{token}"),
        }
    }
}

impl std::fmt::Display for Expected {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expected::Eof => write!(f, "End of File"),
            Expected::Literal => write!(f, "a Literal"),
            Expected::Tuple => write!(f, "a Tuple"),
            Expected::List => write!(f, "a List"),
            Expected::Block => write!(f, "a Block"),
            Expected::Symbol(symbol) => write!(f, "{symbol}"),
            Expected::Identifier => write!(f, "an Identifier"),
            Expected::Keyword(keyword) => write!(f, "{keyword}"),
            Expected::Expression => write!(f, "an Expression"),
            Expected::Type => write!(f, "a Type"),
        }
    }
}
