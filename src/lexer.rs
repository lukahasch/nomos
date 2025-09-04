use crate::{Error, Span, error::Found};
use std::{str::FromStr, sync::Arc};

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub lexeme: Lexeme,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lexeme {
    String(Arc<str>),
    Integer(i64),
    Float(f64),
    Symbol(Symbol),
    Keyword(Keyword),
    Identifier(Arc<str>),
    Group(Group),
    Comment { comment: Arc<str>, doc: bool },
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Symbol {
    Plus,
    Minus,
    Equal,
    Equality,
    Lesser,
    Greater,
    Lambda,
    Arrow,
    FatArrow,
    Colon,
    Comma,
    Multiply,
    Divide,
}

impl Symbol {
    pub const SYMBOLS: &[(&str, Symbol)] = &[
        ("\\", Symbol::Lambda),
        ("->", Symbol::Arrow),
        ("=>", Symbol::FatArrow),
        ("==", Symbol::Equality),
        (">", Symbol::Greater),
        ("<", Symbol::Lesser),
        ("+", Symbol::Plus),
        ("-", Symbol::Minus),
        ("=", Symbol::Equal),
        (":", Symbol::Colon),
        (",", Symbol::Comma),
        ("*", Symbol::Multiply),
        ("/", Symbol::Divide),
    ];
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Keyword {
    Let,
    In,
    If,
    Then,
    Else,
    True,
    False,
}

impl Keyword {
    pub const KEYWORDS: &[(&str, Keyword)] = &[
        ("let", Keyword::Let),
        ("in", Keyword::In),
        ("if", Keyword::If),
        ("then", Keyword::Then),
        ("else", Keyword::Else),
        ("true", Keyword::True),
        ("false", Keyword::False),
    ];
}

/// TODO: smart cache friendly layout
#[derive(Debug, Clone, PartialEq)]
pub enum Group {
    Parentheses(Vec<Token>),
    Brackets(Vec<Token>),
    Braces(Vec<Token>),
}

pub const KNOWN_SEQUENCES: [(&str, char); 3] = [("n", '\n'), ("r", '\r'), ("\"", '"')];
pub const SYMBOLS: &str = ";,:.-+*/!#?&%$=<>'@\\";
pub const OPENERS: &str = "([{";
pub const CLOSERS: &str = ")]}";

pub fn to_opener(c: char) -> char {
    OPENERS.chars().nth(CLOSERS.find(c).unwrap()).unwrap()
}

pub fn to_closer(c: char) -> char {
    CLOSERS.chars().nth(OPENERS.find(c).unwrap()).unwrap()
}

pub struct Context<'a> {
    source: &'static str,
    contents: &'a str,
    index: usize,
    errors: Vec<Error<'a>>,
    output: Vec<(Span, char, Vec<Token>)>,
}

impl<'a> Context<'a> {
    pub fn new(source: &'static str, contents: &'a str) -> Self {
        Self {
            source,
            contents,
            index: 0,
            errors: Vec::new(),
            output: vec![(
                Span {
                    source,
                    range: 0..0,
                },
                '\0',
                Vec::new(),
            )],
        }
    }

    fn push(&mut self, token: Token) {
        self.output.last_mut().unwrap().2.push(token);
    }

    pub fn lex(mut self) -> Result<Vec<Token>, Vec<Error<'a>>> {
        while let Some(c) = self.peek() {
            let out = match c {
                ' ' | '\t' | '\n' => {
                    self.advance();
                    continue;
                }
                '"' => self.string(),
                '/' if self.peek_two() == Some('/') => self.single_line_comment(),
                n if n.is_ascii_digit() => self.number(),
                s if SYMBOLS.contains(s) => self.symbol(),
                o if OPENERS.contains(o) => self.open(),
                c if CLOSERS.contains(c) => self.close(),
                _ => self.keyword_or_identifier(),
            };
            match out {
                Ok(Some(token)) => self.push(token),
                Ok(None) => continue,
                Err(err) => {
                    self.errors.push(err);
                    return Err(self.errors);
                }
            }
        }
        match (self.output.pop().unwrap(), self.errors.is_empty()) {
            ((_, '\0', output), true) => Ok(output),
            ((_, '\0', _), false) => Err(self.errors),
            ((open, kind, _), _) => {
                self.errors.push(Error::UnclosedDelimiter { open, kind });
                Err(self.errors)
            }
        }
    }

    fn single_line_comment(&mut self) -> Result<Option<Token>, Error<'a>> {
        let begin = self.index;
        self.index += "//".len();
        let mut doc = false;
        if self.peek() == Some('/') {
            self.advance();
            doc = true;
        }
        let comment = self.take_while(|c| c != '\n');
        Ok(Some(Token {
            lexeme: Lexeme::Comment {
                comment: Arc::from(comment),
                doc,
            },
            span: self.span().extend_to(begin),
        }))
    }

    fn keyword_or_identifier(&mut self) -> Result<Option<Token>, Error<'a>> {
        let index = self.index;
        if let Some((pattern, keyword)) = Keyword::KEYWORDS
            .iter()
            .find(|(pattern, _)| self.contents[self.index..].starts_with(pattern))
        {
            self.index += pattern.len();
            Ok(Some(Token {
                lexeme: Lexeme::Keyword(*keyword),
                span: self.span().extend_to(index),
            }))
        } else {
            let identifier = self.take_while(|c| c.is_alphanumeric() || c == '_');
            if identifier.is_empty() {
                let found = self.found(self.index);
                self.errors.push(Error::InvalidIdentifier {
                    location: self.span().extend_to(index),
                    found,
                });
                self.advance();
            }
            Ok(Some(Token {
                lexeme: Lexeme::Identifier(Arc::from(identifier)),
                span: self.span().extend_to(index),
            }))
        }
    }

    fn open(&mut self) -> Result<Option<Token>, Error<'a>> {
        let location = self.char_span();
        let o = self.advance().unwrap();
        self.output.push((location, o, Vec::new()));
        Ok(None)
    }

    fn close(&mut self) -> Result<Option<Token>, Error<'a>> {
        let location = self.char_span();
        let closer = self.advance().unwrap();
        let (span, opener, contents) = match self.output.pop().unwrap() {
            (span, '\0', contents) => {
                self.errors
                    .push(Error::UnopenedDelimiter { location, closer });
                self.output.push((span, '\0', contents));
                return Ok(None);
            }
            group => group,
        };
        if to_opener(closer) == opener {
            Ok(Some(Token {
                lexeme: Lexeme::Group(Group::new(opener, contents)),
                span: span + location,
            }))
        } else {
            self.closer_error(closer, location, (span, opener, contents))
        }
    }

    fn closer_error(
        &mut self,
        closer: char,
        location: Span,
        (span, opener, contents): (Span, char, Vec<Token>),
    ) -> Result<Option<Token>, Error<'a>> {
        if self
            .output
            .iter()
            .any(|(_, opener, _)| *opener == to_opener(closer))
        {
            self.errors.push(Error::MismatchedDelimiter {
                mismatch: location.clone(),
                opened: span.clone(),
                expected: to_closer(opener),
                found: closer,
            });
            Ok(Some(Token {
                lexeme: Lexeme::Group(Group::new(opener, contents)),
                span: span + location,
            }))
        } else {
            self.errors
                .push(Error::UnopenedDelimiter { location, closer });
            self.output.push((span, opener, contents));
            Ok(None)
        }
    }

    fn symbol(&mut self) -> Result<Option<Token>, Error<'a>> {
        let found = Symbol::SYMBOLS
            .iter()
            .find(|(pattern, _)| self.contents[self.index..].starts_with(pattern));
        match found {
            Some((pattern, symbol)) => {
                let index = self.index;
                self.index += pattern.len();
                Ok(Some(Token {
                    lexeme: Lexeme::Symbol(*symbol),
                    span: self.span().extend_to(index),
                }))
            }
            None => {
                let found = self.found(self.index);
                self.errors.push(Error::UnknownSymbol {
                    location: self.char_span(),
                    found,
                    known_symbols: Symbol::SYMBOLS,
                });
                self.index += self.peek().unwrap().len_utf8();
                Ok(None)
            }
        }
    }

    fn number(&mut self) -> Result<Option<Token>, Error<'a>> {
        if self.contents[self.index..].starts_with("0x") {
            self.lex_hexadecimal()
        } else if self.contents[self.index..].starts_with("0b") {
            self.lex_binary()
        } else {
            self.lex_decimal()
        }
    }

    fn lex_hexadecimal(&mut self) -> Result<Option<Token>, Error<'a>> {
        todo!()
    }

    fn lex_binary(&mut self) -> Result<Option<Token>, Error<'a>> {
        let begin = self.index;
        self.index += "0b".len();
        let number = self.take_while(|c| c == '0' || c == '1');
        let mut output: i64 = 0;
        for (i, c) in number.chars().rev().enumerate() {
            match c {
                '0' => {}
                '1' => output += i64::pow(2, i as u32),
                _ => unreachable!(),
            }
        }
        Ok(Some(Token {
            lexeme: Lexeme::Integer(output),
            span: self.span().extend_to(begin),
        }))
    }

    fn lex_decimal(&mut self) -> Result<Option<Token>, Error<'a>> {
        let begin = self.index;
        let number = self.take_while(|c| c.is_ascii_digit() || c == '.' || c == '-' || c == 'e');
        let lexeme = match <i64 as FromStr>::from_str(number) {
            Ok(integer) => Lexeme::Integer(integer),
            Err(_) => match <f64 as FromStr>::from_str(number) {
                Ok(float) => Lexeme::Float(float),
                Err(_) => {
                    self.errors.push(Error::InvalidNumber {
                        location: self.span().extend_to(begin),
                        found: Found::Slice(number),
                    });
                    return Ok(None);
                }
            },
        };
        Ok(Some(Token {
            lexeme,
            span: self.span().extend_to(begin),
        }))
    }

    fn string(&mut self) -> Result<Option<Token>, Error<'a>> {
        let has_errored = self.check_error();
        let start = self.index;
        self.index += '"'.len_utf8();
        let mut string = String::new();
        while let Some(c) = self.advance() {
            match c {
                '"' => {
                    return Ok(if !has_errored(self) {
                        Some(Token {
                            lexeme: Lexeme::String(Arc::from(string)),
                            span: self.span().extend_to(start),
                        })
                    } else {
                        None
                    });
                }
                '\\' => self.parse_sequence(&mut string),
                _ => string.push(c),
            }
        }
        Err(Error::UntrerminatedString(self.span().extend_to(start)))
    }

    fn check_error(&self) -> impl Fn(&Self) -> bool + use<'a> {
        let len = self.errors.len();
        move |ctx| ctx.errors.len() != len
    }

    fn parse_sequence(&mut self, string: &mut String) {
        let sequence = KNOWN_SEQUENCES
            .iter()
            .find(|(pattern, _)| self.contents[self.index..].starts_with(pattern));
        match sequence {
            Some((pattern, c)) => {
                self.index += pattern.len();
                string.push(*c);
            }
            None => {
                let found = self.found(self.index - '\\'.len_utf8());
                self.errors.push(Error::UnknownStringEscapeSequence {
                    location: self.span(),
                    found,
                    known_sequences: &KNOWN_SEQUENCES,
                });
            }
        }
    }

    fn found(&mut self, index: usize) -> Found<'a> {
        let swap = self.swap(index);
        #[derive(Debug, Clone, PartialEq)]
        enum Kind {
            Symbol,
            Identifier,
            Number,
            Shrug,
        }
        impl Kind {
            pub fn new(c: char) -> Self {
                if SYMBOLS.contains(c) {
                    Self::Symbol
                } else if c.is_alphanumeric() {
                    Self::Number
                } else if c.is_alphabetic() || c == '_' {
                    Self::Identifier
                } else {
                    Self::Shrug
                }
            }
        }
        let kind = Kind::new(match self.peek() {
            Some(c) => c,
            None => return Found::Eof,
        });
        if kind == Kind::Shrug {
            let found = Found::Slice(self.take_chars(3));
            self.swap(swap);
            return found;
        }
        while let Some(c) = self.peek()
            && Kind::new(c) == kind
        {
            self.advance();
        }
        let end = self.swap(swap);
        Found::Slice(&self.contents[index..end])
    }

    fn take_chars(&mut self, count: usize) -> &'a str {
        let swap = self.swap(self.index);
        for _ in 0..count {
            self.advance();
        }
        let end = self.swap(swap);
        &self.contents[swap..end]
    }

    fn swap(&mut self, mut index: usize) -> usize {
        std::mem::swap(&mut self.index, &mut index);
        index
    }

    fn char_span(&self) -> Span {
        Span {
            source: self.source,
            range: self.index
                ..self.index
                    + if let Some(c) = self.peek() {
                        c.len_utf8()
                    } else {
                        0
                    },
        }
    }

    fn span(&self) -> Span {
        Span {
            source: self.source,
            range: self.index..self.index,
        }
    }

    fn peek(&self) -> Option<char> {
        self.contents[self.index..].chars().next()
    }

    fn peek_two(&self) -> Option<char> {
        self.contents[self.index..].chars().nth(1)
    }

    fn advance(&mut self) -> Option<char> {
        self.contents[self.index..]
            .chars()
            .next()
            .inspect(|c| self.index += c.len_utf8())
    }

    fn take_while(&mut self, mut f: impl FnMut(char) -> bool) -> &'a str {
        let index = self.index;
        while let Some(c) = self.peek()
            && f(c)
        {
            self.advance();
        }
        &self.contents[index..self.index]
    }
}

impl Group {
    fn new(kind: char, contents: Vec<Token>) -> Group {
        match kind {
            '(' => Group::Parentheses(contents),
            '[' => Group::Brackets(contents),
            '{' => Group::Braces(contents),
            _ => unreachable!(),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{{{}}}", self.lexeme, self.span)
    }
}

impl std::fmt::Display for Lexeme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float(float) => write!(f, "Float({float})"),
            Self::Group(group) => group.fmt(f),
            Self::Identifier(ident) => write!(f, "Identifier({ident:?})"),
            Self::Integer(int) => write!(f, "Integer({int})"),
            Self::Keyword(keyword) => write!(f, "Keyword({keyword})"),
            Self::String(string) => write!(f, "String({string:?})"),
            Self::Symbol(symbol) => write!(f, "Symbol({symbol})"),
            Self::Comment { comment, doc } => write!(f, "Comment({comment:?}, doc: {doc})"),
        }
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?}",
            Symbol::SYMBOLS
                .iter()
                .find_map(|(pattern, symbol)| if symbol == self { Some(pattern) } else { None })
                .unwrap()
        )
    }
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?}",
            Keyword::KEYWORDS
                .iter()
                .find_map(|(pattern, symbol)| if symbol == self { Some(pattern) } else { None })
                .unwrap()
        )
    }
}

impl std::fmt::Display for Group {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content = match self {
            Self::Braces(content) => {
                write!(f, "Braces([")?;
                content
            }
            Self::Brackets(content) => {
                write!(f, "Brackets([")?;
                content
            }
            Self::Parentheses(content) => {
                write!(f, "Parentheses([")?;
                content
            }
        };
        for (index, token) in content.iter().enumerate() {
            write!(
                f,
                "{token}{}",
                if index < content.len() - 1 { ", " } else { "" }
            )?;
        }
        write!(f, "])")
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_string() {
        let input = r#""hello \r\"\n""#;
        let source = "<< test >>";
        let lexed = Context::new(source, input).lex();
        assert_eq!(
            Ok(vec![Token {
                span: Span::new(source, 0..input.len()),
                lexeme: Lexeme::String(Arc::from("hello \r\"\n"))
            }]),
            lexed
        )
    }

    #[test]
    fn test_symbol() {
        let input = "+=->==";
        let source = "<< test >>";
        let lexed = Context::new(source, input).lex();
        assert_eq!(
            Ok(vec![
                Token {
                    span: Span::new(source, 0..1),
                    lexeme: Lexeme::Symbol(Symbol::Plus)
                },
                Token {
                    span: Span::new(source, 1..2),
                    lexeme: Lexeme::Symbol(Symbol::Equal)
                },
                Token {
                    span: Span::new(source, 2..4),
                    lexeme: Lexeme::Symbol(Symbol::Arrow)
                },
                Token {
                    span: Span::new(source, 4..6),
                    lexeme: Lexeme::Symbol(Symbol::Equality)
                }
            ]),
            lexed
        )
    }

    #[test]
    fn test_integer() {
        let input = "42 0 1";
        let source = "<< test >>";
        let lexed = Context::new(source, input).lex();
        assert_eq!(
            Ok(vec![
                Token {
                    lexeme: Lexeme::Integer(42),
                    span: Span {
                        source: "<< test >>",
                        range: 0..2
                    }
                },
                Token {
                    lexeme: Lexeme::Integer(0),
                    span: Span {
                        source: "<< test >>",
                        range: 3..4
                    }
                },
                Token {
                    lexeme: Lexeme::Integer(1),
                    span: Span {
                        source: "<< test >>",
                        range: 5..6
                    }
                }
            ]),
            lexed
        )
    }

    #[test]
    #[allow(clippy::approx_constant)]
    fn test_float() {
        let input = "3.14 0.5 -2.718";
        let source = "<< test >>";
        let lexed = Context::new(source, input).lex();
        assert_eq!(
            Ok(vec![
                Token {
                    lexeme: Lexeme::Float(3.14),
                    span: Span {
                        source: "<< test >>",
                        range: 0..4
                    }
                },
                Token {
                    lexeme: Lexeme::Float(0.5),
                    span: Span {
                        source: "<< test >>",
                        range: 5..8
                    }
                },
                Token {
                    lexeme: Lexeme::Symbol(Symbol::Minus),
                    span: Span {
                        source: "<< test >>",
                        range: 9..10
                    }
                },
                Token {
                    lexeme: Lexeme::Float(2.718),
                    span: Span {
                        source: "<< test >>",
                        range: 10..15
                    }
                }
            ]),
            lexed
        )
    }

    #[test]
    fn test_keyword() {
        let input = "let if then else true false in";
        let source = "<< test >>";
        let lexed = Context::new(source, input).lex();
        assert_eq!(
            Ok(vec![
                Token {
                    span: Span::new(source, 0..3),
                    lexeme: Lexeme::Keyword(Keyword::Let)
                },
                Token {
                    span: Span::new(source, 4..6),
                    lexeme: Lexeme::Keyword(Keyword::If)
                },
                Token {
                    span: Span::new(source, 7..11),
                    lexeme: Lexeme::Keyword(Keyword::Then)
                },
                Token {
                    span: Span::new(source, 12..16),
                    lexeme: Lexeme::Keyword(Keyword::Else)
                },
                Token {
                    span: Span::new(source, 17..21),
                    lexeme: Lexeme::Keyword(Keyword::True)
                },
                Token {
                    span: Span::new(source, 22..27),
                    lexeme: Lexeme::Keyword(Keyword::False)
                },
                Token {
                    span: Span::new(source, 28..30),
                    lexeme: Lexeme::Keyword(Keyword::In)
                }
            ]),
            lexed
        )
    }

    #[test]
    fn test_identifier() {
        let input = "x foo_bar baz123";
        let source = "<< test >>";
        let lexed = Context::new(source, input).lex();
        assert_eq!(
            Ok(vec![
                Token {
                    span: Span::new(source, 0..1),
                    lexeme: Lexeme::Identifier(Arc::from("x"))
                },
                Token {
                    span: Span::new(source, 2..9),
                    lexeme: Lexeme::Identifier(Arc::from("foo_bar"))
                },
                Token {
                    span: Span::new(source, 10..16),
                    lexeme: Lexeme::Identifier(Arc::from("baz123"))
                }
            ]),
            lexed
        )
    }

    #[test]
    fn test_comment() {
        let input = "// regular comment\n/// doc comment";
        let source = "<< test >>";
        let lexed = Context::new(source, input).lex();
        assert_eq!(
            Ok(vec![
                Token {
                    lexeme: Lexeme::Comment {
                        comment: Arc::from(" regular comment"),
                        doc: false
                    },
                    span: Span {
                        source: "<< test >>",
                        range: 0..18
                    }
                },
                Token {
                    lexeme: Lexeme::Comment {
                        comment: Arc::from(" doc comment"),
                        doc: true
                    },
                    span: Span {
                        source: "<< test >>",
                        range: 19..34
                    }
                }
            ]),
            lexed
        )
    }

    #[test]
    fn test_binary() {
        let input = "0b101 0b0 0b1";
        let source = "<< test >>";
        let lexed = Context::new(source, input).lex();
        assert_eq!(
            Ok(vec![
                Token {
                    span: Span::new(source, 0..5),
                    lexeme: Lexeme::Integer(5)
                },
                Token {
                    span: Span::new(source, 6..9),
                    lexeme: Lexeme::Integer(0)
                },
                Token {
                    span: Span::new(source, 10..13),
                    lexeme: Lexeme::Integer(1)
                }
            ]),
            lexed
        )
    }

    #[test]
    fn test_group() {
        let input = "(1, 2, (3, 4), [5, 6], {7: 8})";
        let source = "<< test >>";
        let lexed = Context::new(source, input).lex();
        assert_eq!(
            Ok(vec![Token {
                lexeme: Lexeme::Group(Group::Parentheses(vec![
                    Token {
                        lexeme: Lexeme::Integer(1),
                        span: Span {
                            source: "<< test >>",
                            range: 1..2
                        }
                    },
                    Token {
                        lexeme: Lexeme::Symbol(Symbol::Comma),
                        span: Span {
                            source: "<< test >>",
                            range: 2..3
                        }
                    },
                    Token {
                        lexeme: Lexeme::Integer(2),
                        span: Span {
                            source: "<< test >>",
                            range: 4..5
                        }
                    },
                    Token {
                        lexeme: Lexeme::Symbol(Symbol::Comma),
                        span: Span {
                            source: "<< test >>",
                            range: 5..6
                        }
                    },
                    Token {
                        lexeme: Lexeme::Group(Group::Parentheses(vec![
                            Token {
                                lexeme: Lexeme::Integer(3),
                                span: Span {
                                    source: "<< test >>",
                                    range: 8..9
                                }
                            },
                            Token {
                                lexeme: Lexeme::Symbol(Symbol::Comma),
                                span: Span {
                                    source: "<< test >>",
                                    range: 9..10
                                }
                            },
                            Token {
                                lexeme: Lexeme::Integer(4),
                                span: Span {
                                    source: "<< test >>",
                                    range: 11..12
                                }
                            }
                        ])),
                        span: Span {
                            source: "<< test >>",
                            range: 7..13
                        }
                    },
                    Token {
                        lexeme: Lexeme::Symbol(Symbol::Comma),
                        span: Span {
                            source: "<< test >>",
                            range: 13..14
                        }
                    },
                    Token {
                        lexeme: Lexeme::Group(Group::Brackets(vec![
                            Token {
                                lexeme: Lexeme::Integer(5),
                                span: Span {
                                    source: "<< test >>",
                                    range: 16..17
                                }
                            },
                            Token {
                                lexeme: Lexeme::Symbol(Symbol::Comma),
                                span: Span {
                                    source: "<< test >>",
                                    range: 17..18
                                }
                            },
                            Token {
                                lexeme: Lexeme::Integer(6),
                                span: Span {
                                    source: "<< test >>",
                                    range: 19..20
                                }
                            }
                        ])),
                        span: Span {
                            source: "<< test >>",
                            range: 15..21
                        }
                    },
                    Token {
                        lexeme: Lexeme::Symbol(Symbol::Comma),
                        span: Span {
                            source: "<< test >>",
                            range: 21..22
                        }
                    },
                    Token {
                        lexeme: Lexeme::Group(Group::Braces(vec![
                            Token {
                                lexeme: Lexeme::Integer(7),
                                span: Span {
                                    source: "<< test >>",
                                    range: 24..25
                                }
                            },
                            Token {
                                lexeme: Lexeme::Symbol(Symbol::Colon),
                                span: Span {
                                    source: "<< test >>",
                                    range: 25..26
                                }
                            },
                            Token {
                                lexeme: Lexeme::Integer(8),
                                span: Span {
                                    source: "<< test >>",
                                    range: 27..28
                                }
                            }
                        ])),
                        span: Span {
                            source: "<< test >>",
                            range: 23..29
                        }
                    }
                ])),
                span: Span {
                    source: "<< test >>",
                    range: 0..30
                }
            }]),
            lexed
        )
    }

    #[test]
    fn combination() {
        use self::Keyword::*;
        use self::Lexeme::*;
        use self::Symbol::*;
        let input = r#"
        let x: i64 = 42 in
        if x > 0 then
            (x, [1, 2, 3], { key: "value" })
        else
            (0, [], {})
        "#;
        let source = "<< test >>";
        let lexed = Context::new(source, input).lex();
        assert_eq!(
            Ok(vec![
                Token {
                    lexeme: Keyword(Let),
                    span: Span {
                        source: "<< test >>",
                        range: 9..12
                    }
                },
                Token {
                    lexeme: Identifier(Arc::from("x")),
                    span: Span {
                        source: "<< test >>",
                        range: 13..14
                    }
                },
                Token {
                    lexeme: Symbol(Colon),
                    span: Span {
                        source: "<< test >>",
                        range: 14..15
                    }
                },
                Token {
                    lexeme: Identifier(Arc::from("i64")),
                    span: Span {
                        source: "<< test >>",
                        range: 16..19
                    }
                },
                Token {
                    lexeme: Symbol(Equal),
                    span: Span {
                        source: "<< test >>",
                        range: 20..21
                    }
                },
                Token {
                    lexeme: Integer(42),
                    span: Span {
                        source: "<< test >>",
                        range: 22..24
                    }
                },
                Token {
                    lexeme: Keyword(In),
                    span: Span {
                        source: "<< test >>",
                        range: 25..27
                    }
                },
                Token {
                    lexeme: Keyword(If),
                    span: Span {
                        source: "<< test >>",
                        range: 36..38
                    }
                },
                Token {
                    lexeme: Identifier(Arc::from("x")),
                    span: Span {
                        source: "<< test >>",
                        range: 39..40
                    }
                },
                Token {
                    lexeme: Symbol(Greater),
                    span: Span {
                        source: "<< test >>",
                        range: 41..42
                    }
                },
                Token {
                    lexeme: Integer(0),
                    span: Span {
                        source: "<< test >>",
                        range: 43..44
                    }
                },
                Token {
                    lexeme: Keyword(Then),
                    span: Span {
                        source: "<< test >>",
                        range: 45..49
                    }
                },
                Token {
                    lexeme: Group(self::Group::Parentheses(vec![
                        Token {
                            lexeme: Identifier(Arc::from("x")),
                            span: Span {
                                source: "<< test >>",
                                range: 63..64
                            }
                        },
                        Token {
                            lexeme: Symbol(Comma),
                            span: Span {
                                source: "<< test >>",
                                range: 64..65
                            }
                        },
                        Token {
                            lexeme: Group(self::Group::Brackets(vec![
                                Token {
                                    lexeme: Integer(1),
                                    span: Span {
                                        source: "<< test >>",
                                        range: 67..68
                                    }
                                },
                                Token {
                                    lexeme: Symbol(Comma),
                                    span: Span {
                                        source: "<< test >>",
                                        range: 68..69
                                    }
                                },
                                Token {
                                    lexeme: Integer(2),
                                    span: Span {
                                        source: "<< test >>",
                                        range: 70..71
                                    }
                                },
                                Token {
                                    lexeme: Symbol(Comma),
                                    span: Span {
                                        source: "<< test >>",
                                        range: 71..72
                                    }
                                },
                                Token {
                                    lexeme: Integer(3),
                                    span: Span {
                                        source: "<< test >>",
                                        range: 73..74
                                    }
                                }
                            ])),
                            span: Span {
                                source: "<< test >>",
                                range: 66..75
                            }
                        },
                        Token {
                            lexeme: Symbol(Comma),
                            span: Span {
                                source: "<< test >>",
                                range: 75..76
                            }
                        },
                        Token {
                            lexeme: Group(self::Group::Braces(vec![
                                Token {
                                    lexeme: Identifier(Arc::from("key")),
                                    span: Span {
                                        source: "<< test >>",
                                        range: 79..82
                                    }
                                },
                                Token {
                                    lexeme: Symbol(Colon),
                                    span: Span {
                                        source: "<< test >>",
                                        range: 82..83
                                    }
                                },
                                Token {
                                    lexeme: String(Arc::from("value")),
                                    span: Span {
                                        source: "<< test >>",
                                        range: 84..91
                                    }
                                }
                            ])),
                            span: Span {
                                source: "<< test >>",
                                range: 77..93
                            }
                        }
                    ])),
                    span: Span {
                        source: "<< test >>",
                        range: 62..94
                    }
                },
                Token {
                    lexeme: Keyword(Else),
                    span: Span {
                        source: "<< test >>",
                        range: 103..107
                    }
                },
                Token {
                    lexeme: Group(self::Group::Parentheses(vec![
                        Token {
                            lexeme: Integer(0),
                            span: Span {
                                source: "<< test >>",
                                range: 121..122
                            }
                        },
                        Token {
                            lexeme: Symbol(Comma),
                            span: Span {
                                source: "<< test >>",
                                range: 122..123
                            }
                        },
                        Token {
                            lexeme: Group(self::Group::Brackets(vec![])),
                            span: Span {
                                source: "<< test >>",
                                range: 124..126
                            }
                        },
                        Token {
                            lexeme: Symbol(Comma),
                            span: Span {
                                source: "<< test >>",
                                range: 126..127
                            }
                        },
                        Token {
                            lexeme: Group(self::Group::Braces(vec![])),
                            span: Span {
                                source: "<< test >>",
                                range: 128..130
                            }
                        }
                    ])),
                    span: Span {
                        source: "<< test >>",
                        range: 120..131
                    }
                }
            ]),
            lexed
        )
    }
}
