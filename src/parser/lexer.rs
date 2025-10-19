use std::fmt::Display;

use crate::{Span, error::Error};
use logos::{Lexer, Logos};

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(extras = &'static str)]
#[logos(error(Error, Error::from_lexer))]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
    #[regex(r"[0-9]+", callback = lex_integer)]
    Integer(i64),
    #[regex(r"[0-9]+\.[0-9]+(e [0-9]+)?", callback = lex_float)]
    Float(f64),
    #[token("[")]
    OpenBracket,
    #[token("]")]
    CloseBracket,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[token("{")]
    OpenBrace,
    #[token("}")]
    CloseBrace,
    #[token(",")]
    Comma,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[token("match")]
    Match,
    #[token("with")]
    With,
    #[token("->")]
    Arrow,
    #[token("fn")]
    Fn,
    #[token("_")]
    Underscore,
    #[token("...")]
    Ellipsis,
    #[token("@")]
    At,
    #[token("=")]
    Equals,
    #[token("|")]
    Pipe,
    #[token("def")]
    Def,
    #[regex(r"([a-zA-Z][a-zA-Z0-9_]*)|(_[a-zA-Z0-9_]+)|([a-zA-Z])", |lex| lex.slice().to_string())]
    Identifier(String),
    #[token(";")]
    Semicolon,
    #[token("+")]
    Plus,
    #[token(":")]
    Colon,
}

#[allow(clippy::result_large_err)]
fn lex_integer(lex: &Lexer<Token>) -> Result<i64, Error> {
    lex.slice().parse().map_err(|_| {
        Error::InvalidInteger(lex.slice().to_string(), Span::new(lex.extras, lex.span()))
    })
}

#[allow(clippy::result_large_err)]
fn lex_float(lex: &Lexer<Token>) -> Result<f64, Error> {
    lex.slice().parse().map_err(|_| {
        Error::InvalidFloat(lex.slice().to_string(), Span::new(lex.extras, lex.span()))
    })
}

#[must_use = "Lexer does nothing unless you use it"]
pub fn lexer<'a>(source: &'static str, contents: &'a str) -> logos::Lexer<'a, Token> {
    Token::lexer_with_extras(contents, source)
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Integer(i) => write!(f, "'{i}'"),
            Token::Float(fl) => write!(f, "'{fl}'"),
            Token::OpenBracket => write!(f, "'['"),
            Token::CloseBracket => write!(f, "']'"),
            Token::OpenParen => write!(f, "'('"),
            Token::CloseParen => write!(f, "')'"),
            Token::OpenBrace => write!(f, "'{{'"),
            Token::CloseBrace => write!(f, "'}}'"),
            Token::Comma => write!(f, "','"),
            Token::If => write!(f, "'if'"),
            Token::Then => write!(f, "'then'"),
            Token::Else => write!(f, "'else'"),
            Token::Let => write!(f, "'let'"),
            Token::In => write!(f, "'in'"),
            Token::Match => write!(f, "'match'"),
            Token::With => write!(f, "'with'"),
            Token::Arrow => write!(f, "'->'"),
            Token::Fn => write!(f, "'fn'"),
            Token::Underscore => write!(f, "'_'"),
            Token::Ellipsis => write!(f, "'...'"),
            Token::At => write!(f, "'@'"),
            Token::Equals => write!(f, "'='"),
            Token::Identifier(name) => write!(f, "'{name}'"),
            Token::Pipe => write!(f, "'|'"),
            Token::Def => write!(f, "'def'"),
            Token::Semicolon => write!(f, "';'"),
            Token::Plus => write!(f, "'+'"),
            Token::Colon => write!(f, "':'"),
        }
    }
}
