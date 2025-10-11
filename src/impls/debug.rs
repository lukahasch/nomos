use std::fmt::Debug;

use crate::{Pattern, Term, parser::Parsed};

impl Debug for Term<Parsed> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Error(e) => f.debug_tuple("Error").field(e).finish(),
            Term::Integer(i) => f.debug_tuple("Integer").field(i).finish(),
            Term::Float(fl) => f.debug_tuple("Float").field(fl).finish(),
            Term::List(l) => f.debug_tuple("List").field(l).finish(),
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
            Term::Block(terms) => f.debug_tuple("Block").field(terms).finish(),
            Term::Define {
                pattern,
                value,
                body,
            } => f
                .debug_struct("Define")
                .field("pattern", pattern)
                .field("value", value)
                .field("body", body)
                .finish(),
            Term::LangItem(item) => f.debug_tuple("LangItem").field(item).finish(),
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
            Pattern::Error(e) => f.debug_tuple("Error").field(e).finish(),
        }
    }
}
