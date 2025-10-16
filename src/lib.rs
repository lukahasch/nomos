#![feature(
    type_alias_impl_trait,
    const_ops,
    unboxed_closures,
    try_blocks,
    try_trait_v2,
    never_type,
    min_specialization,
    macro_metavar_expr,
    if_let_guard,
    formatting_options
)]
#![warn(clippy::all, clippy::pedantic)]

pub mod error;
pub mod impls;
pub mod parser;
use crate::{error::Error, parser::Parsed};
use ariadne::{Cache, Source};
pub use error::Span;
pub use std::sync::Arc;
use std::{
    collections::HashMap,
    fmt::{Formatter, FormattingOptions},
    sync::atomic::AtomicUsize,
};
static ID: AtomicUsize = AtomicUsize::new(0);

pub fn id() -> usize {
    ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

pub struct Context {
    pub variables: HashMap<Identifier, String>,
    pub sources: HashMap<&'static str, Source>,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq, Copy)]
#[repr(transparent)]
pub struct Identifier(pub usize);

pub trait Types {
    type Identifier;
    type List<T>;
    type Block<T>;
    type TeRec<T>;
    type PaRec<P>;
}

#[rustfmt::skip]
#[derive(Debug, Clone, PartialEq)]
pub enum Term<T: Types> {
    /// An Error has occured, computation will continue until this Error is encountered and then the error will be bubbled up
    Error(Error),
    Integer(i64),
    Float(f64),
    List(T::List<Self>),
    Block(T::Block<Self>),
    Application { function: T::TeRec<Self>, argument: T::TeRec<Self> },
    Function { pattern: T::PaRec<Pattern<T>>, body: T::TeRec<Self> },
    Variable(T::Identifier),
    Let { pattern: T::PaRec<Pattern<T>>, value: T::TeRec<Self>, body: Option<T::TeRec<Self>> },
    Define { pattern: T::PaRec<Pattern<T>>, value: T::TeRec<Self>, body: Option<T::TeRec<Self>> },
    If { condition: T::TeRec<Self>, then: T::TeRec<Self>, r#else: T::TeRec<Self> },
    Match { value: T::TeRec<Self>, branches: Branches<T> },
    Inference(usize),
    LangItem(LangItem),
}

pub type Branches<T> = Vec<(
    <T as Types>::PaRec<Pattern<T>>,
    <T as Types>::TeRec<Term<T>>,
)>;

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern<T: Types> {
    Error(Error),
    Wildcard,
    Capture(T::Identifier),
    Rest,
    List(Vec<T::PaRec<Self>>),
    As {
        pattern: T::PaRec<Self>,
        name: T::Identifier,
    },
    If {
        pattern: T::PaRec<Self>,
        condition: T::TeRec<Term<T>>,
    },
    Integer(i64),
    Float(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LangItem {
    Add,
}

pub trait Show {
    fn show(&self, ctx: &Context, fmt: &mut Formatter<'_>) -> std::fmt::Result;
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl Context {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            sources: HashMap::new(),
        }
    }

    pub fn intern_source<'b>(&'b mut self, name: &'static str, contents: &str) -> &'b str {
        self.sources
            .entry(name)
            .or_insert_with(|| Source::from(contents.to_string()))
            .text()
    }

    pub fn declare(&mut self, v: &str) -> Identifier {
        let id = Identifier(id());
        self.variables.insert(id, v.to_string());
        id
    }

    pub fn show(&self, v: &impl Show) -> String {
        let mut write = String::new();
        let mut fmt = Formatter::new(&mut write, FormattingOptions::default());
        v.show(self, &mut fmt).unwrap();
        write
    }
}

impl Cache<str> for Context {
    type Storage = String;

    fn fetch(&mut self, id: &str) -> Result<&Source, impl std::fmt::Debug> {
        match self.sources.get(id) {
            Some(source) => Ok(source),
            None => Err(()),
        }
    }

    fn display<'a>(&self, id: &'a str) -> Option<impl std::fmt::Display + 'a> {
        Some(id)
    }
}

impl Cache<&'static str> for Context {
    type Storage = String;

    fn fetch(&mut self, id: &&'static str) -> Result<&Source, impl std::fmt::Debug> {
        match self.sources.get(id) {
            Some(source) => Ok(source),
            None => Err(()),
        }
    }

    fn display<'a>(&self, id: &'a &'static str) -> Option<impl std::fmt::Display + 'a> {
        Some(id)
    }
}

impl Term<Parsed> {
    pub fn contains_error(&self) -> bool {
        match self {
            Term::Error(_) => true,
            Term::List(a) => a.iter().any(|v| v.contains_error()),
            Term::Block(a) => a.iter().any(|v| v.contains_error()),
            Term::Application { function, argument } => {
                function.contains_error() || argument.contains_error()
            }
            Term::Function { pattern, body } => pattern.contains_error() || body.contains_error(),
            Term::Let {
                pattern,
                value,
                body,
            } => {
                pattern.contains_error()
                    || value.contains_error()
                    || body.as_ref().is_some_and(|b| b.contains_error())
            }
            Term::Define {
                pattern,
                value,
                body,
            } => {
                pattern.contains_error()
                    || value.contains_error()
                    || body.as_ref().is_some_and(|b| b.contains_error())
            }
            Term::If {
                condition,
                then,
                r#else,
            } => condition.contains_error() || then.contains_error() || r#else.contains_error(),
            Term::Match { value, branches } => {
                value.contains_error()
                    || branches
                        .iter()
                        .any(|(p, t)| p.contains_error() || t.contains_error())
            }
            Term::Integer(_)
            | Term::Float(_)
            | Term::Variable(_)
            | Term::Inference(_)
            | Term::LangItem(_) => false,
        }
    }

    pub fn collect_errors(self) -> Vec<Error> {
        let mut errors = Vec::new();
        self.collect_errors_into(&mut errors);
        errors
    }

    pub fn collect_errors_into(&self, errors: &mut Vec<Error>) {
        match self {
            Term::Error(e) => errors.push(e.clone()),
            Term::List(a) => a.iter().for_each(|v| v.collect_errors_into(errors)),
            Term::Block(a) => a.iter().for_each(|v| v.collect_errors_into(errors)),
            Term::Application { function, argument } => {
                function.collect_errors_into(errors);
                argument.collect_errors_into(errors);
            }
            Term::Function { pattern, body } => {
                pattern.collect_errors_into(errors);
                body.collect_errors_into(errors);
            }
            Term::Let {
                pattern,
                value,
                body,
            } => {
                pattern.collect_errors_into(errors);
                value.collect_errors_into(errors);
                if let Some(b) = body {
                    b.collect_errors_into(errors);
                }
            }
            Term::Define {
                pattern,
                value,
                body,
            } => {
                pattern.collect_errors_into(errors);
                value.collect_errors_into(errors);
                if let Some(b) = body {
                    b.collect_errors_into(errors);
                }
            }
            Term::If {
                condition,
                then,
                r#else,
            } => {
                condition.collect_errors_into(errors);
                then.collect_errors_into(errors);
                r#else.collect_errors_into(errors);
            }
            Term::Match { value, branches } => {
                value.collect_errors_into(errors);
                branches.iter().for_each(|(p, t)| {
                    p.collect_errors_into(errors);
                    t.collect_errors_into(errors);
                });
            }
            Term::Integer(_)
            | Term::Float(_)
            | Term::Variable(_)
            | Term::Inference(_)
            | Term::LangItem(_) => {}
        }
    }
}

impl Pattern<Parsed> {
    fn contains_error(&self) -> bool {
        match self {
            Pattern::Error(_) => true,
            Pattern::As { pattern, .. } => pattern.contains_error(),
            Pattern::If { pattern, condition } => {
                pattern.contains_error() || condition.contains_error()
            }
            Pattern::List(a) => a.iter().any(|p| p.contains_error()),
            Pattern::Wildcard
            | Pattern::Capture(_)
            | Pattern::Rest
            | Pattern::Integer(_)
            | Pattern::Float(_) => false,
        }
    }

    fn collect_errors_into(&self, errors: &mut Vec<Error>) {
        match self {
            Pattern::Error(e) => errors.push(e.clone()),
            Pattern::As { pattern, .. } => pattern.collect_errors_into(errors),
            Pattern::If { pattern, condition } => {
                pattern.collect_errors_into(errors);
                condition.collect_errors_into(errors);
            }
            Pattern::List(a) => a.iter().for_each(|p| p.collect_errors_into(errors)),
            Pattern::Wildcard
            | Pattern::Capture(_)
            | Pattern::Rest
            | Pattern::Integer(_)
            | Pattern::Float(_) => {}
        }
    }
}
