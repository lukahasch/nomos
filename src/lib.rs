#![feature(
    type_alias_impl_trait,
    const_ops,
    unboxed_closures,
    try_blocks,
    try_trait_v2,
    never_type,
    min_specialization
)]

pub mod error;
pub mod parser;
use crate::error::Error;
use ariadne::{Cache, Source};
pub use error::Span;
pub use std::sync::Arc;
use std::{collections::HashMap, sync::atomic::AtomicUsize};
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
    type Term;
    type Pattern;
    type Identifier;
    type List;
    type Tuple;
}

#[rustfmt::skip]
pub enum Term<T: Types> {
    /// An Error has occured, computation will continue until this Error is encountered and then the error will be bubbled up
    Error(Error),
    Integer(i64),
    Float(f64),
    List(T::List),
    Tuple(T::Tuple),
    Application { function: T::Term, argument: T::Term },
    Function { pattern: T::Pattern, body: T::Term },
    Variable(T::Identifier),
    Let { pattern: T::Pattern, value: T::Term, body: T::Term },
    If { condition: T::Term, then: T::Term, r#else: T::Term },
    Match { value: T::Term, branches: Vec<(T::Pattern, T::Term)> },
    Inference(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern<T: Types> {
    Wildcard,
    Capture(T::Identifier),
    Rest,
    Tuple(Vec<T::Pattern>),
    List(Vec<T::Pattern>),
    As {
        pattern: T::Pattern,
        name: T::Identifier,
    },
    If {
        pattern: T::Pattern,
        condition: T::Term,
    },
    Integer(i64),
    Float(f64),
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
