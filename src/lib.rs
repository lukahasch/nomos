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

pub use crate::{
    error::{Error, Span},
    id::{Id, id},
};
use ariadne::{Cache, Source};
use std::{
    collections::HashMap,
    fmt::{Formatter, FormattingOptions},
};

pub mod error;
pub mod id;
pub mod impls;
pub mod parser;

pub struct Context {
    pub variables: HashMap<Identifier, String>,
    pub sources: HashMap<&'static str, Source>,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq, Copy)]
#[repr(transparent)]
pub struct Identifier(Id);

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
    /// # Errors
    /// - On write failure
    fn show(&self, ctx: &Context, fmt: &mut Formatter<'_>) -> std::fmt::Result;
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl Context {
    #[must_use = "Context does nothing unless you use it"]
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

    #[allow(clippy::missing_panics_doc)]
    pub fn show(&self, v: &impl Show) -> String {
        let mut write = String::new();
        let mut fmt = Formatter::new(&mut write, FormattingOptions::default());
        v.show(self, &mut fmt)
            .expect("writing to string should not fail");
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
