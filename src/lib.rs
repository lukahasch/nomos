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
    formatting_options,
    explicit_tail_calls
)]
#![warn(clippy::all)]
#![allow(clippy::match_like_matches_macro, clippy::result_large_err)]

use crate::{
    egraph::{ClassId, Egraph},
    error::Spanned,
    normalize::Namespace,
};
pub use crate::{
    error::{Error, Span},
    id::{Id, id},
};
use ariadne::{Cache, Source};
use std::{
    collections::HashMap,
    fmt::{Display, Formatter, FormattingOptions},
    sync::Arc,
};
use strum::AsRefStr;

pub mod egraph;
pub mod error;
pub mod id;
pub mod impls;
pub mod normalize;
pub mod parser;

pub struct Context {
    pub variables: HashMap<Identifier, String>,
    pub sources: HashMap<&'static str, Source>,
    pub egraph: Egraph,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq, Copy)]
#[repr(transparent)]
pub struct Identifier(Id);

pub trait Types {
    type Identifier;
    type List;
    type Block;
    type Term;
    type Pattern;
}

pub struct Normalized;

impl Types for Normalized {
    type Identifier = Identifier;
    type List = Vec<ClassId>;
    type Block = Vec<ClassId>;
    type Term = ClassId;
    type Pattern = Arc<Spanned<Pattern<Self>>>;
}

#[derive(AsRefStr)]
pub enum Term<T: Types> {
    /// An Error has occured, computation will continue until this Error is encountered
    /// and then the error will be bubbled up
    Error(Error),
    Type(Type<T>),
    Integer(i64),
    Float(f64),
    List(T::List),
    Block(T::Block),
    Application {
        function: T::Term,
        argument: T::Term,
    },
    Function {
        pattern: T::Pattern,
        body: T::Term,
    },
    Variable(T::Identifier),
    Let {
        pattern: T::Pattern,
        value: T::Term,
        body: Option<T::Term>,
    },
    Define {
        pattern: T::Pattern,
        value: T::Term,
        body: Option<T::Term>,
    },
    If {
        condition: T::Term,
        then: T::Term,
        r#else: T::Term,
    },
    Match {
        value: T::Term,
        branches: Branches<T>,
    },
    Inference(usize),
    LangItem(LangItem),
}

pub type Branches<T> = Vec<(<T as Types>::Pattern, <T as Types>::Term)>;

pub enum Pattern<T: Types> {
    Error(Error),
    Typed {
        pattern: T::Pattern,
        ty: T::Term,
    },
    Wildcard,
    Capture(T::Identifier),
    Rest,
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

pub enum Type<T: Types> {
    Error(Error),
    Variable(T::Identifier),
    Function { parameter: T::Term, result: T::Term },
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

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#x}", self.0.0)
    }
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
            egraph: Egraph::new(),
        }
    }

    pub fn intern_source<'b>(&'b mut self, name: &'static str, contents: &str) -> &'b str {
        self.sources
            .entry(name)
            .or_insert_with(|| Source::from(contents.to_string()))
            .text()
    }

    pub fn declare(&mut self, v: impl Into<String>) -> Identifier {
        let id = Identifier(id());
        self.variables.insert(id, v.into());
        id
    }

    pub fn store(&mut self, source: &'static str) -> Result<Id, Vec<Error>> {
        match parser::parse(self, source)?.map(|t| t.normalize(self, &mut Namespace::default())) {
            Spanned {
                item: Ok(term),
                span,
            } => Ok(self.egraph.store(Spanned { item: term, span })),
            Spanned {
                item: Err(errors), ..
            } => Err(errors),
        }
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
