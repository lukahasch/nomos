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

pub use crate::{error::Error, id::id};
use crate::{
    error::{Span, Spanned},
    id::UId,
};
use ariadne::{Cache, Source};
use egg::{EGraph, Id};
use ordered_float::OrderedFloat;
use smallvec::SmallVec;
use std::{
    collections::HashMap,
    fmt::{Formatter, FormattingOptions},
};
use strum::AsRefStr;

pub mod error;
pub mod id;
pub mod impls;
pub mod parser;

#[derive(Debug)]
pub struct Context {
    pub variables: HashMap<Identifier, String>,
    pub sources: HashMap<&'static str, Source>,
    pub egraph: (), /*EGraph<Node<Term>, ()>*/
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq, Hash, Copy)]
#[repr(transparent)]
pub struct Identifier(UId);

pub const INLINE_CAPACITY: usize = 4;

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct Node<T: Arrangement> {
    pub children: SmallVec<[Id; INLINE_CAPACITY]>,
    pub arrangement: T,
    pub span: Span,
}

pub trait Arrangement: Sized {
    fn update_children(&mut self, f: impl FnMut(ChildID) -> ChildID);

    fn map_children(mut self, f: impl FnMut(ChildID) -> ChildID) -> Self {
        self.update_children(f);
        self
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq, Hash, Copy)]
#[repr(transparent)]
pub struct ChildID(u16);

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq, Hash, AsRefStr)]
pub enum Term {
    /// An Error has occured, computation will continue until this Error is encountered
    /// and then the error will be bubbled up
    Error(Error),
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum Pattern {
    Error(Error),
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum Type {
    Error(Error),
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq, Hash)]
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
            egraph: (),
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

    pub fn parse(&mut self, source: &'static str) -> Result<Spanned<Term>, Vec<Error>> {
        todo!() //parse(self, source)
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

impl<T: Arrangement> Node<T> {
    pub fn new(children: SmallVec<[Id; INLINE_CAPACITY]>, arrangement: T, span: Span) -> Self {
        Self {
            children,
            arrangement,
            span,
        }
    }
}
