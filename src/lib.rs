#![feature(type_alias_impl_trait, const_ops)]

pub mod error;
pub mod parser;
use crate::error::Error;
pub use error::Span;
use gc_arena::{Gc, Mutation};
pub use std::sync::Arc;
use std::{collections::HashMap, sync::atomic::AtomicUsize};

static ID: AtomicUsize = AtomicUsize::new(0);

pub fn id() -> usize {
    ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

/*
 * Query(a1, a2, ...) -> Value @ (e1, e2, ...)
 * load("main.stream")
 */
