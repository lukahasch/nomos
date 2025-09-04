#![feature(
    stmt_expr_attributes,
    try_trait_v2,
    never_type,
    box_patterns,
    slice_ptr_get,
    allocator_api,
    ptr_as_ref_unchecked,
    unsafe_cell_access,
    impl_trait_in_assoc_type,
    unsized_fn_params,
    ptr_metadata,
    str_from_raw_parts
)]

use std::ops::Range;

pub mod error;
pub mod lexer;
pub mod parser;
pub mod typing;

pub use error::Error;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Span {
    pub source: &'static str,
    pub range: Range<usize>,
}

impl Span {
    pub fn new(source: &'static str, range: Range<usize>) -> Self {
        Span { source, range }
    }

    pub fn extend_to(&self, to: usize) -> Self {
        Span {
            source: self.source,
            range: self.range.start.min(to)..self.range.end.max(to),
        }
    }

    pub fn begin(self) -> Self {
        Span {
            source: self.source,
            range: self.range.start..self.range.start,
        }
    }
}

impl std::ops::Add<Span> for Span {
    type Output = Span;

    fn add(self, rhs: Span) -> Self::Output {
        assert_eq!(self.source, rhs.source);
        Span {
            source: self.source,
            range: self.range.start.min(rhs.range.start)..self.range.end.max(rhs.range.end),
        }
    }
}

impl std::ops::Add<Span> for &Span {
    type Output = Span;

    fn add(self, rhs: Span) -> Self::Output {
        assert_eq!(self.source, rhs.source);
        Span {
            source: self.source,
            range: self.range.start.min(rhs.range.start)..self.range.end.max(rhs.range.end),
        }
    }
}

impl<T> std::ops::Add<T> for &Span
where
    T: AsRef<Span>,
{
    type Output = Span;

    fn add(self, rhs: T) -> Self::Output {
        assert_eq!(self.source, rhs.as_ref().source);
        Span {
            source: self.source,
            range: self.range.start.min(rhs.as_ref().range.start)
                ..self.range.end.max(rhs.as_ref().range.end),
        }
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}..{}",
            self.source, self.range.start, self.range.end
        )
    }
}

impl ariadne::Span for Span {
    type SourceId = &'static str;

    fn source(&self) -> &Self::SourceId {
        &self.source
    }

    fn end(&self) -> usize {
        self.range.end
    }

    fn start(&self) -> usize {
        self.range.start
    }
}
