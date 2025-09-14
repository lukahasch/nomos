use std::ops::Range;
use thiserror::Error;

#[derive(Debug, Error, Clone, PartialEq, Hash, Eq)]
pub enum Error {}

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

impl chumsky::span::Span for Span {
    type Context = &'static str;
    type Offset = usize;
    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        Span {
            source: context,
            range,
        }
    }
    fn context(&self) -> Self::Context {
        self.source
    }
    fn end(&self) -> Self::Offset {
        self.range.end
    }
    fn start(&self) -> Self::Offset {
        self.range.start
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Spanned<T> {
    pub item: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(item: T, span: Span) -> Self {
        Spanned { item, span }
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.item
    }
}

impl<T> std::ops::DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.item
    }
}
