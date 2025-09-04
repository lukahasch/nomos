use log::*;
use std::ops::{ControlFlow, Range};

use crate::{
    Error, Span,
    error::{Expected, Found},
    lexer::{Group, Keyword, Lexeme, Symbol, Token},
};

#[derive(Debug, PartialEq)]
pub struct Context<'a> {
    pub tokens: &'a [Token],
    pub index: usize,
    pub errors: Vec<Error<'static>>,
}

pub enum Output<T> {
    Ok(T),
    Error(Error<'static>),
    Fatal(Error<'static>),
}

impl<'a> Context<'a> {
    #[allow(clippy::result_unit_err)]
    pub fn new(tokens: &'a [Token]) -> Result<Self, ()> {
        if tokens.is_empty() {
            return Err(());
        }
        Ok(Self {
            tokens,
            index: 0,
            errors: Vec::new(),
        })
    }

    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    pub fn last(&self) -> Option<&Token> {
        if self.index == 0 {
            None
        } else {
            Some(&self.tokens[self.index - 1])
        }
    }

    pub fn span(&self) -> Span {
        match self.peek() {
            Some(token) => token.span.clone(),
            None => {
                let span = self.last().unwrap().span.clone();
                Span {
                    source: span.source,
                    range: span.range.end..span.range.end,
                }
            }
        }
    }

    pub fn advance(&mut self) -> Option<&Token> {
        if self.index < self.tokens.len() {
            self.index += 1;
            Some(&self.tokens[self.index - 1])
        } else {
            None
        }
    }

    pub fn next(&mut self, expected: Expected) -> Output<&Token> {
        let location = self.span();
        match self.advance() {
            Some(token) => Output::Ok(token),
            None => Output::Error(Error::ExpectedFound {
                location,
                expected,
                found: Found::Eof,
            }),
        }
    }

    pub fn found(&self) -> Found<'static> {
        match self.peek() {
            Some(token) => Found::Token(token.clone()),
            None => Found::Eof,
        }
    }

    pub fn or<T, O>(&mut self, or: T) -> Output<O>
    where
        Or<T>: Parser<O>,
    {
        Or(or).parse(self)
    }

    pub fn child(&mut self) -> Context<'a> {
        Context {
            tokens: self.tokens,
            index: self.index,
            errors: Vec::new(),
        }
    }

    pub fn into_errors(self) -> Vec<Error<'static>> {
        self.errors
    }

    pub fn at_end(&self) -> bool {
        self.index == self.tokens.len()
    }

    pub fn parse<O>(mut self, parser: &mut impl Parser<O>) -> Result<O, Vec<Error<'static>>> {
        let out = parser.parse(&mut self);
        if let Some(token) = self.advance().cloned()
            && self.errors.is_empty()
            && out.is_ok()
        {
            self.errors.push(Error::ExpectedFound {
                location: self.span(),
                expected: Expected::Eof,
                found: Found::Token(token),
            })
        }
        match out {
            Output::Error(err) | Output::Fatal(err) => {
                self.errors.push(err);
                Err(self.errors)
            }
            Output::Ok(ok) if self.errors.is_empty() => Ok(ok),
            Output::Ok(_) => Err(self.errors),
        }
    }
}

pub struct Unary<Out, F = Box<dyn FnOnce(Out) -> Out>>
where
    F: FnOnce(Out) -> Out,
{
    f: F,
    precedence: u8,
    phantom: std::marker::PhantomData<fn(Out)>,
}

impl<Out, F> std::fmt::Debug for Unary<Out, F>
where
    F: FnOnce(Out) -> Out,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Unary")
            .field("precedence", &self.precedence)
            .finish_non_exhaustive()
    }
}

impl<Out, F> Unary<Out, F>
where
    F: FnOnce(Out) -> Out,
{
    pub fn new(precedence: u8, f: F) -> Self {
        Self {
            f,
            precedence,
            phantom: std::marker::PhantomData,
        }
    }
}

pub struct Binary<Out, F = Box<dyn FnOnce(Out, Out) -> Out>>
where
    F: FnOnce(Out, Out) -> Out,
{
    f: F,
    precedence: u8,
    phantom: std::marker::PhantomData<fn(Out)>,
}

impl<Out, F> std::fmt::Debug for Binary<Out, F>
where
    F: FnOnce(Out, Out) -> Out,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Binary")
            .field("precedence", &self.precedence)
            .finish_non_exhaustive()
    }
}

impl<Out, F> Binary<Out, F>
where
    F: FnOnce(Out, Out) -> Out,
{
    pub fn new(precedence: u8, f: F) -> Self {
        Self {
            f,
            precedence,
            phantom: std::marker::PhantomData,
        }
    }
}

pub trait Parser<Out>: Sized {
    fn parse<'a, 'b>(&'b mut self, ctx: &'b mut Context<'a>) -> Output<Out>;

    fn or(self, other: impl Parser<Out>) -> Or<(Self, impl Parser<Out>)> {
        Or((self, other))
    }

    fn with_operators<Pre, Post, B>(
        mut self,
        prefix: impl Parser<Unary<Out, Pre>>,
        postfix: impl Parser<Unary<Out, Post>>,
        mut binary: impl Parser<Binary<Out, B>>,
        expected: Expected,
    ) -> impl Parser<Out>
    where
        Pre: FnOnce(Out) -> Out,
        Post: FnOnce(Out) -> Out,
        B: FnOnce(Out, Out) -> Out,
        Out: std::fmt::Debug,
    {
        pub enum Item<Pre, Post, B, Out>
        where
            Pre: FnOnce(Out) -> Out,
            Post: FnOnce(Out) -> Out,
            B: FnOnce(Out, Out) -> Out,
        {
            Value(Out),
            Prefix(Unary<Out, Pre>),
            Postfix(Unary<Out, Post>),
            Binary(Binary<Out, B>),
        }

        impl<Pre, Post, B, Out> std::fmt::Debug for Item<Pre, Post, B, Out>
        where
            Pre: FnOnce(Out) -> Out,
            Post: FnOnce(Out) -> Out,
            B: FnOnce(Out, Out) -> Out,
            Out: std::fmt::Debug,
        {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    Self::Value(_) => f.debug_tuple("Value").finish_non_exhaustive(),
                    Self::Prefix(op) => f.debug_tuple("Prefix").field(op).finish(),
                    Self::Postfix(op) => f.debug_tuple("Postfix").field(op).finish(),
                    Self::Binary(op) => f.debug_tuple("Binary").field(op).finish(),
                }
            }
        }

        fn operation<Pre, Post, B, Out>(
            stack: &[Item<Pre, Post, B, Out>],
            index: usize,
        ) -> Option<(u8, Range<usize>)>
        where
            Pre: FnOnce(Out) -> Out,
            Post: FnOnce(Out) -> Out,
            B: FnOnce(Out, Out) -> Out,
            Out: std::fmt::Debug,
        {
            debug!(target: "parser::with_operators", "checking precedence at index {index} in stack: {stack:?}");
            match stack.get(index)? {
                Item::Prefix(op) => Some((op.precedence, index..index + 2)),
                Item::Postfix(op) => Some((op.precedence, index - 1..index + 1)),
                Item::Binary(op) => Some((op.precedence, index - 1..index + 2)),
                Item::Value(_) if index == 0 => operation(stack, index + 1),
                Item::Value(_) => {
                    match (operation(stack, index - 1), operation(stack, index + 1)) {
                        (None, None) => None,
                        (Some(x), None) => Some(x),
                        (None, Some(y)) => Some(y),
                        (Some(x), Some(y)) if x.0 > y.0 => Some(x),
                        (Some(x), Some(y)) if x.0 < y.0 => Some(y),
                        (Some(x), Some(_)) => Some(x),
                    }
                }
            }
        }
        let mut prefix = prefix.optional();
        let mut prefixes =
            move |stack: &mut Vec<Item<Pre, Post, B, Out>>, ctx: &mut Context| -> Output<()> {
                while let Some(op) = prefix.parse(ctx)? {
                    stack.push(Item::Prefix(op));
                }
                Output::Ok(())
            };
        let mut postfix = postfix.optional();
        let mut postfixes =
            move |stack: &mut Vec<Item<Pre, Post, B, Out>>, ctx: &mut Context| -> Output<()> {
                while let Some(op) = postfix.parse(ctx)? {
                    stack.push(Item::Postfix(op));
                }
                Output::Ok(())
            };
        pub fn apply<Pre, Post, B, Out>(
            stack: &mut Vec<Item<Pre, Post, B, Out>>,
            index: Range<usize>,
        ) -> Option<()>
        where
            Pre: FnOnce(Out) -> Out,
            Post: FnOnce(Out) -> Out,
            B: FnOnce(Out, Out) -> Out,
            Out: std::fmt::Debug,
        {
            let mut iter = stack.drain(index.clone());
            let operands = (iter.next(), iter.next(), iter.next());
            assert!(iter.next().is_none());
            drop(iter);
            match operands {
                (Some(Item::Value(v)), None, None) => {
                    stack.insert(index.start, Item::Value(v));
                    Some(())
                }
                (Some(Item::Prefix(op)), Some(Item::Value(v)), None) => {
                    stack.insert(index.start, Item::Value((op.f)(v)));
                    Some(())
                }
                (Some(Item::Value(v)), Some(Item::Postfix(op)), None) => {
                    stack.insert(index.start, Item::Value((op.f)(v)));
                    Some(())
                }
                (Some(Item::Value(lhs)), Some(Item::Binary(op)), Some(Item::Value(rhs))) => {
                    stack.insert(index.start, Item::Value((op.f)(lhs, rhs)));
                    Some(())
                }
                _ => panic!("Invalid operator application: {:#?}", operands),
            }
        }
        fn collapse<Pre, Post, B, Out>(
            stack: &mut Vec<Item<Pre, Post, B, Out>>,
            index: usize,
            continuos: bool,
        ) -> Option<usize>
        where
            Pre: FnOnce(Out) -> Out,
            Post: FnOnce(Out) -> Out,
            B: FnOnce(Out, Out) -> Out,
            Out: std::fmt::Debug,
        {
            let (precedence, range) = operation(stack, index)?;
            let (before, r0) = if range.start == 0 {
                None
            } else {
                operation(stack, range.start - 1)
            }
            .unwrap_or((u8::MAX, 0..0));
            let (after, r1) = if range.end >= stack.len() {
                None
            } else {
                operation(stack, range.end)
            }
            .unwrap_or((if continuos { 0 } else { u8::MAX }, 0..0));
            debug!(target: "parser::with_operators", "collapse at index {index} with precedence {precedence}, before = {before:?}, after = {after:?}, range = {range:?}, r0 = {r0:?}, r1 = {r1:?}, stack = {stack:?}");
            if precedence < before && precedence <= after {
                apply(stack, range.clone());
                Some(range.len())
            } else {
                let delta = if before <= precedence {
                    collapse(stack, r0.start - 1, continuos)?
                } else {
                    0
                };
                let delta2 = if after < precedence {
                    collapse(stack, r1.start - delta + 1, continuos)?
                } else {
                    0
                };
                let sum = delta + delta2;
                debug!(target: "parser::with_operators", "after collapsing neighbors, sum = {sum}, range={range:?}, stack = {stack:?}");
                apply(stack, range.start..range.end);
                Some(range.len() + sum)
            }
        }
        move |ctx: &mut Context| -> Output<Out> {
            debug_ctx(ctx, "parser::with_operators", "beginning");
            let mut stack: Vec<Item<Pre, Post, B, Out>> = Vec::new();
            loop {
                debug!(target: "parser::with_operators", "begin loop");
                prefixes(&mut stack, ctx)?;
                debug!(target: "parser::with_operators", "after prefixes: {stack:?}");
                match self.parse(ctx) {
                    Output::Ok(ok) => stack.push(Item::Value(ok)),
                    Output::Error(err) => return Output::Error(err),
                    Output::Fatal(fatal) => return Output::Fatal(fatal),
                };
                debug!(target: "parser::with_operators", "after value: {stack:?}");
                postfixes(&mut stack, ctx)?;
                debug!(target: "parser::with_operators", "after postfixes: {stack:?}");
                match binary.parse(ctx) {
                    Output::Ok(op) => stack.push(Item::Binary(op)),
                    Output::Error(_) => break,
                    Output::Fatal(fatal) => return Output::Fatal(fatal),
                }
                debug!(target: "parser::with_operators", "after binary: {stack:?}");
            }
            while !stack.is_empty() && collapse(&mut stack, 0, false).is_some() {}
            debug!(target: "parser::with_operators", "final stack: {stack:?}");
            if let Item::Value(v) = stack.pop().unwrap() {
                Output::Ok(v)
            } else {
                Output::Error(Error::ExpectedFound {
                    location: ctx.span(),
                    expected: expected.clone(),
                    found: ctx.found(),
                })
            }
        }
    }

    fn seperated_by(mut self, seperator: Symbol) -> impl Parser<Vec<Out>> {
        move |ctx: &mut Context| {
            let mut output = Vec::new();
            let mut cont = true;
            while cont {
                match self.parse(ctx) {
                    Output::Ok(ok) => output.push(ok),
                    Output::Error(_) => cont = false,
                    Output::Fatal(fatal) => return Output::Fatal(fatal),
                }
                match ctx.advance() {
                    Some(Token {
                        lexeme: Lexeme::Symbol(sym),
                        ..
                    }) if *sym == seperator => {}
                    None => cont = false,
                    Some(token) => {
                        return Output::Error(Error::ExpectedFound {
                            location: token.span.clone(),
                            expected: Expected::Symbol(seperator),
                            found: Found::Token(token.clone()),
                        });
                    }
                }
            }
            Output::Ok(output)
        }
    }

    fn fatal(mut self) -> impl Parser<Out> {
        move |ctx: &mut Context| match self.parse(ctx) {
            Output::Ok(ok) => Output::Ok(ok),
            Output::Error(err) => Output::Fatal(err),
            Output::Fatal(fatal) => Output::Fatal(fatal),
        }
    }

    fn and_then<O>(mut self, mut parser: impl Parser<O>) -> impl Parser<(Out, O)> {
        move |ctx: &mut Context| {
            let first = self.parse(ctx)?;
            let second = parser.parse(ctx)?;
            Output::Ok((first, second))
        }
    }

    fn and_ignore<O>(mut self, mut parser: impl Parser<O>) -> impl Parser<Out> {
        move |ctx: &mut Context| {
            let first = self.parse(ctx)?;
            let _ = parser.parse(ctx)?;
            Output::Ok(first)
        }
    }

    fn ignore_then<O>(mut self, mut parser: impl Parser<O>) -> impl Parser<O> {
        move |ctx: &mut Context| {
            let _ = self.parse(ctx)?;
            let second = parser.parse(ctx)?;
            Output::Ok(second)
        }
    }

    fn map<O>(mut self, mut f: impl FnMut(Out) -> O) -> impl Parser<O> {
        move |ctx: &mut Context| match self.parse(ctx) {
            Output::Ok(ok) => Output::Ok(f(ok)),
            Output::Error(err) => Output::Error(err),
            Output::Fatal(fatal) => Output::Fatal(fatal),
        }
    }

    fn reverse_on_fail(mut self) -> impl Parser<Out> {
        move |ctx: &mut Context| {
            let start_index = ctx.index;
            match self.parse(ctx) {
                Output::Ok(ok) => Output::Ok(ok),
                Output::Error(err) => {
                    ctx.index = start_index;
                    Output::Error(err)
                }
                Output::Fatal(fatal) => Output::Fatal(fatal),
            }
        }
    }

    fn map_err(self, mut f: impl FnMut(Error<'static>) -> Error<'static>) -> impl Parser<Out> {
        let mut parser = self;
        move |ctx: &mut Context| match parser.parse(ctx) {
            Output::Ok(ok) => Output::Ok(ok),
            Output::Error(err) => Output::Error(f(err)),
            Output::Fatal(fatal) => Output::Fatal(f(fatal)),
        }
    }

    fn map_error(self, mut f: impl FnMut(Error<'static>) -> Error<'static>) -> impl Parser<Out> {
        let mut parser = self;
        move |ctx: &mut Context| match parser.parse(ctx) {
            Output::Ok(ok) => Output::Ok(ok),
            Output::Error(err) => Output::Error(f(err)),
            Output::Fatal(fatal) => Output::Fatal(fatal),
        }
    }

    fn map_error_with_span(
        self,
        mut f: impl FnMut(Error<'static>, Span) -> Error<'static>,
    ) -> impl Parser<Out> {
        let mut parser = self;
        move |ctx: &mut Context| {
            let start_span = ctx.span();
            match parser.parse(ctx) {
                Output::Ok(ok) => Output::Ok(ok),
                Output::Error(err) => Output::Error(f(err, start_span + ctx.span())),
                Output::Fatal(fatal) => Output::Fatal(fatal),
            }
        }
    }

    fn map_error_with_ctx(
        self,
        mut f: impl FnMut(Error<'static>, &mut Context) -> Error<'static>,
    ) -> impl Parser<Out> {
        let mut parser = self;
        move |ctx: &mut Context| match parser.parse(ctx) {
            Output::Ok(ok) => Output::Ok(ok),
            Output::Error(err) => Output::Error(f(err, ctx)),
            Output::Fatal(fatal) => Output::Fatal(fatal),
        }
    }

    fn map_with_span<O>(mut self, mut f: impl FnMut(Out, Span) -> O) -> impl Parser<O> {
        move |ctx: &mut Context| {
            let start_span = ctx.span();
            match self.parse(ctx) {
                Output::Ok(ok) => Output::Ok(f(ok, start_span + ctx.span())),
                Output::Error(err) => Output::Error(err),
                Output::Fatal(fatal) => Output::Fatal(fatal),
            }
        }
    }

    fn optional(self) -> impl Parser<Option<Out>> {
        let mut parser = self.reverse_on_fail();
        move |ctx: &mut Context| match parser.parse(ctx) {
            Output::Ok(ok) => Output::Ok(Some(ok)),
            Output::Error(_) => Output::Ok(None),
            Output::Fatal(fatal) => Output::Fatal(fatal),
        }
    }

    /// opener: '(' | '[' | '{'
    fn in_group(mut self, opener: char, expected: Expected) -> impl Parser<Out> {
        move |ctx: &mut Context| {
            let contents = match ctx.next(expected.clone())? {
                Token {
                    lexeme: Lexeme::Group(group),
                    span,
                } => match (opener, group) {
                    ('(', Group::Parentheses(contents))
                    | ('[', Group::Brackets(contents))
                    | ('{', Group::Braces(contents)) => contents,
                    (_, _) => {
                        return Output::Error(Error::ExpectedFound {
                            location: span.clone(),
                            expected: expected.clone(),
                            found: Found::Token(Token {
                                lexeme: Lexeme::Group(group.clone()),
                                span: span.clone(),
                            }),
                        });
                    }
                },
                i => {
                    return Output::Error(Error::ExpectedFound {
                        location: i.span.clone(),
                        expected: expected.clone(),
                        found: Found::Token(i.clone()),
                    });
                }
            };
            let child_ctx = Context::new(contents).unwrap();
            let out = child_ctx.parse(&mut self);
            match out.map_err(|errs| errs.into_boxed_slice()) {
                Ok(ok) => Output::Ok(ok),
                Err(box [ref errs @ .., ref last]) => {
                    ctx.errors.extend(errs.iter().cloned());
                    Output::Error(last.clone())
                }
                Err(box []) => unreachable!(),
            }
        }
    }
}

pub trait DebugParser<Out>
where
    Out: std::fmt::Debug,
    Self: Parser<Out>,
{
    #[track_caller]
    fn debug(mut self, name: &'static str) -> impl Parser<Out> {
        let caller = std::panic::Location::caller();
        move |ctx: &mut Context| {
            let out = self.parse(ctx);
            match &out {
                Output::Ok(ok) => {
                    debug!(
                        target: "parser",
                        "[{}:{}] Parser '{}' succeeded with output: {:?}",
                        caller.file(),
                        caller.line(),
                        name,
                        ok
                    );
                }
                Output::Error(err) => {
                    debug!(
                        target: "parser",
                        "[{}:{}] Parser '{}' failed with error: {:?}",
                        caller.file(),
                        caller.line(),
                        name,
                        err
                    );
                }
                Output::Fatal(fatal) => {
                    debug!(
                        target: "parser",
                        "[{}:{}] Parser '{}' fatally failed with error: {:?}",
                        caller.file(),
                        caller.line(),
                        name,
                        fatal
                    );
                }
            }
            debug_ctx(ctx, "parser", name);
            out
        }
    }
}

#[track_caller]
pub fn debug_ctx(ctx: &Context, target: &'static str, name: &str) {
    let caller = std::panic::Location::caller();
    debug!(target: target, "[{}:{}] Context state '{name}': {{next = {:?}, last = {:?}, index = {}}}",
        caller.file(),
        caller.line(),
        ctx.peek(),
        ctx.last(),
        ctx.index
    );
}

impl<T, Out> DebugParser<Out> for T
where
    Out: std::fmt::Debug,
    T: Parser<Out>,
{
}

pub fn symbol(symbol: Symbol) -> impl for<'a> FnMut(&'a mut Context) -> Output<()> {
    move |ctx: &mut Context| {
        let location = ctx.span();
        match ctx.advance() {
            Some(Token {
                lexeme: Lexeme::Symbol(sym),
                ..
            }) if *sym == symbol => Output::Ok(()),
            Some(i) => Output::Error(Error::ExpectedFound {
                location,
                expected: Expected::Symbol(symbol),
                found: Found::Token(i.clone()),
            }),
            None => Output::Error(Error::ExpectedFound {
                location: ctx.span(),
                expected: Expected::Symbol(symbol),
                found: Found::Eof,
            }),
        }
    }
}

pub fn never<Out>(_: &mut Context) -> Output<Out> {
    Output::Error(Error::internal())
}

pub fn keyword(keyword: Keyword) -> impl for<'a> FnMut(&'a mut Context) -> Output<()> {
    move |ctx: &mut Context| match ctx.advance() {
        Some(Token {
            lexeme: Lexeme::Keyword(kw),
            ..
        }) if *kw == keyword => Output::Ok(()),
        Some(i) => Output::Error(Error::ExpectedFound {
            location: i.span.clone(),
            expected: Expected::Keyword(keyword),
            found: Found::Token(i.clone()),
        }),
        None => Output::Error(Error::ExpectedFound {
            location: ctx.span(),
            expected: Expected::Keyword(keyword),
            found: Found::Eof,
        }),
    }
}

pub fn eof(ctx: &mut Context) -> Output<()> {
    if ctx.at_end() {
        Output::Ok(())
    } else {
        Output::Error(Error::ExpectedFound {
            location: ctx.span(),
            expected: Expected::Eof,
            found: Found::Token(ctx.peek().cloned().unwrap()),
        })
    }
}

impl<O, F> Parser<O> for F
where
    F: for<'a, 'b> FnMut(&'b mut Context<'a>) -> Output<O>,
{
    fn parse<'a, 'b>(&'b mut self, ctx: &'b mut Context<'a>) -> Output<O> {
        self(ctx)
    }
}

pub struct Or<T>(pub T);

macro_rules! impl_or {
    (@next $ident:ident) => {};
    (@next $first:ident $($ident:ident)*) => {
        impl_or!($($ident)*);
    };
    ($($ident:ident)*) => {
        impl<__O, $($ident,)*> Parser<__O> for Or<($($ident,)*)>
        where $($ident: Parser<__O>,)*
        {

            #[allow(non_snake_case)]
            fn parse(&mut self, ctx: &mut Context) -> Output<__O> {
                let mut error: Option<Output<__O>> = None;
                let Or(($($ident,)*)) = self;
                $(
                    {
                        let mut child = ctx.child();
                        let output = $ident.parse(&mut child);
                        match output {
                            Output::Ok(ok) => {
                                ctx.index = child.index;
                                ctx.errors.extend(child.errors.drain(0..));
                                return Output::Ok(ok)
                            }
                            Output::Error(err) if error.is_none() => error = Some(Output::Error(err)),
                            Output::Fatal(err) => return Output::Fatal(err),
                            _ => {},
                        }
                    }
                )*
                if let Some(err) = error {
                    err
                } else {
                    unreachable!()
                }
            }
        }
        impl_or!(@next $($ident)*);
    };
}

impl_or!(A B C D E F G H I J K L M N O P);

impl<T> std::ops::Try for Output<T> {
    type Residual = Output<!>;
    type Output = T;

    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self {
            Self::Ok(ok) => ControlFlow::Continue(ok),
            Self::Error(err) => ControlFlow::Break(Output::Error(err)),
            Self::Fatal(err) => ControlFlow::Break(Output::Fatal(err)),
        }
    }

    fn from_output(output: Self::Output) -> Self {
        Self::Ok(output)
    }
}

impl<T> std::ops::FromResidual<Output<!>> for Output<T> {
    fn from_residual(residual: <Self as std::ops::Try>::Residual) -> Self {
        match residual {
            Output::Error(err) => Output::Error(err),
            Output::Fatal(fatal) => Output::Fatal(fatal),
            _ => unreachable!(),
        }
    }
}

impl<T> Output<T> {
    pub fn is_ok(&self) -> bool {
        match self {
            Self::Ok(_) => true,
            Self::Error(_) | Self::Fatal(_) => false,
        }
    }

    pub fn fatal(self) -> Self {
        match self {
            Output::Ok(ok) => Output::Ok(ok),
            Output::Error(err) => Output::Fatal(err),
            Output::Fatal(fatal) => Output::Fatal(fatal),
        }
    }
}
