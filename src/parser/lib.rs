use std::ops::{Deref, DerefMut};

use logos::Lexer;

use crate::{
    Span, Term,
    error::{Error, Expected, Found},
    parser::{Parsed, lexer::Token},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Output<T> {
    Ok(T),
    Error(Error),
    Fatal(Error),
}

#[derive(Debug, Clone)]
pub struct ParseContext<'a> {
    pub lexer: Lexer<'a, Token>,
    /// (span, open, close)
    pub delimiters: Vec<(Span, Token, Token)>,
}

impl<'a> ParseContext<'a> {
    #[must_use = "ParseContext should not be created without being used"]
    pub fn new(lexer: Lexer<'a, Token>) -> Self {
        Self {
            lexer,
            delimiters: Vec::new(),
        }
    }

    #[must_use = "Pure function, non use calls should be removed"]
    pub fn extras(&self) -> &'static str {
        self.lexer.extras
    }

    #[must_use = "Pure function, non use calls should be removed"]
    pub fn span(&self) -> std::ops::Range<usize> {
        self.lexer.span()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub item: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn into_inner(self) -> T {
        self.item
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.item
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.item
    }
}

macro_rules! statemachine {
    (
        $(<$($generic:ident = $ty:ty),* $(,)?>)?
        $start:ident => {
            $(
                $spattern:pat $(if ($($sif:tt)*))? => $saction:expr
            ),* $(,)*
        } $(,)?
        $(
            $state:ident $({
                $($field:ident : $ftype:ty),* $(,)*
            })? $(($($ttype:ty),* $(,)?))? => {
                $(
                    $pattern:pat $(if ($($if:tt)*))? => $action:expr
                ),* $(,)*
            }
        ),* $(,)?
    ) => {
        {
            enum State $(<$($generic),*>)? {
                $start,
                $(
                    $state $( { $($field : $ftype),* } )? $( ( $($ttype),* ) )?,
                )*
            }

            use State::*;
            let mut state: State $(<$($ty),*>)? = $start;

            loop {
                state = match state {
                    $start => {
                        match state {
                            $(
                                $spattern $(if $($sif)*)? => $saction,
                            )*
                            _ => unreachable!("unreachable state in statemachine! macro at {}, with state: <no dbg>", stringify!($start)),
                        }
                    }
                    $(
                        $state $( { $($field),* } )? $( ( $( ${ignore($ttype)} _),* ) )? => {
                            match state {
                                $(
                                    $pattern $(if $($if)*)? => $action,
                                )*
                                _ => unreachable!("unreachable state in statemachine! macro at {}, with state: <no dbg>", stringify!($state)),
                            }
                        }
                    )*
                };
            }
        }
    };
}

pub trait Parser {
    type Output;
    fn parse<'b, 'a>(&self, px: &'b mut ParseContext<'a>) -> Output<Self::Output>;

    fn spanned(self) -> impl Parser<Output = Spanned<Self::Output>> + Sized
    where
        Self: Sized,
    {
        move |px: &mut ParseContext<'_>| {
            let start = {
                let mut lex = px.lexer.clone();
                lex.next();
                lex.span().start
            };
            let item = self.parse(px)?;
            let end = px.span().end;
            let span = Span::new(px.extras(), start..end);
            Output::Ok(Spanned { item, span })
        }
    }

    fn optional(self) -> impl Parser<Output = Option<Self::Output>> + Sized
    where
        Self: Sized,
    {
        move |px: &mut ParseContext<'_>| {
            let clone = px.lexer.clone();
            let lexer = std::mem::replace(&mut px.lexer, clone);
            let clone = px.delimiters.clone();
            let delimiters = std::mem::replace(&mut px.delimiters, clone);
            match self.parse(px) {
                Output::Ok(t) => Output::Ok(Some(t)),
                Output::Error(_) => {
                    px.lexer = lexer;
                    px.delimiters = delimiters;
                    Output::Ok(None)
                }
                Output::Fatal(e) => {
                    px.lexer = lexer;
                    px.delimiters = delimiters;
                    Output::Fatal(e)
                }
            }
        }
    }

    fn r#backtrack(self) -> impl Parser<Output = Self::Output> + Sized
    where
        Self: Sized,
    {
        move |px: &mut ParseContext<'_>| {
            let clone = px.lexer.clone();
            let lexer = std::mem::replace(&mut px.lexer, clone);
            let clone = px.delimiters.clone();
            let delimiters = std::mem::replace(&mut px.delimiters, clone);
            match self.parse(px) {
                Output::Ok(t) => Output::Ok(t),
                Output::Error(e) => {
                    px.lexer = lexer;
                    px.delimiters = delimiters;
                    Output::Error(e)
                }
                Output::Fatal(e) => {
                    px.lexer = lexer;
                    px.delimiters = delimiters;
                    Output::Fatal(e)
                }
            }
        }
    }

    fn r#ref(&self) -> impl Parser<Output = Self::Output> + '_
    where
        Self: Sized,
    {
        move |px: &mut ParseContext<'_>| self.parse(px)
    }

    fn seperated_by(self, token: Token) -> impl Parser<Output = Vec<Self::Output>>
    where
        Self: Sized,
    {
        move |px: &mut ParseContext<'_>| {
            let mut items = Vec::new();
            match self.parse(px) {
                Output::Ok(t) => items.push(t),
                Output::Error(_) => return Output::Ok(items),
                Output::Fatal(e) => return Output::Fatal(e),
            }
            while let Output::Ok(_) = just(token.clone()).backtrack().parse(px) {
                match self.parse(px) {
                    Output::Ok(t) => items.push(t),
                    Output::Error(_) => return Output::Ok(items),
                    Output::Fatal(e) => return Output::Fatal(e),
                }
            }
            Output::Ok(items)
        }
    }

    fn delimited_by(self, open: Token, close: Token) -> impl Parser<Output = Self::Output> + Sized
    where
        Self: Sized,
    {
        let open_token = open.clone();
        let close_token = close.clone();
        let open = just(open).spanned();
        move |px: &mut ParseContext<'_>| {
            let Spanned {
                span: open_span, ..
            } = open.parse(px)?;
            px.delimiters
                .push((open_span, open_token.clone(), close_token.clone()));
            let item = self.parse(px)?;
            match px.lexer.next() {
                None => {
                    return Output::Fatal(Error::Unclosed(
                        open_token.clone(),
                        Span::new(px.extras(), px.span()),
                    ));
                }
                Some(Ok(t)) if t == close_token => px.delimiters.pop(),
                Some(Ok(t)) if px.delimiters.iter().any(|(_, _, c)| *c == t) => {
                    let (m_opened, m_open_token, _) = px
                        .delimiters
                        .iter()
                        .rev()
                        .find(|(_, _, c)| *c == t)
                        .unwrap()
                        .clone();
                    let (e_opened, e_open_token, e_close_token) = px.delimiters.pop().unwrap();
                    let span = Span::new(px.extras(), px.span());
                    return Output::Fatal(Error::MismatchedClosing {
                        expected: Expected::from(&e_close_token),
                        opened: e_opened,
                        opened_token: e_open_token,
                        found: t,
                        found_span: span,
                        matched: m_open_token,
                        matched_span: m_opened,
                    });
                }
                Some(Ok(t)) => {
                    let expected = px.delimiters.pop().unwrap();
                    return Output::Fatal(Error::FoundExpected {
                        expected: Expected::from(&expected.2),
                        opened: expected.0,
                        opened_token: expected.1,
                        found: t,
                        found_span: Span::new(px.extras(), px.span()),
                    });
                }
                Some(Err(e)) => return Output::Fatal(e),
            };
            Output::Ok(item)
        }
    }

    fn delimited_sequence(
        self,
        open: Token,
        seperator: Token,
        close: Token,
    ) -> impl Parser<Output = Vec<Self::Output>> + Sized
    where
        Self: Sized,
    {
        self.seperated_by(seperator).delimited_by(open, close)
    }

    fn map<U, F>(self, f: F) -> impl Parser<Output = U> + Sized
    where
        Self: Sized,
        F: Fn(Self::Output) -> U,
    {
        move |px: &mut ParseContext<'_>| {
            let item = self.parse(px)?;
            Output::Ok(f(item))
        }
    }

    fn and_then<P>(self, p: P) -> impl Parser<Output = (Self::Output, P::Output)> + Sized
    where
        Self: Sized,
        P: Parser,
    {
        move |px: &mut ParseContext<'_>| {
            let first = self.parse(px)?;
            let second = p.parse(px)?;
            Output::Ok((first, second))
        }
    }

    fn and_ignore<P>(self, p: P) -> impl Parser<Output = Self::Output> + Sized
    where
        Self: Sized,
        P: Parser,
    {
        move |px: &mut ParseContext<'_>| {
            let first = self.parse(px)?;
            let _ = p.parse(px)?;
            Output::Ok(first)
        }
    }

    fn ignore_and<P>(self, p: P) -> impl Parser<Output = P::Output> + Sized
    where
        Self: Sized,
        P: Parser,
    {
        move |px: &mut ParseContext<'_>| {
            let _ = self.parse(px)?;
            let second = p.parse(px)?;
            Output::Ok(second)
        }
    }

    fn inspect(
        self,
        f: impl Fn(&Self::Output, &ParseContext),
    ) -> impl Parser<Output = Self::Output> + Sized
    where
        Self: Sized,
    {
        move |px: &mut ParseContext<'_>| {
            let item = self.parse(px)?;
            f(&item, px);
            Output::Ok(item)
        }
    }

    fn fatal(self) -> impl Parser<Output = Self::Output> + Sized
    where
        Self: Sized,
    {
        move |px: &mut ParseContext<'_>| match self.parse(px) {
            Output::Ok(t) => Output::Ok(t),
            Output::Error(e) | Output::Fatal(e) => Output::Fatal(e),
        }
    }

    fn recover_with<R>(self, r: R) -> impl Parser<Output = Self::Output> + Sized
    where
        Self: Sized,
        R: Recoverer<Self::Output>,
    {
        move |px: &mut ParseContext<'_>| match self.r#ref().backtrack().parse(px) {
            Output::Ok(t) => Output::Ok(t),
            Output::Error(e) | Output::Fatal(e) => r.recover(e, px),
        }
    }

    fn operators(
        self,
        prefix: impl Parser<Output = Prefix<Self::Output>>,
        binary: impl Parser<Output = Binary<Self::Output>>,
        postfix: impl Parser<Output = Postfix<Self::Output>>,
    ) -> impl Parser<Output = Self::Output>
    where
        Self: Sized,
    {
        move |px: &mut ParseContext| {
            let out = operators_with_min_binding_power(
                &self,
                &prefix.r#ref().spanned().optional(),
                &binary.r#ref().spanned().optional(),
                &postfix.r#ref().spanned().optional(),
                0,
                px,
            )?
            .unwrap();
            Output::Ok(out.item)
        }
    }

    fn repeated1(self) -> impl Parser<Output = Vec<Self::Output>> + Sized
    where
        Self: Sized,
    {
        move |px: &mut ParseContext<'_>| {
            let mut items = Vec::new();
            items.push(self.r#ref().parse(px)?);
            while let Some(t) = self.r#ref().optional().parse(px)? {
                items.push(t);
            }
            Output::Ok(items)
        }
    }
}

fn operators_with_min_binding_power<P>(
    this: &P,
    prefix: &impl Parser<Output = Option<Spanned<Prefix<P::Output>>>>,
    binary: &impl Parser<Output = Option<Spanned<Binary<P::Output>>>>,
    postfix: &impl Parser<Output = Option<Spanned<Postfix<P::Output>>>>,
    min: u8,
    px: &mut ParseContext<'_>,
) -> Output<Option<Spanned<P::Output>>>
where
    P: Parser + Sized,
{
    statemachine!(
        <O = P::Output>
        SPrefix => {
            SPrefix => match prefix.parse(px)? {
                Some(Spanned { item: Prefix { map, power }, span: lhs }) if (power >= min) => {
                    let Spanned { item, span: rhs } = match operators_with_min_binding_power(this, prefix, binary, postfix, power, px)? {
                        Some(i) => i,
                        None => this.r#ref().spanned().parse(px)?,
                    };
                    let next = (map)(item, lhs.clone() + rhs.clone());
                    Operand(Spanned { item: next, span: lhs.clone() + rhs.clone() })
                },
                Some(_) => return Output::Ok(None),
                None => Next,
            }
        },
        Next => {
            Next => Operand(this.r#ref().spanned().parse(px)?)
        },
        Operand(Spanned<O>) => {
            Operand(o) if (let Some(Spanned { item: Binary { map, power }, span: lhs }) = binary.parse(px)?) => {
                if power < min {
                    return Output::Ok(Some(o));
                }
                let Spanned { item, span: rhs } = match operators_with_min_binding_power(this, prefix, binary, postfix, power, px)? {
                    Some(i) => i,
                    None => this.r#ref().spanned().parse(px)?,
                };
                let next = (map)(o.item, item, lhs.clone() + rhs.clone());
                Operand(Spanned { item: next, span: lhs.clone() + rhs.clone() })
            },
            Operand(o) if (let Some(Spanned { item: Postfix { map, power }, span }) = postfix.parse(px)?) => {
                if power < min {
                    return Output::Ok(Some(o));
                }
                let next = (map)(o.item, span.clone() + o.span.clone());
                Operand(Spanned { item: next, span: span.clone() + o.span.clone() })
            },
            Operand(o) => return Output::Ok(Some(o)),
        }
    );
}

#[derive(Clone, Copy, Debug)]
pub struct Binary<O> {
    map: fn(O, O, Span) -> O,
    power: u8,
}

impl<O> Binary<O> {
    pub fn new(map: fn(O, O, Span) -> O, power: u8) -> Self {
        Self { map, power }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Prefix<O> {
    map: fn(O, Span) -> O,
    power: u8,
}

impl<O> Prefix<O> {
    pub fn new(map: fn(O, Span) -> O, power: u8) -> Self {
        Self { map, power }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Postfix<O> {
    map: fn(O, Span) -> O,
    power: u8,
}

impl<O> Postfix<O> {
    pub fn new(map: fn(O, Span) -> O, power: u8) -> Self {
        Self { map, power }
    }
}

pub trait Recoverer<O> {
    fn recover(&self, err: Error, px: &mut ParseContext) -> Output<O>;
}

impl<F, O> Recoverer<O> for F
where
    F: Fn(Error, &mut ParseContext<'_>) -> Output<O>,
{
    fn recover(&self, err: Error, px: &mut ParseContext) -> Output<O> {
        self(err, px)
    }
}

#[must_use = "Parsers do nothing unless you use them"]
pub fn error<T>(err: Error) -> impl Parser<Output = T> {
    move |_: &mut ParseContext<'_>| Output::Error(err.clone())
}

#[must_use = "Parsers do nothing unless you use them"]
pub fn okay<T>(t: T) -> impl Parser<Output = T>
where
    T: Clone,
{
    move |_: &mut ParseContext<'_>| Output::Ok(t.clone())
}

#[must_use = "Recoverers do nothing unless you use them"]
pub fn skip_delimited(open: Token, close: Token) -> impl Recoverer<Term<Parsed>> {
    move |err: Error, px: &mut ParseContext<'_>| {
        match just(open.clone()).parse(px) {
            Output::Ok(_) => {}
            Output::Error(_) => return Output::Error(err),
            Output::Fatal(e) => return Output::Fatal(e),
        }
        let mut depth = 1;
        loop {
            match px.lexer.next() {
                Some(Ok(t)) if t == open => depth += 1,
                Some(Ok(t)) if t == close => {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                Some(_) => {}
                None => {
                    return Output::Error(Error::ExpectedFound {
                        expected: Expected::from(&close),
                        found: Found::Eof,
                        span: Span::new(px.extras(), px.span()),
                    });
                }
            }
        }
        Output::Ok(Term::Error(err))
    }
}

impl<F, O> Parser for F
where
    F: Fn(&mut ParseContext<'_>) -> Output<O>,
{
    type Output = O;
    fn parse(&self, px: &mut ParseContext) -> Output<Self::Output> {
        self(px)
    }
}

pub fn token(px: &mut ParseContext<'_>, expected: Expected) -> Output<Token> {
    match px.lexer.next() {
        Some(Ok(t)) => Output::Ok(t),
        Some(Err(e)) => Output::Fatal(e),
        None => Output::Error(Error::ExpectedFound {
            expected,
            found: Found::Eof,
            span: Span::new(px.extras(), px.span()),
        }),
    }
}

#[must_use = "Parsers do nothing unless you use them"]
pub fn just(token: Token) -> impl Parser<Output = Token> {
    move |px: &mut ParseContext<'_>| match px.lexer.next() {
        Some(Ok(t)) if t == token => Output::Ok(t),
        Some(Ok(t)) => Output::Error(Error::exp_found(Expected::from(&token), t, px)),
        Some(Err(e)) => Output::Fatal(e),
        None => Output::Error(Error::ExpectedFound {
            expected: Expected::from(&token),
            found: Found::Eof,
            span: Span::new(px.extras(), px.span()),
        }),
    }
}

pub fn identifier(px: &mut ParseContext) -> Output<Spanned<String>> {
    match token(px, Expected::Identifier)? {
        Token::Identifier(name) => Output::Ok(Spanned {
            item: name,
            span: Span::new(px.extras(), px.span()),
        }),
        t => Output::Error(Error::exp_found(Expected::Identifier, t, px)),
    }
}

pub struct Or<T>(pub T);

#[macro_export]
macro_rules! impl_or {
    () => {};
    ($first:ident $($ident:ident)*) => {
        #[allow(non_snake_case)]
        impl<$first, $($ident,)*> Parser for Or<($first, $($ident,)*)>
        where
            $first: Parser,
            $($ident: Parser<Output = $first::Output>,)*
        {
            type Output = $first::Output;

            #[allow(non_snake_case)]
            fn parse(&self, px: &mut ParseContext) -> $crate::parser::lib::Output<Self::Output> {
                let (ref $first, $(ref $ident,)*) = self.0;
                let mut err = Error::Shrug;

                match ($first).r#ref().backtrack().parse(px) {
                    Output::Ok(t) => return Output::Ok(t),
                    Output::Error(e) => err = err + e,
                    Output::Fatal(e) => return Output::Fatal(e),
                }

                $(
                    match ($ident).r#ref().backtrack().parse(px) {
                        Output::Ok(t) => return Output::Ok(t),
                        Output::Error(e) => err = err + e,
                        Output::Fatal(e) => return Output::Fatal(e),
                    }
                )*

                Output::Error(err)
            }
        }
        impl_or!($($ident)*);
    };
}

impl_or!(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z);
