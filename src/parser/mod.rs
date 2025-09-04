use crate::{
    Error, Span,
    arena::Arena,
    error::{Expected, Found},
    lexer::{Keyword, Lexeme, Symbol, Token},
    parser::lib::{
        Binary, Context, DebugParser, Or, Output, Parser, Unary, keyword, never, symbol,
    },
    typing::{TyVar, id},
};
use std::sync::Arc;

pub mod lib;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Identifier {
    identifier: Arc<str>,
    span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression<'a> {
    expr: Expr<'a>,
    span: Span,
    r#type: TyVar,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Variable(Identifier),
    Literal(Literal),
    Tuple(Vec<Expression<'a>, &'a Arena>),
    List(Vec<Expression<'a>, &'a Arena>),
    Function(Identifier, Option<Type>, &'a Expression<'a>),
    Application(&'a Expression<'a>, &'a Expression<'a>),
    Let(
        Identifier,
        Option<Type>,
        &'a Expression<'a>,
        &'a Expression<'a>,
    ),
    BinOp(BinOp, &'a Expression<'a>, &'a Expression<'a>),
    If(
        &'a Expression<'a>,
        &'a Expression<'a>,
        Option<&'a Expression<'a>>,
    ),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(Arc<str>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add(Span),
    Sub(Span),
    Multiply(Span),
    Divide(Span),
    Greater(Span),
    Lesser(Span),
    Equal(Span),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    ty: Ty,
    span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Named(Identifier),
    Function(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
    List(Box<Type>),
    TypeVar(usize),
}

impl Expression {
    fn new(expr: Expr, span: Span) -> Self {
        Self {
            expr,
            span,
            r#type: TyVar::Anonymous(id()),
        }
    }
}

impl Type {
    fn new(ty: Ty, span: Span) -> Self {
        Type { ty, span }
    }
}

impl Identifier {
    pub fn name(&self) -> &str {
        &self.identifier
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn new(identifier: Arc<str>, span: Span) -> Self {
        Self { identifier, span }
    }
}

fn identifier(ctx: &mut Context) -> Output<Identifier> {
    let location = ctx.span();
    match ctx.advance() {
        Some(Token {
            lexeme: Lexeme::Identifier(ident),
            span,
        }) => Output::Ok(Identifier {
            identifier: Arc::clone(ident),
            span: span.clone(),
        }),
        Some(i) => Output::Error(Error::ExpectedFound {
            location,
            expected: Expected::Identifier,
            found: Found::Token(i.clone()),
        }),
        None => Output::Error(Error::ExpectedFound {
            location,
            expected: Expected::Identifier,
            found: Found::Eof,
        }),
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Expression, Vec<Error<'static>>> {
    let ctx = match Context::new(&tokens) {
        Ok(ctx) => ctx,
        Err(()) => return Err(vec![Error::internal()]),
    };
    ctx.parse(&mut expression)
}

fn expression(ctx: &mut Context) -> Output<Expression> {
    primitive
        .with_operators(
            never::<Unary<Expression>>,
            primitive.map(|primitive| {
                Unary::new(0, move |function: Expression| Expression {
                    span: primitive.span.clone() + function.span.clone(),
                    expr: Expr::Application(Box::new(function), Box::new(primitive)),
                    r#type: TyVar::Anonymous(id()),
                })
            }),
            {
                let greater =
                    symbol(Symbol::Greater).map_with_span(|_, span| (3, BinOp::Greater(span)));
                let lesser =
                    symbol(Symbol::Lesser).map_with_span(|_, span| (3, BinOp::Lesser(span)));
                let equal = symbol(Symbol::Equal).map_with_span(|_, span| (3, BinOp::Equal(span)));
                let add = symbol(Symbol::Plus).map_with_span(|_, span| (2, BinOp::Add(span)));
                let sub = symbol(Symbol::Minus).map_with_span(|_, span| (2, BinOp::Sub(span)));
                let multiply =
                    symbol(Symbol::Multiply).map_with_span(|_, span| (1, BinOp::Multiply(span)));
                let divide =
                    symbol(Symbol::Divide).map_with_span(|_, span| (1, BinOp::Divide(span)));
                Or((greater, lesser, equal, add, sub, multiply, divide))
                    .map(|(precedence, bin_op)| {
                        Binary::new(
                            precedence,
                            Box::new(move |left: Expression, right: Expression| Expression {
                                span: left.span.clone() + right.span.clone(),
                                expr: Expr::BinOp(bin_op, Box::new(left), Box::new(right)),
                                r#type: TyVar::Anonymous(id()),
                            }),
                        )
                    })
                    .debug("binary operator")
            },
            Expected::Expression,
        )
        .debug("expression")
        .parse(ctx)
}

fn primitive(ctx: &mut Context) -> Output<Expression> {
    ctx.or((function, r#let, block, list, tuple, r#if, variable, literal))
}

fn function(ctx: &mut Context) -> Output<Expression> {
    symbol(Symbol::Lambda)
        .ignore_then(identifier.fatal())
        .and_then(
            symbol(Symbol::Colon)
                .ignore_then(type_primitive.fatal())
                .optional(),
        )
        .and_ignore(symbol(Symbol::Arrow).fatal())
        .and_then(expression.fatal())
        .map(|((identifier, r#type), body)| Expr::Function(identifier, r#type, Box::new(body)))
        .map_with_span(Expression::new)
        .parse(ctx)
}

fn r#let(ctx: &mut Context) -> Output<Expression> {
    keyword(Keyword::Let)
        .ignore_then(identifier.fatal())
        .and_then(symbol(Symbol::Colon).ignore_then(r#type.fatal()).optional())
        .and_ignore(symbol(Symbol::Equal).fatal())
        .and_then(expression.fatal())
        .and_ignore(keyword(Keyword::In).fatal())
        .and_then(expression.fatal())
        .map(|(((identifier, r#type), value), body)| {
            Expr::Let(identifier, r#type, Box::new(value), Box::new(body))
        })
        .map_with_span(Expression::new)
        .parse(ctx)
}

fn tuple(ctx: &mut Context) -> Output<Expression> {
    expression
        .seperated_by(Symbol::Comma)
        .in_group('(', Expected::Tuple)
        .map(Expr::Tuple)
        .map_with_span(Expression::new)
        .parse(ctx)
}

fn list(ctx: &mut Context) -> Output<Expression> {
    expression
        .seperated_by(Symbol::Comma)
        .in_group('[', Expected::List)
        .map(Expr::List)
        .map_with_span(Expression::new)
        .parse(ctx)
}

fn block(ctx: &mut Context) -> Output<Expression> {
    expression.in_group('{', Expected::Block).parse(ctx)
}

fn r#if(ctx: &mut Context) -> Output<Expression> {
    keyword(Keyword::If)
        .ignore_then(expression.fatal())
        .and_ignore(keyword(Keyword::Then).fatal())
        .and_then(expression.fatal())
        .and_then(
            keyword(Keyword::Else)
                .ignore_then(expression.fatal())
                .optional(),
        )
        .map(|((cond, then), otherwise)| {
            Expr::If(Box::new(cond), Box::new(then), otherwise.map(Box::new))
        })
        .map_with_span(Expression::new)
        .parse(ctx)
}

fn literal(ctx: &mut Context) -> Output<Expression> {
    let location = ctx.span();
    match ctx.advance() {
        Some(Token { lexeme, span }) => match lexeme {
            Lexeme::Integer(i) => Output::Ok(Expression::new(
                Expr::Literal(Literal::Integer(*i)),
                span.clone(),
            )),
            Lexeme::Float(f) => Output::Ok(Expression::new(
                Expr::Literal(Literal::Float(*f)),
                span.clone(),
            )),
            Lexeme::Keyword(Keyword::True) => Output::Ok(Expression::new(
                Expr::Literal(Literal::Boolean(true)),
                span.clone(),
            )),
            Lexeme::Keyword(Keyword::False) => Output::Ok(Expression::new(
                Expr::Literal(Literal::Boolean(false)),
                span.clone(),
            )),
            Lexeme::String(s) => Output::Ok(Expression::new(
                Expr::Literal(Literal::String(Arc::clone(s))),
                span.clone(),
            )),
            _ => Output::Error(Error::ExpectedFound {
                location,
                expected: Expected::Literal,
                found: Found::Token(Token {
                    lexeme: lexeme.clone(),
                    span: span.clone(),
                }),
            }),
        },
        None => Output::Error(Error::ExpectedFound {
            location,
            expected: Expected::Literal,
            found: Found::Eof,
        }),
    }
}

fn variable(ctx: &mut Context) -> Output<Expression> {
    identifier
        .map(Expr::Variable)
        .map_with_span(Expression::new)
        .parse(ctx)
}

fn r#type(ctx: &mut Context) -> Output<Type> {
    type_primitive
        .with_operators(
            never::<Unary<Type>>,
            never::<Unary<Type>>,
            symbol(Symbol::Arrow)
                .map_with_span(|_, span| (1, span))
                .map(|(precedence, _)| {
                    Binary::new(
                        precedence,
                        Box::new(move |left: Type, right: Type| Type {
                            span: left.span.clone() + right.span.clone(),
                            ty: Ty::Function(Box::new(left), Box::new(right)),
                        }),
                    )
                }),
            Expected::Type,
        )
        .map_error_with_ctx(|_, ctx| Error::ExpectedFound {
            location: ctx.span(),
            expected: Expected::Type,
            found: Found::Token(match ctx.next(Expected::Type) {
                Output::Ok(token) => token.clone(),
                Output::Error(err) => return err,
                Output::Fatal(err) => return err,
            }),
        })
        .debug("type")
        .parse(ctx)
}

fn type_primitive(ctx: &mut Context) -> Output<Type> {
    Or((tuple_type, list_type, block_type, named_type)).parse(ctx)
}

fn tuple_type(ctx: &mut Context) -> Output<Type> {
    r#type
        .seperated_by(Symbol::Comma)
        .in_group('(', Expected::Tuple)
        .map(Ty::Tuple)
        .map_with_span(Type::new)
        .parse(ctx)
}

fn list_type(ctx: &mut Context) -> Output<Type> {
    r#type
        .in_group('[', Expected::List)
        .map(Box::new)
        .map(Ty::List)
        .map_with_span(Type::new)
        .parse(ctx)
}

fn block_type(ctx: &mut Context) -> Output<Type> {
    r#type.in_group('{', Expected::Type).parse(ctx)
}

fn named_type(ctx: &mut Context) -> Output<Type> {
    identifier
        .map(Ty::Named)
        .map_with_span(Type::new)
        .parse(ctx)
}
