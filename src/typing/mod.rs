use std::{
    collections::HashMap,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering::Relaxed},
    },
};

use crate::{Error, Span, parser::Identifier};

static ID: AtomicUsize = AtomicUsize::new(0);

pub fn id() -> usize {
    ID.fetch_add(1, Relaxed)
}

#[derive(Debug, Default)]
pub struct Context<'a> {
    pub parent: Parent<'a>,
    pub types: HashMap<TyVar, Type>,
}

#[derive(Debug, Default)]
pub enum Parent<'a> {
    #[default]
    None,
    Scope(&'a mut Context<'a>),
    Outside(&'a Context<'a>),
}

#[derive(Debug)]
pub struct SpanContext<'a> {
    pub ctx: &'a mut Context<'a>,
    pub span: &'a Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub ty: Ty,
    pub state: State,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum TyVar {
    Named(Identifier),
    Anonymous(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    TyVar(TyVar),
    Integer,
    Float,
    Boolean,
    String,
    Function(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
    List(Box<Type>),
    Generic(TyVar, Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum State {
    Declared(Span),
    Eq(Span),
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
            parent: Parent::None,
        }
    }

    pub fn query(&self, var: &TyVar) -> Option<&Type> {
        self.types.get(var).or_else(|| match &self.parent {
            Parent::Scope(ctx) => ctx.query(var),
            Parent::Outside(ctx) => ctx.query(var),
            Parent::None => None,
        })
    }

    pub fn exists(&self, var: &TyVar) -> bool {
        self.types.contains_key(var)
            || match &self.parent {
                Parent::Scope(ctx) => ctx.exists(var),
                Parent::Outside(ctx) => ctx.exists(var),
                Parent::None => false,
            }
    }

    pub fn span<'b>(&'b mut self, span: &'b Span) -> SpanContext<'b>
    where
        'b: 'a,
    {
        SpanContext { ctx: self, span }
    }

    pub fn insert(&mut self, var: TyVar, ty: Type) -> &mut Type {
        self.types.entry(var).or_insert(ty)
    }
}

impl<'a> SpanContext<'a> {
    fn declare(&mut self, ty_var: TyVar) -> Result<&mut Self, Error<'static>> {
        if self.ctx.exists(&ty_var) {
            return Err(Error::internal());
        }
        self.ctx.insert(
            ty_var,
            Type {
                ty: Ty::TyVar(TyVar::Anonymous(id())),
                state: State::Declared(self.span.clone()),
            },
        );
        Ok(self)
    }

    fn unify(&mut self, a: &Type, b: &Type) -> Result<&mut Self, Error<'static>> {
        match (&a.ty, &b.ty) {
            (ref a, ref b) if a == b => Ok(self),
            _ => todo!(),
        }
    }

    fn eq(&mut self, _: &TyVar, _: &TyVar) -> Result<&mut Self, Error<'static>> {
        todo!()
    }
}

pub fn test() -> Result<(), Error<'static>> {
    let mut ctx = Context::new();
    let span = Span::new("test.stream", 0..0);
    let x = TyVar::Named(Identifier::new(Arc::from("x"), span.clone()));
    let y = TyVar::Named(Identifier::new(Arc::from("y"), span.clone()));
    ctx.span(&span).declare(x)?.declare(y)?;
    Ok(())
}

/*
let x = 10
tyx.span(..).declare(x)?
tyx.span(..).set(x, Type::Integer)?
tyx.query(x)? -> Type::Integer
*/
