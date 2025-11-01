use crate::{
    Context, Error, Identifier, Normalized, Pattern, Term, Type, error::Spanned, parser::Parsed,
};
use std::{collections::HashMap, sync::Arc};

#[derive(Default)]
pub struct Namespace<'a> {
    pub parent: Option<&'a Namespace<'a>>,
    pub variables: HashMap<String, Identifier>,
}

impl Term<Parsed> {
    pub fn normalize(
        self,
        ctx: &mut Context,
        namespace: &mut Namespace,
    ) -> Result<Term<Normalized>, Vec<Error>> {
        match self {
            Term::Error(e) => Ok(Term::Error(e)),
            Term::Type(ty) => Ok(Term::Type(ty.normalize(ctx, namespace)?)),
            Term::Integer(i) => Ok(Term::Integer(i)),
            Term::Float(f) => Ok(Term::Float(f)),
            Term::List(l) => l
                .into_iter()
                .map(|t| {
                    let normalized = match t.map(|t| t.normalize(ctx, namespace)) {
                        Spanned { item: Ok(t), span } => Spanned { item: t, span },
                        Spanned { item: Err(e), .. } => return Err(e),
                    };
                    Ok(ctx.egraph.insert(normalized))
                })
                .collect::<Result<_, _>>()
                .map(Term::List),
            Term::Block(b) => {
                let mut namespace = Namespace {
                    parent: Some(namespace),
                    variables: HashMap::new(),
                };
                b.into_iter()
                    .map(|t| {
                        let normalized = match t.map(|t| t.normalize(ctx, &mut namespace)) {
                            Spanned { item: Ok(t), span } => Spanned { item: t, span },
                            Spanned { item: Err(e), .. } => return Err(e),
                        };
                        Ok(ctx.egraph.insert(normalized))
                    })
                    .collect::<Result<_, _>>()
                    .map(Term::Block)
            }
            Term::Function { pattern, body } => {
                let mut namespace = Namespace {
                    parent: Some(namespace),
                    variables: HashMap::new(),
                };
                let normalized_pattern = match pattern.map(|p| p.normalize(ctx, &mut namespace)) {
                    Spanned { item: Ok(p), span } => Spanned { item: p, span },
                    Spanned { item: Err(e), .. } => return Err(e),
                };
                let normalized_body = match body.map(|b| b.normalize(ctx, &mut namespace)) {
                    Spanned { item: Ok(b), span } => Spanned { item: b, span },
                    Spanned { item: Err(e), .. } => return Err(e),
                };
                Ok(Term::Function {
                    pattern: Arc::new(normalized_pattern),
                    body: ctx.egraph.insert(normalized_body),
                })
            }
            _ => unimplemented!("Normalization for this term is not yet implemented"),
        }
    }
}

impl Pattern<Parsed> {
    pub fn normalize(
        self,
        ctx: &mut Context,
        namespace: &mut Namespace,
    ) -> Result<Pattern<Normalized>, Vec<Error>> {
        match self {
            Pattern::Error(err) => Ok(Pattern::Error(err)),
            Pattern::Typed { pattern, ty } => {
                let normalized_pattern = match pattern.map(|p| p.normalize(ctx, namespace)) {
                    Spanned { item: Ok(p), span } => Spanned { item: p, span },
                    Spanned { item: Err(e), .. } => return Err(e),
                };
                let normalized_type = match ty.map(|t| t.normalize(ctx, namespace)) {
                    Spanned { item: Ok(t), span } => Spanned { item: t, span },
                    Spanned { item: Err(e), .. } => return Err(e),
                };
                Ok(Pattern::Typed {
                    pattern: Arc::new(normalized_pattern),
                    ty: ctx.egraph.insert(normalized_type),
                })
            }
            Pattern::Wildcard => Ok(Pattern::Wildcard),
            Pattern::Capture(name) => {
                if let Some(id) = namespace.variables.get(&name.item) {
                    Ok(Pattern::Capture(*id))
                } else {
                    let id = ctx.declare(name.item.clone());
                    namespace.variables.insert(name.item.clone(), id);
                    Ok(Pattern::Capture(id))
                }
            }
            Pattern::As { pattern, name } => {
                let normalized_pattern = match pattern.map(|p| p.normalize(ctx, namespace)) {
                    Spanned { item: Ok(p), span } => Spanned { item: p, span },
                    Spanned { item: Err(e), .. } => return Err(e),
                };
                let id = if let Some(id) = namespace.variables.get(&name.item) {
                    *id
                } else {
                    let id = ctx.declare(name.item.clone());
                    namespace.variables.insert(name.item.clone(), id);
                    id
                };
                Ok(Pattern::As {
                    pattern: Arc::new(normalized_pattern),
                    name: id,
                })
            }
            Pattern::Rest => Ok(Pattern::Rest),
            Pattern::List(l) => l
                .into_iter()
                .map(|p| {
                    let normalized = match p.map(|p| p.normalize(ctx, namespace)) {
                        Spanned { item: Ok(p), span } => Spanned { item: p, span },
                        Spanned { item: Err(e), .. } => return Err(e),
                    };
                    Ok(Arc::new(normalized))
                })
                .collect::<Result<_, _>>()
                .map(Pattern::List),
            Pattern::If { pattern, condition } => {
                let normalized_pattern = match pattern.map(|p| p.normalize(ctx, namespace)) {
                    Spanned { item: Ok(p), span } => Spanned { item: p, span },
                    Spanned { item: Err(e), .. } => return Err(e),
                };
                let normalized_condition = match condition.map(|c| c.normalize(ctx, namespace)) {
                    Spanned { item: Ok(c), span } => Spanned { item: c, span },
                    Spanned { item: Err(e), .. } => return Err(e),
                };
                Ok(Pattern::If {
                    pattern: Arc::new(normalized_pattern),
                    condition: ctx.egraph.insert(normalized_condition),
                })
            }
            Pattern::Integer(i) => Ok(Pattern::Integer(i)),
            Pattern::Float(f) => Ok(Pattern::Float(f)),
        }
    }
}

impl Type<Parsed> {
    pub fn normalize(
        self,
        _ctx: &mut Context,
        _namespace: &mut Namespace,
    ) -> Result<Type<Normalized>, Vec<Error>> {
        unimplemented!("Normalization for this type is not yet implemented")
    }
}
