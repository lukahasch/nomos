use crate::{Pattern, Term, Type, error::Error, parser::Parsed};

pub trait ExtractError: Sized {
    fn contains_error(&self) -> bool;
    fn collect_errors_into(self, errors: &mut Vec<Error>);
    fn collect_errors(self) -> Vec<Error> {
        let mut errors = Vec::new();
        self.collect_errors_into(&mut errors);
        errors
    }
}

impl ExtractError for Term<Parsed> {
    fn contains_error(&self) -> bool {
        match self {
            Term::Error(_) => true,
            Term::Type(ty) => ty.contains_error(),
            Term::List(a) => a.iter().any(|v| v.contains_error()),
            Term::Block(a) => a.iter().any(|v| v.contains_error()),
            Term::Application { function, argument } => {
                function.contains_error() || argument.contains_error()
            }
            Term::Function { pattern, body } => pattern.contains_error() || body.contains_error(),
            Term::Let {
                pattern,
                value,
                body,
            } => {
                pattern.contains_error()
                    || value.contains_error()
                    || body.as_ref().is_some_and(|b| b.contains_error())
            }
            Term::Define {
                pattern,
                value,
                body,
            } => {
                pattern.contains_error()
                    || value.contains_error()
                    || body.as_ref().is_some_and(|b| b.contains_error())
            }
            Term::If {
                condition,
                then,
                r#else,
            } => condition.contains_error() || then.contains_error() || r#else.contains_error(),
            Term::Match { value, branches } => {
                value.contains_error()
                    || branches
                        .iter()
                        .any(|(p, t)| p.contains_error() || t.contains_error())
            }
            Term::Integer(_)
            | Term::Float(_)
            | Term::Variable(_)
            | Term::Inference(_)
            | Term::LangItem(_) => false,
        }
    }

    fn collect_errors_into(self, errors: &mut Vec<Error>) {
        match self {
            Term::Error(e) => errors.push(e.clone()),
            Term::Type(ty) => ty.collect_errors_into(errors),
            Term::List(a) => a
                .into_iter()
                .for_each(|v| v.into_inner().collect_errors_into(errors)),
            Term::Block(a) => a
                .into_iter()
                .for_each(|v| v.into_inner().collect_errors_into(errors)),
            Term::Application { function, argument } => {
                function.into_inner().collect_errors_into(errors);
                argument.into_inner().collect_errors_into(errors);
            }
            Term::Function { pattern, body } => {
                pattern.into_inner().collect_errors_into(errors);
                body.into_inner().collect_errors_into(errors);
            }
            Term::Let {
                pattern,
                value,
                body,
            }
            | Term::Define {
                pattern,
                value,
                body,
            } => {
                pattern.into_inner().collect_errors_into(errors);
                value.into_inner().collect_errors_into(errors);
                if let Some(b) = body {
                    b.into_inner().collect_errors_into(errors);
                }
            }
            Term::If {
                condition,
                then,
                r#else,
            } => {
                condition.into_inner().collect_errors_into(errors);
                then.into_inner().collect_errors_into(errors);
                r#else.into_inner().collect_errors_into(errors);
            }
            Term::Match { value, branches } => {
                value.into_inner().collect_errors_into(errors);
                for (p, t) in branches {
                    p.into_inner().collect_errors_into(errors);
                    t.into_inner().collect_errors_into(errors);
                }
            }
            Term::Integer(_)
            | Term::Float(_)
            | Term::Variable(_)
            | Term::Inference(_)
            | Term::LangItem(_) => {}
        }
    }
}

impl ExtractError for Pattern<Parsed> {
    fn contains_error(&self) -> bool {
        match self {
            Pattern::Error(_) => true,
            Pattern::As { pattern, .. } => pattern.contains_error(),
            Pattern::If { pattern, condition } => {
                pattern.contains_error() || condition.contains_error()
            }
            Pattern::List(a) => a.iter().any(|p| p.contains_error()),
            Pattern::Typed { pattern, ty } => pattern.contains_error() || ty.contains_error(),
            Pattern::Wildcard
            | Pattern::Capture(_)
            | Pattern::Rest
            | Pattern::Integer(_)
            | Pattern::Float(_) => false,
        }
    }

    fn collect_errors_into(self, errors: &mut Vec<Error>) {
        match self {
            Pattern::Error(e) => errors.push(e.clone()),
            Pattern::Typed { pattern, ty } => {
                pattern.into_inner().collect_errors_into(errors);
                ty.into_inner().collect_errors_into(errors);
            }
            Pattern::As { pattern, .. } => pattern.into_inner().collect_errors_into(errors),
            Pattern::If { pattern, condition } => {
                pattern.into_inner().collect_errors_into(errors);
                condition.into_inner().collect_errors_into(errors);
            }
            Pattern::List(a) => a
                .into_iter()
                .for_each(|p| p.into_inner().collect_errors_into(errors)),
            Pattern::Wildcard
            | Pattern::Capture(_)
            | Pattern::Rest
            | Pattern::Integer(_)
            | Pattern::Float(_) => {}
        }
    }
}

impl ExtractError for Type<Parsed> {
    fn contains_error(&self) -> bool {
        match self {
            Type::Error(_) => true,
            _ => false,
        }
    }

    fn collect_errors_into(self, errors: &mut Vec<Error>) {
        match self {
            Type::Error(e) => errors.push(e.clone()),
            Type::Function {
                parameter: argument,
                result,
            } => {
                argument.into_inner().collect_errors_into(errors);
                result.into_inner().collect_errors_into(errors);
            }
            Type::Variable(_) => {}
        }
    }
}
