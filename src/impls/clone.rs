use crate::{Normalized, Term, Type};

impl Clone for Term<Normalized> {
    fn clone(&self) -> Self {
        match self {
            Term::Integer(i) => Term::Integer(*i),
            Term::Float(f) => Term::Float(*f),
            Term::Variable(v) => Term::Variable(*v),
            Term::Inference(i) => Term::Inference(*i),
            Term::LangItem(li) => Term::LangItem(li.clone()),
            Term::List(l) => Term::List(l.clone()),
            Term::Block(b) => Term::Block(b.clone()),
            Term::Application { function, argument } => Term::Application {
                function: function.clone(),
                argument: argument.clone(),
            },
            Term::Function { pattern, body } => Term::Function {
                pattern: pattern.clone(),
                body: body.clone(),
            },
            Term::Error(e) => Term::Error(e.clone()),
            Term::Type(t) => Term::Type(t.clone()),
            Term::Symbol(s) => Term::Symbol(*s),
            Term::Let {
                pattern,
                value,
                body,
            } => Term::Let {
                pattern: pattern.clone(),
                value: value.clone(),
                body: body.clone(),
            },
            Term::Define {
                pattern,
                value,
                body,
            } => Term::Define {
                pattern: pattern.clone(),
                value: value.clone(),
                body: body.clone(),
            },
            Term::If {
                condition,
                then,
                r#else,
            } => Term::If {
                condition: condition.clone(),
                then: then.clone(),
                r#else: r#else.clone(),
            },
            Term::Match { value, branches } => Term::Match {
                value: value.clone(),
                branches: branches.clone(),
            },
        }
    }
}

impl Clone for Type<Normalized> {
    fn clone(&self) -> Self {
        match self {
            Type::Error(e) => Type::Error(e.clone()),
            Type::Variable(v) => Type::Variable(*v),
            Type::Function { parameter, result } => Type::Function {
                parameter: parameter.clone(),
                result: result.clone(),
            },
            Type::Application { function, argument } => Type::Application {
                function: function.clone(),
                argument: argument.clone(),
            },
        }
    }
}
