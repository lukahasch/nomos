use crate::{Normalized, Term};

impl PartialEq for Term<Normalized> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Term::Integer(a), Term::Integer(b)) => a == b,
            (Term::Float(a), Term::Float(b)) => a == b,
            (Term::Variable(a), Term::Variable(b)) => a == b,
            (Term::Inference(a), Term::Inference(b)) => a == b,
            (Term::LangItem(a), Term::LangItem(b)) => a == b,
            (Term::List(a), Term::List(b)) => a == b,
            (Term::Block(a), Term::Block(b)) => a == b,
            (a, b) if std::mem::discriminant(a) != std::mem::discriminant(b) => false,
            _ => todo!("{}", self.as_ref()),
        }
    }
}
