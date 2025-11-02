use crate::{Normalized, Term};

impl Ord for Term<Normalized> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Term::Integer(a), Term::Integer(b)) => a.cmp(b),
            (Term::Float(a), Term::Float(b)) => a.partial_cmp(b).unwrap(),
            (Term::Variable(a), Term::Variable(b)) => a.cmp(b),
            (Term::Inference(a), Term::Inference(b)) => a.cmp(b),
            (Term::LangItem(a), Term::LangItem(b)) => a.cmp(b),
            (Term::List(a), Term::List(b)) => a.cmp(b),
            (Term::Block(a), Term::Block(b)) => a.cmp(b),
            (a, b) if std::mem::discriminant(a) != std::mem::discriminant(b) => {
                std::mem::discriminant(a).cmp(&std::mem::discriminant(b))
            }
            _ => todo!("{}", self.as_ref()),
        }
    }
}
