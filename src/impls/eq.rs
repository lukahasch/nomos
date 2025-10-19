use crate::{Normalized, Term};

impl PartialEq for Term<Normalized> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            _ => false,
        }
    }
}
