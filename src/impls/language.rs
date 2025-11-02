use std::mem::Discriminant;

use egg::Language;

use crate::{Normalized, Term};

impl std::fmt::Debug for Term<Normalized> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Should use Show to format Terms")
    }
}

impl Language for Term<Normalized> {
    type Discriminant = Discriminant<Self>;

    fn discriminant(&self) -> Self::Discriminant {
        std::mem::discriminant(self)
    }
}
