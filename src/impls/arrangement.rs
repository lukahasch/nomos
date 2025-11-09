use crate::{Arrangement, ChildID, Term};

impl Arrangement for Term {
    fn update_children(&mut self, f: impl FnMut(ChildID) -> ChildID) {
        match self {
            Term::Error(_) => {}
        }
    }
}
