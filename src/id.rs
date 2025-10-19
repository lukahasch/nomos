use uuid::Uuid;

pub fn id() -> Id {
    Id(Uuid::new_v4().as_u128())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Id(pub u128);
