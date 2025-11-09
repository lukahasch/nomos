use uuid::Uuid;

pub fn id() -> UId {
    UId(Uuid::new_v4().as_u128())
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq, Hash, Copy)]
#[repr(transparent)]
pub struct UId(pub u128);
