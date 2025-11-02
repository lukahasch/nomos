use uuid::Uuid;

pub fn id() -> UId {
    UId(Uuid::new_v4().as_u128())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct UId(pub u128);
