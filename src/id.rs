use std::sync::atomic::AtomicUsize;

static ID_ATOMIC: AtomicUsize = AtomicUsize::new(0);

pub fn id() -> Id {
    Id(ID_ATOMIC.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Id(pub usize);
