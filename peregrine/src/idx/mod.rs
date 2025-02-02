use std::{fmt, hash, marker::PhantomData};

mod indexed_vec;
mod tests;

pub use indexed_vec::IndexedVec;

pub struct Id<T> {
    idx: usize,
    marker: PhantomData<fn() -> T>,
}

impl<T> Id<T> {
    fn new(idx: usize) -> Id<T> {
        Id {
            idx,
            marker: PhantomData,
        }
    }
}

pub struct Generation<T> {
    current: Id<T>,
}

impl<T> Generation<T> {
    pub fn new() -> Generation<T> {
        Generation {
            current: Id::new(0),
        }
    }

    pub fn next(&mut self) -> Id<T> {
        let id = self.current;
        self.current = Id::new(id.idx + 1);
        id
    }
}

// The stuff below is because of the lack of perfect derive in Rust.

impl<T> fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Id").field("idx", &self.idx).finish()
    }
}

impl<T> fmt::Display for Id<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Id({})", self.idx)
    }
}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
    }
}

impl<T> Eq for Id<T> {}

impl<T> hash::Hash for Id<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.idx.hash(state);
    }
}

impl<T> PartialOrd for Id<T> {
    fn partial_cmp(&self, other: &Id<T>) -> Option<std::cmp::Ordering> {
        self.idx.partial_cmp(&other.idx)
    }
}

impl<T> Ord for Id<T> {
    fn cmp(&self, other: &Id<T>) -> std::cmp::Ordering {
        self.idx.cmp(&other.idx)
    }
}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Id<T> {
        Id::new(self.idx)
    }
}

impl<T> Copy for Id<T> {}
