use std::marker::PhantomData;
use std::ops;

use crate::idx::Idx;

#[derive(Debug, Clone)]
pub struct IndexedVec<I, T> {
    inner: Vec<T>,
    marker: PhantomData<fn(I)>,
}

impl<I: Idx, T> IndexedVec<I, T> {
    pub fn new() -> IndexedVec<I, T> {
        IndexedVec {
            inner: Vec::new(),
            marker: PhantomData,
        }
    }

    pub fn with_capacity(capacity: usize) -> IndexedVec<I, T> {
        IndexedVec {
            inner: Vec::with_capacity(capacity),
            marker: PhantomData,
        }
    }

    pub fn allocate<F>(&mut self, f: F) -> I
    where
        F: FnOnce(I) -> T,
    {
        let id = I::new(self.inner.len());
        self.inner.push(f(id));
        id
    }

    pub fn push(&mut self, value: T) -> I {
        let id = I::new(self.inner.len());
        self.inner.push(value);
        id
    }

    pub fn get(&self, id: I) -> Option<&T> {
        self.inner.get(id.index())
    }

    pub fn get_mut(&mut self, id: I) -> Option<&mut T> {
        self.inner.get_mut(id.index())
    }

    pub fn next(id: I) -> I {
        I::new(id.index() + 1)
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }
}

impl<I: Idx, T> Default for IndexedVec<I, T> {
    fn default() -> Self {
        IndexedVec::new()
    }
}

impl<I: Idx, T> ops::Index<I> for IndexedVec<I, T> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        &self.inner[index.index()]
    }
}

impl<I: Idx, T> ops::IndexMut<I> for IndexedVec<I, T> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.inner[index.index()]
    }
}

impl<I, T> IntoIterator for IndexedVec<I, T> {
    type Item = T;

    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<I: Idx, T> FromIterator<T> for IndexedVec<I, T> {
    fn from_iter<Iter: IntoIterator<Item = T>>(iter: Iter) -> Self {
        let mut ivec = IndexedVec::new();

        for item in iter {
            ivec.push(item);
        }

        ivec
    }
}

#[cfg(test)]
mod tests {
    use crate::idx;

    use super::Idx;
    use super::IndexedVec;

    idx::newindex!(TestId);

    #[derive(Debug, PartialEq, Eq)]
    struct Test {
        id: TestId,
        value: u32,
    }

    impl Test {
        fn new(id: TestId, value: u32) -> Test {
            Test { id, value }
        }
    }

    #[test]
    fn get() {
        let mut vec = IndexedVec::new();

        let id = vec.allocate(|id| Test::new(id, 5));
        assert_eq!(vec.get(id), Some(&Test::new(id, 5)));
    }

    #[test]
    fn get_mut() {
        let mut vec = IndexedVec::new();

        let id = vec.allocate(|id| Test::new(id, 5));
        vec.get_mut(id).map(|t| t.value = 7);
        assert_eq!(vec.get(id), Some(&Test::new(id, 7)));
    }

    #[test]
    fn overwriting() {
        let mut vec = IndexedVec::new();

        let id = vec.allocate(|id| Test::new(id, 5));
        assert_eq!(vec.get(id), Some(&Test::new(id, 5)));

        vec[id].value = 7;
        assert_eq!(vec.get(id), Some(&Test::new(id, 7)));
    }

    #[test]
    fn you_can_have_a_bunch() {
        let mut vec = IndexedVec::new();

        let mut last_id = TestId::new(usize::MAX);
        for i in 0..200 {
            last_id = vec.allocate(|id| Test::new(id, i));
        }

        assert_eq!(vec.inner.len(), vec.len());
        assert_eq!(vec.inner.len(), 200);
        assert_eq!(vec.inner.last(), Some(&Test::new(last_id, 199)));
    }

    #[test]
    fn you_can_have_some_things() {
        let mut vec = IndexedVec::new();

        let mut last_id = TestId::new(usize::MAX);
        for i in 0..200 {
            if i % 5 == 0 {
                last_id = vec.allocate(|id| Test::new(id, i));
            }
        }

        assert_eq!(vec.inner.len(), vec.len());
        assert_eq!(vec.inner.len(), 40);
        assert_eq!(vec.inner.last(), Some(&Test::new(last_id, 195)));
    }

    #[test]
    fn index() {
        let mut vec = IndexedVec::new();

        let id = vec.allocate(|id| Test::new(id, 0));
        assert_eq!(&vec[id], &Test::new(id, 0));
    }

    #[test]
    fn index_mut() {
        let mut vec = IndexedVec::new();

        let id = vec.allocate(|id| Test::new(id, 0));
        assert_eq!(&mut vec[id], &mut Test::new(id, 0));
    }
}
