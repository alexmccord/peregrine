use std::ops;

use crate::idx::Id;

#[derive(Debug, Clone)]
pub struct IndexedVec<T> {
    sparse: Vec<Option<usize>>,
    dense: Vec<T>,
}

impl<T> IndexedVec<T> {
    pub fn new() -> IndexedVec<T> {
        IndexedVec {
            sparse: Vec::new(),
            dense: Vec::new(),
        }
    }

    pub fn insert(&mut self, id: Id<T>, value: T) -> Option<T> {
        if id.idx >= self.sparse.len() {
            self.sparse.resize_with(id.idx + 1, Default::default);
        }

        match self.sparse[id.idx] {
            Some(i) => Some(std::mem::replace(&mut self.dense[i], value)),
            None => {
                self.sparse[id.idx].replace(self.dense.len());
                self.dense.push(value);
                None
            }
        }
    }

    pub fn get(&self, id: Id<T>) -> Option<&T> {
        self.sparse[id.idx].and_then(|i| self.dense.get(i))
    }

    pub fn get_mut(&mut self, id: Id<T>) -> Option<&mut T> {
        self.sparse[id.idx].and_then(|i| self.dense.get_mut(i))
    }

    pub fn len(&self) -> usize {
        self.dense.len()
    }
}

impl<T> ops::Index<Id<T>> for IndexedVec<T> {
    type Output = T;

    fn index(&self, index: Id<T>) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl<T> ops::IndexMut<Id<T>> for IndexedVec<T> {
    fn index_mut(&mut self, index: Id<T>) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::Id;
    use super::IndexedVec;

    #[test]
    fn get() {
        let mut ivec = IndexedVec::new();

        ivec.insert(Id::new(0), 5);
        assert_eq!(ivec.get(Id::new(0)), Some(&5));
    }

    #[test]
    fn get_mut() {
        let mut ivec = IndexedVec::new();

        ivec.insert(Id::new(0), 5);
        ivec.get_mut(Id::new(0)).map(|v| *v = 7);
        assert_eq!(ivec.get(Id::new(0)), Some(&7));
    }

    #[test]
    fn overwriting() {
        let mut ivec = IndexedVec::new();

        ivec.insert(Id::new(0), 5);
        assert_eq!(ivec.get(Id::new(0)), Some(&5));

        ivec.insert(Id::new(0), 7);
        assert_eq!(ivec.get(Id::new(0)), Some(&7));
    }

    #[test]
    fn you_can_have_a_bunch() {
        let mut ivec = IndexedVec::new();

        for i in 0..200 {
            ivec.insert(Id::new(i), i);
        }

        assert_eq!(ivec.sparse.len(), 200);
        assert_eq!(ivec.sparse.last(), Some(&Some(199)));
        assert_eq!(ivec.dense.len(), 200);
        assert_eq!(ivec.len(), ivec.dense.len());
    }

    #[test]
    fn you_can_have_some_things() {
        let mut ivec = IndexedVec::new();

        for i in 0..200 {
            if i % 5 == 0 {
                ivec.insert(Id::new(i), i);
            }
        }

        assert_eq!(ivec.sparse.len(), 196);
        assert_eq!(ivec.sparse.last(), Some(&Some(39)));
        assert_eq!(ivec.dense.len(), 40);
        assert_eq!(ivec.len(), ivec.dense.len());
    }

    #[test]
    fn index() {
        let mut ivec = IndexedVec::new();

        ivec.insert(Id::new(0), 0);
        assert_eq!(ivec[Id::new(0)], 0);
    }

    #[test]
    fn index_mut() {
        let mut ivec = IndexedVec::new();

        ivec.insert(Id::new(0), 0);
        assert_eq!(&mut ivec[Id::new(0)], &mut 0);
    }
}
