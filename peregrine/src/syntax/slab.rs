pub(crate) struct Slab<T> {
    vec: Vec<T>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub(crate) struct SlabId(usize);

impl<T> Slab<T> {
    pub(crate) fn insert(&mut self, item: T) -> SlabId {
        let id = self.vec.len();
        self.vec.push(item);
        SlabId(id)
    }

    pub(crate) fn get(&self, SlabId(id): SlabId) -> &T {
        &self.vec[id]
    }
}

impl<T> Default for Slab<T> {
    fn default() -> Slab<T> {
        Slab {
            vec: Vec::default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Slab;

    #[test]
    fn slab_works() {
        let mut slab = Slab::default();
        let id1 = slab.insert(5);
        let id2 = id1.clone();
        assert_eq!(id1, id2);
        assert_eq!(slab.get(id1), slab.get(id2));
    }
}
