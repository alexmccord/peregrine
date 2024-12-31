use std::marker::PhantomData;

pub(crate) struct Slab<'a, T> {
    vec: Vec<T>,
    _marker: PhantomData<&'a ()>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub(crate) struct SlabId<'a>(usize, PhantomData<&'a ()>);

impl<'a, T> Slab<'a, T> {
    pub(crate) fn insert(&mut self, item: T) -> SlabId<'a> {
        let id = self.vec.len();
        self.vec.push(item);
        SlabId(id, PhantomData)
    }

    pub(crate) fn get(&self, SlabId(id, _): SlabId<'a>) -> &T {
        &self.vec[id]
    }

    pub(crate) fn get_mut(&mut self, SlabId(id, _): SlabId<'a>) -> &mut T {
        &mut self.vec[id]
    }
}

impl<'a, T> Default for Slab<'a, T> {
    fn default() -> Slab<'a, T> {
        Slab {
            vec: Vec::default(),
            _marker: PhantomData,
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
