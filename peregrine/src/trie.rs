use std::{borrow::Borrow, collections::HashMap, hash::Hash, marker::PhantomData};

#[derive(Debug, Clone)]
pub struct TrieMap<K: ?Sized, A, V> {
    map: HashMap<A, V>,
    children: HashMap<A, TrieMap<K, A, V>>,
    _marker: PhantomData<K>,
}

impl<K: ?Sized, A, V> TrieMap<K, A, V> {
    pub fn new() -> TrieMap<K, A, V> {
        TrieMap::default()
    }
}

impl<K: ?Sized, A, V> TrieMap<K, A, V>
where
    A: Eq + Hash,
{
    fn traverse<'a, Q: ?Sized, B: ?Sized>(&self, keys: &'a Q) -> Option<(&Self, &'a B)>
    where
        A: Borrow<B>,
        B: Eq + Hash,
        &'a Q: IntoIterator<Item = &'a B>,
    {
        let mut current = self;
        let mut last_k = None;

        for k in keys {
            if let Some(k) = last_k.take() {
                current = current.children.get(k)?;
            }

            last_k = Some(k);
        }

        Some((current, last_k?))
    }

    fn traverse_mut<'a, Q: ?Sized, B: ?Sized>(&mut self, keys: &'a Q) -> Option<(&mut Self, &'a B)>
    where
        A: Borrow<B>,
        B: Eq + Hash,
        &'a Q: IntoIterator<Item = &'a B>,
    {
        let mut current = self;
        let mut last_k = None;

        for k in keys {
            if let Some(k) = last_k.take() {
                current = current.children.get_mut(k)?;
            }

            last_k = Some(k);
        }

        Some((current, last_k?))
    }

    pub fn get<Q: ?Sized, B: ?Sized>(&self, keys: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        A: Borrow<B>,
        B: Eq + Hash,
        for<'a> &'a Q: IntoIterator<Item = &'a B>,
    {
        let (trie, k) = self.traverse(keys)?;
        trie.map.get(&k)
    }

    pub fn get_mut<Q: ?Sized, B: ?Sized>(&mut self, keys: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        A: Borrow<B>,
        B: Eq + Hash,
        for<'a> &'a Q: IntoIterator<Item = &'a B>,
    {
        let (trie, k) = self.traverse_mut(keys)?;
        trie.map.get_mut(&k)
    }

    pub fn contains_key<Q: ?Sized, B: ?Sized>(&self, keys: &Q) -> bool
    where
        A: Borrow<B>,
        B: Eq + Hash,
        for<'a> &'a Q: IntoIterator<Item = &'a B>,
    {
        self.traverse(keys).is_some()
    }

    pub fn contains_prefix_key<Q: ?Sized, B: ?Sized>(&self, keys: &Q) -> bool
    where
        A: Borrow<B>,
        B: Eq + Hash,
        for<'a> &'a Q: IntoIterator<Item = &'a B>,
    {
        self.traverse(keys)
            .and_then(|(trie, k)| trie.children.get(&k))
            .is_some()
    }
}

impl<K, A: ?Sized, V> TrieMap<K, A::Owned, V>
where
    for<'a> &'a K: IntoIterator<Item = &'a A>,
    A: ToOwned<Owned: Eq + Hash>,
{
    fn traverse_and_insert_mut(&mut self, keys: K) -> Option<(&mut Self, A::Owned)> {
        let mut current = self;
        let mut last_k = None;

        for k in &keys {
            if let Some(k) = Option::take(&mut last_k) {
                current = current.children.entry(k).or_default();
            }

            last_k = Some(k.to_owned());
        }

        Some((current, last_k?))
    }

    pub fn insert(&mut self, keys: K, v: V) -> Option<V> {
        let (trie, k) = self.traverse_and_insert_mut(keys)?;
        trie.map.insert(k, v)
    }
}

impl<K: ?Sized, A, V> Default for TrieMap<K, A, V> {
    fn default() -> Self {
        Self {
            map: Default::default(),
            children: Default::default(),
            _marker: Default::default(),
        }
    }
}

impl<K, A, V> FromIterator<(K, V)> for TrieMap<K, A::Owned, V>
where
    for<'a> &'a K: IntoIterator<Item = &'a A>,
    A: ToOwned<Owned: Eq + Hash>,
{
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let mut trie = TrieMap::new();

        for (keys, v) in iter {
            trie.insert(keys, v);
        }

        trie
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::trie::TrieMap;

    #[test]
    fn empty_trie() {
        let trie: TrieMap<Vec<String>, String, ()> = TrieMap::new();
        let val = trie.get(&Vec::new());
        assert_eq!(val, None)
    }

    #[test]
    fn empty_insert() {
        let mut trie: TrieMap<Vec<()>, (), i32> = TrieMap::new();
        trie.insert(vec![], 0);

        assert_eq!(trie.map.len(), 0);
        assert_eq!(trie.children.len(), 0);
    }

    #[test]
    fn populate_from_vec() {
        let trie = TrieMap::from_iter(vec![
            (vec!["abc"], 0),
            (vec!["abc", "abc"], 1),
            (vec!["abc", "def"], 2),
            (vec!["def", "ghi"], 3),
        ]);

        assert_eq!(trie.get(&vec!["abc"]), Some(&0));
        assert_eq!(trie.get(&vec!["abc", "abc"]), Some(&1));
        assert_eq!(trie.get(&vec!["abc", "def"]), Some(&2));
        assert_eq!(trie.get(&vec!["def", "ghi"]), Some(&3));
        assert_eq!(trie.get(&vec![]), None);
        assert_eq!(trie.get(&vec!["abc", "abc", "abc"]), None);
    }

    #[test]
    fn data_structure_sanity() {
        let trie = TrieMap::from_iter(vec![
            (vec!["abc"], 0),
            (vec!["abc", "abc"], 1),
            (vec!["abc", "def"], 2),
            (vec!["def", "ghi"], 3),
        ]);

        assert_eq!(trie.map.len(), 1);
        assert_eq!(trie.children.len(), 2);

        let abc_trie = &trie.children[&"abc"];
        assert_eq!(abc_trie.map.len(), 2);
        assert_eq!(abc_trie.children.len(), 0);

        let def_trie = &trie.children[&"def"];
        assert_eq!(def_trie.map.len(), 1);
        assert_eq!(def_trie.children.len(), 0);
    }

    #[test]
    fn more_of_prefix_sharing() {
        let trie = TrieMap::from_iter(vec![
            (vec!["a", "b", "c"], 0),
            (vec!["a", "b", "d"], 1),
            (vec!["a", "x", "y"], 2),
        ]);

        assert_eq!(trie.children.len(), 1);

        let a_trie = &trie.children[&"a"];
        assert_eq!(a_trie.map.len(), 0);
        assert_eq!(a_trie.children.len(), 2);
    }

    #[test]
    fn insertions() {
        let mut trie = TrieMap::new();

        trie.insert(vec!["a", "b"], 0);
        assert_eq!(trie.get(&vec!["a", "b"]), Some(&0));

        trie.insert(vec!["a", "b"], 5);
        assert_eq!(trie.get(&vec!["a", "b"]), Some(&5));
    }

    #[test]
    fn hash_map_to_trie_map() {
        let map: HashMap<Vec<&str>, i32> = HashMap::from_iter(vec![
            (vec!["a", "b", "c"], 0),
            (vec!["a", "b", "d"], 1),
            (vec!["a", "x", "y"], 2),
        ]);

        let trie = TrieMap::from_iter(map);

        assert_eq!(trie.children.len(), 1);

        let a_trie = &trie.children[&"a"];
        assert_eq!(a_trie.map.len(), 0);
        assert_eq!(a_trie.children.len(), 2);
    }
}
