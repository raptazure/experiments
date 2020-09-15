use std::{collections::hash_map::DefaultHasher, hash::Hash, hash::Hasher, mem};

const INITIAL_NBUCKETS: usize = 1;

pub struct HashMap<K, V> {
    buckets: Vec<Vec<(K, V)>>,
    items: usize,
}

impl<K, V> HashMap<K, V>
where
    K: Hash + Eq,
{
    pub fn new() -> Self {
        HashMap {
            buckets: Vec::new(),
            items: 0,
        }
    }

    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        if self.buckets.is_empty() || self.items > 3 * self.buckets.len() / 4 {
            self.resize();
        }
        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);
        let bucket: usize = (hasher.finish() % self.buckets.len() as u64) as usize;
        let bucket = &mut self.buckets[bucket];

        self.items += 1;
        for &mut (ref ekey, ref mut evalue) in bucket.iter_mut() {
            if ekey == &key {
                use std::mem;
                return Some(mem::replace(evalue, value));
            }
        }
        bucket.push((key, value));
        None
    }

    fn resize(&mut self) {
        let target_size = match self.buckets.len() {
            0 => INITIAL_NBUCKETS,
            n => n * 2,
        };

        let mut new_buckets = vec![Vec::new(); target_size];
        for (key, value) in self.buckets.drain(..) {
            let mut hasher = DefaultHasher::new();
            key.hash(&mut hasher);
            let bucket: usize = (hasher.finish() % self.buckets.len() as u64) as usize;
            new_buckets[bucket].push((key, value));
        }
        mem::replace(&mut self.buckets, new_buckets);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn insert() {
        let mut map = HashMap::new();
        map.insert("foo", 42);
    }
}
