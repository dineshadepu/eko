#[derive(Debug)]
pub(crate) struct Pool<T>(Vec<T>);

impl<T> Pool<T> {
    pub(crate) fn len(&self) -> usize {
        self.0.len()
    }

    pub(crate) fn new() -> Pool<T> {
        Pool(Vec::new())
    }

    pub(crate) fn push(&mut self, value: T) -> usize {
        let index = self.0.len();
        self.0.push(value);
        index
    }

    pub(crate) fn pop(&mut self) -> Option<T> {
        self.0.pop()
    }

    pub(crate) fn get(&self, index: usize) -> Option<&T> {
        self.0.get(index)
    }

    pub(crate) fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.0.get_mut(index)
    }
}

impl<T: PartialEq> Pool<T> {
    pub(crate) fn insert(&mut self, value: T) -> usize {
        self.entry_by_value(&value).or_insert(value)
    }

    pub(crate) fn get_by_value(&self, value: &T) -> Option<usize> {
        for (index, item) in self.0.iter().enumerate() {
            if item == value {
                return Some(index);
            }
        }
        None
    }

    pub(crate) fn entry_by_value(&mut self, value: &T) -> Entry<T> {
        if let Some(index) = self.get_by_value(value) {
            Entry::Occupied(OccupiedEntry {
                index,
                value: &self.0[index],
            })
        } else {
            Entry::Vacant(VacantEntry { pool: self })
        }
    }
}

impl<T: Clone> Clone for Pool<T> {
    fn clone(&self) -> Self {
        Pool(self.0.clone())
    }
}

pub(crate) enum Entry<'a, T> {
    Vacant(VacantEntry<'a, T>),
    Occupied(OccupiedEntry<'a, T>),
}

impl<'a, T> Entry<'a, T> {
    pub(crate) fn or_insert(self, value: T) -> usize {
        use self::Entry::*;

        match self {
            Vacant(entry) => entry.insert(value),
            Occupied(entry) => entry.index(),
        }
    }

    pub(crate) fn or_insert_with<F>(self, value: F) -> usize
    where
        F: FnOnce() -> T,
    {
        use self::Entry::*;

        match self {
            Vacant(entry) => entry.insert(value()),
            Occupied(entry) => entry.index(),
        }
    }
}

pub(crate) struct VacantEntry<'a, T> {
    pool: &'a mut Pool<T>,
}

impl<'a, T> VacantEntry<'a, T> {
    pub(crate) fn insert(self, value: T) -> usize {
        self.pool.push(value)
    }
}

pub(crate) struct OccupiedEntry<'a, T> {
    index: usize,
    value: &'a T,
}

impl<'a, T> OccupiedEntry<'a, T> {
    pub(crate) fn index(&self) -> usize {
        self.index
    }
}
