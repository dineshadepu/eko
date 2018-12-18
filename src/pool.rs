pub struct Pool<T>(Vec<T>);

impl<T> Pool<T> {
    pub fn new() -> Pool<T> {
        Pool(Vec::new())
    }

    pub fn push(&mut self, value: T) -> usize {
        let index = self.0.len();
        self.0.push(value);
        index
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        self.0.get(index)
    }
}

impl<T: PartialEq> Pool<T> {
    pub fn insert(&mut self, value: T) -> usize {
        self.entry_by_value(&value).or_insert(value)
    }

    pub fn get_by_value(&self, value: &T) -> Option<usize> {
        for (index, item) in self.0.iter().enumerate() {
            if item == value {
                return Some(index);
            }
        }
        None
    }

    pub fn entry_by_value(&mut self, value: &T) -> Entry<T> {
        if let Some(index) = self.get_by_value(value) {
            Entry::Occupied(OccupiedEntry {
                index,
                value: self.0.get(index).unwrap(),
            })
        } else {
            Entry::Vacant(VacantEntry { pool: self })
        }
    }
}

pub enum Entry<'a, T> {
    Vacant(VacantEntry<'a, T>),
    Occupied(OccupiedEntry<'a, T>),
}

impl<'a, T> Entry<'a, T> {
    pub fn or_insert(self, value: T) -> usize {
        use self::Entry::*;

        match self {
            Vacant(entry) => entry.insert(value),
            Occupied(entry) => entry.index(),
        }
    }

    pub fn or_insert_with<F>(self, value: F) -> usize
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

pub struct VacantEntry<'a, T> {
    pool: &'a mut Pool<T>,
}

impl<'a, T> VacantEntry<'a, T> {
    pub fn insert(self, value: T) -> usize {
        self.pool.push(value)
    }
}

pub struct OccupiedEntry<'a, T> {
    index: usize,
    value: &'a T,
}

impl<'a, T> OccupiedEntry<'a, T> {
    pub fn index(&self) -> usize {
        self.index
    }
}
