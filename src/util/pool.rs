/// A wrapper around `Vec` that provides conveniences for assigning and
/// retrieving indices and values.
///
/// Acts like a normal `Vec` when performing normal operations like `push` and
/// `pop` (in fact it just forwards these calls). However, when `T` implements
/// `PartialEq`, then operations like `insert_or_push` and `get_index` can be
/// used.
#[derive(Debug)]
pub struct Pool<T>(Vec<T>);

impl<T> Pool<T> {
    pub fn new() -> Pool<T> {
        Pool(Vec::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Appends the value to the back of the pool and returns the index.
    pub fn push(&mut self, value: T) -> usize {
        let index = self.0.len();
        self.0.push(value);
        index
    }

    pub fn pop(&mut self) -> Option<T> {
        self.0.pop()
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        self.0.get(index)
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.0.get_mut(index)
    }
}

impl<T: PartialEq> Pool<T> {
    /// Attempts to find the value within the pool. If found, returns the index,
    /// otherwise, appends the value to the back and returns the new index.
    pub fn insert_or_push(&mut self, value: T) -> usize {
        if let Some(index) = self.get_index(&value) {
            index
        } else {
            self.push(value)
        }
    }

    /// Attempts to find the value within the collection and returns the index
    /// if found.
    pub fn get_index(&self, value: &T) -> Option<usize> {
        for (index, item) in self.0.iter().enumerate() {
            if item == value {
                return Some(index);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::Pool;

    #[test]
    fn push() {
        let mut pool = Pool::new();

        pool.push(0);
        pool.push(1);
        pool.push(2);
        let index = pool.push(1);

        assert_eq!(index, 3);
        assert_eq!(pool.get(index), Some(&1));
    }

    #[test]
    fn insert_or_push() {
        let mut pool = Pool::new();

        pool.insert_or_push(0);
        pool.insert_or_push(1);
        pool.insert_or_push(2);
        let index = pool.insert_or_push(1);

        assert_eq!(index, 1);
        assert_eq!(pool.get(index), Some(&1));
    }
}
