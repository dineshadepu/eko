#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Null,
    Integer(i64),
    Float(f64),
    Boolean(bool),
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        !self.is_falsey()
    }

    pub fn is_falsey(&self) -> bool {
        use self::Value::*;

        match self {
            Null | Boolean(false) => true,
            _ => false,
        }
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Value {
        Value::Integer(i)
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Value {
        Value::Float(f)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Value {
        Value::Boolean(b)
    }
}

impl<T> From<Option<T>> for Value
where
    T: Into<Value>,
{
    fn from(option: Option<T>) -> Value {
        match option {
            Some(value) => value.into(),
            None => Value::Null,
        }
    }
}
