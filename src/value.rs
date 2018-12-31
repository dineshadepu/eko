use std::rc::Rc;

use crate::parser::{Block, Params};

#[derive(Clone, Debug)]
pub enum Value {
    Null,
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Strong(Rc<Reference>),
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

impl PartialEq for Value {
    fn eq(&self, right: &Value) -> bool {
        use self::Value::*;

        match (self, right) {
            (Null, Null) => true,
            (Integer(left), Integer(right)) => left == right,
            (Float(left), Float(right)) => left == right,
            (Boolean(left), Boolean(right)) => left == right,
            (Strong(left), Strong(right)) => Rc::ptr_eq(left, right),
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

impl<R> From<R> for Value
where
    R: Into<Reference>,
{
    fn from(reference: R) -> Value {
        Value::Strong(Rc::new(reference.into()))
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

#[derive(Debug)]
pub enum Reference {
    InternalFunc(InternalFuncReference),
}

impl From<InternalFuncReference> for Reference {
    fn from(func: InternalFuncReference) -> Reference {
        Reference::InternalFunc(func)
    }
}

#[derive(Debug)]
pub struct InternalFuncReference {
    pub params: Params,
    pub block: Block,
}
