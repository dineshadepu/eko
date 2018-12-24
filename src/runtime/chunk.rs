use crate::runtime::{Instruction, Value};
use crate::Pool;

#[derive(Debug)]
pub struct Chunk {
    pub parent: Option<usize>,
    pub identifiers: Pool<usize>,
    pub instructions: Vec<Instruction>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            parent: None,
            identifiers: Pool::new(),
            instructions: Vec::new(),
        }
    }
}

#[derive(PartialEq)]
pub enum Constant {
    Integer(i64),
    Float(f64),
}

impl Into<Value> for &Constant {
    fn into(self) -> Value {
        match self {
            Constant::Integer(integer) => Value::Integer(*integer),
            Constant::Float(float) => Value::Float(*float),
        }
    }
}
