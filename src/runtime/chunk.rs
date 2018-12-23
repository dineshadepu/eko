use crate::runtime::Value;
use crate::syntax::ast::*;
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

    pub(crate) fn with_parent(parent: usize) -> Chunk {
        Chunk {
            parent: Some(parent),
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

#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    PushConstant(usize),
    PushBoolean(bool),
    PushNull,
    Pop,

    PushLocal(usize),
    PushClosed(Closed),
    PopLocal(usize),
    PopClosed(Closed),

    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,

    Equal,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    And,
    Or,
    Not,

    JumpFalsey(usize),
    Jump(usize),
}

impl Instruction {
    pub fn is_binary(&self) -> bool {
        use self::Instruction::*;

        match self {
            Add | Subtract | Multiply | Divide | Equal | Less | LessEqual | Greater
            | GreaterEqual | And | Or => true,
            _ => false,
        }
    }

    pub fn is_unary(&self) -> bool {
        use self::Instruction::*;

        match self {
            Negate | Not => true,
            _ => false,
        }
    }
}

impl From<Binary> for Instruction {
    fn from(binary: Binary) -> Instruction {
        match binary {
            Binary::Add => Instruction::Add,
            Binary::Subtract => Instruction::Subtract,
            Binary::Multiply => Instruction::Multiply,
            Binary::Divide => Instruction::Divide,

            Binary::Equal => Instruction::Equal,
            Binary::Less => Instruction::Less,
            Binary::LessEqual => Instruction::LessEqual,
            Binary::Greater => Instruction::Greater,
            Binary::GreaterEqual => Instruction::GreaterEqual,

            Binary::And => Instruction::And,
            Binary::Or => Instruction::Or,
        }
    }
}

impl From<Unary> for Instruction {
    fn from(unary: Unary) -> Instruction {
        match unary {
            Unary::Negate => Instruction::Negate,
            Unary::Not => Instruction::Not,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Closed(usize, usize);

impl Closed {
    pub fn new(parents: usize, closed: usize) -> Closed {
        Closed(parents, closed)
    }

    pub fn parents(&self) -> usize {
        self.0
    }

    pub fn closed(&self) -> usize {
        self.1
    }
}
