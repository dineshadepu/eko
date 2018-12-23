use crate::ast::*;
use crate::Pool;

#[derive(Debug)]
pub(crate) struct Chunk {
    pub(crate) parent: Option<usize>,
    pub(crate) identifiers: Pool<usize>,
    pub(crate) instructions: Vec<Instruction>,
}

impl Chunk {
    pub(crate) fn new() -> Chunk {
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
pub(crate) enum Constant {
    Integer(i64),
    Float(f64),
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum Instruction {
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
    pub(crate) fn is_binary(&self) -> bool {
        use self::Instruction::*;

        match self {
            Add | Subtract | Multiply | Divide | Equal | Less | LessEqual | Greater
            | GreaterEqual | And | Or => true,
            _ => false,
        }
    }

    pub(crate) fn is_unary(&self) -> bool {
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
pub(crate) struct Closed(usize, usize);

impl Closed {
    pub(crate) fn new(parents: usize, closed: usize) -> Closed {
        Closed(parents, closed)
    }

    pub(crate) fn parents(&self) -> usize {
        self.0
    }

    pub(crate) fn closed(&self) -> usize {
        self.1
    }
}
