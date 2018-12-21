use std::mem::size_of;

use failure::{bail, format_err};

use crate::ast::*;
use crate::{Pool, Result, State};

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

#[derive(PartialEq)]
pub(crate) enum Constant {
    Integer(i64),
    Float(f64),
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct Closed(usize);

impl Closed {
    const CLOSED_BITS: usize = size_of::<usize>() - 2;

    fn new(parents: usize, closed: usize) -> Closed {
        Closed((parents << Self::CLOSED_BITS) + closed)
    }

    pub(crate) fn parents(&self) -> usize {
        self.0 >> Self::CLOSED_BITS
    }

    pub(crate) fn closed(&self) -> usize {
        self.0 % (1 << Self::CLOSED_BITS)
    }
}

pub(crate) struct Generator<'a> {
    state: &'a mut State,
}

impl<'a> Generator<'a> {
    pub(crate) fn new(state: &'a mut State) -> Generator<'a> {
        Generator { state }
    }

    pub(crate) fn generate(&mut self, mut block: Block) -> Result<usize> {
        let mut chunk = Chunk::new();
        let last_statement = match block.statements.pop() {
            Some(statement) => statement,
            None => return Ok(self.state.chunks.push(chunk)),
        };
        for statement in block.statements {
            self.statement(&mut chunk, statement)?;
        }

        // If the last statement is an expression, the last pop instruction
        // should be discarded, to preserve the return value. Otherwise, a
        // null value should be inserted as the return value.
        if last_statement.is_expression() {
            self.statement(&mut chunk, last_statement)?;
            chunk.instructions.pop();
        } else {
            self.statement(&mut chunk, last_statement)?;
            chunk.instructions.push(Instruction::PushNull);
        }

        println!("{:?}", chunk);

        Ok(self.state.chunks.push(chunk))
    }

    fn block(&mut self, chunk: &mut Chunk, block: Block) -> Result<()> {
        for statement in block.statements {
            self.statement(chunk, statement)?;
        }
        chunk.instructions.pop();
        Ok(())
    }

    fn statement(&mut self, chunk: &mut Chunk, statement: Statement) -> Result<()> {
        match statement {
            Statement::VarDeclaration(expression) => {
                self.var_declaration(chunk, expression)?;
            }
            Statement::Expression(expression) => {
                self.expression(chunk, expression)?;
                chunk.instructions.push(Instruction::Pop);
            }
        }
        Ok(())
    }

    fn var_declaration(&mut self, chunk: &mut Chunk, expression: Expression) -> Result<()> {
        match expression {
            Expression::Identifier(identifier) => {
                let local = self.define_local(chunk, identifier)?;
                chunk.instructions.push(Instruction::PushNull);
                chunk.instructions.push(Instruction::PopLocal(local));
            }
            Expression::Assignment(left, right) => {
                let identifier = match *left {
                    Expression::Identifier(identifier) => identifier,
                    _ => bail!("invalid left expression in var declaration"),
                };
                self.expression(chunk, *right)?;
                let local = self.define_local(chunk, identifier)?;
                chunk.instructions.push(Instruction::PopLocal(local));
            }
            _ => bail!("invalid expression in var declaration"),
        }
        Ok(())
    }

    fn expression(&mut self, chunk: &mut Chunk, expression: Expression) -> Result<()> {
        match expression {
            Expression::Integer(integer) => {
                let constant = self.state.constants.insert(Constant::Integer(integer));
                chunk.instructions.push(Instruction::PushConstant(constant));
            }
            Expression::Float(float) => {
                let constant = self.state.constants.insert(Constant::Float(float));
                chunk.instructions.push(Instruction::PushConstant(constant));
            }
            Expression::Boolean(boolean) => {
                chunk.instructions.push(Instruction::PushBoolean(boolean));
            }
            Expression::Assignment(left, right) => {
                let identifier = match *left {
                    Expression::Identifier(identifier) => identifier,
                    _ => bail!("invalid left expression in var declaration"),
                };
                self.expression(chunk, *right)?;

                if let Some(local) = chunk.identifiers.get_by_value(&identifier) {
                    chunk.instructions.push(Instruction::PopLocal(local));
                    chunk.instructions.push(Instruction::PushLocal(local));
                    return Ok(());
                }
                let closed = self.search_closed(chunk, identifier)?;
                chunk.instructions.push(Instruction::PopClosed(closed));
                chunk.instructions.push(Instruction::PushClosed(closed));
            }
            Expression::Binary(binary, left, right) => {
                self.expression(chunk, *left)?;
                self.expression(chunk, *right)?;
                chunk.instructions.push(binary.into());
            }
            Expression::Unary(unary, expression) => {
                self.expression(chunk, *expression)?;
                chunk.instructions.push(unary.into());
            }
            Expression::Identifier(identifier) => {
                if let Some(local) = chunk.identifiers.get_by_value(&identifier) {
                    chunk.instructions.push(Instruction::PushLocal(local));
                    return Ok(());
                }
                let closed = self.search_closed(chunk, identifier)?;
                chunk.instructions.push(Instruction::PushClosed(closed));
            }
            Expression::If(condition, success, failure) => {
                self.expression(chunk, *condition)?;
                let failure_jump = chunk.instructions.len();
                self.block(chunk, success)?;
                let success_jump = chunk.instructions.len() + 1;
                chunk.instructions.insert(
                    failure_jump,
                    Instruction::JumpFalsey(chunk.instructions.len() + 2),
                );
                if let Some(failure) = failure {
                    self.block(chunk, failure)?;
                } else {
                    chunk.instructions.push(Instruction::PushNull);
                }
                chunk.instructions.insert(
                    success_jump,
                    Instruction::Jump(chunk.instructions.len() + 1),
                );
            }
        }
        Ok(())
    }

    fn define_local(&mut self, chunk: &mut Chunk, identifier: usize) -> Result<usize> {
        if chunk.identifiers.get_by_value(&identifier).is_some() {
            let symbol = self
                .state
                .symbols
                .get(identifier)
                .ok_or_else(|| format_err!("failed to find symbol: '{}'", identifier))?;
            bail!("variable already declared: '{}'", symbol);
        }
        Ok(chunk.identifiers.insert(identifier))
    }

    fn search_closed(&mut self, chunk: &mut Chunk, identifier: usize) -> Result<Closed> {
        let mut parents = 0;
        let mut cur_parent = chunk.parent;
        while let Some(parent) = cur_parent {
            let chunk = self
                .state
                .chunks
                .get_mut(parent)
                .ok_or_else(|| format_err!("failed to find chunk: {}", parent))?;
            if let Some(closed) = chunk.identifiers.get_by_value(&identifier) {
                return Ok(Closed::new(parents, closed));
            }
            parents += 1;
            cur_parent = chunk.parent;
        }

        let symbol = self
            .state
            .symbols
            .get(identifier)
            .ok_or_else(|| format_err!("failed to find symbol: '{}'", identifier))?;
        bail!("identifier not declared: {}", symbol)
    }
}
