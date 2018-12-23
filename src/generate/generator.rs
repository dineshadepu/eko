use failure::{bail, format_err};

use crate::syntax::ast::*;
use crate::{Chunk, Closed, Constant, Instruction, Result, State};

pub struct Generator<'a> {
    state: &'a mut State,
}

impl<'a> Generator<'a> {
    pub fn new(state: &'a mut State) -> Generator<'a> {
        Generator { state }
    }

    pub fn generate(&mut self, block: Block) -> Result<usize> {
        let mut chunk = Chunk::new();
        self.block(&mut chunk, block)?;
        Ok(self.state.chunks.push(chunk))
    }

    fn block(&mut self, chunk: &mut Chunk, block: Block) -> Result<()> {
        let is_empty = block.expressions.is_empty();

        for expression in block.expressions {
            self.expression(chunk, expression)?;

            // After each expression/line, the final result should be popped to
            // avoid polluting the operand stack.
            chunk.instructions.push(Instruction::Pop);
        }

        // Result of the last expression (if there is one) should not be popped,
        // as it is the return value of the block.
        if !is_empty {
            chunk.instructions.pop();
        }

        Ok(())
    }

    fn expression(&mut self, chunk: &mut Chunk, expression: Expression) -> Result<()> {
        match expression {
            Expression::Null => chunk.instructions.push(Instruction::PushNull),
            Expression::Integer(integer) => self.constant(chunk, Constant::Integer(integer))?,
            Expression::Float(integer) => self.constant(chunk, Constant::Float(integer))?,
            Expression::Boolean(boolean) => self.boolean(chunk, boolean),
            Expression::Identifier(identifier) => self.identifier(chunk, identifier)?,
            Expression::Assignment(left, right) => self.assignment(chunk, *left, *right)?,
            Expression::VarDeclaration(expression) => self.var_declaration(chunk, *expression)?,
            Expression::Binary(binary, left, right) => self.binary(chunk, binary, *left, *right)?,
            Expression::Unary(unary, expression) => self.unary(chunk, unary, *expression)?,
            Expression::If(condition, success, failure) => {
                self.r#if(chunk, *condition, success, failure)?
            }
        }
        Ok(())
    }

    fn constant(&mut self, chunk: &mut Chunk, constant: Constant) -> Result<()> {
        let constant = self.state.constants.insert(constant);
        chunk.instructions.push(Instruction::PushConstant(constant));
        Ok(())
    }

    fn boolean(&mut self, chunk: &mut Chunk, boolean: bool) {
        chunk.instructions.push(Instruction::PushBoolean(boolean));
    }

    fn identifier(&mut self, chunk: &mut Chunk, identifier: usize) -> Result<()> {
        if let Some(local) = chunk.identifiers.get_by_value(&identifier) {
            chunk.instructions.push(Instruction::PushLocal(local));
            return Ok(());
        }

        let closed = self.chunk_closed(chunk, identifier)?;
        chunk.instructions.push(Instruction::PushClosed(closed));
        Ok(())
    }

    fn assignment(&mut self, chunk: &mut Chunk, left: Expression, right: Expression) -> Result<()> {
        // Verify that the left expression is a valid target for assignment.
        let identifier = match left {
            Expression::Identifier(identifier) => identifier,
            _ => bail!("invalid left expression in var declaration"),
        };

        self.expression(chunk, right)?;

        if let Some(local) = chunk.identifiers.get_by_value(&identifier) {
            chunk.instructions.push(Instruction::PopLocal(local));
            chunk.instructions.push(Instruction::PushLocal(local));
            return Ok(());
        }

        let closed = self.chunk_closed(chunk, identifier)?;
        chunk.instructions.push(Instruction::PopClosed(closed));
        chunk.instructions.push(Instruction::PushClosed(closed));
        Ok(())
    }

    fn var_declaration(&mut self, chunk: &mut Chunk, expression: Expression) -> Result<()> {
        match expression {
            Expression::Identifier(identifier) => {
                let local = self.chunk_define_local(chunk, identifier)?;
                chunk.instructions.push(Instruction::PushNull);
                chunk.instructions.push(Instruction::PopLocal(local));
                chunk.instructions.push(Instruction::PushLocal(local));
            }
            Expression::Assignment(left, right) => {
                // Verify that the left expression is a valid target for
                // a variable declaration.
                let identifier = match *left {
                    Expression::Identifier(identifier) => identifier,
                    _ => bail!("invalid left expression in var declaration"),
                };

                self.expression(chunk, *right)?;

                let local = self.chunk_define_local(chunk, identifier)?;
                chunk.instructions.push(Instruction::PopLocal(local));
                chunk.instructions.push(Instruction::PushLocal(local));
            }
            _ => bail!("invalid expression in var declaration"),
        }
        Ok(())
    }

    fn binary(
        &mut self,
        chunk: &mut Chunk,
        binary: Binary,
        left: Expression,
        right: Expression,
    ) -> Result<()> {
        self.expression(chunk, left)?;
        self.expression(chunk, right)?;
        chunk.instructions.push(binary.into());
        Ok(())
    }

    fn unary(&mut self, chunk: &mut Chunk, unary: Unary, expression: Expression) -> Result<()> {
        self.expression(chunk, expression)?;
        chunk.instructions.push(unary.into());
        Ok(())
    }

    fn r#if(
        &mut self,
        chunk: &mut Chunk,
        condition: Expression,
        success: Block,
        failure: Option<Block>,
    ) -> Result<()> {
        self.expression(chunk, condition)?;

        // Save the index where the jump-to-failure instruction should be
        // inserted later.
        let failure_jump = chunk.instructions.len();

        self.block(chunk, success)?;

        // Save the index where the jump-past-failure instruction should be
        // inserted later.
        let success_jump = chunk.instructions.len() + 1;

        // Insert the jump-to-failure instruction at the index saved earlier.
        chunk.instructions.insert(
            failure_jump,
            Instruction::JumpFalsey(chunk.instructions.len() + 2),
        );

        if let Some(failure) = failure {
            self.block(chunk, failure)?;
        } else {
            chunk.instructions.push(Instruction::PushNull);
        }

        // Insert the jump-past-failure instruction at the index saved earlier.
        chunk.instructions.insert(
            success_jump,
            Instruction::Jump(chunk.instructions.len() + 1),
        );

        Ok(())
    }

    /// Defines a new local of the given identifier on the given chunk.
    ///
    /// Fails if the local is already defined, be it under the same identifier
    /// or a different one.
    fn chunk_define_local(&mut self, chunk: &mut Chunk, identifier: usize) -> Result<usize> {
        if chunk.identifiers.get_by_value(&identifier).is_some() {
            let symbol = self.state_symbol(identifier)?;
            bail!("identifier already declared: '{}'", symbol);
        }
        Ok(chunk.identifiers.insert(identifier))
    }

    /// Gets the closed of the given identifier from the given chunk.
    ///
    /// Starts the search from the given chunk's parent. Fails if the given
    /// identifier cannot be found in the given chunk or any of its parent
    /// chunks.
    fn chunk_closed(&mut self, chunk: &mut Chunk, identifier: usize) -> Result<Closed> {
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

        let symbol = self.state_symbol(identifier)?;
        bail!("identifier not declared: {}", symbol)
    }

    /// Gets the symbol of the given identifier from the current state.
    fn state_symbol(&self, identifier: usize) -> Result<&str> {
        let symbol = self
            .state
            .symbols
            .get(identifier)
            .ok_or_else(|| format_err!("failed to find symbol: {}", identifier))?
            .as_str();
        Ok(symbol)
    }
}
