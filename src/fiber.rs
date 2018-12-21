use failure::format_err;

use crate::{Instruction, Result, State, Value};

pub(crate) struct Fiber {
    operands: Vec<Value>,
    frames: Vec<Frame>,
}

impl Fiber {
    pub(crate) fn new(state: &State, entry: usize) -> Result<Fiber> {
        let locals = state
            .chunks
            .get(entry)
            .ok_or_else(|| format_err!("failed to find chunk: {}", entry))?
            .identifiers
            .len();
        let fiber = Fiber {
            operands: Vec::new(),
            frames: vec![Frame::new(entry, locals)],
        };
        Ok(fiber)
    }

    pub(crate) fn return_value(&self) -> Option<&Value> {
        if self.is_finished() {
            self.operands.last()
        } else {
            None
        }
    }

    pub(crate) fn is_finished(&self) -> bool {
        self.frames.is_empty()
    }

    pub(crate) fn finish(&mut self, state: &State) -> Result<()> {
        while self.step(state)?.is_some() {}
        Ok(())
    }

    pub(crate) fn step(&mut self, state: &State) -> Result<Option<()>> {
        use self::Instruction::*;

        if self.frames.is_empty() {
            return Ok(None);
        }

        let frame = self.cur_frame_mut();
        let instruction = state
            .chunks
            .get(frame.chunk)
            .ok_or_else(|| format_err!("failed to find chunk: {}", frame.chunk))?
            .instructions
            .get(frame.step());
        let instruction = match instruction {
            Some(instruction) => instruction,
            None => {
                self.frames.pop();
                return Ok(None);
            }
        };

        match instruction {
            PushConstant(constant) => self.push_constant(state, *constant)?,
            PushBoolean(boolean) => self.operands.push((*boolean).into()),
            PushNull => self.operands.push(Value::Null),
            Pop => self.pop()?,

            PushLocal(local) => self.push_local(*local)?,
            PopLocal(local) => self.pop_local(*local)?,

            instruction if instruction.is_binary() => self.binary(*instruction)?,
            instruction if instruction.is_unary() => self.unary(*instruction)?,

            JumpFalsey(instruction) => self.jump_falsey(*instruction)?,
            Jump(instruction) => self.jump(*instruction),

            _ => unimplemented!("instruction not yet implemented"),
        }

        Ok(Some(()))
    }

    fn push_constant(&mut self, state: &State, constant: usize) -> Result<()> {
        let constant = state
            .constants
            .get(constant)
            .ok_or_else(|| format_err!("failed to find constant: {}", constant))?;
        self.operands.push(constant.into());
        Ok(())
    }

    fn pop(&mut self) -> Result<()> {
        self.operands_pop()?;
        Ok(())
    }

    fn push_local(&mut self, local: usize) -> Result<()> {
        let value = self
            .cur_frame()
            .locals
            .get(local)
            .ok_or_else(|| format_err!("failed to find local: {}", local))?;
        self.operands.push(value.clone());
        Ok(())
    }

    fn pop_local(&mut self, local: usize) -> Result<()> {
        let value = self.operands_pop()?;
        *self
            .cur_frame_mut()
            .locals
            .get_mut(local)
            .ok_or_else(|| format_err!("failed to find local: {}", local))? = value;
        Ok(())
    }

    fn binary(&mut self, operation: Instruction) -> Result<()> {
        use self::Instruction::*;
        use self::Value::*;

        let right = self.operands_pop()?;
        let left = self.operands_pop()?;

        let result = match (operation, left, right) {
            (Add, Integer(left), Integer(right)) => Integer(left + right),
            (Add, Integer(left), Float(right)) => Float(left as f64 + right),
            (Add, Float(left), Integer(right)) => Float(left + right as f64),
            (Add, Float(left), Float(right)) => Float(left + right),

            (Subtract, Integer(left), Integer(right)) => Integer(left - right),
            (Subtract, Integer(left), Float(right)) => Float(left as f64 - right),
            (Subtract, Float(left), Integer(right)) => Float(left - right as f64),
            (Subtract, Float(left), Float(right)) => Float(left - right),

            (Multiply, Integer(left), Integer(right)) => Integer(left * right),
            (Multiply, Integer(left), Float(right)) => Float(left as f64 * right),
            (Multiply, Float(left), Integer(right)) => Float(left * right as f64),
            (Multiply, Float(left), Float(right)) => Float(left * right),

            (Divide, Integer(left), Integer(right)) => Integer(left / right),
            (Divide, Integer(left), Float(right)) => Float(left as f64 / right),
            (Divide, Float(left), Integer(right)) => Float(left / right as f64),
            (Divide, Float(left), Float(right)) => Float(left / right),

            (Equal, left, right) => Boolean(left == right),

            (Less, Integer(left), Integer(right)) => Boolean(left < right),
            (Less, Integer(left), Float(right)) => Boolean((left as f64) < right),
            (Less, Float(left), Integer(right)) => Boolean(left < right as f64),
            (Less, Float(left), Float(right)) => Boolean(left < right),

            (LessEqual, Integer(left), Integer(right)) => Boolean(left <= right),
            (LessEqual, Integer(left), Float(right)) => Boolean((left as f64) <= right),
            (LessEqual, Float(left), Integer(right)) => Boolean(left <= right as f64),
            (LessEqual, Float(left), Float(right)) => Boolean(left <= right),

            (Greater, Integer(left), Integer(right)) => Boolean(left > right),
            (Greater, Integer(left), Float(right)) => Boolean(left as f64 > right),
            (Greater, Float(left), Integer(right)) => Boolean(left > right as f64),
            (Greater, Float(left), Float(right)) => Boolean(left > right),

            (GreaterEqual, Integer(left), Integer(right)) => Boolean(left >= right),
            (GreaterEqual, Integer(left), Float(right)) => Boolean(left as f64 >= right),
            (GreaterEqual, Float(left), Integer(right)) => Boolean(left >= right as f64),
            (GreaterEqual, Float(left), Float(right)) => Boolean(left >= right),

            (And, left, right) => Boolean(left.is_truthy() && right.is_truthy()),

            (Or, left, right) => Boolean(left.is_truthy() || right.is_truthy()),

            _ => unimplemented!("binary not yet implemented"),
        };

        self.operands.push(result);
        Ok(())
    }

    fn unary(&mut self, operation: Instruction) -> Result<()> {
        use self::Instruction::*;
        use self::Value::*;

        let value = self.operands_pop()?;

        let result = match (operation, value) {
            (Negate, Integer(integer)) => Integer(-integer),
            (Negate, Float(float)) => Float(-float),

            (Not, value) => Boolean(!value.is_truthy()),

            _ => unimplemented!("unary not yet implemented"),
        };

        self.operands.push(result);
        Ok(())
    }

    fn jump_falsey(&mut self, instruction: usize) -> Result<()> {
        let value = self.operands_pop()?;
        if !value.is_truthy() {
            self.cur_frame_mut().cur_instruction = instruction;
        }
        Ok(())
    }

    fn jump(&mut self, instruction: usize) {
        self.cur_frame_mut().cur_instruction = instruction;
    }

    fn operands_pop(&mut self) -> Result<Value> {
        self.operands
            .pop()
            .ok_or_else(|| format_err!("failed to pop empty operand stack"))
    }

    fn cur_frame(&self) -> &Frame {
        self.frames.last().expect("`frames` should not be empty")
    }

    fn cur_frame_mut(&mut self) -> &mut Frame {
        self.frames
            .last_mut()
            .expect("`frames` should not be empty")
    }
}

struct Frame {
    locals: Vec<Value>,
    cur_instruction: usize,
    chunk: usize,
}

impl Frame {
    fn new(chunk: usize, locals: usize) -> Frame {
        Frame {
            locals: vec![Value::Null; locals],
            cur_instruction: 0,
            chunk,
        }
    }

    fn step(&mut self) -> usize {
        let instruction = self.cur_instruction;
        self.cur_instruction += 1;
        instruction
    }
}
