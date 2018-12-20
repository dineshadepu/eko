use std::io::{Cursor, Read};
use std::mem::size_of;

use failure::{bail, format_err, Error};

use crate::ast::*;
pub(crate) use crate::lexer::{Lexer, Token};
pub(crate) use crate::parser::Parser;
pub(crate) use crate::pool::Pool;

pub mod ast;
mod lexer;
mod parser;
mod pool;

pub type Result<T> = std::result::Result<T, Error>;

pub struct Engine {
    state: State,
}

impl Engine {
    pub fn new() -> Engine {
        Engine {
            state: State::new(),
        }
    }

    pub fn evaluate_expression(&mut self, source: &str) -> Result<Value> {
        let mut compiler = Compiler::new(&mut self.state);
        let entry = compiler.compile_str(source)?;
        let mut fiber = Fiber::new(&self.state, entry)?;
        fiber.finish(&self.state)?;
        fiber.operands_pop()
    }
}

struct Compiler<'a> {
    state: &'a mut State,
}

impl<'a> Compiler<'a> {
    fn new(state: &'a mut State) -> Compiler<'a> {
        Compiler { state }
    }

    fn compile_str(&mut self, source: &str) -> Result<usize> {
        self.compile(Cursor::new(source))
    }

    fn compile<R: Read>(&mut self, source: R) -> Result<usize> {
        let mut lexer = Lexer::new(&mut self.state, source);
        let mut parser = Parser::new(&mut lexer);
        let expression = parser.block()?;
        Generator::new(&mut self.state).block(expression)
    }
}

struct State {
    symbols: Pool<String>,
    constants: Pool<Constant>,
    chunks: Pool<Chunk>,
}

impl State {
    fn new() -> State {
        State {
            symbols: Pool::new(),
            constants: Pool::new(),
            chunks: Pool::new(),
        }
    }
}

#[derive(Debug)]
struct Chunk {
    parent: Option<usize>,
    identifiers: Pool<usize>,
    instructions: Vec<Instruction>,
}

impl Chunk {
    fn new() -> Chunk {
        Chunk {
            parent: None,
            identifiers: Pool::new(),
            instructions: Vec::new(),
        }
    }

    fn with_parent(parent: usize) -> Chunk {
        Chunk {
            parent: Some(parent),
            identifiers: Pool::new(),
            instructions: Vec::new(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum Instruction {
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
}

impl Instruction {
    fn is_binary(&self) -> bool {
        use self::Instruction::*;

        match self {
            Add | Subtract | Multiply | Divide | Equal | Less | LessEqual | Greater
            | GreaterEqual | And | Or => true,
            _ => false,
        }
    }

    fn is_unary(&self) -> bool {
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
pub struct Closed(usize);

impl Closed {
    const CLOSED_BITS: usize = size_of::<usize>() - 2;

    fn new(parents: usize, closed: usize) -> Closed {
        Closed((parents << Self::CLOSED_BITS) + closed)
    }

    fn parents(&self) -> usize {
        self.0 >> Self::CLOSED_BITS
    }

    fn closed(&self) -> usize {
        self.0 % (1 << Self::CLOSED_BITS)
    }
}

#[derive(PartialEq)]
enum Constant {
    Integer(i64),
    Float(f64),
}

impl From<i64> for Constant {
    fn from(i: i64) -> Constant {
        Constant::Integer(i)
    }
}

struct Generator<'a> {
    state: &'a mut State,
}

impl<'a> Generator<'a> {
    fn new(state: &'a mut State) -> Generator<'a> {
        Generator { state }
    }

    fn block(&mut self, mut block: Block) -> Result<usize> {
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

        Ok(self.state.chunks.push(chunk))
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

struct Fiber {
    operands: Vec<Value>,
    frames: Vec<Frame>,
}

impl Fiber {
    fn new(state: &State, entry: usize) -> Result<Fiber> {
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

    fn finish(&mut self, state: &State) -> Result<()> {
        while self.step(state)?.is_some() {}
        Ok(())
    }

    fn step(&mut self, state: &State) -> Result<Option<()>> {
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
            none => return Ok(None),
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

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Null,
    Integer(i64),
    Float(f64),
    Boolean(bool),
}

impl Value {
    fn is_truthy(&self) -> bool {
        use self::Value::*;

        match self {
            Null | Boolean(false) => false,
            Integer(_) | Float(_) | Boolean(true) => true,
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

impl From<&Constant> for Value {
    fn from(constant: &Constant) -> Value {
        match constant {
            Constant::Integer(integer) => Value::Integer(*integer),
            Constant::Float(float) => Value::Float(*float),
        }
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
