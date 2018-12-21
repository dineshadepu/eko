use std::io::{Cursor, Read};

use failure::{format_err, Error};

pub(crate) use crate::fiber::Fiber;
pub(crate) use crate::generator::{Chunk, Constant, Generator, Instruction};
pub(crate) use crate::lexer::{Lexer, Token};
pub(crate) use crate::parser::Parser;
pub(crate) use crate::pool::Pool;

pub mod ast;
mod fiber;
mod generator;
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

    pub fn evaluate(&mut self, source: &str) -> Result<Value> {
        let mut compiler = Compiler::new(&mut self.state);
        let entry = compiler.compile_str(source)?;
        let mut fiber = Fiber::new(&self.state, entry)?;
        fiber.finish(&self.state)?;
        fiber
            .return_value()
            .cloned()
            .ok_or_else(|| format_err!("return value is empty"))
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
        Generator::new(&mut self.state).generate(expression)
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
