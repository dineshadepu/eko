use std::io::{Cursor, Read};

use failure::{format_err, Error};

use crate::compiler::{Chunk, Compiler, Constant};
use crate::fiber::Fiber;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::pool::Pool;
use crate::value::Value;

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

    pub fn evaluate_str(&mut self, source: &str) -> Result<Value> {
        self.evaluate(Cursor::new(source))
    }

    pub fn evaluate<R: Read>(&mut self, source: R) -> Result<Value> {
        let mut lexer = Lexer::new(&mut self.state, source);
        let block = Parser::new(&mut lexer).parse()?;
        let entry = Compiler::new(&mut self.state).compile(block)?;

        let mut fiber = Fiber::new(&self.state, entry)?;
        fiber.finish(&self.state)?;

        fiber
            .return_value()
            .cloned()
            .ok_or_else(|| format_err!("return value is empty"))
    }
}

pub struct State {
    pub identifiers: Pool<String>,
    pub constants: Pool<Constant>,
    pub chunks: Pool<Chunk>,
}

impl State {
    pub fn new() -> State {
        State {
            identifiers: Pool::new(),
            constants: Pool::new(),
            chunks: Pool::new(),
        }
    }
}
