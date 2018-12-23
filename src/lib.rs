use std::io::{Cursor, Read};

use failure::{format_err, Error};

use crate::generate::Generator;
use crate::runtime::{Chunk, Constant, Fiber, Instruction, Value};
use crate::syntax::{Lexer, Parser, Token};
use crate::util::Pool;

pub mod generate;
pub mod runtime;
pub mod syntax;
pub mod util;

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
        let expression = parser.parse()?;
        Generator::new(&mut self.state).generate(expression)
    }
}

pub struct State {
    identifiers: Pool<String>,
    constants: Pool<Constant>,
    chunks: Pool<Chunk>,
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
