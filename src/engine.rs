use std::io::{Cursor, Read};

use crate::interpreter::{Context, Interpreter};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::result::Result;
use crate::value::Value;

pub struct Engine {
    ctx: Context,
}

impl Engine {
    pub fn new() -> Engine {
        Engine {
            ctx: Context::new(),
        }
    }

    pub fn evaluate_str(&mut self, source: &str) -> Result<Value> {
        self.evaluate(Cursor::new(source))
    }

    pub fn evaluate<R: Read>(&mut self, source: R) -> Result<Value> {
        let mut lexer = Lexer::new(source);
        let block = Parser::new(&mut lexer).parse()?;
        Interpreter::new().evaluate(&mut self.ctx, &block)
    }
}
