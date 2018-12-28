use std::io::{Cursor, Read};

use crate::interpreter::{Interpreter, Scope};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::result::Result;
use crate::value::Value;

pub struct Engine {
    scope: Scope,
}

impl Engine {
    pub fn new() -> Engine {
        Engine {
            scope: Scope::new(),
        }
    }

    pub fn evaluate_str(&mut self, source: &str) -> Result<Value> {
        self.evaluate(Cursor::new(source))
    }

    pub fn evaluate<R: Read>(&mut self, source: R) -> Result<Value> {
        let mut lexer = Lexer::new(source);
        let block = Parser::new(&mut lexer).parse()?;
        Interpreter::new().evaluate(&mut self.scope, block)
    }
}
