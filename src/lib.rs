pub use crate::engine::Engine;
pub use crate::result::Result;
pub use crate::value::Value;

mod engine;
mod interpreter;
mod lexer;
mod parser;
mod value;

mod result {
    use failure::Error;

    pub type Result<T> = std::result::Result<T, Error>;
}
