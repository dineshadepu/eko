#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Add,
    Subtract,
    Multiply,
    Divide,

    Assign,
    Equal,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    And,
    Or,
    Not,

    Null,
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Identifier(usize),

    Var,
    If,
    Else,

    Newline,
}
