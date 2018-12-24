use crate::syntax::Token;

#[derive(Debug)]
pub struct Block {
    pub expressions: Vec<Expression>,
}

impl Block {
    pub fn new(expressions: Vec<Expression>) -> Block {
        Block { expressions }
    }
}

#[derive(Debug)]
pub enum Expression {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Null,
    Identifier(usize),

    VarDeclaration(Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>),
    Binary(Binary, Box<Expression>, Box<Expression>),
    Unary(Unary, Box<Expression>),

    If(Box<Expression>, Block, Option<Block>),
}

#[derive(Debug)]
pub enum Binary {
    Add,
    Subtract,
    Multiply,
    Divide,

    Equal,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    And,
    Or,
}

impl Binary {
    pub(crate) fn from_token(token: &Token) -> Option<Binary> {
        let binary = match token {
            Token::Add => Binary::Add,
            Token::Subtract => Binary::Subtract,
            Token::Multiply => Binary::Multiply,
            Token::Divide => Binary::Divide,

            Token::Equal => Binary::Equal,
            Token::Less => Binary::Less,
            Token::LessEqual => Binary::LessEqual,
            Token::Greater => Binary::Greater,
            Token::GreaterEqual => Binary::GreaterEqual,

            Token::And => Binary::And,
            Token::Or => Binary::Or,

            _ => return None,
        };
        Some(binary)
    }
}

#[derive(Debug)]
pub enum Unary {
    Negate,
    Not,
}

impl Unary {
    pub(crate) fn from_token(token: &Token) -> Option<Unary> {
        let unary = match token {
            Token::Subtract => Unary::Negate,
            Token::Not => Unary::Not,
            _ => return None,
        };
        Some(unary)
    }
}
