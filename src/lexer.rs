use std::io::{ErrorKind, Read};

use failure::{bail, format_err};

use crate::result::Result;

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
    Ident(String),

    Var,
    If,
    Else,
    While,

    Return,
    Break,

    Newline,
}

pub struct Lexer<R: Read> {
    source: R,
    peek: Option<u8>,
}

impl<R: Read> Lexer<R> {
    pub fn new(source: R) -> Lexer<R> {
        Lexer { source, peek: None }
    }

    pub fn next(&mut self) -> Result<Option<Token>> {
        self.whitespace()?;

        let byte = match self.source_peek()? {
            Some(byte) => byte,
            None => return Ok(None),
        };

        let token = match byte {
            b'\n' => self.newline()?,
            byte if is_digit(byte) => self.number()?,
            byte if is_op(byte) => self.op()?,
            byte if is_alpha(byte) || byte == b'_' => self.ident()?,
            _ => bail!("unexpected byte: '{}'", byte),
        };

        Ok(Some(token))
    }

    fn newline(&mut self) -> Result<Token> {
        assert_eq!(self.source_advance()?, b'\n');
        Ok(Token::Newline)
    }

    fn number(&mut self) -> Result<Token> {
        let mut buf = Vec::new();
        let mut float = false;
        while let Some(byte) = self.source_peek()? {
            match byte {
                b'.' if float => bail!("unexpected second decimal point"),
                b'.' => float = true,
                byte if !is_digit(byte) => break,
                _ => {}
            }
            self.source_advance()?;
            buf.push(byte);
        }
        let buf = String::from_utf8(buf)?;
        if float {
            Ok(Token::Float(buf.parse()?))
        } else {
            Ok(Token::Integer(buf.parse()?))
        }
    }

    fn op(&mut self) -> Result<Token> {
        let first = self.source_advance()?;
        let second = self.source_peek()?;
        let token = match (first, second) {
            (b'+', _) => Token::Add,
            (b'-', _) => Token::Subtract,
            (b'*', _) => Token::Multiply,
            (b'/', _) => Token::Divide,
            (b'=', Some(b'=')) => {
                self.source_advance()?;
                Token::Equal
            }
            (b'=', _) => Token::Assign,
            (b'<', Some(b'=')) => {
                self.source_advance()?;
                Token::LessEqual
            }
            (b'<', _) => Token::Less,
            (b'>', Some(b'=')) => {
                self.source_advance()?;
                Token::GreaterEqual
            }
            (b'>', _) => Token::Greater,
            (b'(', _) => Token::LeftParen,
            (b')', _) => Token::RightParen,
            (b'{', _) => Token::LeftBrace,
            (b'}', _) => Token::RightBrace,
            (b'&', Some(b'&')) => {
                self.source_advance()?;
                Token::And
            }
            (b'|', Some(b'|')) => {
                self.source_advance()?;
                Token::Or
            }
            (b'!', _) => Token::Not,
            _ => unreachable!("forgot to match op in `is_op`"),
        };
        Ok(token)
    }

    fn ident(&mut self) -> Result<Token> {
        let mut buf = Vec::new();
        while let Some(byte) = self.source_peek()? {
            match byte {
                byte if !is_alpha(byte) && byte != b'_' => break,
                _ => {}
            }
            self.source_advance()?;
            buf.push(byte);
        }
        let buf = String::from_utf8(buf)?;
        let token = match buf.as_str() {
            "null" => Token::Null,
            "true" => Token::Boolean(true),
            "false" => Token::Boolean(false),
            "var" => Token::Var,
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "return" => Token::Return,
            "break" => Token::Break,
            _ => Token::Ident(buf),
        };
        Ok(token)
    }

    fn whitespace(&mut self) -> Result<()> {
        while let Some(byte) = self.source_peek()? {
            match byte {
                // Newlines are treated as tokens.
                b'\n' => break,
                byte if !is_whitespace(byte) => break,
                _ => {}
            }
            self.source_advance()?;
        }
        Ok(())
    }

    fn source_peek(&mut self) -> Result<Option<u8>> {
        if self.peek.is_none() {
            self.peek = read_single_byte(&mut self.source)?;
        }
        Ok(self.peek)
    }

    fn source_advance(&mut self) -> Result<u8> {
        self.source_peek()?;
        self.peek
            .take()
            .ok_or_else(|| format_err!("unexpected end of source"))
    }
}

fn read_single_byte<R: Read>(read: &mut R) -> Result<Option<u8>> {
    let mut buf = [0u8; 1];
    loop {
        return match read.read(&mut buf) {
            Ok(0) => Ok(None),
            Ok(_) => Ok(Some(buf[0])),
            Err(error) => {
                if let ErrorKind::Interrupted = error.kind() {
                    continue;
                }
                Err(error.into())
            }
        };
    }
}

fn is_alpha(byte: u8) -> bool {
    (byte >= b'a' && byte <= b'z') || (byte >= b'A' && byte <= b'Z')
}

fn is_digit(byte: u8) -> bool {
    byte >= b'0' && byte <= b'9'
}

fn is_op(byte: u8) -> bool {
    match byte {
        b'+' | b'-' | b'*' | b'/' | b'<' | b'>' | b'=' | b'(' | b')' | b'{' | b'}' | b'&'
        | b'|' | b'!' => true,
        _ => false,
    }
}

fn is_whitespace(byte: u8) -> bool {
    match byte {
        b' ' | b'\t' | b'\r' | b'\n' => true,
        _ => false,
    }
}
