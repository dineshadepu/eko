use std::io::{ErrorKind, Read};

use failure::{bail, format_err};

use crate::{Result, State};

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Token {
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

pub(crate) struct Lexer<'a, R: Read> {
    state: &'a mut State,
    source: R,
    peek: Option<u8>,
}

impl<'a, R: Read> Lexer<'a, R> {
    pub(crate) fn new(state: &'a mut State, source: R) -> Lexer<'a, R> {
        Lexer {
            state,
            source,
            peek: None,
        }
    }

    pub(crate) fn next(&mut self) -> Result<Option<Token>> {
        self.whitespace()?;

        let byte = match self.source_peek()? {
            Some(byte) => byte,
            None => return Ok(None),
        };

        let token = match byte {
            b'\n' => {
                self.source_advance()?;
                Token::Newline
            }
            byte if is_digit(byte) => self.number()?,
            byte if is_operator(byte) => self.operator()?,
            byte if is_alpha(byte) || byte == b'_' => self.identifier()?,
            _ => bail!("unexpected byte: '{}'", byte),
        };

        Ok(Some(token))
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

    fn operator(&mut self) -> Result<Token> {
        let first = self.source_advance()?;
        let second = self.source_peek()?;
        let token = match (first, second) {
            (b'+', _) => Token::Add,
            (b'-', _) => Token::Subtract,
            (b'*', _) => Token::Multiply,
            (b'/', _) => Token::Divide,
            (b'=', Some(b'=')) => Token::Equal,
            (b'=', _) => Token::Assign,
            (b'<', Some(b'=')) => Token::LessEqual,
            (b'<', _) => Token::Less,
            (b'>', Some(b'=')) => Token::GreaterEqual,
            (b'>', _) => Token::Greater,
            (b'(', _) => Token::LeftParen,
            (b')', _) => Token::RightParen,
            (b'{', _) => Token::LeftBrace,
            (b'}', _) => Token::RightBrace,
            _ => unimplemented!("operator not yet implemented"),
        };
        Ok(token)
    }

    fn identifier(&mut self) -> Result<Token> {
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
            "not" => Token::Not,
            "null" => Token::Null,
            "true" => Token::Boolean(true),
            "false" => Token::Boolean(false),
            "and" => Token::And,
            "or" => Token::Or,
            "var" => Token::Var,
            "if" => Token::If,
            "else" => Token::Else,
            _ => Token::Identifier(self.state.symbols.insert(buf)),
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

fn is_operator(byte: u8) -> bool {
    match byte {
        b'+' | b'-' | b'*' | b'/' | b'<' | b'>' | b'=' | b'(' | b')' | b'{' | b'}' => true,
        _ => false,
    }
}

fn is_whitespace(byte: u8) -> bool {
    match byte {
        b' ' | b'\t' | b'\r' | b'\n' => true,
        _ => false,
    }
}
