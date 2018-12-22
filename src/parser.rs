use std::io::Read;

use failure::{bail, format_err};

use crate::ast::*;
use crate::{Lexer, Result, Token};

pub(crate) struct Parser<'a, R: Read> {
    lexer: &'a mut Lexer<'a, R>,
    peek: Option<Token>,
}

impl<'a, R: Read> Parser<'a, R> {
    pub(crate) fn new(lexer: &'a mut Lexer<'a, R>) -> Parser<'a, R> {
        Parser { lexer, peek: None }
    }

    pub(crate) fn block(&mut self) -> Result<Block> {
        let mut expressions = Vec::new();
        while self.lexer_peek()?.is_some() {
            expressions.push(self.expression()?);
        }
        Ok(Block::new(expressions))
    }

    fn block_with_terminal(&mut self, terminal: &Token) -> Result<Block> {
        let mut expressions = Vec::new();
        while let Some(token) = self.lexer_peek()? {
            if token == terminal {
                return Ok(Block::new(expressions));
            }
            expressions.push(self.expression()?);
        }
        Ok(Block::new(expressions))
    }

    fn expression(&mut self) -> Result<Expression> {
        let token = self
            .lexer_peek()?
            .cloned()
            .ok_or_else(|| self.lexer_advance().unwrap_err())?;
        match token {
            Token::Var => self.var_declaration(),
            Token::If => self.r#if(),
            _ => self.assignment(),
        }
    }

    fn var_declaration(&mut self) -> Result<Expression> {
        assert_eq!(self.lexer_advance()?, Token::Var);
        Ok(Expression::VarDeclaration(self.expression()?.into()))
    }

    fn r#if(&mut self) -> Result<Expression> {
        assert_eq!(self.lexer_advance()?, Token::If);
        let condition = self.expression()?;
        self.lexer_advance_expect(Token::LeftBrace)?;
        self.newlines()?;
        let success = self.block_with_terminal(&Token::RightBrace)?;
        self.newlines()?;
        self.lexer_advance_expect(Token::RightBrace)?;
        Ok(Expression::If(condition.into(), success, None))
    }

    fn assignment(&mut self) -> Result<Expression> {
        let mut left = self.logical()?;
        loop {
            let token = match self.lexer_peek()? {
                Some(token) => token,
                None => return Ok(left),
            };
            match token {
                Token::Assign => {}
                _ => return Ok(left),
            }
            match left {
                Expression::Identifier(_) => {}
                _ => bail!("invalid left expression in assignment"),
            };
            self.lexer_advance()?;
            left = Expression::Assignment(left.into(), self.logical()?.into());
        }
    }

    fn logical(&mut self) -> Result<Expression> {
        let mut left = self.compare()?;
        loop {
            let token = match self.lexer_peek()? {
                Some(token) => token,
                None => return Ok(left),
            };
            match token {
                Token::And | Token::Or => {}
                _ => return Ok(left),
            }
            let binary = Binary::from_token(&self.lexer_advance()?).unwrap();
            left = Expression::Binary(binary, left.into(), self.compare()?.into());
        }
    }

    fn compare(&mut self) -> Result<Expression> {
        let mut left = self.add()?;
        loop {
            let token = match self.lexer_peek()? {
                Some(token) => token,
                None => return Ok(left),
            };
            match token {
                Token::Equal
                | Token::Less
                | Token::LessEqual
                | Token::Greater
                | Token::GreaterEqual => {}
                _ => return Ok(left),
            }
            let binary = Binary::from_token(&self.lexer_advance()?).unwrap();
            left = Expression::Binary(binary, left.into(), self.add()?.into());
        }
    }

    fn add(&mut self) -> Result<Expression> {
        let mut left = self.multiply()?;
        loop {
            let token = match self.lexer_peek()? {
                Some(token) => token,
                None => return Ok(left),
            };
            match token {
                Token::Add | Token::Subtract => {}
                _ => return Ok(left),
            }
            let binary = Binary::from_token(&self.lexer_advance()?).unwrap();
            left = Expression::Binary(binary, left.into(), self.multiply()?.into());
        }
    }

    fn multiply(&mut self) -> Result<Expression> {
        let mut left = self.unary()?;
        loop {
            let token = match self.lexer_peek()? {
                Some(token) => token,
                None => return Ok(left),
            };
            match token {
                Token::Multiply | Token::Divide => {}
                _ => return Ok(left),
            }
            let binary = Binary::from_token(&self.lexer_advance()?).unwrap();
            left = Expression::Binary(binary, left.into(), self.unary()?.into());
        }
    }

    fn unary(&mut self) -> Result<Expression> {
        let token = match self.lexer_peek()? {
            Some(token) => token,
            None => return self.term(),
        };
        match token {
            Token::Subtract | Token::Not => {}
            _ => return self.term(),
        }
        let unary = Unary::from_token(&self.lexer_advance()?).unwrap();
        Ok(Expression::Unary(unary, self.unary()?.into()))
    }

    fn term(&mut self) -> Result<Expression> {
        let expression = match self.lexer_advance()? {
            Token::Integer(integer) => Expression::Integer(integer),
            Token::Float(integer) => Expression::Float(integer),
            Token::Boolean(boolean) => Expression::Boolean(boolean),
            Token::Identifier(identifier) => Expression::Identifier(identifier),
            Token::LeftParen => {
                let expression = self.expression()?;
                self.lexer_advance_expect(Token::RightParen)?;
                expression
            }
            token => bail!("unexpected token: '{:?}'", token),
        };
        Ok(expression)
    }

    fn newlines(&mut self) -> Result<()> {
        while self.lexer_peek()? == Some(&Token::Newline) {
            self.lexer_advance()?;
        }
        Ok(())
    }

    fn lexer_peek(&mut self) -> Result<Option<&Token>> {
        if self.peek.is_none() {
            self.peek = self.lexer.next()?;
        }
        Ok(self.peek.as_ref())
    }

    fn lexer_advance_expect(&mut self, expect: Token) -> Result<()> {
        let token = self.lexer_advance()?;
        if token == expect {
            Ok(())
        } else {
            Err(format_err!(
                "unexpected token: '{:?}', expected: '{:?}'",
                token,
                expect
            ))
        }
    }

    fn lexer_advance(&mut self) -> Result<Token> {
        self.lexer_peek()?;
        self.peek
            .take()
            .ok_or_else(|| format_err!("unexpected end of source"))
    }
}
