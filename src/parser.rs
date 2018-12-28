use std::io::Read;

use failure::{bail, format_err};

use crate::lexer::{Lexer, Token};
use crate::result::Result;

#[derive(Debug)]
pub struct Block {
    pub exprs: Vec<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Null,
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Ident(String),

    VarDecl(Box<Expr>),

    If(Box<IfExpr>),

    Assign(Box<AssignExpr>),
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),
}

#[derive(Debug)]
pub struct IfExpr {
    pub condition: Expr,
    pub truthy: Block,
    pub falsey: Block,
}

#[derive(Debug)]
pub struct AssignExpr {
    pub target: Expr,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub op: BinaryOp,
    pub left: Expr,
    pub right: Expr,
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Expr,
}

#[derive(Debug)]
pub enum BinaryOp {
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

impl BinaryOp {
    fn from_token(token: &Token) -> Option<BinaryOp> {
        let binary_op = match token {
            Token::Add => BinaryOp::Add,
            Token::Subtract => BinaryOp::Subtract,
            Token::Multiply => BinaryOp::Multiply,
            Token::Divide => BinaryOp::Divide,

            Token::Equal => BinaryOp::Equal,
            Token::Less => BinaryOp::Less,
            Token::LessEqual => BinaryOp::LessEqual,
            Token::Greater => BinaryOp::Greater,
            Token::GreaterEqual => BinaryOp::GreaterEqual,

            Token::And => BinaryOp::And,
            Token::Or => BinaryOp::Or,

            _ => return None,
        };
        Some(binary_op)
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Negate,
    Not,
}

impl UnaryOp {
    fn from_token(token: &Token) -> Option<UnaryOp> {
        let unary_op = match token {
            Token::Subtract => UnaryOp::Negate,
            Token::Not => UnaryOp::Not,
            _ => return None,
        };
        Some(unary_op)
    }
}

pub struct Parser<'a, R: Read> {
    lexer: &'a mut Lexer<R>,
    peek: Option<Token>,
}

impl<'a, R: Read> Parser<'a, R> {
    pub fn new(lexer: &'a mut Lexer<R>) -> Parser<'a, R> {
        Parser { lexer, peek: None }
    }

    pub fn parse(&mut self) -> Result<Block> {
        self.block_with_terminal(None)
    }

    fn block_with_braces(&mut self) -> Result<Block> {
        self.lexer_advance_expect(Token::LeftBrace)?;
        let block = self.block_with_terminal(Some(Token::RightBrace))?;
        assert_eq!(self.lexer_advance()?, Token::RightBrace);
        Ok(block)
    }

    fn block_with_terminal(&mut self, terminal: Option<Token>) -> Result<Block> {
        // Get rid of any preceding newlines first.
        self.newlines()?;

        let mut exprs = Vec::new();
        // Whether a newline was found in the previous iteration.
        let mut newline = true;

        while let Some(token) = self.lexer_peek()? {
            // Stop if `terminal` has been reached.
            if let Some(terminal) = &terminal {
                if token == terminal {
                    break;
                }
            }

            // If the previous line did not end with a newline, there cannot
            // be a new expression. Therefore, this should fail.
            if !newline {
                // This will fail with the correct error message.
                self.lexer_advance_expect(Token::Newline)?;
            }

            // Parse the expression and the newlines after it.
            exprs.push(self.expr()?);
            newline = self.newline()?;
            self.newlines()?;
        }

        Ok(Block { exprs })
    }

    fn expr(&mut self) -> Result<Expr> {
        if self.lexer_peek()?.is_none() {
            // Using this call for the error message it provides. There is not
            // actually a next token which means this call WILL be an `Err`
            // (which is exactly what we want).
            self.lexer_advance()?;
            unreachable!();
        }

        let expr = match self.lexer_peek()?.unwrap() {
            Token::Var => Expr::VarDecl(self.var_decl_expr()?.into()),

            Token::If => Expr::If(self.if_expr()?.into()),

            _ => self.expr_assign()?,
        };

        Ok(expr)
    }

    fn var_decl_expr(&mut self) -> Result<Expr> {
        assert_eq!(self.lexer_advance()?, Token::Var);
        Ok(self.expr()?.into())
    }

    fn if_expr(&mut self) -> Result<IfExpr> {
        assert_eq!(self.lexer_advance()?, Token::If);

        let condition = self.expr()?;
        let truthy = self.block_with_braces()?;
        let falsey = if let Some(Token::Else) = self.lexer_peek()? {
            assert_eq!(self.lexer_advance()?, Token::Else);
            if let Some(Token::If) = self.lexer_peek()? {
                Block {
                    exprs: vec![Expr::If(self.if_expr()?.into())],
                }
            } else {
                self.block_with_braces()?
            }
        } else {
            Block {
                exprs: vec![Expr::Null],
            }
        };

        let if_expr = IfExpr {
            condition,
            truthy,
            falsey,
        };

        Ok(if_expr)
    }

    fn expr_assign(&mut self) -> Result<Expr> {
        let target = self.expr_logical()?;

        let token = match self.lexer_peek()? {
            Some(token) => token,
            None => return Ok(target),
        };
        match token {
            Token::Assign => {}
            _ => return Ok(target),
        }
        match target {
            Expr::Ident(_) => {}
            _ => bail!("invalid left expression in assignment"),
        };
        self.lexer_advance()?;

        let assign_expr = AssignExpr {
            target,
            expr: self.expr()?,
        };

        Ok(Expr::Assign(assign_expr.into()))
    }

    fn expr_logical(&mut self) -> Result<Expr> {
        let mut left = self.expr_compare()?;
        loop {
            let token = match self.lexer_peek()? {
                Some(token) => token,
                None => return Ok(left),
            };

            match token {
                Token::And | Token::Or => {}
                _ => return Ok(left),
            }

            let op = BinaryOp::from_token(&self.lexer_advance()?)
                .ok_or_else(|| unreachable!("did not match op in `BinaryOp::from_token`"))
                .unwrap();
            let binary_expr = BinaryExpr {
                op,
                left,
                right: self.expr_compare()?,
            };

            left = Expr::Binary(binary_expr.into());
        }
    }

    fn expr_compare(&mut self) -> Result<Expr> {
        let mut left = self.expr_add()?;
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

            let op = BinaryOp::from_token(&self.lexer_advance()?)
                .ok_or_else(|| unreachable!("did not match op in `BinaryOp::from_token`"))
                .unwrap();
            let binary_expr = BinaryExpr {
                op,
                left,
                right: self.expr_add()?,
            };

            left = Expr::Binary(binary_expr.into());
        }
    }

    fn expr_add(&mut self) -> Result<Expr> {
        let mut left = self.expr_multiply()?;
        loop {
            let token = match self.lexer_peek()? {
                Some(token) => token,
                None => return Ok(left),
            };

            match token {
                Token::Add | Token::Subtract => {}
                _ => return Ok(left),
            }

            let op = BinaryOp::from_token(&self.lexer_advance()?)
                .ok_or_else(|| unreachable!("did not match op in `BinaryOp::from_token`"))
                .unwrap();
            let binary_expr = BinaryExpr {
                op,
                left,
                right: self.expr_multiply()?,
            };

            left = Expr::Binary(binary_expr.into());
        }
    }

    fn expr_multiply(&mut self) -> Result<Expr> {
        let mut left = self.expr_unary()?;
        loop {
            let token = match self.lexer_peek()? {
                Some(token) => token,
                None => return Ok(left),
            };

            match token {
                Token::Multiply | Token::Divide => {}
                _ => return Ok(left),
            }

            let op = BinaryOp::from_token(&self.lexer_advance()?)
                .ok_or_else(|| unreachable!("did not match op in `BinaryOp::from_token`"))
                .unwrap();
            let binary_expr = BinaryExpr {
                op,
                left,
                right: self.expr_unary()?,
            };

            left = Expr::Binary(binary_expr.into());
        }
    }

    fn expr_unary(&mut self) -> Result<Expr> {
        let token = match self.lexer_peek()? {
            Some(token) => token,
            None => return self.expr_term(),
        };

        match token {
            Token::Subtract | Token::Not => {}
            _ => return self.expr_term(),
        }

        let op = UnaryOp::from_token(&self.lexer_advance()?)
            .ok_or_else(|| unreachable!("did not match op in `UnaryOp::from_token`"))
            .unwrap();
        let unary_expr = UnaryExpr {
            op,
            expr: self.expr_unary()?,
        };

        Ok(Expr::Unary(unary_expr.into()))
    }

    fn expr_term(&mut self) -> Result<Expr> {
        let expr = match self.lexer_advance()? {
            Token::Null => Expr::Null,
            Token::Integer(integer) => Expr::Integer(integer),
            Token::Float(float) => Expr::Float(float),
            Token::Boolean(boolean) => Expr::Boolean(boolean),
            Token::Ident(ident) => Expr::Ident(ident),
            Token::LeftParen => {
                let expr = self.expr()?;
                self.lexer_advance_expect(Token::RightParen)?;
                expr
            }
            token => bail!("unexpected token: '{:?}'", token),
        };
        Ok(expr)
    }

    fn newline(&mut self) -> Result<bool> {
        if let Some(Token::Newline) = self.lexer_peek()? {
            Ok(true)
        } else {
            Ok(false)
        }
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
