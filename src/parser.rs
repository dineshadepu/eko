use std::io::Read;

use failure::{bail, format_err};

use crate::lexer::{Lexer, Token};
use crate::result::Result;

#[derive(Clone, Debug)]
pub struct Block {
    pub exprs: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Null,
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Ident(String),

    VarDecl(Box<Expr>),
    FuncDecl(Box<FuncDeclExpr>),

    If(Box<IfExpr>),
    While(Box<WhileExpr>),
    TryCatch(Box<TryCatchExpr>),

    Return(Box<Expr>),
    Break(Box<Expr>),
    Throw(Box<Expr>),

    Assign(Box<AssignExpr>),
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),
}

#[derive(Clone, Debug)]
pub struct FuncDeclExpr {
    pub name: Option<String>,
    pub params: Params,
    pub block: Block,
}

#[derive(Clone, Debug)]
pub struct Params {
    pub idents: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct IfExpr {
    pub condition: Expr,
    pub truthy_block: Block,
    pub falsey_block: Block,
}

#[derive(Clone, Debug)]
pub struct WhileExpr {
    pub condition: Expr,
    pub block: Block,
}

#[derive(Clone, Debug)]
pub struct TryCatchExpr {
    pub try_block: Block,
    pub error_ident: String,
    pub catch_block: Block,
}

#[derive(Clone, Debug)]
pub struct AssignExpr {
    pub target: Expr,
    pub value: Expr,
}

#[derive(Clone, Debug)]
pub struct BinaryExpr {
    pub op: BinaryOp,
    pub left: Expr,
    pub right: Expr,
}

#[derive(Clone, Debug)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub value: Expr,
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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
        self.block()
    }

    /// Parses a block of expressions separated by newlines.
    ///
    /// For better documentation on how the function works internally, refer to
    /// `block_with_braces`, as the two are mostly similar (with the exceptions
    /// of the initial and final checking for braces).
    fn block(&mut self) -> Result<Block> {
        self.newlines()?;

        let mut exprs = Vec::new();
        let mut newline = true;

        while self.lexer_peek()?.is_some() {
            if !newline {
                self.lexer_advance_expect(Token::Newline)?;
            }
            exprs.push(self.expr()?);
            newline = self.newline()?;
            self.newlines()?;
        }

        Ok(Block { exprs })
    }

    /// Parses a block of expressions, separated by newlines and surrounded by
    /// braces.
    fn block_with_braces(&mut self) -> Result<Block> {
        self.lexer_advance_expect(Token::LeftBrace)?;

        // Get rid of any preceding newlines first.
        self.newlines()?;

        let mut exprs = Vec::new();
        // Whether a newline was found in the previous iteration.
        let mut newline = true;

        while let Some(token) = self.lexer_peek()? {
            // Stop if a `Token::RightBrace` has been reached.
            if token == &Token::RightBrace {
                break;
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

        assert_eq!(self.lexer_advance()?, Token::RightBrace);

        Ok(Block { exprs })
    }

    fn expr(&mut self) -> Result<Expr> {
        if self.lexer_peek()?.is_none() {
            // Using this call for the error message it provides. There is not
            // actually a next token which means this call WILL be an `Err`
            // (which is exactly what we want).
            self.lexer_advance()?;
            unreachable!("`lexer_advance` did not return `Err` when lexer is empty");
        }

        let expr = match self.lexer_peek()?.unwrap() {
            Token::Var => Expr::VarDecl(self.var_decl_expr()?.into()),
            Token::Func => Expr::FuncDecl(self.func_decl_expr()?.into()),

            Token::If => Expr::If(self.if_expr()?.into()),
            Token::While => Expr::While(self.while_expr()?.into()),
            Token::Try => Expr::TryCatch(self.try_catch_expr()?.into()),

            Token::Return => Expr::Return(self.return_expr()?.into()),
            Token::Break => Expr::Break(self.break_expr()?.into()),
            Token::Throw => Expr::Throw(self.throw_expr()?.into()),

            _ => self.expr_assign()?,
        };

        Ok(expr)
    }

    fn var_decl_expr(&mut self) -> Result<Expr> {
        assert_eq!(self.lexer_advance()?, Token::Var);
        let expr = self.expr()?;
        match &expr {
            // Only supported l-value in variable declarations is
            // `Expr::Ident`, since variable declaration is local.
            Expr::Assign(assign_expr) => match &assign_expr.target {
                Expr::Ident(_) => {}
                _ => bail!("invalid left expression in variable delcaration"),
            },
            Expr::Ident(_) => {}
            _ => bail!("invalid expression in variable declaration"),
        }
        Ok(expr)
    }

    fn func_decl_expr(&mut self) -> Result<FuncDeclExpr> {
        assert_eq!(self.lexer_advance()?, Token::Func);

        let name = if let Some(Token::Ident(_)) = self.lexer_peek()? {
            if let Token::Ident(ident) = self.lexer_advance()? {
                Some(ident)
            } else {
                unreachable!("`lexer_advance` did not return the same token as `lexer_peek`");
            }
        } else {
            None
        };

        let params = self.params()?;

        let block = self.block_with_braces()?;

        Ok(FuncDeclExpr {
            name,
            params,
            block,
        })
    }

    /// Parses parameters separated by commas.
    ///
    /// For better documentation on how the function works internally, refer to
    /// `block_with_braces`, as the two are mostly similar (with the exceptions
    /// of the initial, final checking for parens and the different
    /// separators and delimeters).
    fn params(&mut self) -> Result<Params> {
        self.lexer_advance_expect(Token::LeftParen)?;

        self.newlines()?;

        let mut idents = Vec::new();
        let mut comma = true;

        while let Some(token) = self.lexer_peek()? {
            if token == &Token::RightParen {
                break;
            }

            if !comma {
                self.lexer_advance_expect(Token::Comma)?;
            }

            match self.lexer_advance()? {
                Token::Ident(ident) => idents.push(ident),
                token => bail!("unexpected token: '{:?}', expected identifier", token),
            }

            comma = self.comma()?;
            self.newlines()?;
        }

        assert_eq!(self.lexer_advance()?, Token::RightParen);

        Ok(Params { idents })
    }

    fn if_expr(&mut self) -> Result<IfExpr> {
        assert_eq!(self.lexer_advance()?, Token::If);

        let condition = self.expr()?;

        let truthy_block = self.block_with_braces()?;
        self.newlines()?;

        let falsey_block = if let Some(Token::Else) = self.lexer_peek()? {
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
            truthy_block,
            falsey_block,
        };

        Ok(if_expr)
    }

    fn while_expr(&mut self) -> Result<WhileExpr> {
        assert_eq!(self.lexer_advance()?, Token::While);

        let condition = self.expr()?;

        let block = self.block_with_braces()?;

        let while_expr = WhileExpr { condition, block };

        Ok(while_expr)
    }

    fn try_catch_expr(&mut self) -> Result<TryCatchExpr> {
        assert_eq!(self.lexer_advance()?, Token::Try);

        let try_block = self.block_with_braces()?;
        self.newlines()?;

        self.lexer_advance_expect(Token::Catch)?;

        let error_ident = match self.lexer_advance()? {
            Token::Ident(ident) => ident,
            token => bail!("unexpected token: '{:?}', expected 'catch'", token),
        };

        let catch_block = self.block_with_braces()?;

        let try_catch_expr = TryCatchExpr {
            try_block,
            error_ident,
            catch_block,
        };
        Ok(try_catch_expr)
    }

    fn return_expr(&mut self) -> Result<Expr> {
        assert_eq!(self.lexer_advance()?, Token::Return);
        self.expr_or_null()
    }

    fn break_expr(&mut self) -> Result<Expr> {
        assert_eq!(self.lexer_advance()?, Token::Break);
        self.expr_or_null()
    }

    fn throw_expr(&mut self) -> Result<Expr> {
        assert_eq!(self.lexer_advance()?, Token::Throw);
        self.expr_or_null()
    }

    fn expr_or_null(&mut self) -> Result<Expr> {
        let expr = if let Ok(expr) = self.expr() {
            expr
        } else {
            Expr::Null
        };
        Ok(expr)
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
            value: self.expr()?,
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
            value: self.expr_unary()?,
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

    fn comma(&mut self) -> Result<bool> {
        if let Some(Token::Comma) = self.lexer_peek()? {
            assert_eq!(Token::Comma, self.lexer_advance()?);
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn newline(&mut self) -> Result<bool> {
        if let Some(Token::Newline) = self.lexer_peek()? {
            assert_eq!(Token::Newline, self.lexer_advance()?);
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
