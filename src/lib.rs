use std::io::{Cursor, ErrorKind, Read};
use std::mem::size_of;

use failure::{bail, format_err, Error};

use crate::pool::Pool;

mod pool;

type Result<T> = std::result::Result<T, Error>;

pub struct Engine {
    state: State,
}

impl Engine {
    pub fn new() -> Engine {
        Engine {
            state: State::new(),
        }
    }

    pub fn evaluate_expression(&mut self, source: &str) -> Result<Value> {
        let mut compiler = Compiler::new(&mut self.state);
        let entry = compiler.compile_str(source)?;
        let mut fiber = Fiber::new(&self.state, entry)?;
        fiber.finish(&self.state)?;
        fiber.operands_pop()
    }
}

struct Compiler<'a> {
    state: &'a mut State,
}

impl<'a> Compiler<'a> {
    fn new(state: &'a mut State) -> Compiler<'a> {
        Compiler { state }
    }

    fn compile_str(&mut self, source: &str) -> Result<usize> {
        self.compile(Cursor::new(source))
    }

    fn compile<R: Read>(&mut self, source: R) -> Result<usize> {
        let mut lexer = Lexer::new(&mut self.state, source);
        let mut parser = Parser::new(&mut lexer);
        let expression = parser.block()?;
        Generator::new(&mut self.state).block(expression)
    }
}

struct Lexer<'a, R: Read> {
    state: &'a mut State,
    source: R,
    peek: Option<u8>,
}

impl<'a, R: Read> Lexer<'a, R> {
    fn new(state: &'a mut State, source: R) -> Lexer<'a, R> {
        Lexer {
            state,
            source,
            peek: None,
        }
    }

    fn next(&mut self) -> Result<Option<Token>> {
        self.whitespace()?;

        let byte = match self.source_peek()? {
            Some(byte) => byte,
            None => return Ok(None),
        };

        let token = match byte {
            b'\n' => Token::Newline,
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
            _ => Token::Identifier(self.state.symbols.insert(buf)),
        };
        Ok(token)
    }

    fn whitespace(&mut self) -> Result<()> {
        while let Some(byte) = self.source_peek()? {
            match byte {
                b'\n' => {}
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
        b'+' | b'-' | b'*' | b'/' | b'<' | b'>' | b'=' => true,
        _ => false,
    }
}

fn is_whitespace(byte: u8) -> bool {
    match byte {
        b' ' | b'\t' | b'\r' | b'\n' => true,
        _ => false,
    }
}

#[derive(Debug)]
enum Token {
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

    And,
    Or,
    Not,

    Null,
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Identifier(usize),

    Var,

    Newline,
}

struct Parser<'a, R: Read> {
    lexer: &'a mut Lexer<'a, R>,
    peek: Option<Token>,
}

impl<'a, R: Read> Parser<'a, R> {
    fn new(lexer: &'a mut Lexer<'a, R>) -> Parser<'a, R> {
        Parser { lexer, peek: None }
    }

    fn block(&mut self) -> Result<Block> {
        let mut statements = Vec::new();
        while self.lexer_peek()?.is_some() {
            statements.push(self.statement()?);
        }
        Ok(Block::new(statements))
    }

    fn statement(&mut self) -> Result<Statement> {
        let token = match self.lexer_peek()? {
            Some(token) => token,
            None => return Err(self.lexer_advance().unwrap_err()),
        };
        let statement = match token {
            Token::Var => Statement::Declaration(self.declaration_var()?),
            _ => Statement::Expression(self.expression()?),
        };
        Ok(statement)
    }

    fn declaration_var(&mut self) -> Result<Declaration> {
        self.lexer_advance()?;
        let identifier = self.expression()?;
        match self.lexer_advance()? {
            Token::Assign => {}
            token => bail!("unexpected token: '{:?}'", token),
        }
        let expression = self.expression()?;
        Ok(Declaration::Var(identifier, expression))
    }

    fn expression(&mut self) -> Result<Expression> {
        self.logical()
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
            token => bail!("unexpected token: '{:?}'", token),
        };
        Ok(expression)
    }

    fn lexer_peek(&mut self) -> Result<Option<&Token>> {
        if self.peek.is_none() {
            self.peek = self.lexer.next()?;
        }
        Ok(self.peek.as_ref())
    }

    fn lexer_advance(&mut self) -> Result<Token> {
        self.lexer_peek()?;
        self.peek
            .take()
            .ok_or_else(|| format_err!("unexpected end of source"))
    }
}

struct Block {
    statements: Vec<Statement>,
}

impl Block {
    fn new(statements: Vec<Statement>) -> Block {
        Block { statements }
    }
}

enum Statement {
    Declaration(Declaration),
    Expression(Expression),
}

#[derive(Debug)]
enum Declaration {
    Var(Expression, Expression),
}

#[derive(Debug)]
enum Expression {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Identifier(usize),
    Binary(Binary, Box<Expression>, Box<Expression>),
    Unary(Unary, Box<Expression>),
}

#[derive(Debug)]
enum Binary {
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
    fn from_token(token: &Token) -> Option<Binary> {
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
enum Unary {
    Negate,
    Not,
}

impl Unary {
    fn from_token(token: &Token) -> Option<Unary> {
        let unary = match token {
            Token::Subtract => Unary::Negate,
            Token::Not => Unary::Not,
            _ => return None,
        };
        Some(unary)
    }
}

struct State {
    symbols: Pool<String>,
    constants: Pool<Constant>,
    chunks: Pool<Chunk>,
}

impl State {
    fn new() -> State {
        State {
            symbols: Pool::new(),
            constants: Pool::new(),
            chunks: Pool::new(),
        }
    }
}

#[derive(Debug)]
struct Chunk {
    parent: Option<usize>,
    identifiers: Pool<usize>,
    instructions: Vec<Instruction>,
}

impl Chunk {
    fn new() -> Chunk {
        Chunk {
            parent: None,
            identifiers: Pool::new(),
            instructions: Vec::new(),
        }
    }

    fn with_parent(parent: usize) -> Chunk {
        Chunk {
            parent: Some(parent),
            identifiers: Pool::new(),
            instructions: Vec::new(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum Instruction {
    PushConstant(usize),
    PushBoolean(bool),
    PushNull,
    Pop,

    PushLocal(usize),
    PushClosed(Closed),
    PopLocal(usize),
    PopClosed(Closed),

    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,

    Equal,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    And,
    Or,
    Not,
}

impl Instruction {
    fn is_binary(&self) -> bool {
        use self::Instruction::*;

        match self {
            Add | Subtract | Multiply | Divide | Equal | Less | LessEqual | Greater
            | GreaterEqual | And | Or => true,
            _ => false,
        }
    }

    fn is_unary(&self) -> bool {
        use self::Instruction::*;

        match self {
            Negate | Not => true,
            _ => false,
        }
    }
}

impl From<Binary> for Instruction {
    fn from(binary: Binary) -> Instruction {
        match binary {
            Binary::Add => Instruction::Add,
            Binary::Subtract => Instruction::Subtract,
            Binary::Multiply => Instruction::Multiply,
            Binary::Divide => Instruction::Divide,

            Binary::Equal => Instruction::Equal,
            Binary::Less => Instruction::Less,
            Binary::LessEqual => Instruction::LessEqual,
            Binary::Greater => Instruction::Greater,
            Binary::GreaterEqual => Instruction::GreaterEqual,

            Binary::And => Instruction::And,
            Binary::Or => Instruction::Or,
        }
    }
}

impl From<Unary> for Instruction {
    fn from(unary: Unary) -> Instruction {
        match unary {
            Unary::Negate => Instruction::Negate,
            Unary::Not => Instruction::Not,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Closed(usize);

impl Closed {
    const CLOSED_BITS: usize = size_of::<usize>() - 2;

    fn new(parents: usize, closed: usize) -> Closed {
        Closed((parents << Self::CLOSED_BITS) + closed)
    }

    fn parents(&self) -> usize {
        self.0 >> Self::CLOSED_BITS
    }

    fn closed(&self) -> usize {
        self.0 % (1 << Self::CLOSED_BITS)
    }
}

#[derive(PartialEq)]
enum Constant {
    Integer(i64),
    Float(f64),
}

impl From<i64> for Constant {
    fn from(i: i64) -> Constant {
        Constant::Integer(i)
    }
}

struct Generator<'a> {
    state: &'a mut State,
}

impl<'a> Generator<'a> {
    fn new(state: &'a mut State) -> Generator<'a> {
        Generator { state }
    }

    fn block(&mut self, block: Block) -> Result<usize> {
        let mut chunk = Chunk::new();
        for statement in block.statements {
            self.statement(&mut chunk, statement)?;
        }

        // The last expression should be returned, instead of being popped.
        if let Some(Instruction::Pop) = chunk.instructions.last() {
            chunk.instructions.pop();
        }

        Ok(self.state.chunks.push(chunk))
    }

    fn statement(&mut self, chunk: &mut Chunk, statement: Statement) -> Result<()> {
        match statement {
            Statement::Declaration(declaration) => {
                self.declaration(chunk, declaration)?;
            }
            Statement::Expression(expression) => {
                self.expression(chunk, expression)?;
                chunk.instructions.push(Instruction::Pop);
            }
        }
        Ok(())
    }

    fn declaration(&mut self, chunk: &mut Chunk, declaration: Declaration) -> Result<()> {
        match declaration {
            Declaration::Var(identifier, expression) => {
                let identifier = match identifier {
                    Expression::Identifier(identifier) => identifier,
                    _ => bail!("invalid left expression in assignment"),
                };
                if chunk.identifiers.get_by_value(&identifier).is_some() {
                    let symbol =
                        self.state.symbols.get(identifier).ok_or_else(|| {
                            format_err!("failed to find symbol: '{}'", identifier)
                        })?;
                    bail!("variable already declared: {}", symbol);
                }
                self.expression(chunk, expression)?;
                chunk
                    .instructions
                    .push(Instruction::PopLocal(chunk.identifiers.insert(identifier)))
            }
        }
        Ok(())
    }

    fn expression(&mut self, chunk: &mut Chunk, expression: Expression) -> Result<()> {
        match expression {
            Expression::Integer(integer) => {
                let constant = self.state.constants.insert(Constant::Integer(integer));
                chunk.instructions.push(Instruction::PushConstant(constant));
            }
            Expression::Float(float) => {
                let constant = self.state.constants.insert(Constant::Float(float));
                chunk.instructions.push(Instruction::PushConstant(constant));
            }
            Expression::Boolean(boolean) => {
                chunk.instructions.push(Instruction::PushBoolean(boolean));
            }
            Expression::Binary(binary, left, right) => {
                self.expression(chunk, *left)?;
                self.expression(chunk, *right)?;
                chunk.instructions.push(binary.into());
            }
            Expression::Unary(unary, expression) => {
                self.expression(chunk, *expression)?;
                chunk.instructions.push(unary.into());
            }
            Expression::Identifier(identifier) => {
                if let Some(local) = chunk.identifiers.get_by_value(&identifier) {
                    chunk.instructions.push(Instruction::PushLocal(local));
                    return Ok(());
                }

                let mut parents = 0;
                let mut cur_parent = chunk.parent;
                while let Some(parent) = cur_parent {
                    let chunk = self
                        .state
                        .chunks
                        .get_mut(parent)
                        .ok_or_else(|| format_err!("failed to find chunk: {}", parent))?;
                    if let Some(closed) = chunk.identifiers.get_by_value(&identifier) {
                        chunk
                            .instructions
                            .push(Instruction::PushClosed(Closed::new(parents, closed)));
                        return Ok(());
                    }
                    parents += 1;
                    cur_parent = chunk.parent;
                }

                let symbol = self
                    .state
                    .symbols
                    .get(identifier)
                    .ok_or_else(|| format_err!("failed to find symbol: '{}'", identifier))?;
                bail!("identifier not declared: {}", symbol)
            }
        }
        Ok(())
    }
}

struct Fiber {
    operands: Vec<Value>,
    frames: Vec<Frame>,
}

impl Fiber {
    fn new(state: &State, entry: usize) -> Result<Fiber> {
        let locals = state
            .chunks
            .get(entry)
            .ok_or_else(|| format_err!("failed to find chunk: {}", entry))?
            .identifiers
            .len();
        let fiber = Fiber {
            operands: Vec::new(),
            frames: vec![Frame::new(entry, locals)],
        };
        Ok(fiber)
    }

    fn finish(&mut self, state: &State) -> Result<()> {
        while self.step(state)?.is_some() {}
        Ok(())
    }

    fn step(&mut self, state: &State) -> Result<Option<()>> {
        use self::Instruction::*;

        if self.frames.is_empty() {
            return Ok(None);
        }

        let frame = self.cur_frame_mut();
        let instruction = state
            .chunks
            .get(frame.chunk)
            .ok_or_else(|| format_err!("failed to find chunk: {}", frame.chunk))?
            .instructions
            .get(frame.step());
        let instruction = match instruction {
            Some(instruction) => instruction,
            None => return Ok(None),
        };

        match instruction {
            PushConstant(constant) => self.push_constant(state, *constant)?,
            PushBoolean(boolean) => self.operands.push((*boolean).into()),
            PushNull => self.operands.push(Value::Null),
            Pop => self.pop()?,

            PushLocal(local) => self.push_local(state, *local)?,
            PopLocal(local) => self.pop_local(state, *local)?,

            instruction if instruction.is_binary() => self.binary(*instruction)?,
            instruction if instruction.is_unary() => self.unary(*instruction)?,

            _ => unimplemented!("instruction not yet implemented"),
        }

        Ok(Some(()))
    }

    fn push_constant(&mut self, state: &State, constant: usize) -> Result<()> {
        let constant = state
            .constants
            .get(constant)
            .ok_or_else(|| format_err!("failed to find constant: {}", constant))?;
        self.operands.push(constant.into());
        Ok(())
    }

    fn pop(&mut self) -> Result<()> {
        self.operands_pop()?;
        Ok(())
    }

    fn push_local(&mut self, state: &State, local: usize) -> Result<()> {
        let value = self
            .cur_frame()
            .locals
            .get(local)
            .ok_or_else(|| format_err!("failed to find local: {}", local))?;
        self.operands.push(value.clone());
        Ok(())
    }

    fn pop_local(&mut self, state: &State, local: usize) -> Result<()> {
        let value = self.operands_pop()?;
        *self
            .cur_frame_mut()
            .locals
            .get_mut(local)
            .ok_or_else(|| format_err!("failed to find local: {}", local))? = value;
        Ok(())
    }

    fn binary(&mut self, operation: Instruction) -> Result<()> {
        use self::Instruction::*;
        use self::Value::*;

        let right = self.operands_pop()?;
        let left = self.operands_pop()?;

        let result = match (operation, left, right) {
            (Add, Integer(left), Integer(right)) => Integer(left + right),
            (Add, Integer(left), Float(right)) => Float(left as f64 + right),
            (Add, Float(left), Integer(right)) => Float(left + right as f64),
            (Add, Float(left), Float(right)) => Float(left + right),

            (Subtract, Integer(left), Integer(right)) => Integer(left - right),
            (Subtract, Integer(left), Float(right)) => Float(left as f64 - right),
            (Subtract, Float(left), Integer(right)) => Float(left - right as f64),
            (Subtract, Float(left), Float(right)) => Float(left - right),

            (Multiply, Integer(left), Integer(right)) => Integer(left * right),
            (Multiply, Integer(left), Float(right)) => Float(left as f64 * right),
            (Multiply, Float(left), Integer(right)) => Float(left * right as f64),
            (Multiply, Float(left), Float(right)) => Float(left * right),

            (Divide, Integer(left), Integer(right)) => Integer(left / right),
            (Divide, Integer(left), Float(right)) => Float(left as f64 / right),
            (Divide, Float(left), Integer(right)) => Float(left / right as f64),
            (Divide, Float(left), Float(right)) => Float(left / right),

            (Equal, left, right) => Boolean(left == right),

            (Less, Integer(left), Integer(right)) => Boolean(left < right),
            (Less, Integer(left), Float(right)) => Boolean((left as f64) < right),
            (Less, Float(left), Integer(right)) => Boolean(left < right as f64),
            (Less, Float(left), Float(right)) => Boolean(left < right),

            (LessEqual, Integer(left), Integer(right)) => Boolean(left <= right),
            (LessEqual, Integer(left), Float(right)) => Boolean((left as f64) <= right),
            (LessEqual, Float(left), Integer(right)) => Boolean(left <= right as f64),
            (LessEqual, Float(left), Float(right)) => Boolean(left <= right),

            (Greater, Integer(left), Integer(right)) => Boolean(left > right),
            (Greater, Integer(left), Float(right)) => Boolean(left as f64 > right),
            (Greater, Float(left), Integer(right)) => Boolean(left > right as f64),
            (Greater, Float(left), Float(right)) => Boolean(left > right),

            (GreaterEqual, Integer(left), Integer(right)) => Boolean(left >= right),
            (GreaterEqual, Integer(left), Float(right)) => Boolean(left as f64 >= right),
            (GreaterEqual, Float(left), Integer(right)) => Boolean(left >= right as f64),
            (GreaterEqual, Float(left), Float(right)) => Boolean(left >= right),

            (And, left, right) => Boolean(left.is_truthy() && right.is_truthy()),

            (Or, left, right) => Boolean(left.is_truthy() || right.is_truthy()),

            _ => unimplemented!("binary not yet implemented"),
        };

        self.operands.push(result);
        Ok(())
    }

    fn unary(&mut self, operation: Instruction) -> Result<()> {
        use self::Instruction::*;
        use self::Value::*;

        let value = self.operands_pop()?;

        let result = match (operation, value) {
            (Negate, Integer(integer)) => Integer(-integer),
            (Negate, Float(float)) => Float(-float),

            (Not, value) => Boolean(!value.is_truthy()),

            _ => unimplemented!("unary not yet implemented"),
        };

        self.operands.push(result);
        Ok(())
    }

    fn operands_pop(&mut self) -> Result<Value> {
        self.operands
            .pop()
            .ok_or_else(|| format_err!("failed to pop empty operand stack"))
    }

    fn cur_frame(&self) -> &Frame {
        self.frames.last().expect("`frames` should not be empty")
    }

    fn cur_frame_mut(&mut self) -> &mut Frame {
        self.frames
            .last_mut()
            .expect("`frames` should not be empty")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Null,
    Integer(i64),
    Float(f64),
    Boolean(bool),
}

impl Value {
    fn is_truthy(&self) -> bool {
        use self::Value::*;

        match self {
            Null | Boolean(false) => false,
            Integer(_) | Float(_) | Boolean(true) => true,
        }
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Value {
        Value::Integer(i)
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Value {
        Value::Float(f)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Value {
        Value::Boolean(b)
    }
}

impl<T> From<Option<T>> for Value
where
    T: Into<Value>,
{
    fn from(option: Option<T>) -> Value {
        match option {
            Some(value) => value.into(),
            None => Value::Null,
        }
    }
}

impl From<&Constant> for Value {
    fn from(constant: &Constant) -> Value {
        match constant {
            Constant::Integer(integer) => Value::Integer(*integer),
            Constant::Float(float) => Value::Float(*float),
        }
    }
}

struct Frame {
    locals: Vec<Value>,
    cur_instruction: usize,
    chunk: usize,
}

impl Frame {
    fn new(chunk: usize, locals: usize) -> Frame {
        Frame {
            locals: vec![Value::Null; locals],
            cur_instruction: 0,
            chunk,
        }
    }

    fn step(&mut self) -> usize {
        let instruction = self.cur_instruction;
        self.cur_instruction += 1;
        instruction
    }
}
