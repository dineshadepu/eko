use std::io::{Cursor, ErrorKind, Read};

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq)]
struct Error;

struct Engine;

impl Engine {
    fn evaluate_expression(source: &str) -> Result<Value> {
        let mut state = State::new();

        let mut lexer = Lexer::new(Cursor::new(source));
        let mut parser = Parser::new(&mut lexer);

        let expression = parser.expression()?;
        let mut chunk = Chunk::new();
        Generator::new(&mut state).expression(&mut chunk, expression);
        let chunk = state.chunks.push_unchecked(chunk);

        let mut fiber = Fiber::new(chunk);
        fiber.finish(&state)?;
        fiber.operands_pop()
    }
}

struct Lexer<R: Read> {
    source: R,
    peek: Option<u8>,
}

impl<R: Read> Lexer<R> {
    fn new(source: R) -> Lexer<R> {
        Lexer { source, peek: None }
    }

    fn next(&mut self) -> Result<Option<Token>> {
        self.whitespace()?;

        let byte = match self.source_peek()? {
            Some(byte) => byte,
            None => return Ok(None),
        };

        if is_digit(byte) {
            return Ok(Some(self.number()?));
        } else if is_alpha(byte) || byte == b'_' {
            if let Token::Identifier(identifier) = self.identifier()? {
                let token = match identifier.as_str() {
                    "not" => Token::Not,
                    "null" => Token::Null,
                    _ => Token::Identifier(identifier),
                };
                return Ok(Some(token));
            } else {
                unreachable!("`Lexer::identifier` did not return `Token::Identifier`");
            }
        }

        let token = match byte {
            b'+' => Token::Add,
            b'-' => Token::Subtract,
            b'*' => Token::Multiply,
            b'/' => Token::Divide,
            b'\n' => Token::Newline,
            _ => return Err(Error),
        };
        self.source_advance()?;
        Ok(Some(token))
    }

    fn number(&mut self) -> Result<Token> {
        let mut buf = Vec::new();
        let mut float = false;
        while let Some(byte) = self.source_peek()? {
            match byte {
                b'.' if float => return Err(Error),
                b'.' => float = true,
                byte if !is_digit(byte) => break,
                _ => {}
            }
            self.source_advance()?;
            buf.push(byte);
        }
        let buf = String::from_utf8(buf).map_err(|_| Error)?;
        if float {
            Ok(Token::Float(buf.parse().map_err(|_| Error)?))
        } else {
            Ok(Token::Integer(buf.parse().map_err(|_| Error)?))
        }
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
        let buf = String::from_utf8(buf).map_err(|_| Error)?;
        Ok(Token::Identifier(buf))
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
        Ok(self.peek.clone())
    }

    fn source_advance(&mut self) -> Result<u8> {
        self.source_peek()?;
        self.peek.take().ok_or_else(|| Error)
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
                Err(Error)
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

    Not,

    Null,
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Identifier(String),

    Newline,
}

struct Parser<'a, R: Read> {
    lexer: &'a mut Lexer<R>,
    peek: Option<Token>,
}

impl<'a, R: Read> Parser<'a, R> {
    fn new(lexer: &'a mut Lexer<R>) -> Parser<'a, R> {
        Parser { lexer, peek: None }
    }

    fn block(&mut self) -> Result<Block> {
        Ok(Block::new(vec![self.statement()?]))
    }

    fn statement(&mut self) -> Result<Statement> {
        Ok(Statement::Expression(self.expression()?))
    }

    fn expression(&mut self) -> Result<Expression> {
        self.add()
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
            _ => return Err(Error),
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
        self.peek.take().ok_or_else(|| Error)
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
    Expression(Expression),
}

enum Expression {
    Integer(i64),
    Float(f64),
    Binary(Binary, Box<Expression>, Box<Expression>),
    Unary(Unary, Box<Expression>),
}

enum Binary {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl Binary {
    fn from_token(token: &Token) -> Option<Binary> {
        let binary = match token {
            Token::Add => Binary::Add,
            Token::Subtract => Binary::Subtract,
            Token::Multiply => Binary::Multiply,
            Token::Divide => Binary::Divide,
            _ => return None,
        };
        Some(binary)
    }
}

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
    constants: Pool<Constant>,
    chunks: Pool<Chunk>,
}

impl State {
    fn new() -> State {
        State {
            constants: Pool::new(),
            chunks: Pool::new(),
        }
    }
}

struct Pool<T>(Vec<T>);

impl<T> Pool<T> {
    fn new() -> Pool<T> {
        Pool(Vec::new())
    }

    fn push_unchecked(&mut self, item: T) -> usize {
        let index = self.0.len();
        self.0.push(item);
        index
    }

    fn get(&self, index: usize) -> Option<&T> {
        self.0.get(index)
    }
}

impl<T: PartialEq> Pool<T> {
    fn push(&mut self, item: T) -> usize {
        if let Some(index) = self.get_index(&item) {
            return index;
        }
        self.push_unchecked(item)
    }

    fn get_index(&self, item: &T) -> Option<usize> {
        self.0.iter().position(|ref i| i == &item)
    }
}

#[derive(Debug)]
struct Chunk {
    instructions: Vec<Instruction>,
}

impl Chunk {
    fn new() -> Chunk {
        Chunk {
            instructions: Vec::new(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum Instruction {
    PushConstant(usize),
    PushTrue,
    PushFalse,
    PushNull,
    Pop,

    Add,
    Subtract,
    Multiply,
    Divide,

    Negate,
    Not,
}

impl Instruction {
    fn is_binary(&self) -> bool {
        use self::Instruction::*;

        match self {
            Add | Subtract | Multiply | Divide => true,
            PushConstant(_) | PushTrue | PushFalse | PushNull | Pop | Negate | Not => false,
        }
    }

    fn is_unary(&self) -> bool {
        use self::Instruction::*;

        match self {
            Negate | Not => true,
            PushConstant(_) | PushTrue | PushFalse | PushNull | Pop | Add | Subtract | Multiply
            | Divide => false,
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

#[derive(PartialEq)]
enum Constant {
    Integer(i64),
    Float(f64),
}

struct Generator<'a> {
    state: &'a mut State,
}

impl<'a> Generator<'a> {
    fn new(state: &'a mut State) -> Generator<'a> {
        Generator { state }
    }

    fn block(&mut self, block: Block) -> usize {
        let mut chunk = Chunk::new();
        for statement in block.statements {
            self.statement(&mut chunk, statement);
        }
        self.state.chunks.push_unchecked(chunk)
    }

    fn statement(&mut self, chunk: &mut Chunk, statement: Statement) {
        match statement {
            Statement::Expression(expression) => {
                self.expression(chunk, expression);
                chunk.instructions.push(Instruction::Pop);
            }
        }
    }

    fn expression(&mut self, chunk: &mut Chunk, expression: Expression) {
        match expression {
            Expression::Integer(integer) => {
                let constant = self
                    .state
                    .constants
                    .push_unchecked(Constant::Integer(integer));
                chunk.instructions.push(Instruction::PushConstant(constant));
            }
            Expression::Float(float) => {
                let constant = self.state.constants.push_unchecked(Constant::Float(float));
                chunk.instructions.push(Instruction::PushConstant(constant));
            }
            Expression::Binary(binary, left, right) => {
                self.expression(chunk, *left);
                self.expression(chunk, *right);
                chunk.instructions.push(binary.into());
            }
            Expression::Unary(unary, expression) => {
                self.expression(chunk, *expression);
                chunk.instructions.push(unary.into());
            }
        }
    }
}

struct Fiber {
    operands: Vec<Value>,
    frames: Vec<Frame>,
}

impl Fiber {
    fn new(chunk: usize) -> Fiber {
        Fiber {
            operands: Vec::new(),
            frames: vec![Frame::new(chunk)],
        }
    }

    fn finish(&mut self, state: &State) -> Result<()> {
        while let Some(_) = self.step(state)? {}
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
            .ok_or_else(|| Error)?
            .instructions
            .get(frame.step());
        let instruction = match instruction {
            Some(instruction) => instruction,
            None => return Ok(None),
        };

        match instruction {
            PushConstant(constant) => self.push_constant(state, *constant)?,
            PushTrue => self.operands.push(Value::Boolean(true)),
            PushFalse => self.operands.push(Value::Boolean(false)),
            PushNull => self.operands.push(Value::Null),
            Pop => self.pop()?,

            instruction if instruction.is_binary() => self.binary(*instruction)?,
            instruction if instruction.is_unary() => self.unary(*instruction)?,

            _ => unimplemented!("instruction not yet implemented"),
        }

        Ok(Some(()))
    }

    fn push_constant(&mut self, state: &State, constant: usize) -> Result<()> {
        let constant = state.constants.get(constant).ok_or_else(|| Error)?;
        self.operands.push(constant.into());
        Ok(())
    }

    fn pop(&mut self) -> Result<()> {
        self.operands_pop()?;
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
        self.operands.pop().ok_or_else(|| Error)
    }

    fn cur_frame_mut(&mut self) -> &mut Frame {
        self.frames
            .last_mut()
            .expect("`frames` should not be empty")
    }
}

#[derive(Debug, PartialEq)]
enum Value {
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

impl From<&Constant> for Value {
    fn from(constant: &Constant) -> Value {
        match constant {
            Constant::Integer(integer) => Value::Integer(*integer),
            Constant::Float(float) => Value::Float(*float),
        }
    }
}

struct Frame {
    local_context: Context,
    cur_instruction: usize,
    chunk: usize,
}

impl Frame {
    fn new(chunk: usize) -> Frame {
        Frame {
            local_context: Context::new(),
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

struct Context {
    variables: Vec<Value>,
}

impl Context {
    fn new() -> Context {
        Context {
            variables: Vec::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Engine, Value};

    #[test]
    fn simple_add() {
        assert_eq!(Engine::evaluate_expression("1 + 1"), Ok(Value::Integer(2)));
    }

    #[test]
    fn simple_subtract() {
        assert_eq!(Engine::evaluate_expression("1 - 1"), Ok(Value::Integer(0)));
    }
}
