use std::io::{Cursor, ErrorKind, Read};

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq)]
struct Error;

struct Engine<'a> {
    state: EngineState<'a>,
}

impl<'a> Engine<'a> {
    fn new() -> Engine<'a> {
        Engine {
            state: State::new().into(),
        }
    }

    fn with_state(state: &'a mut State) -> Engine<'a> {
        Engine {
            state: state.into(),
        }
    }

    fn evaluate_expression(&mut self, source: &str) -> Result<Value> {
        let mut compiler = Compiler::new(self.state.as_mut());
        let entry = compiler.compile_str(source)?;
        let mut fiber = Fiber::new(entry);
        fiber.finish(self.state.as_mut())?;
        fiber.operands_pop()
    }
}

enum EngineState<'a> {
    Owned(State),
    Borrowed(&'a mut State),
}

impl<'a> EngineState<'a> {
    fn as_mut(&mut self) -> &mut State {
        use self::EngineState::*;

        match self {
            Owned(ref mut state) => state,
            Borrowed(state) => state,
        }
    }
}

impl<'a> From<&'a mut State> for EngineState<'a> {
    fn from(state: &'a mut State) -> EngineState<'a> {
        EngineState::Borrowed(state)
    }
}

impl<'a> From<State> for EngineState<'a> {
    fn from(state: State) -> EngineState<'a> {
        EngineState::Owned(state)
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
        let expression = parser.expression()?;
        let mut chunk = Chunk::new();
        Generator::new(&mut self.state).expression(&mut chunk, expression);
        Ok(self.state.chunks.push_unchecked(chunk))
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
            byte if is_alpha(byte) || byte == b'_' => {
                if let Token::Identifier(identifier) = self.identifier()? {
                    let token = match self.state.symbols.get(identifier).unwrap().as_str() {
                        "not" => Token::Not,
                        "null" => Token::Null,
                        "true" => Token::Boolean(true),
                        "false" => Token::Boolean(false),
                        "and" => Token::And,
                        "or" => Token::Or,
                        _ => Token::Identifier(identifier),
                    };
                    token
                } else {
                    unreachable!("`Lexer::identifier` did not return `Token::Identifier`");
                }
            }
            _ => return Err(Error),
        };

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
            _ => return Err(Error),
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
        let buf = String::from_utf8(buf).map_err(|_| Error)?;
        Ok(Token::Identifier(self.state.symbols.push(buf)))
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
        Ok(Block::new(vec![self.statement()?]))
    }

    fn statement(&mut self) -> Result<Statement> {
        Ok(Statement::Expression(self.expression()?))
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

#[derive(Debug)]
enum Expression {
    Integer(i64),
    Float(f64),
    Boolean(bool),
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
    PushBoolean(bool),
    PushNull,
    Pop,

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
            Expression::Boolean(boolean) => {
                chunk.instructions.push(Instruction::PushBoolean(boolean));
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
    fn new(entry: usize) -> Fiber {
        Fiber {
            operands: Vec::new(),
            frames: vec![Frame::new(entry)],
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
            PushBoolean(boolean) => self.operands.push(Value::from(*boolean)),
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
    fn add() {
        let mut engine = Engine::new();
        assert_eq!(engine.evaluate_expression("1 + 1"), Ok(2.into()));
    }

    #[test]
    fn subtract() {
        let mut engine = Engine::new();
        assert_eq!(engine.evaluate_expression("1 - 1"), Ok(0.into()));
    }

    #[test]
    fn less() {
        let mut engine = Engine::new();
        assert_eq!(engine.evaluate_expression("2 < 1"), Ok(false.into()));
    }

    #[test]
    fn greater() {
        let mut engine = Engine::new();
        assert_eq!(engine.evaluate_expression("2 > 1"), Ok(true.into()));
    }

    #[test]
    fn and() {
        let mut engine = Engine::new();
        assert_eq!(
            engine.evaluate_expression("true and false"),
            Ok(false.into())
        );
    }

    #[test]
    fn or() {
        let mut engine = Engine::new();
        assert_eq!(engine.evaluate_expression("true or true"), Ok(true.into()));
    }

    #[test]
    fn complex() {
        let mut engine = Engine::new();
        assert_eq!(engine.evaluate_expression("1 + 4 * 5"), Ok(21.into()));
    }
}
