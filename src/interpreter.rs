use failure::bail;
use indexmap::IndexMap;

use crate::parser::{
    AssignExpr, BinaryExpr, BinaryOp, Block, Expr, IfExpr, UnaryExpr, UnaryOp, WhileExpr,
};
use crate::result::Result;
use crate::value::Value;

/// Represents the return value of a block.
struct ReturnValue {
    kind: ReturnValueKind,
    value: Value,
}

impl From<Value> for ReturnValue {
    fn from(value: Value) -> ReturnValue {
        ReturnValue {
            kind: ReturnValueKind::ImplicitReturn,
            value,
        }
    }
}

/// Represents the various kinds of values can be returned from a block.
///
/// It is important to distinguish between an implicit and explicit return since
/// in `if` and `while` expressions explicit returns need to propogate to the
/// parent block.
#[derive(PartialEq)]
enum ReturnValueKind {
    /// The value of the last expression in the block.
    ImplicitReturn,
    /// A value that was explicitly returned using `return`.
    ExplicitReturn,
    /// The value passed to `break` when within a loop.
    Break,
    /// A error value thrown from within the block.
    Throw,
}

pub struct Scope {
    variables: IndexMap<String, Value>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            variables: IndexMap::new(),
        }
    }
}

/// Returns early only if the kind of expression is `Throw`.
macro_rules! try_throw {
    ($expr:expr) => {{
        let result: crate::interpreter::ReturnValue = $expr;
        match &result.kind {
            crate::interpreter::ReturnValueKind::ImplicitReturn => result.value,
            crate::interpreter::ReturnValueKind::ExplicitReturn => result.value,
            crate::interpreter::ReturnValueKind::Break => result.value,
            crate::interpreter::ReturnValueKind::Throw => return Ok(result),
        }
    }};
}

/// Returns early if the kind of expression is `Throw` or `ExplicitReturn`.
macro_rules! try_explicit_return {
    ($expr:expr) => {{
        let result: crate::interpreter::ReturnValue = $expr;
        match &result.kind {
            crate::interpreter::ReturnValueKind::ImplicitReturn => result.value,
            crate::interpreter::ReturnValueKind::ExplicitReturn => return Ok(result),
            crate::interpreter::ReturnValueKind::Break => result.value,
            crate::interpreter::ReturnValueKind::Throw => return Ok(result),
        }
    }};
}

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {}
    }

    pub fn evaluate(&mut self, scope: &mut Scope, block: Block) -> Result<Value> {
        use self::ReturnValueKind::*;

        let return_value = self.block(scope, block)?;
        match return_value.kind {
            Throw => unimplemented!("convert `Value` to `failure::Error`"),
            _ => Ok(return_value.value),
        }
    }

    fn block(&mut self, scope: &mut Scope, mut block: Block) -> Result<ReturnValue> {
        let last_expr = match block.exprs.pop() {
            Some(expr) => expr,
            None => return Ok(Value::Null.into()),
        };
        for expr in block.exprs {
            try_explicit_return!(self.expr(scope, expr)?);
        }
        self.expr(scope, last_expr)
    }

    fn expr(&mut self, scope: &mut Scope, expr: Expr) -> Result<ReturnValue> {
        use self::Expr::*;

        let return_value = match expr {
            Null => Value::Null.into(),
            Integer(integer) => Value::Integer(integer).into(),
            Float(float) => Value::Float(float).into(),
            Boolean(boolean) => Value::Boolean(boolean).into(),
            Ident(ident) => self.ident(scope, ident)?,

            VarDecl(expr) => self.var_decl_expr(scope, *expr)?,

            If(if_expr) => self.if_expr(scope, *if_expr)?,
            While(while_expr) => self.while_expr(scope, *while_expr)?,

            Assign(assign_expr) => self.assign_expr(scope, *assign_expr)?,
            Binary(binary_expr) => self.binary_expr(scope, *binary_expr)?,
            Unary(unary_expr) => self.unary_expr(scope, *unary_expr)?,
        };
        Ok(return_value)
    }

    fn ident(&mut self, scope: &mut Scope, ident: String) -> Result<ReturnValue> {
        if let Some(value) = scope.variables.get(&ident) {
            Ok(value.clone().into())
        } else {
            bail!("failed to find variable: {}", ident);
        }
    }

    fn var_decl_expr(&mut self, scope: &mut Scope, expr: Expr) -> Result<ReturnValue> {
        match expr {
            Expr::Assign(assign_expr) => {
                if let Expr::Ident(ident) = &assign_expr.target {
                    // Declare the variable with `Value::Null` first, before
                    // performing the assignment.
                    self.var_decl_expr(scope, Expr::Ident(ident.clone()))?;
                    self.assign_expr(scope, *assign_expr)
                } else {
                    unreachable!("did not check l-value in `Parser::var_decl_expr`");
                }
            }
            Expr::Ident(ident) => {
                if scope.variables.get(&ident).is_none() {
                    scope.variables.insert(ident, Value::Null);
                    Ok(Value::Null.into())
                } else {
                    bail!("variable is already declared: {}", ident);
                }
            }
            _ => unreachable!("did not check expression in `Parser::var_decl_expr`"),
        }
    }

    fn if_expr(&mut self, scope: &mut Scope, if_expr: IfExpr) -> Result<ReturnValue> {
        if try_throw!(self.expr(scope, if_expr.condition)?).is_truthy() {
            self.block(scope, if_expr.truthy)
        } else {
            self.block(scope, if_expr.falsey)
        }
    }

    fn while_expr(&mut self, scope: &mut Scope, while_expr: WhileExpr) -> Result<ReturnValue> {
        while try_throw!(self.expr(scope, while_expr.condition.clone())?).is_truthy() {
            self.block(scope, while_expr.block.clone())?;
        }
        Ok(Value::Null.into())
    }

    fn assign_expr(&mut self, scope: &mut Scope, assign_expr: AssignExpr) -> Result<ReturnValue> {
        let value = try_throw!(self.expr(scope, assign_expr.expr)?);
        match assign_expr.target {
            Expr::Ident(ident) => {
                if scope.variables.get(&ident).is_some() {
                    scope.variables.insert(ident, value.clone());
                    Ok(value.into())
                } else {
                    bail!("variable is not declared: {}", ident);
                }
            }
            _ => unreachable!("did not check l-value in `Parser::expr_assign"),
        }
    }

    fn binary_expr(&mut self, scope: &mut Scope, binary_expr: BinaryExpr) -> Result<ReturnValue> {
        use self::BinaryOp::*;
        use self::Value::*;

        let left = try_throw!(self.expr(scope, binary_expr.left)?);
        let right = try_throw!(self.expr(scope, binary_expr.right)?);
        let result = match (binary_expr.op, left, right) {
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

            (Greater, Integer(left), Integer(right)) => Boolean(left > right),
            (Greater, Integer(left), Float(right)) => Boolean(left as f64 > right),
            (Greater, Float(left), Integer(right)) => Boolean(left > right as f64),
            (Greater, Float(left), Float(right)) => Boolean(left > right),
            (Less, Integer(left), Integer(right)) => Boolean(left < right),
            (Less, Integer(left), Float(right)) => Boolean((left as f64) < right),
            (Less, Float(left), Integer(right)) => Boolean(left < right as f64),
            (Less, Float(left), Float(right)) => Boolean(left < right),

            (And, left, right) => Boolean(left.is_truthy() && right.is_truthy()),
            (Or, left, right) => Boolean(left.is_truthy() || right.is_truthy()),

            (op, left, right) => bail!(
                "invalid operation '{:?}' on operands: {:?}, {:?}",
                op,
                left,
                right
            ),
        };
        Ok(result.into())
    }

    fn unary_expr(&mut self, scope: &mut Scope, unary_expr: UnaryExpr) -> Result<ReturnValue> {
        use self::UnaryOp::*;
        use self::Value::*;

        let operand = try_throw!(self.expr(scope, unary_expr.expr)?);
        let result = match (unary_expr.op, operand) {
            (Negate, Integer(integer)) => Integer(-integer),
            (Negate, Float(float)) => Float(-float),
            (Not, operand) => Boolean(operand.is_falsey()),
            (op, operand) => bail!("invalid operation '{:?}' on operand: {:?}", op, operand),
        };
        Ok(result.into())
    }
}
