use fnv::FnvHashMap;

use crate::parser::{AssignExpr, BinaryExpr, BinaryOp, Block, Expr, IfExpr, UnaryExpr, UnaryOp};
use crate::result::Result;
use crate::value::Value;

pub struct Scope {
    local_variables: FnvHashMap<String, Value>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            local_variables: FnvHashMap::default(),
        }
    }
}

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {}
    }

    pub fn evaluate(&mut self, scope: &mut Scope, block: Block) -> Result<Value> {
        self.block(scope, block)
    }

    fn block(&mut self, scope: &mut Scope, mut block: Block) -> Result<Value> {
        let last_expr = match block.exprs.pop() {
            Some(expr) => expr,
            None => return Ok(Value::Null),
        };
        for expr in block.exprs {
            self.expr(scope, expr)?;
        }
        self.expr(scope, last_expr)
    }

    fn expr(&mut self, scope: &mut Scope, expr: Expr) -> Result<Value> {
        use self::Expr::*;

        let return_value = match expr {
            Null => Value::Null,
            Integer(integer) => integer.into(),
            Float(float) => float.into(),
            Boolean(boolean) => boolean.into(),
            Ident(ident) => self.ident(scope, ident)?,

            VarDecl(expr) => self.var_decl_expr(scope, *expr)?,

            If(if_expr) => self.if_expr(scope, *if_expr)?,

            Assign(assign_expr) => self.assign_expr(scope, *assign_expr)?,
            Binary(binary_expr) => self.binary_expr(scope, *binary_expr)?,
            Unary(unary_expr) => self.unary_expr(scope, *unary_expr)?,
        };
        Ok(return_value)
    }

    fn ident(&mut self, scope: &mut Scope, ident: String) -> Result<Value> {
        if let Some(value) = scope.local_variables.get(&ident) {
            Ok(value.clone())
        } else {
            unimplemented!("error handling not yet implemented");
        }
    }

    fn var_decl_expr(&mut self, scope: &mut Scope, expr: Expr) -> Result<Value> {
        match expr {
            Expr::Assign(assign_expr) => {
                // Only supported l-value in variable declarations is
                // `Expr::Ident`, since variable declaration is local.
                if let Expr::Ident(ident) = &assign_expr.target {
                    // Declare the variable with `Value::Null` first, before
                    // performing the assignment.
                    self.var_decl_expr(scope, Expr::Ident(ident.clone()))?;
                    self.assign_expr(scope, *assign_expr)
                } else {
                    unimplemented!("error handling not yet implemented");
                }
            }
            Expr::Ident(ident) => {
                if scope.local_variables.get(&ident).is_none() {
                    scope.local_variables.insert(ident, Value::Null);
                    Ok(Value::Null)
                } else {
                    unimplemented!("error handling not yet implemented");
                }
            }
            _ => unreachable!("did not check expression in `Parser::var_decl_expr`"),
        }
    }

    fn if_expr(&mut self, scope: &mut Scope, if_expr: IfExpr) -> Result<Value> {
        if self.expr(scope, if_expr.condition)?.is_truthy() {
            self.block(scope, if_expr.truthy)
        } else {
            self.block(scope, if_expr.falsey)
        }
    }

    fn assign_expr(&mut self, scope: &mut Scope, assign_expr: AssignExpr) -> Result<Value> {
        let value = self.expr(scope, assign_expr.expr)?;
        match assign_expr.target {
            Expr::Ident(ident) => {
                if scope.local_variables.get(&ident).is_some() {
                    scope.local_variables.insert(ident, value.clone());
                    Ok(value)
                } else {
                    unimplemented!("error handling not yet implemented");
                }
            }
            _ => unreachable!("did not check l-value in `Parser::assign_expr"),
        }
    }

    fn binary_expr(&mut self, scope: &mut Scope, binary_expr: BinaryExpr) -> Result<Value> {
        use self::BinaryOp::*;
        use self::Value::*;

        let left_operand = self.expr(scope, binary_expr.left)?;
        let right_operand = self.expr(scope, binary_expr.right)?;
        let return_value = match (binary_expr.op, left_operand, right_operand) {
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

            _ => unimplemented!("error handling not yet implemented"),
        };
        Ok(return_value)
    }

    fn unary_expr(&mut self, scope: &mut Scope, unary_expr: UnaryExpr) -> Result<Value> {
        use self::UnaryOp::*;
        use self::Value::*;

        let operand = self.expr(scope, unary_expr.expr)?;
        let return_value = match (unary_expr.op, operand) {
            (Negate, Integer(integer)) => Integer(-integer),
            (Negate, Float(float)) => Float(-float),
            (Not, operand) => Boolean(operand.is_falsey()),
            _ => unimplemented!("error handling not yet implemented"),
        };
        Ok(return_value)
    }
}
