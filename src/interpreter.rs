use std::cell::RefCell;
use std::rc::Rc;

use failure::bail;
use indexmap::IndexMap;

use crate::parser::{
    AssignExpr, BinaryExpr, BinaryOp, Block, Expr, FuncDeclExpr, IfExpr, TryCatchExpr, UnaryExpr,
    UnaryOp, WhileExpr,
};
use crate::result::Result;
use crate::value::{InternalFuncReference, Value};

/// Represents the return value of a block.
#[derive(Debug)]
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
#[derive(Debug, PartialEq)]
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

pub struct Context(Option<ContextData>);

impl Context {
    pub fn new() -> Context {
        Scope::new().into()
    }

    fn escape(&mut self) -> Rc<RefCell<Scope>> {
        use self::ContextData::*;

        match self
            .0
            .take()
            .expect("`ContextData` should never be `None`, except in `escape`")
        {
            Local(scope) => {
                let escaped_scope = Rc::new(RefCell::new(scope));
                self.0 = Some(Escaped(escaped_scope.clone()));
                escaped_scope
            }
            Escaped(scope) => {
                self.0 = Some(Escaped(scope.clone()));
                scope
            }
        }
    }

    fn declare_variable(&mut self, ident: String, value: Value) -> bool {
        use self::ContextData::*;

        match self.data_mut() {
            Local(scope) => scope.declare_variable(ident, value),
            Escaped(scope) => scope.borrow_mut().declare_variable(ident, value),
        }
    }

    fn set_variable(&mut self, ident: String, value: Value) -> bool {
        use self::ContextData::*;

        match self.data_mut() {
            Local(scope) => scope.set_variable(ident, value),
            Escaped(scope) => scope.borrow_mut().set_variable(ident, value),
        }
    }

    fn variable(&self, ident: &String) -> Option<Value> {
        use self::ContextData::*;

        match self.data() {
            Local(scope) => scope.variable(ident),
            Escaped(scope) => scope.borrow_mut().variable(ident),
        }
    }

    fn data(&self) -> &ContextData {
        self.0
            .as_ref()
            .expect("`ContextData` should never be `None`, except in `escape`")
    }

    fn data_mut(&mut self) -> &mut ContextData {
        self.0
            .as_mut()
            .expect("`ContextData` should never be `None`, except in `escape`")
    }
}

impl From<Scope> for Context {
    fn from(scope: Scope) -> Context {
        Context(Some(ContextData::Local(scope)))
    }
}

/// Contains the actual context data.
///
/// Used to get around the inability to modify `Context` in place, if it were
/// an enum.
enum ContextData {
    Local(Scope),
    Escaped(Rc<RefCell<Scope>>),
}

#[derive(Debug)]
pub struct Scope {
    parent: Option<Rc<RefCell<Scope>>>,
    variables: IndexMap<String, Value>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            parent: None,
            variables: IndexMap::new(),
        }
    }

    fn with_parent(parent: Rc<RefCell<Scope>>) -> Scope {
        Scope {
            parent: Some(parent),
            variables: IndexMap::new(),
        }
    }

    /// Declares a variable if it doesn't exist and returns whether the variable
    /// was successfully declared.
    fn declare_variable(&mut self, ident: String, value: Value) -> bool {
        if self.variables.get(&ident).is_some() {
            return false;
        }
        self.variables.insert(ident, value);
        true
    }

    /// Sets the value of a variable if it exists and returns whether the
    /// variable was successfully set.
    ///
    /// This fails is the variable is not declared.
    ///
    /// FIXME: Implement this iteratively.
    fn set_variable(&mut self, ident: String, value: Value) -> bool {
        if self.local_variable(&ident).is_some() {
            self.variables.insert(ident, value);
            true
        } else if let Some(parent) = &self.parent {
            parent.borrow_mut().set_variable(ident, value)
        } else {
            false
        }
    }

    /// Finds and returns the value of a local variable.
    fn local_variable(&self, ident: &String) -> Option<Value> {
        self.variables.get(ident).cloned()
    }

    /// Finds and returns the value of a variable.
    ///
    /// FIXME: Implement this iteratively.
    fn variable(&self, ident: &String) -> Option<Value> {
        if let Some(value) = self.variables.get(ident) {
            Some(value.clone())
        } else {
            self.parent
                .as_ref()
                .and_then(|parent| parent.borrow().variable(ident))
        }
    }
}

/// Returns early if the kind of return value is not `ImplicitReturn`.
///
/// Can be thought of as similar to the `?` operator where in this case
/// `ImplicitReturn` is `Ok` and the rest are all `Err`.
macro_rules! try_return {
    ($expr:expr) => {{
        let return_value: crate::interpreter::ReturnValue = $expr;
        match &return_value.kind {
            crate::interpreter::ReturnValueKind::ImplicitReturn => return_value.value,
            crate::interpreter::ReturnValueKind::ExplicitReturn => return Ok(return_value),
            crate::interpreter::ReturnValueKind::Break => return Ok(return_value),
            crate::interpreter::ReturnValueKind::Throw => return Ok(return_value),
        }
    }};
}

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {}
    }

    pub fn evaluate(&mut self, ctx: &mut Context, block: Block) -> Result<Value> {
        use self::ReturnValueKind::*;

        let return_value = self.block(ctx, block)?;
        match return_value.kind {
            Throw => unimplemented!("convert `Value` to `failure::Error`"),
            _ => Ok(return_value.value),
        }
    }

    fn block(&mut self, ctx: &mut Context, mut block: Block) -> Result<ReturnValue> {
        let last_expr = match block.exprs.pop() {
            Some(expr) => expr,
            None => return Ok(Value::Null.into()),
        };
        for expr in block.exprs {
            try_return!(self.expr(ctx, expr)?);
        }
        self.expr(ctx, last_expr)
    }

    fn expr(&mut self, ctx: &mut Context, expr: Expr) -> Result<ReturnValue> {
        use self::Expr::*;

        let return_value = match expr {
            Null => Value::Null.into(),
            Integer(integer) => Value::Integer(integer).into(),
            Float(float) => Value::Float(float).into(),
            Boolean(boolean) => Value::Boolean(boolean).into(),
            Ident(ident) => self.ident(ctx, ident)?,

            VarDecl(expr) => self.var_decl_expr(ctx, *expr)?,
            FuncDecl(func_decl_expr) => self.func_decl_expr(ctx, *func_decl_expr)?,

            If(if_expr) => self.if_expr(ctx, *if_expr)?,
            While(while_expr) => self.while_expr(ctx, *while_expr)?,
            TryCatch(try_catch_expr) => self.try_catch_expr(ctx, *try_catch_expr)?,

            Return(return_expr) => self.return_expr(ctx, *return_expr)?,
            Break(break_expr) => self.break_expr(ctx, *break_expr)?,
            Throw(throw_expr) => self.throw_expr(ctx, *throw_expr)?,

            Assign(assign_expr) => self.assign_expr(ctx, *assign_expr)?,
            Binary(binary_expr) => self.binary_expr(ctx, *binary_expr)?,
            Unary(unary_expr) => self.unary_expr(ctx, *unary_expr)?,
        };
        Ok(return_value)
    }

    fn ident(&mut self, ctx: &mut Context, ident: String) -> Result<ReturnValue> {
        if let Some(value) = ctx.variable(&ident) {
            Ok(value.clone().into())
        } else {
            bail!("failed to find variable: {}", ident);
        }
    }

    fn var_decl_expr(&mut self, ctx: &mut Context, expr: Expr) -> Result<ReturnValue> {
        match expr {
            Expr::Assign(assign_expr) => {
                if let Expr::Ident(ident) = &assign_expr.target {
                    // Declare the variable with `Value::Null` first, before
                    // performing the assignment.
                    self.var_decl_expr(ctx, Expr::Ident(ident.clone()))?;
                    self.assign_expr(ctx, *assign_expr)
                } else {
                    unreachable!("did not check target in `Parser::var_decl_expr`");
                }
            }
            Expr::Ident(ident) => {
                // FIXME: `ident` shouldn't be cloned here (hot path).
                if !ctx.declare_variable(ident.clone(), Value::Null) {
                    bail!("variable is already declared: {}", ident);
                } else {
                    Ok(Value::Null.into())
                }
            }
            _ => unreachable!("did not check expression in `Parser::var_decl_expr`"),
        }
    }

    fn func_decl_expr(
        &mut self,
        ctx: &mut Context,
        func_decl_expr: FuncDeclExpr,
    ) -> Result<ReturnValue> {
        let value = Value::from(InternalFuncReference {
            scope: ctx.escape(),
            func: func_decl_expr.0.clone(),
        });

        if let Some(ident) = func_decl_expr.0.name.as_ref() {
            // FIXME: `ident` shouldn't be cloned here (hot path).
            if !ctx.declare_variable(ident.clone(), value.clone()) {
                bail!("variable is already declared: {}", ident);
            }
        }

        Ok(value.into())
    }

    fn if_expr(&mut self, ctx: &mut Context, if_expr: IfExpr) -> Result<ReturnValue> {
        let scope = Scope::with_parent(ctx.escape());
        if try_return!(self.expr(ctx, if_expr.condition)?).is_truthy() {
            self.block(&mut scope.into(), if_expr.truthy_block)
        } else {
            self.block(&mut scope.into(), if_expr.falsey_block)
        }
    }

    fn while_expr(&mut self, ctx: &mut Context, while_expr: WhileExpr) -> Result<ReturnValue> {
        use self::ReturnValueKind::*;

        loop {
            let return_value = self.expr(ctx, while_expr.condition.clone())?;
            let condition = match return_value.kind {
                ImplicitReturn => return_value.value,
                ExplicitReturn => return Ok(return_value),
                Throw => return Ok(return_value),
                Break => {
                    let return_value = ReturnValue {
                        kind: ImplicitReturn,
                        value: return_value.value,
                    };
                    return Ok(return_value);
                }
            };

            if condition.is_falsey() {
                break;
            }

            let scope = Scope::with_parent(ctx.escape());
            try_return!(self.block(&mut scope.into(), while_expr.block.clone())?);
        }
        Ok(Value::Null.into())
    }

    fn try_catch_expr(
        &mut self,
        ctx: &mut Context,
        try_catch_expr: TryCatchExpr,
    ) -> Result<ReturnValue> {
        use self::ReturnValueKind::*;

        let return_value = self.block(ctx, try_catch_expr.try_block)?;

        let value = match return_value.kind {
            ImplicitReturn => return_value.value,
            ExplicitReturn => return Ok(return_value),
            Break => return Ok(return_value),
            Throw => {
                let mut scope = Scope::with_parent(ctx.escape());
                scope.declare_variable(try_catch_expr.error_ident, return_value.value);
                try_return!(self.block(&mut scope.into(), try_catch_expr.catch_block)?)
            }
        };
        Ok(value.into())
    }

    fn return_expr(&mut self, ctx: &mut Context, return_expr: Expr) -> Result<ReturnValue> {
        Ok(ReturnValue {
            kind: ReturnValueKind::ExplicitReturn,
            value: try_return!(self.expr(ctx, return_expr)?),
        })
    }

    fn break_expr(&mut self, ctx: &mut Context, break_expr: Expr) -> Result<ReturnValue> {
        Ok(ReturnValue {
            kind: ReturnValueKind::Break,
            value: try_return!(self.expr(ctx, break_expr)?),
        })
    }

    fn throw_expr(&mut self, ctx: &mut Context, throw_expr: Expr) -> Result<ReturnValue> {
        Ok(ReturnValue {
            kind: ReturnValueKind::Throw,
            value: try_return!(self.expr(ctx, throw_expr)?),
        })
    }

    fn assign_expr(&mut self, ctx: &mut Context, assign_expr: AssignExpr) -> Result<ReturnValue> {
        let value = try_return!(self.expr(ctx, assign_expr.value)?);

        match assign_expr.target {
            Expr::Ident(ident) => {
                // FIXME: `ident` shouldn't be cloned here (hot path).
                if !ctx.set_variable(ident.clone(), value.clone()) {
                    bail!("variable is not declared: {}", ident);
                } else {
                    Ok(value.into())
                }
            }
            _ => unreachable!("did not check target in `Parser::expr_assign"),
        }
    }

    fn binary_expr(&mut self, ctx: &mut Context, binary_expr: BinaryExpr) -> Result<ReturnValue> {
        use self::BinaryOp::*;
        use self::Value::*;

        let left = try_return!(self.expr(ctx, binary_expr.left)?);
        let right = try_return!(self.expr(ctx, binary_expr.right)?);

        let result_value = match (binary_expr.op, left, right) {
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
        Ok(result_value.into())
    }

    fn unary_expr(&mut self, ctx: &mut Context, unary_expr: UnaryExpr) -> Result<ReturnValue> {
        use self::UnaryOp::*;
        use self::Value::*;

        let value = try_return!(self.expr(ctx, unary_expr.value)?);

        let result_value = match (unary_expr.op, value) {
            (Negate, Integer(integer)) => Integer(-integer),
            (Negate, Float(float)) => Float(-float),
            (Not, operand) => Boolean(operand.is_falsey()),
            (op, operand) => bail!("invalid operation '{:?}' on operand: {:?}", op, operand),
        };
        Ok(result_value.into())
    }
}
