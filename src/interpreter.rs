use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::Block;
use crate::result::Result;
use crate::value::Value;

pub struct Scope {
    parent: Option<Rc<RefCell<Scope>>>,
    local_variables: HashMap<String, Value>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            parent: None,
            local_variables: HashMap::new(),
        }
    }

    fn with_parent(parent: Scope) -> Scope {
        Scope {
            parent: Some(Rc::new(RefCell::new(parent))),
            local_variables: HashMap::new(),
        }
    }
}

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {}
    }

    pub fn evaluate(&mut self, scope: &mut Scope, block: Block) -> Result<Value> {
        unimplemented!();
    }
}
