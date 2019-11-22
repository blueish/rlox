use crate::value::Value;
use crate::ast::stmt::Statement;
use crate::interp::interpreter::{ Interpreter, InterpErr};
use std::rc::Rc;

pub mod callable;
pub mod builtins;

#[derive(Debug)]
pub enum FunKind {
    Function,
}

pub struct LoxCallable {
    pub parameter_names: Vec<String>,
    pub body: Rc<Statement>,
}

impl callable::Callable for LoxCallable {
    fn arity(&self) -> usize {
        self.parameter_names.len()
    }

    fn call(&self, _interp: &mut Interpreter, _args: Vec<Value>) -> Result<Value, InterpErr> {
        Ok(Value::NilV)
    }
}
