use crate::value::Value;
use crate::ast::stmt::Statement;
use crate::interp::interpreter::{ Interpreter, InterpErr};

pub mod callable;
pub mod builtins;

pub struct LoxCallable {
    parameter_names: Vec<String>,
    body: Statement,
}

impl callable::Callable for LoxCallable {
    fn arity(&self) -> usize {
        self.parameter_names.len()
    }

    fn call(&self, interp: &mut Interpreter, args: Vec<Value>) -> Result<Value, InterpErr> {
        Ok(Value::NilV)
    }
}
