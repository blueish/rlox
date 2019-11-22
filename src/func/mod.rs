use crate::value::Value;
use crate::ast::stmt::Statement;
use crate::interp::interpreter::{ Interpreter, InterpErr};

pub mod callable;
pub mod builtins;

#[derive(Debug)]
pub enum FunKind {
    Function,
}

pub struct LoxCallable {
    parameter_names: Vec<String>,
    body: Box<Statement>,
}

impl callable::Callable for LoxCallable {
    fn arity(&self) -> usize {
        self.parameter_names.len()
    }

    fn call(&self, _interp: &mut Interpreter, _args: Vec<Value>) -> Result<Value, InterpErr> {
        Ok(Value::NilV)
    }
}
