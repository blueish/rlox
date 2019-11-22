use std::fmt;
use std::cmp;

use crate::value::Value;

use crate::interp::interpreter::{Interpreter, InterpErr};

pub trait Callable {
    fn call(&self, interp: &mut Interpreter, args: Vec<Value>) -> Result<Value, InterpErr>;
    fn arity(&self) -> usize;
}

impl fmt::Debug for Callable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "<no repr for callable>")
    }
}

impl PartialEq for Callable {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

