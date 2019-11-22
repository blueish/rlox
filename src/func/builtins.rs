use std::time::{SystemTime, UNIX_EPOCH};

use crate::value::Value;
use crate::interp::interpreter::{Interpreter, InterpErr};

use super::callable::Callable;

pub struct BuiltinClock {}

impl Callable for BuiltinClock {
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, _: &mut Interpreter, _: Vec<Value>) -> Result<Value, InterpErr> {
        let start = SystemTime::now();
        let since_the_epoch = start.duration_since(UNIX_EPOCH)
            .expect("Time went backwards");

        Ok(Value::NumberV(since_the_epoch.as_secs() as f64))
    }
}
