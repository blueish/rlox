use std::collections::HashMap;

use crate::value::Value;

#[derive(Clone, Debug)]
pub struct Environment {
    values: HashMap<String, Value>,
    pub enclosing_scope: Option<Box<Environment>>,
}

impl Environment {
    pub fn new(enclosing_scope: Option<Box<Environment>>) -> Environment {
        Environment {
            values: HashMap::new(),
            enclosing_scope: enclosing_scope,
        }
    }

    pub fn define(&mut self, name: String, val: Value) {
        self.values.insert(name, val);
    }

    pub fn get(&self, name: &String) -> Option<&Value> {
        match self.values.get(name) {
            Some(val) => Some(val),
            None => match &self.enclosing_scope {
                Some(env) => env.get(name),
                None => None,
            }
        }
    }

    pub fn assign(&mut self, name: &String, val: Value) -> Result<(), String> {
        if self.values.get(name).is_some() {
            self.values.insert(name.clone(), val);
            return Ok(());
        }

        match &mut self.enclosing_scope {
            Some(env) => (*env).assign(name, val),
            None => Err(format!("Undefined variable {}", name)),
        }
    }
}
