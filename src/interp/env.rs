use std::collections::HashMap;

use crate::token::{Literal};

pub struct Environment {
    values: HashMap<String, Literal>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: HashMap::new()
        }
    }

    pub fn define(&mut self, name: String, val: Literal) {
        self.values.insert(name, val);
    }

    pub fn get(&self, name: &String) -> Option<&Literal> {
        self.values.get(name)
    }

    pub fn assign(&mut self, name: &String, val: Literal) -> Result<(), String> {
        if self.get(name).is_some() {
            self.values.insert(name.clone(), val);
            return Ok(());
        }

        Err(format!("Undefined variable {}", name))
    }
}
