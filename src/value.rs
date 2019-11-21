use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    StringV(String),
    NumberV(f64),
    TrueV,
    FalseV,
    NilV,
    ClosureV,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::StringV(s) => write!(f, "{}", s),
            Value::NumberV(n) => write!(f, "{}", n),
            Value::TrueV => write!(f, "{}", true),
            Value::FalseV => write!(f, "{}", false),
            Value::NilV => write!(f, "Nil", ),
            Value::ClosureV => write!(f, "ClosureV", ),
        }
    }
}


