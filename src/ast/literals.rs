use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    StringLit(String),
    Number(f64),
    True,
    False,
    Nil,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::StringLit(s) => write!(f, "{}", s),
            Literal::Number(n) => write!(f, "{}", n),
            Literal::True => write!(f, "{}", true),
            Literal::False => write!(f, "{}", false),
            Literal::Nil => write!(f, "Nil", ),
        }
    }
}

