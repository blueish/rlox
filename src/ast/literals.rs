#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    StringLit(String),
    Number(f64),
    True,
    False,
    Nil,
}

