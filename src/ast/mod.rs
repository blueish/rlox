pub mod expr;
pub mod printer;

use expr::Expr;


pub trait Visitor<T> {
    fn visit_expr(&mut self, e: &Expr) -> T;
}

pub trait Node {
    fn accept<T>(&self, v: &mut Visitor<T>) -> T;
}
