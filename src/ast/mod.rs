pub mod expr;
pub mod printer;
pub mod stmt;

use stmt::Statement;
use expr::Expr;


pub trait Visitor<T> {
    fn visit_expr(&mut self, e: &Expr) -> T;
    fn visit_stmt(&mut self, e: &Statement) -> T;
}

pub trait Node {
    fn accept<T>(&self, v: &mut Visitor<T>) -> T;
}
