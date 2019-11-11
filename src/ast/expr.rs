use crate::token;
use crate::ast::{Visitor, Node};


pub enum Expr {
    Literal(token::Literal),
    Grouping(Box<Expr>),
    Unary(token::Token, Box<Expr>),
    Binary(token::Token, Box<Expr>, Box<Expr>),
}

impl Node for Expr {
    fn accept<T>(&self, v: &mut Visitor<T>) -> T {
        v.visit_expr(self)
    }
}
