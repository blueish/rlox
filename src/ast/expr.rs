use crate::token;
use crate::ast::{Visitor, Node};


#[derive(PartialEq, Debug)]
pub enum Expr {
    Identifier(String),
    Assignment(String, Box<Expr>),
    LiteralExpr(token::Literal),
    Grouping(Box<Expr>),
    Unary(token::Token, Box<Expr>),
    Binary(token::Token, Box<Expr>, Box<Expr>),
}

impl Node for Expr {
    fn accept<T>(&self, v: &mut Visitor<T>) -> T {
        v.visit_expr(self)
    }
}
