use crate::token;
use crate::ast::{Visitor, Node};
use crate::ast::literals::Literal;


#[derive(PartialEq, Debug)]
pub enum Expr {
    Identifier(String),
    Assignment(String, Box<Expr>),
    LiteralExpr(Literal),
    Grouping(Box<Expr>),
    Unary(token::Token, Box<Expr>),
    Binary(token::Token, Box<Expr>, Box<Expr>),
    Call(Box<Expr>, usize, Vec<Expr>),
}

impl Node for Expr {
    fn accept<T>(&self, v: &mut Visitor<T>) -> T {
        v.visit_expr(self)
    }
}
