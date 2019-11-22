use crate::token::Token;
use super::expr::Expr;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression(Expr),
    Print(Expr),
    VarDec(Token, Expr),
    Block(Vec<Statement>),
    IfStmt(Expr, Box<Statement>, Option<Box<Statement>>),
    WhileStmt(Expr, Box<Statement>),
}
