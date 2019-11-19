use crate::token::Token;
use super::expr::Expr;

#[derive(PartialEq, Debug)]
pub enum Statement {
    Expression(Expr),
    Print(Expr),
    VarDec(Token, Expr),
    Block(Vec<Statement>),
}
