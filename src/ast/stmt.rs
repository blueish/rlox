use super::expr::Expr;

#[derive(PartialEq, Debug)]
pub enum Statement {
    Expression(Expr),
    Print(Expr),
}
