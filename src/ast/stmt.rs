use super::expr::Expr;

#[derive(PartialEq, Debug)]
pub enum Statement {
    Empty,
    Expression(Expr),
    Print(Expr),
    Decl(String, Option<Expr>),
    Block(Vec<Statement>),
}
