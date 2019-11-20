use crate::ast::Visitor;
use crate::parser::ParseError;
use crate::ast::expr::Expr;
use crate::ast::stmt::Statement;
use Statement::*;

use Expr::*;

pub struct PrettyPrinter {}

impl PrettyPrinter {
    pub fn print_ast(statements: &Vec<Result<Statement, ParseError>>) -> String {
        let mut pp = PrettyPrinter{};
        let mut res = "AST:\n".to_string();

        for result in statements {
            match result {
                Ok(statement) => {
                    res.push_str(&format!("{}\n", pp.visit_stmt(statement)));
                }
                Err(e) => {
                    res.push_str(&format!("ParseError: {:?}\n", e));
                },
            };
        }

        return format!("{}}}", res);
    }
}

impl Visitor<String> for PrettyPrinter {
    fn visit_stmt(&mut self, s: &Statement) -> String {
        match s {
            Expression(e) => self.visit_expr(e),
            Print(e) => format!("PRINT {}", self.visit_expr(e)),
            VarDec(t, e) => format!("VARDEC {} -> {}", t.lexeme, self.visit_expr(e)),
            Block(stmts) => {
                let mut res = "BLOCK {\n".to_string();
                for stmt in stmts {
                    res.push_str(&format!("\t{}\n", self.visit_stmt(stmt)));
                }

                format!("{}}}", res)
            },
            IfStmt(c, t, e) => format!("IF {}\nTHEN -> {}\nELSE -> {}",
                                       self.visit_expr(c),
                                       self.visit_stmt(t),
                                       match e {
                                           Some(else_clause) => self.visit_stmt(else_clause),
                                           None => "<no else clause>".to_string(),
                                       }),
        }
    }

    fn visit_expr(&mut self, e: &Expr) -> String {
        match e {
            Identifier(id) => format!("id: {{ {} }}", id),
            Assignment(id, boxed_expr) => format!("id {} -> {}", id, self.visit_expr(boxed_expr)),
            LiteralExpr(typ) => format!("{}", typ),
            Grouping(bx) => format!("(group {})", self.visit_expr(bx)),
            Unary(typ, bx) => format!("({} {})", typ, self.visit_expr(bx)),
            Binary(tok, left, right) => format!("({} {} {})", self.visit_expr(left), tok, self.visit_expr(right)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token;
    use crate::token::Literal::*;

    #[test]
    fn simple_print() {
        let mut pp = PrettyPrinter{};

        let res = pp.visit_expr(&Binary(
            token::Token {
                token_type: token::TokenType::STAR,
                lexeme: String::from(""),
                literal: None,
                line: 0,
            },
            Box::new(
                Unary(
                    token::Token {
                        token_type: token::TokenType::MINUS,
                        lexeme: String::from(""),
                        literal: None,
                        line: 0,
                    },
                    Box::new(LiteralExpr(Number(123.0))),
                )
            ),
            Box::new(
                Grouping(
                    Box::new(LiteralExpr(Number(45.67)))
                )
            )
        ));

        assert_eq!(res, "((MINUS 123) STAR (group 45.67))");
    }
}
