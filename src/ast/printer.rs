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
        let mut res = "".to_string();

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

        return res;
    }
}

impl Visitor<String> for PrettyPrinter {
    fn visit_stmt(&mut self, s: &Statement) -> String {
        match s {
            Empty => "<empty>".to_string(),
            Expression(e) => self.visit_expr(e),
            Print(e) => format!("PRINT {}", self.visit_expr(e)),
            Decl(id, Some(e)) => format!("DECL {} -> {}", id,  self.visit_expr(e)),
            Decl(id, None) => format!("DECL {} -> <none>", id),
            Block(statements) => {
                let mut ret = "{".to_string();

                for s in statements {
                    ret.push_str("\t");
                    ret.push_str(&self.visit_stmt(s));
                }

                ret.push_str(&"}".to_string());

                return ret;
            },
        }
    }

    fn visit_expr(&mut self, e: &Expr) -> String {
        match e {
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
