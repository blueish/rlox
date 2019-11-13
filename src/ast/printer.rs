use crate::ast::Visitor;
use crate::ast::expr::Expr;

use Expr::*;

pub struct PrettyPrinter {}

impl Visitor<String> for PrettyPrinter {
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
