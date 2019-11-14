use log::{info};
use crate::ast::expr::Expr;
use crate::ast::expr::Expr::{LiteralExpr, Unary, Binary, Grouping};
use crate::ast::stmt::Statement;
use crate::errors::ErrorReporter;
use crate::token::{Token, TokenType, Literal};

use Literal::{True, False, Nil};
use TokenType::*;

#[derive(Debug)]
pub struct ParseError {
    line: usize,
    lexeme: String,
    message: String,
}

pub struct Parser<'a, 'b> {
    tokens: &'a Vec<Token>,
    current: usize,
    error_reporter: &'b mut ErrorReporter,
}


impl<'a, 'b> Parser<'a, 'b> {
    pub fn parse(tokens: &Vec<Token>, error_reporter: &mut ErrorReporter) -> Vec<Result<Statement, ParseError>> {
        let mut parser = Parser {
            tokens: tokens,
            current: 0,
            error_reporter: error_reporter,
        };

        let mut statements: Vec<Result<Statement, ParseError>> = Vec::new();

        while !parser.is_at_end() {
            statements.push(parser.statement());
        }

        return statements;
    }

    fn statement(&mut self) -> Result<Statement, ParseError> {
        if self.matches_single(&PRINT) {
            return self.print_statement();
        }

        return self.expression_statement();
    }

    fn print_statement(&mut self) -> Result<Statement, ParseError> {
        let value = self.expression()?;
        self.consume(&SEMICOLON, "Expect ; after value.")?;
        return Ok(Statement::Print(value));
    }

    fn expression_statement(&mut self) -> Result<Statement, ParseError> {
        let value = self.expression()?;
        self.consume(&SEMICOLON, "Expect ; after value.")?;
        return Ok(Statement::Expression(value));
    }


    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_binary_exprs(&vec!(
            BANG_EQUAL, EQUAL_EQUAL,
            GREATER, GREATER_EQUAL, LESS, LESS_EQUAL,
            PLUS, MINUS,
            STAR, SLASH
        ), 0)
    }

    fn parse_binary_exprs(&mut self, operands: &Vec<TokenType>, level: usize) -> Result<Expr, ParseError> {
        let parse_level = operands.get(level);

        match parse_level {
            Some(token) => {
                let mut expr: Expr = self.parse_binary_exprs(operands, level + 1)?;

                while self.matches_single(token) {
                    let m_op = self.previous().clone();
                    let op = match m_op {
                        Some(op) => op,
                        None => return Err(ParseError {
                            line: 0,
                            lexeme: "".to_string(),
                            message: "No previous operation".to_string(),
                        }),
                    }.clone();

                    let right: Expr = self.parse_binary_exprs(operands, level + 1)?;


                    expr = Binary(op, Box::new(expr), Box::new(right));
                }


                return Ok(expr);
            }
            None => self.parse_unary_exprs(&vec!(BANG, MINUS), 0),
        }
    }

    fn parse_unary_exprs(&mut self, operands: &Vec<TokenType>, level: usize) -> Result<Expr, ParseError> {
        let parse_level = operands.get(level);

        return match parse_level {
            Some(token) => {
                while self.matches_single(token) {
                    let right: Expr = self.parse_unary_exprs(operands, level + 1)?;

                    let m_op = self.previous().clone();
                    let op = match m_op {
                        Some(op) => op.clone(),
                        None => return Err(ParseError {
                            line: 0,
                            lexeme: "".to_string(),
                            message: "No previous operation".to_string(),
                        }),
                    };

                    return Ok(Unary(op, Box::new(right)));
                }

                return self.parse_primary();
            }
            None => self.parse_primary(),
        };
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match &self.peek().token_type {
            FALSE =>  {
                self.advance();
                Ok(LiteralExpr(False))
            },
            TRUE => {
                self.advance();
                Ok(LiteralExpr(True))
            },
            NIL => {
                self.advance();
                Ok(LiteralExpr(Nil))
            },
            LEFT_PAREN => {
                info!("left paren");
                self.advance();
                let expr: Expr = self.expression()?;
                info!("sub expr done {:?}", &expr);
                self.consume(&RIGHT_PAREN, "Expect ')' after expression.")?;
                info!("consumed right paren");
                return Ok(Grouping(Box::new(expr)));
            },
            a => {
                info!("{:?}", a);
                let curr = self.peek().clone();
                self.advance();
                match &curr.literal {
                    Some(lit) => {
                        Ok(LiteralExpr(lit.clone()))
                    },
                    None => Err(ParseError {
                        line: curr.line,
                        lexeme: curr.lexeme.clone(),
                        message: format!("Parsing token {:?} yielded no literal", curr),
                    }),
                }
            },
        }
    }

    fn synchronize(&mut self) {
        self.advance();

        while self.is_at_end() {
            match self.previous() {
                Some(tok) => {
                    if tok.token_type == SEMICOLON {
                        return
                    }
                },
                None => (),
            }


            let tok = self.peek();

            match tok.token_type {
                CLASS => return,
                FUN => return,
                VAR => return,
                FOR => return,
                IF => return,
                WHILE => return,
                PRINT => return,
                RETURN => return,
                _ => self.advance(),
            }

        }
    }

    fn matches_single(&mut self, token: &TokenType) -> bool {
        if self.check(token) {
            self.advance();
            return true;
        }

        return false;
    }

    fn consume(&mut self, token: &TokenType, err_msg: &'static str) -> Result<(), ParseError> {
        if self.check(token) {
            self.advance();
            return Ok(());
        }

        let curr = self.peek().clone();
        let lex = curr.lexeme.clone();
        let err = ParseError {
            line: curr.line,
            lexeme: curr.lexeme,
            message: err_msg.to_string(),
        };
        self.error_reporter.report(err.line, lex, err_msg.to_string());
        return Err(err);
    }

    // Methods for getting tokens

    fn previous(&self) -> Option<&Token> {
        self.tokens.get(self.current - 1)
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false
        }

        self.peek().token_type == *token_type
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }


    // Helpers

    fn is_at_end(&self) -> bool {
        self.peek().token_type == EOF
    }

    fn advance(&mut self) {
        if !self.is_at_end() {
            self.current += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::{Token, Literal};
    use Literal::{Number, StringLit};


    #[test]
    fn num_lit() {
        let er = &mut ErrorReporter{ had_errors: false };
        let results = Parser::parse(&vec!(
            Token {
                token_type: NUMBER,
                lexeme: String::from(""),
                literal: Some(Number(12.0)),
                line: 1,
            },
            Token {
                token_type: SEMICOLON,
                lexeme: String::from(""),
                literal: None,
                line: 1,
            },
            Token {
                token_type: EOF,
                lexeme: String::from(""),
                literal: None,
                line: 1,
            },
        ), er);

        assert_eq!(results.len(), 1);

        match &results[0] {
            Ok(e) => {
                assert_eq!(*e, Statement::Expression(LiteralExpr(Number(12.0))));
            },
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn str_lit() {
        let er = &mut ErrorReporter{ had_errors: false };
        let results = Parser::parse(&vec!(
            Token {
                token_type: STRING,
                lexeme: String::from(""),
                literal: Some(Literal::StringLit("test".to_string())),
                line: 1,
            },
            Token {
                token_type: SEMICOLON,
                lexeme: String::from(""),
                literal: None,
                line: 1,
            },
            Token {
                token_type: EOF,
                lexeme: String::from(""),
                literal: None,
                line: 1,
            },
        ), er);

        assert_eq!(results.len(), 1);

        match &results[0] {
            Ok(e) => {
                assert_eq!(*e, Statement::Expression(LiteralExpr(StringLit("test".to_string()))));
            },
            Err(_) => assert!(false),
        }
    }



    #[test]
    fn binary_exprs() {
        let er = &mut ErrorReporter{ had_errors: false };
        let results = Parser::parse(&vec!(
            Token {
                token_type: NUMBER,
                lexeme: String::from("1"),
                literal: Some(Number(1.0)),
                line: 1,
            },
            Token {
                token_type: PLUS,
                lexeme: String::from("+"),
                literal: None,
                line: 1,
            },
            Token {
                token_type: NUMBER,
                lexeme: String::from("2"),
                literal: Some(Number(2.0)),
                line: 1,
            },
            Token {
                token_type: SEMICOLON,
                lexeme: String::from(""),
                literal: None,
                line: 1,
            },
            Token {
                token_type: EOF,
                lexeme: String::from(""),
                literal: None,
                line: 1,
            },
        ), er);

        assert_eq!(results.len(), 1);

        match &results[0] {
            Ok(e) => {
                assert_eq!(*e, Statement::Expression(Binary(
                    Token {
                        token_type: PLUS,
                        lexeme: "+".to_string(),
                        literal: None,
                        line: 1,
                    },
                    Box::new(LiteralExpr(Number(1.0))),
                    Box::new(LiteralExpr(Number(2.0)))
                )));
            },
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn nested_exprs() {
        let er = &mut ErrorReporter{ had_errors: false };
        let results = Parser::parse(&vec!(
            Token {
                token_type: NUMBER,
                lexeme: String::from("1"),
                literal: Some(Number(1.0)),
                line: 1,
            },
            Token {
                token_type: PLUS,
                lexeme: String::from("+"),
                literal: None,
                line: 1,
            },
            Token {
                token_type: LEFT_PAREN,
                lexeme: String::from("("),
                literal: None,
                line: 1,
            },
            Token {
                token_type: NUMBER,
                lexeme: String::from(""),
                literal: Some(Number(2.0)),
                line: 1,
            },
            Token {
                token_type: RIGHT_PAREN,
                lexeme: String::from(""),
                literal: None,
                line: 1,
            },
            Token {
                token_type: SEMICOLON,
                lexeme: String::from(""),
                literal: None,
                line: 1,
            },
            Token {
                token_type: EOF,
                lexeme: String::from(""),
                literal: None,
                line: 1,
            },
        ), er);

        assert_eq!(results.len(), 1);

        match &results[0] {
            Ok(e) => {
                assert_eq!(*e, Statement::Expression(Binary(
                    Token {
                        token_type: PLUS,
                        lexeme: "+".to_string(),
                        literal: None,
                        line: 1,
                    },
                    Box::new(LiteralExpr(Number(1.0))),
                    Box::new(Grouping(Box::new(LiteralExpr(Number(2.0)))))
                )));
            },
            Err(_) => assert!(false),
        }
    }
}
