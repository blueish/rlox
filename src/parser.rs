use crate::ast::expr::Expr;
use crate::ast::expr::Expr::*;
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

impl From<Vec<ParseError>> for ParseError {
    fn from(errors: Vec<ParseError>) -> Self {
        let (line, msg) = errors.iter()
            .fold((0, "Errors in block:\n\t".to_string()),|acc, e| match acc {
                (0, m) => (e.line, format!("{}\n\t{}", m, e.message)),
                (n, m) => (n, format!("{}\n\t{}", m, e.message)),
            });

        ParseError {
            line: line,
            lexeme: "<block value>".to_string(),
            message: msg,
        }
    }
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
            statements.push(parser.declaration());
        }

        return statements;
    }

    fn declaration(&mut self) -> Result<Statement, ParseError> {
        let res = match (self.matches_single(&VAR), self.matches_single(&LEFT_BRACE)) {
            (false, false) => self.statement(),
            (false, true) => self.block_statement(),
            (true, false) => self.var_declaration(),
            (true, true) => Err(ParseError {
                line: self.peek().line,
                lexeme: "var {".to_string(),
                message: "var { is not valid syntax".to_string(),
            })
        };

        match res {
            Ok(s) => Ok(s),
            Err(e) => {
                self.synchronize();
                Err(e)
            }
        }
    }

    fn block_statement(&mut self) -> Result<Statement, ParseError> {
        let mut statements: Vec<Statement> = Vec::new();
        let mut errors: Vec<ParseError> = Vec::new();

        while !self.check(&RIGHT_BRACE) && !self.is_at_end() {
            match self.declaration() {
                Ok(s) => statements.push(s),
                Err(e) => errors.push(e),
            }
        }

        self.consume(&RIGHT_BRACE, "Expected '}' after block")?;

        match errors.len() {
            0 => Ok(Statement::Block(statements)),
            _ => Err(ParseError::from(errors)),
        }
    }

    fn var_declaration(&mut self) -> Result<Statement, ParseError> {
        let name = self.consume(&IDENTIFIER, "Needed identifier after var")?
            .clone();

        let mut initializer = LiteralExpr(Nil);
        if self.matches_single(&EQUAL) {
            initializer = self.expression()?;
        }

        self.consume(&SEMICOLON, "Expected ';' after var decl")?;
        return Ok(Statement::VarDec(name,  initializer));
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
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_binary_exprs(&vec!(
            BANG_EQUAL, EQUAL_EQUAL,
            GREATER, GREATER_EQUAL, LESS, LESS_EQUAL,
            PLUS, MINUS,
            STAR, SLASH
        ), 0)?;

        if self.matches_single(&EQUAL) {
            let tok = self.previous().unwrap().line;
            let r_val = self.assignment()?;

            match expr {
                Identifier(name) => {
                    expr = Assignment(name, Box::new(r_val));
                },
                _ => return Err(ParseError {
                    line: tok,
                    lexeme: "=".to_string(),
                    message: "Invalid assignment target".to_string(),
                }),
            }
        }

        return Ok(expr);
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
                self.advance();
                let expr: Expr = self.expression()?;
                self.consume(&RIGHT_PAREN, "Expect ')' after expression.")?;
                return Ok(Grouping(Box::new(expr)));
            },
            IDENTIFIER => {
                let curr = self.peek().clone();
                self.advance();
                Ok(Identifier(curr.lexeme))
            },
            _ => {
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

        while !self.is_at_end() {
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

    fn consume(&mut self, token: &TokenType, err_msg: &'static str) -> Result<&Token, ParseError> {
        if self.check(token) {
            self.advance();
            return Ok(self.previous().unwrap());
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

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }   use Literal::{Number, StringLit};


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
    fn var() {
        let er = &mut ErrorReporter{ had_errors: false };
        let results = Parser::parse(&vec!(
            Token {
                token_type: VAR,
                lexeme: String::from("var"),
                literal: None,
                line: 1,
            },
            Token {
                token_type: IDENTIFIER,
                lexeme: String::from("a"),
                literal: None,
                line: 1,
            },
            Token {
                token_type: SEMICOLON,
                lexeme: String::from(";"),
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
                assert_eq!(*e, Statement::VarDec(
                    Token {
                        token_type: IDENTIFIER,
                        lexeme: String::from("a"),
                        literal: None,
                        line: 1,
                    },
                    LiteralExpr(Literal::Nil)));
            },
            Err(_) => assert!(false),
        }
    }
    #[test]
    fn var_assignment() {
        let er = &mut ErrorReporter{ had_errors: false };
        let results = Parser::parse(&vec!(
            Token {
                token_type: IDENTIFIER,
                lexeme: String::from("a"),
                literal: None,
                line: 1,
            },
            Token {
                token_type: EQUAL,
                lexeme: String::from("="),
                literal: None,
                line: 1,
            },
            Token {
                token_type: NUMBER,
                lexeme: String::from("1"),
                literal: Some(Number(1.0)),
                line: 1,
            },
            Token {
                token_type: SEMICOLON,
                lexeme: String::from(";"),
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
                assert_eq!(*e, Statement::Expression(Assignment(
                    String::from("a"),
                    Box::new(LiteralExpr(Number(1.0)))))
                );
            },
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn block_statements() {
        let er = &mut ErrorReporter{ had_errors: false };
        let results = Parser::parse(&vec!(
            Token {
                token_type: LEFT_BRACE,
                lexeme: String::from("{"),
                literal: None,
                line: 1,
            },
            Token {
                token_type: IDENTIFIER,
                lexeme: String::from("a"),
                literal: None,
                line: 1,
            },
            Token {
                token_type: EQUAL,
                lexeme: String::from("="),
                literal: None,
                line: 1,
            },
            Token {
                token_type: NUMBER,
                lexeme: String::from("1"),
                literal: Some(Number(1.0)),
                line: 1,
            },
            Token {
                token_type: SEMICOLON,
                lexeme: String::from(";"),
                literal: None,
                line: 1,
            },
            Token {
                token_type: RIGHT_BRACE,
                lexeme: String::from("}"),
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
                assert_eq!(*e, Statement::Block(vec!(Statement::Expression(Assignment(
                    String::from("a"),
                    Box::new(LiteralExpr(Number(1.0)))))
                )));
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
