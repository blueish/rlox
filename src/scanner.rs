use std::ops::Index;
use std::str;

use crate::token;
use crate::errors;
use crate::constants;

use token::TokenType::*;
use token::Literal;
use token::Literal::{Number, StringLit};

pub struct Scanner<'a, 'b> {
    source: &'a [u8],
    tokens: Vec<token::Token>,
    pub error_reporter: &'b mut errors::ErrorReporter,
    start: usize,
    current: usize,
    line: usize,
}

impl<'a, 'b> Scanner<'a, 'b> {
    pub fn new(source: &'a String, error_reporter: &'b mut errors::ErrorReporter) -> Scanner<'a, 'b> {
        Scanner {
            source: source.as_bytes(),
            tokens: Vec::new(),
            error_reporter: error_reporter,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> bool {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan();
        }

        let token: token::Token = token::Token {
            token_type: EOF,
            lexeme: String::from(""),
            literal: None,
            line: self.line,
        };

        self.tokens.push(token);

        return self.error_reporter.had_errors;
    }

    pub fn tokens(self) -> Vec<token::Token> {
        self.tokens
    }

    fn scan(&mut self) {
        let ch = self.advance();
        match ch {
            '(' => self.add_token(LEFT_PAREN, None),
            ')' => self.add_token(RIGHT_PAREN, None),
            '{' => self.add_token(LEFT_BRACE, None),
            '}' => self.add_token(RIGHT_BRACE, None),
            ',' => self.add_token(COMMA, None),
            '.' => self.add_token(DOT, None),
            '-' => self.add_token(MINUS, None),
            '+' => self.add_token(PLUS, None),
            ';' => self.add_token(SEMICOLON, None),
            '*' => self.add_token(STAR, None),
            '!' => {
                if self.lookahead_with_consume('=') {
                    self.add_token(BANG_EQUAL, None)
                } else {
                    self.add_token(BANG, None)
                }
            },
            '=' => {
                if self.lookahead_with_consume('=') {
                    self.add_token(EQUAL_EQUAL, None)
                } else {
                    self.add_token(EQUAL, None)
                }
            },
            '<' => {
                if self.lookahead_with_consume('=') {
                    self.add_token(LESS_EQUAL, None)
                } else {
                    self.add_token(LESS, None)
                }
            },
            '>' => {
                if self.lookahead_with_consume('=') {
                    self.add_token(GREATER_EQUAL, None)
                } else {
                    self.add_token(GREATER, None)
                }
            },
            '/' => {
                if self.lookahead_with_consume('/') {
                    // A comment, advance until end of line
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(SLASH, None);
                }
            },
            ' ' | '\r' | '\t' => (), // ignore whitespace
            '\n' => self.line += 1,
            '\"' => self.consume_string(),
            default => {
                if is_digit(default) {
                    self.consume_number();
                } else if default.is_alphabetic() {
                    self.consume_identifier();
                } else {
                    self.error_reporter.report(self.line, String::from(""), String::from("Invalid character"));
                }
            }
        }
    }


    // Lookahead Fuctions

    fn lookahead_with_consume(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            false
        } else if self.source[self.current] as char != expected {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.source[self.current] as char
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }
        self.source[self.current + 1] as char
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        return *self.source.index(self.current - 1) as char;
    }


    // Literal scanning

    fn consume_string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.error_reporter.report(self.line, String::from(""), String::from("Unterminated string"));
            return;
        }

        // Consume closing "
        self.advance();

        let value: String = String::from_utf8(self.source[self.start + 1..self.current - 1].to_vec())
            .unwrap();

        self.add_token(STRING, Some(StringLit(value)));
    }

    fn consume_number(&mut self) {
        while is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == '.' && is_digit(self.peek_next()) {
            // Consume the "."
            self.advance();

            while is_digit(self.peek()) {
                self.advance();
            }
        }

        let maybe_num = String::from_utf8(self.source[self.start..self.current].to_vec())
            .map(|s| s.parse::<f64>());

        match maybe_num {
            Ok(s) => self.add_token(NUMBER, Some(Number(s.unwrap()))),
            Err(e) => self.error_reporter.report(self.line, String::from(""), format!("{:?}", e)),
        }
    }

    fn consume_identifier(&mut self) {
        while self.peek().is_alphanumeric() {
            self.advance();
        }
        let text_result = str::from_utf8(&self.source[self.start..self.current]);

        match text_result {
            Ok(t) => match constants::KEYWORDS.get(&t) {
                Some(token_type) => {
                    let t_type: token::TokenType = token_type.clone();
                    self.add_token(t_type, None);
                },
                None => self.add_token(IDENTIFIER, None),
            },
            Err(e) => self.error_reporter.report(self.line, String::from(""), format!("{:?}", e)),
        }

    }


    // Token manipulation

    fn add_token(&mut self, token: token::TokenType, literal: Option<Literal>) {
        let text = self.source[self.start..self.current].to_vec();

        self.tokens.push(token::Token {
            token_type: token,
            lexeme: String::from_utf8(text).unwrap(),
            literal: literal,
            line: self.line,
        });
    }


    // Helpers
    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
}

fn is_digit(c: char) -> bool {
    c >=  '0' && c <='9'
}
