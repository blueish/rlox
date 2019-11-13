// use std::collections::HashMap;
use crate::token::{Literal};
use Literal::*;

use crate::ast::Visitor;
use crate::ast::expr::Expr;

use Expr::*;

#[derive(Debug)]
pub struct InterpErr {
    msg: String,
}

pub struct Interpreter {
    // symbols: HashMap<String, Literal>,
}

impl Visitor<Result<Literal, InterpErr>> for Interpreter {
    fn visit_expr(&mut self, e: &Expr) -> Result<Literal, InterpErr> {
        use crate::token::TokenType::*;
        match e {
            LiteralExpr(lit) => Ok(lit.clone()),
            Grouping(bx) => Ok(self.visit_expr(bx)?),
            Unary(tok, bx) => {
                let right = self.visit_expr(bx)?;
                match tok.token_type {
                    MINUS => match right {
                        Literal::Number(n) => Ok(Literal::Number(-n)),
                        _ => Err(InterpErr{
                            msg: format!("Value {:?} not allowed for unary minus", right)
                        }),
                    },
                    BANG => Ok(negate(is_truthy(&right))),
                    _ => Err(InterpErr{
                        msg: format!("Token {:?} not allowed for unary expression", tok),
                    }),
                }
            },
            Binary(tok, left, right) => {
                let left = &self.visit_expr(left)?;
                let right = &self.visit_expr(right)?;
                match tok.token_type {
                    BANG_EQUAL => Ok(negate(are_equal(left, right))),
                    EQUAL_EQUAL => Ok(are_equal(left, right)),
                    GREATER => {
                        match unwrap_num_lits(left, right) {
                            Some((l, r)) => {
                                if l > r {
                                    return Ok(True)
                                }
                                return Ok(False)
                            },
                            None => Err(InterpErr {
                                msg: format!("One of {}, {} was not a number", left, right),
                            }),
                        }
                    },
                    GREATER_EQUAL => {
                        match unwrap_num_lits(left, right) {
                            Some((l, r)) => {
                                if l >= r {
                                    return Ok(True)
                                }
                                return Ok(False)
                            },
                            None => Err(InterpErr {
                                msg: format!("One of {}, {} was not a number", left, right),
                            }),
                        }
                    },
                    LESS => {
                        match unwrap_num_lits(left, right) {
                            Some((l, r)) => {
                                if l < r {
                                    return Ok(True)
                                }
                                return Ok(False)
                            },
                            None => Err(InterpErr {
                                msg: format!("One of {}, {} was not a number", left, right),
                            }),
                        }
                    },
                    LESS_EQUAL => {
                        match unwrap_num_lits(left, right) {
                            Some((l, r)) => {
                                if l <= r {
                                    return Ok(True)
                                }
                                return Ok(False)
                            },
                            None => Err(InterpErr {
                                msg: format!("One of {}, {} was not a number", left, right),
                            }),
                        }
                    },
                    // Simple number ops
                    MINUS => {
                        match unwrap_num_lits(left, right) {
                            Some((l, r)) => Ok(Number(l - r)),
                            None => Err(InterpErr {
                                msg: format!("One of {}, {} was not a number", left, right),
                            }),
                        }
                    },
                    STAR => {
                        match unwrap_num_lits(left, right) {
                            Some((l, r)) => Ok(Number(l * r)),
                            None => Err(InterpErr {
                                msg: format!("One of {}, {} was not a number", left, right),
                            }),
                        }
                    },
                    SLASH => {
                        match unwrap_num_lits(left, right) {
                            Some((l, r)) => Ok(Number(l / r)),
                            None => Err(InterpErr {
                                msg: format!("One of {}, {} was not a number", left, right),
                            }),
                        }
                    },
                    PLUS => {
                        // Special case: overloaded strings, concat them both
                        match (left, right) {
                            (Number(a), Number(b)) => Ok(Number(a + b)),
                            (StringLit(s), StringLit(s2)) => {
                                let mut res = s.clone();
                                res.push_str(s2);
                                Ok(StringLit(res))
                            },
                            _ =>  Err(InterpErr {
                                msg: format!("One of {}, {} was not a number", left, right),
                            }),
                        }
                        // match unwrap_num_lits(left, right) {
                        //     Some((l, r)) => Ok(Number(l + r)),
                        //     None => Err(InterpErr {
                        //         msg: format!("One of {}, {} was not a number", left, right),
                        //     }),
                        // }
                    },
                    _ => Err(InterpErr {
                        msg: format!("Invalid binary token type {}", tok),
                    }),
                }
            },
        }
    }
}

fn are_equal(a: &Literal, b: &Literal) -> Literal {
    match (a, b) {
        (StringLit(ref s1), StringLit(ref s2)) if s1 == s2 => Literal::True,
        (Number(n), Number(m)) if n == m => Literal::True,
        (True, True) => Literal::True,
        (False, True) => Literal::True,
        (Nil, Nil) => Literal::True,
        _ => Literal::False,
    }
}

fn unwrap_num_lits(a: &Literal, b: &Literal) -> Option<(f64, f64)> {
    match a {
        Number(an) => {
            match b {
                Number(bn) => Some((*an, *bn)),
                _ => None,
            }
        },
        _ => None,
    }
}

fn is_truthy(lit: &Literal) -> Literal {
    match lit {
        StringLit(_) => Literal::True,
        Number(_) => Literal::True,
        True => Literal::True,
        False => Literal::False,
        Nil => Literal::False,
    }
}

fn negate(lit: Literal) -> Literal {
    match lit {
        True => Literal::False,
        False => Literal::True,
        _ => panic!("not a boolean literal"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::expr::Expr::{LiteralExpr, Unary, Binary, Grouping};
    use crate::token::{Token, TokenType, Literal};
    use TokenType::*;

    #[test]
    fn test_one_plus_two() {
        let mut interpreter = Interpreter{};

        let val = interpreter.visit_expr(&Binary(
            Token {
                token_type: PLUS,
                lexeme: "+".to_string(),
                literal: None,
                line: 1,
            },
            Box::new(LiteralExpr(Number(1.0))),
            Box::new(LiteralExpr(Number(2.0)))
        ));

        assert!(val.is_ok());
        assert_eq!(val.unwrap(), Literal::Number(3.0))
    }

    #[test]
    fn test_two_strings() {
        let mut interpreter = Interpreter{};

        let val = interpreter.visit_expr(&Binary(
            Token {
                token_type: PLUS,
                lexeme: "+".to_string(),
                literal: None,
                line: 1,
            },
            Box::new(LiteralExpr(StringLit("as".to_string()))),
            Box::new(LiteralExpr(StringLit("df".to_string())))
        ));

        assert!(val.is_ok());
        assert_eq!(val.unwrap(), StringLit("asdf".to_string()))
    }

    #[test]
    fn test_groupings() {
        let mut interpreter = Interpreter{};

        let val = interpreter.visit_expr(&Binary(
            Token {
                token_type: PLUS,
                lexeme: "+".to_string(),
                literal: None,
                line: 1,
            },
            Box::new(LiteralExpr(Number(1.0))),
            Box::new(Grouping(Box::new(
                Unary(
                    Token {
                        token_type: MINUS,
                        lexeme: "-".to_string(),
                        literal: None,
                        line: 1,
                    },
                    Box::new(LiteralExpr(Number(2.0)))
                )
            )))
        ));

        assert!(val.is_ok());
        assert_eq!(val.unwrap(), Literal::Number(-1.0))
    }
}
