use crate::interp::env::Environment;

use crate::ast::Visitor;
use crate::ast::expr::Expr;
use crate::ast::stmt::Statement;
use crate::token::{Literal};

use Statement::*;
use Expr::*;
use Literal::*;

#[derive(Debug)]
pub struct InterpErr {
    msg: String,
}

impl From<String> for InterpErr {
    fn from(error: String) -> Self {
        InterpErr {
            msg: error,
        }
    }
}

pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            environment: Environment::new(None)
        }
    }

    pub fn interp(&mut self, statements: Vec<Statement>) -> Result<Option<Literal>, Vec<InterpErr>> {
        let mut last = None;
        let mut errs = Vec::new();

        for s in statements {
            match self.visit_stmt(&s) {
                Ok(res) => {
                    last = Some(res);
                }
                Err(e) => {
                    errs.push(e);
                },
            }
        }

        if errs.len() > 0 {
            return Err(errs)
        }

        Ok(last)
    }
}

impl Visitor<Result<Literal, InterpErr>> for Interpreter {
    fn visit_expr(&mut self, e: &Expr) -> Result<Literal, InterpErr> {
        use crate::token::TokenType::*;
        match e {
            Identifier(id) => match self.environment.get(id) {
                Some(val) => Ok(val.clone()),
                None => {
                    Err(InterpErr {
                        msg: format!("Identifier {:?} not bound in scope", id)
                    })
                },
            },
            Assignment(id, boxed_expr) => {
                let val = self.visit_expr(boxed_expr)?;
                self.environment.assign(&id, val.clone())?;

                Ok(val)
            },
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
                // AND and OR operators short circuit, so handle them separately
                if tok.token_type == AND {
                    let left = &self.visit_expr(left)?;
                    if is_truthy(left) == Literal::True {
                        return Ok(self.visit_expr(right)?);
                    }

                    return Ok(left.clone());
                }

                if tok.token_type == OR {
                    let left = &self.visit_expr(left)?;
                    if is_truthy(left) == Literal::True {
                        return Ok(left.clone());
                    }

                    return Ok(self.visit_expr(right)?);
                }

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
                    },
                    _ => Err(InterpErr {
                        msg: format!("Invalid binary token type {}", tok),
                    }),
                }
            },
        }
    }

    fn visit_stmt(&mut self, s: &Statement) -> Result<Literal, InterpErr> {
        match s {
            Expression(e) => self.visit_expr(e),
            Print(e) => {
                println!("{}", self.visit_expr(e)?);
                Ok(Nil)
            },
            VarDec(tok, exp) => {
                let val = self.visit_expr(exp)?;
                self.environment.define(tok.lexeme.clone(), val);
                Ok(Nil)
            },
            Block(stmts) => {
                let old_env = self.environment.clone();
                self.environment = Environment::new(Some(Box::new(old_env)));
                for stmt in stmts {
                    self.visit_stmt(stmt)?;
                }

                match &self.environment.enclosing_scope {
                    Some(boxed_env) => {
                        let env = *boxed_env.clone();
                        std::mem::replace(&mut self.environment, env);
                    },
                    None => return Err(InterpErr {
                        msg: "Attempted to return from a block without an enclosing environment.".to_string(),
                    }),
                }

                Ok(Nil)
            },
            IfStmt(cond_expr, then_expr, else_expr) => {
                match is_truthy(&self.visit_expr(cond_expr)?) {
                    Literal::True => self.visit_stmt(then_expr),
                    Literal::False => match else_expr {
                        Some(boxed_else) => self.visit_stmt(boxed_else),
                        None => Ok(Nil)
                    },
                    _ => unreachable!(), // Impossible due to is_truthy returning only lit t/f
                }
            }
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
        let mut interpreter = Interpreter{
            environment: Environment::new(None),
        };

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
        let mut interpreter = Interpreter{
            environment: Environment::new(None),
        };

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
    fn test_negation_unary() {
        let mut interpreter = Interpreter{
            environment: Environment::new(None),
        };

        let val = interpreter.visit_expr(&Unary(
            Token {
                token_type: BANG,
                lexeme: "!".to_string(),
                literal: None,
                line: 1,
            },
            Box::new(LiteralExpr(True)),
        ));

        assert!(val.is_ok());
        assert_eq!(val.unwrap(), Literal::False)
    }

    #[test]
    fn test_negative_unary() {
        let mut interpreter = Interpreter{
            environment: Environment::new(None),
        };

        let val = interpreter.visit_expr(&Unary(
            Token {
                token_type: MINUS,
                lexeme: "-".to_string(),
                literal: None,
                line: 1,
            },
            Box::new(LiteralExpr(Number(2.0)))
        ));

        assert!(val.is_ok());
        assert_eq!(val.unwrap(), Literal::Number(-2.0));
    }

    #[test]
    fn test_var() {
        let mut interpreter = Interpreter{
            environment: Environment::new(None),
        };

        interpreter.environment.define("a".to_string(), Number(1.0));

        let val = interpreter.visit_expr(
            &Identifier("a".to_string()),
        );

        assert!(val.is_ok());
        assert_eq!(val.unwrap(), Number(1.0));
    }

    #[test]
    fn test_groupings() {
        let mut interpreter = Interpreter{
            environment: Environment::new(None),
        };

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
