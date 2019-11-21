use crate::interp::env::Environment;
use crate::value::Value;

use crate::ast::Visitor;
use crate::ast::expr::Expr;
use crate::ast::stmt::Statement;

use Statement::*;
use Expr::*;
use Value::*;

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

    pub fn interp(&mut self, statements: Vec<Statement>) -> Result<Option<Value>, Vec<InterpErr>> {
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

impl Visitor<Result<Value, InterpErr>> for Interpreter {
    fn visit_expr(&mut self, e: &Expr) -> Result<Value, InterpErr> {
        use crate::token::TokenType::*;
        match e {
            Call(callee, _,  args) => {
                // Evaluate fun to a function declaration
                let f = self.visit_expr(callee)?;

                // Evaluate body in new environment:
                let callable = match f {
                    ClosureV(c) => c,
                    _ => return Err(InterpErr {
                        msg: format!("Can only call functions and classes"),
                    }),
                };

                if callable.arity() != args.len() {
                    return Err(InterpErr {
                        msg: format!("Arity mismatch: {} params vs {} args", callable.arity(), args.len()),
                    });
                }

                let mut arguments = Vec::new();

                for arg in args {
                    arguments.push(self.visit_expr(arg)?);
                }

                callable.call(self, arguments)
            },
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
            ValueExpr(lit) => Ok(lit.clone()),
            Grouping(bx) => Ok(self.visit_expr(bx)?),
            Unary(tok, bx) => {
                let right = self.visit_expr(bx)?;
                match tok.token_type {
                    MINUS => match right {
                        Value::NumberV(n) => Ok(Value::NumberV(-n)),
                        _ => Err(InterpErr{
                            msg: format!("Value {:?} not allowed for unary minus", right)
                        }),
                    },
                    BANG => Ok(negate(is_truthy_lit(&right))),
                    _ => Err(InterpErr{
                        msg: format!("Token {:?} not allowed for unary expression", tok),
                    }),
                }
            },
            Binary(tok, left, right) => {
                // AND and OR operators short circuit, so handle them separately
                if tok.token_type == AND {
                    let left = &self.visit_expr(left)?;
                    if is_truthy(left) {
                        return Ok(self.visit_expr(right)?);
                    }

                    return Ok(left.clone());
                }

                if tok.token_type == OR {
                    let left = &self.visit_expr(left)?;
                    if is_truthy(left) {
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
                                    return Ok(TrueV)
                                }
                                return Ok(FalseV)
                            },
                            None => Err(InterpErr {
                                msg: format!("One of {:?}, {:?} was not a number", left, right),
                            }),
                        }
                    },
                    GREATER_EQUAL => {
                        match unwrap_num_lits(left, right) {
                            Some((l, r)) => {
                                if l >= r {
                                    return Ok(TrueV)
                                }
                                return Ok(FalseV)
                            },
                            None => Err(InterpErr {
                                msg: format!("One of {:#?}, {:#?} was not a number", left, right),
                            }),
                        }
                    },
                    LESS => {
                        match unwrap_num_lits(left, right) {
                            Some((l, r)) => {
                                if l < r {
                                    return Ok(TrueV)
                                }
                                return Ok(FalseV)
                            },
                            None => Err(InterpErr {
                                msg: format!("One of {:#?}, {:#?} was not a number", left, right),
                            }),
                        }
                    },
                    LESS_EQUAL => {
                        match unwrap_num_lits(left, right) {
                            Some((l, r)) => {
                                if l <= r {
                                    return Ok(TrueV)
                                }
                                return Ok(FalseV)
                            },
                            None => Err(InterpErr {
                                msg: format!("One of {:#?}, {:#?} was not a number", left, right),
                            }),
                        }
                    },
                    // Simple number ops
                    MINUS => {
                        match unwrap_num_lits(left, right) {
                            Some((l, r)) => Ok(NumberV(l - r)),
                            None => Err(InterpErr {
                                msg: format!("One of {:#?}, {:#?} was not a number", left, right),
                            }),
                        }
                    },
                    STAR => {
                        match unwrap_num_lits(left, right) {
                            Some((l, r)) => Ok(NumberV(l * r)),
                            None => Err(InterpErr {
                                msg: format!("One of {:#?}, {:#?} was not a number", left, right),
                            }),
                        }
                    },
                    SLASH => {
                        match unwrap_num_lits(left, right) {
                            Some((l, r)) => Ok(NumberV(l / r)),
                            None => Err(InterpErr {
                                msg: format!("One of {:#?}, {:#?} was not a number", left, right),
                            }),
                        }
                    },
                    PLUS => {
                        // Special case: overloaded strings, concat them both
                        match (left, right) {
                            (NumberV(a), NumberV(b)) => Ok(NumberV(a + b)),
                            (StringV(s), StringV(s2)) => {
                                let mut res = s.clone();
                                res.push_str(s2);
                                Ok(StringV(res))
                            },
                            _ =>  Err(InterpErr {
                                msg: format!("One of {:#?}, {:#?} was not a number", left, right),
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

    fn visit_stmt(&mut self, s: &Statement) -> Result<Value, InterpErr> {
        match s {
            Expression(e) => self.visit_expr(e),
            Print(e) => {
                println!("{}", self.visit_expr(e)?);
                Ok(NilV)
            },
            VarDec(tok, exp) => {
                let val = self.visit_expr(exp)?;
                self.environment.define(tok.lexeme.clone(), val);
                Ok(NilV)
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

                Ok(NilV)
            },
            IfStmt(cond_expr, then_expr, else_expr) => {
                match is_truthy_lit(&self.visit_expr(cond_expr)?) {
                    Value::TrueV => self.visit_stmt(then_expr),
                    Value::FalseV => match else_expr {
                        Some(boxed_else) => self.visit_stmt(boxed_else),
                        None => Ok(NilV)
                    },
                    _ => unreachable!(), // Impossible due to is_truthy_lit returning only lit t/f
                }
            },
            WhileStmt(cond_expr, stmt_body) => {
                while is_truthy(&self.visit_expr(cond_expr)?) {
                    self.visit_stmt(stmt_body)?;
                }

                Ok(NilV)
            },
        }
    }
}

fn are_equal(a: &Value, b: &Value) -> Value {
    match (a, b) {
        (StringV(ref s1), StringV(ref s2)) if s1 == s2 => Value::TrueV,
        (NumberV(n), NumberV(m)) if n == m => Value::TrueV,
        (TrueV, TrueV) => Value::TrueV,
        (FalseV, TrueV) => Value::TrueV,
        (NilV, NilV) => Value::TrueV,
        _ => Value::FalseV,
    }
}

fn unwrap_num_lits(a: &Value, b: &Value) -> Option<(f64, f64)> {
    match a {
        NumberV(an) => {
            match b {
                NumberV(bn) => Some((*an, *bn)),
                _ => None,
            }
        },
        _ => None,
    }
}

fn is_truthy(lit: &Value) -> bool {
    match is_truthy_lit(lit) {
        Value::TrueV => true,
        _ => false,
    }
}

fn is_truthy_lit(lit: &Value) -> Value {
    match lit {
        ClosureV(_) => Value::TrueV,
        StringV(_) => Value::TrueV,
        NumberV(_) => Value::TrueV,
        TrueV => Value::TrueV,
        FalseV => Value::FalseV,
        NilV => Value::FalseV,
    }
}

fn negate(lit: Value) -> Value {
    match lit {
        TrueV => Value::FalseV,
        FalseV => Value::TrueV,
        _ => panic!("not a boolean literal"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::expr::Expr::{ValueExpr, Unary, Binary, Grouping};
    use crate::value::Value;
    use crate::token::{Token, TokenType};
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
            Box::new(ValueExpr(NumberV(1.0))),
            Box::new(ValueExpr(NumberV(2.0)))
        ));

        assert!(val.is_ok());
        assert_eq!(val.unwrap(), Value::NumberV(3.0))
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
            Box::new(ValueExpr(StringV("as".to_string()))),
            Box::new(ValueExpr(StringV("df".to_string())))
        ));

        assert!(val.is_ok());
        assert_eq!(val.unwrap(), StringV("asdf".to_string()))
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
            Box::new(ValueExpr(TrueV)),
        ));

        assert!(val.is_ok());
        assert_eq!(val.unwrap(), Value::FalseV)
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
            Box::new(ValueExpr(NumberV(2.0)))
        ));

        assert!(val.is_ok());
        assert_eq!(val.unwrap(), Value::NumberV(-2.0));
    }

    #[test]
    fn test_var() {
        let mut interpreter = Interpreter{
            environment: Environment::new(None),
        };

        interpreter.environment.define("a".to_string(), NumberV(1.0));

        let val = interpreter.visit_expr(
            &Identifier("a".to_string()),
        );

        assert!(val.is_ok());
        assert_eq!(val.unwrap(), NumberV(1.0));
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
            Box::new(ValueExpr(NumberV(1.0))),
            Box::new(Grouping(Box::new(
                Unary(
                    Token {
                        token_type: MINUS,
                        lexeme: "-".to_string(),
                        literal: None,
                        line: 1,
                    },
                    Box::new(ValueExpr(NumberV(2.0)))
                )
            )))
        ));

        assert!(val.is_ok());
        assert_eq!(val.unwrap(), Value::NumberV(-1.0))
    }
}
