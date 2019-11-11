
use std::collections::HashMap;

use crate::token;
use token::TokenType::*;

lazy_static! {
    pub static ref KEYWORDS: HashMap<&'static str, token::TokenType> = [
        ("and",    AND),
        ("class",  CLASS),
        ("else",   ELSE),
        ("false",  FALSE),
        ("for",    FOR),
        ("fun",    FUN),
        ("if",     IF),
        ("nil",    NIL),
        ("or",     OR),
        ("print",  PRINT),
        ("return", RETURN),
        ("super",  SUPER),
        ("this",   THIS),
        ("true",   TRUE),
        ("var",    VAR),
        ("while",  WHILE),
    ].iter().cloned().collect();
}
