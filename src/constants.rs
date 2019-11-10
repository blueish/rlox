
use std::collections::HashMap;

use crate::token;
use token::TokenType::*;

lazy_static! {
    pub static ref KEYWORDS: HashMap<String, &'static token::TokenType> = {
        let mut hash = HashMap::new();
        let _keywords = [
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
        ]
            .iter()
            .map(|tup| (String::from(tup.0), &tup.1))
            .map(|e| hash.insert(e.0, e.1));

        hash
    };

}
