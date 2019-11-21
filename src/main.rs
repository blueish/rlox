#[macro_use]
extern crate lazy_static;
extern crate log;

use log::{info, error, debug};
use env_logger::Env;

use std::fs;
use std::io;
use std::env;

use std::io::Write;
use std::time::{Instant};

mod ast;
mod constants;
mod scanner;
mod token;
mod errors;
mod parser;
mod interp;
mod value;
mod callable;

fn main() {
    env_logger::from_env(Env::default()
                         // .default_filter_or("info")
                         .default_filter_or("debug")
    ).init();

    let args: Vec<String> = env::args().collect();

    let res = match args.len() {
        1 => run_prompt(),
        2 => run_file(args.get(1).unwrap().to_string()),
        _ => panic!("Usage: cargo run [script]"),
    };

    match res {
        Ok(()) => return,
        Err(e) => {
            error!("Error executing lox code: {}", e);
            panic!("Ran with errors");
        },
    }
}

fn run_file(filename: String) -> Result<(), String> {
    let file = fs::read_to_string(filename);

    let interpreter = &mut interp::interpreter::Interpreter::new();
    let error_reporter: &mut errors::ErrorReporter = &mut errors::ErrorReporter{ had_errors: false };

    match file {
        Ok(s) => {
            match run(s, interpreter,  error_reporter) {
                Ok(_) => Ok(()),
                Err(e) => Err(e.to_string()),
            }
        },
        Err(e) => Err(e.to_string()),
    }
}

fn run_prompt() -> Result<(), String> {
    let interpreter = &mut interp::interpreter::Interpreter::new();
    loop {
        print!("lox > ");
        io::stdout().flush().unwrap();

        let mut command = String::new();

        io::stdin()
            .read_line(&mut command)
            .expect("Failed to read line");

        let command = command.trim();

        let error_reporter: &mut errors::ErrorReporter = &mut errors::ErrorReporter{ had_errors: false };

        match run(command.to_string(), interpreter, error_reporter) {
            Ok(result) => println!("{:?}", result),
            Err(e) => {
                println!("Error executing lox code: {}", e);
            }
        };
    }
}

fn run(input: String, interpreter: &mut interp::interpreter::Interpreter, error_reporter: &mut errors::ErrorReporter) -> Result<Option<value::Value>, String> {
    let mut time = Instant::now();
    let mut scanner: scanner::Scanner = scanner::Scanner::new(&input, error_reporter);

    let had_errors = scanner.scan_tokens();
    info!("Scanning finished in {}micros", time.elapsed().as_micros());
    if had_errors {
        return Err(String::from("Had errors parsing."));
    }

    let tokens: Vec<token::Token> = scanner.tokens();

    time = Instant::now();
    let statements = parser::Parser::parse(&tokens, error_reporter);
    info!("Parsing finished in {}micros", time.elapsed().as_micros());

    debug!("ast: {}", ast::printer::PrettyPrinter::print_ast(&statements));

    let mut errs = Vec::new();
    let mut valid_statements = Vec::new();

    for stmt in statements {
        if stmt.is_err() {
            errs.push(stmt.unwrap_err());
        } else {
            valid_statements.push(stmt.unwrap());
        }
    }

    if errs.len() > 0 {
        return Err(format!("Could not parse all statements: {:?}", errs));
    }

    time = Instant::now();
    let res = interpreter.interp(valid_statements);
    info!("Interp finished in {}micros", time.elapsed().as_micros());

    match res {
        Ok(e) => {
            Ok(e)
        },
        Err(e) => Err(format!("Interp errors: {:?}", e)),
    }
}
