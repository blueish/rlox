use std::fs;
use std::io;
use std::env;

use std::io::Write;

mod scanner;
mod token;
mod errors;

fn main() {
    let args: Vec<String> = env::args().collect();

    let res = match args.len() {
        1 => run_prompt(),
        2 => run_file(args.get(1).unwrap().to_string()),
        _ => panic!("Usage: cargo run [script]"),
    };

    match res {
        Ok(()) => return,
        Err(_) => panic!("Ran with errors"),
    }
}

fn run_file(filename: String) -> Result<(), String> {
    let file = fs::read_to_string(filename);

    let error_reporter: &mut errors::ErrorReporter = &mut errors::ErrorReporter{ had_errors: false };

    match file {
        Ok(s) => run(s, error_reporter),
        Err(e) => Err(e.to_string()),
    }
}

fn run_prompt() -> Result<(), String> {
    loop {
        print!("lox > ");
        io::stdout().flush().unwrap();

        let mut command = String::new();

        io::stdin()
            .read_line(&mut command)
            .expect("Failed to read line");

        let command = command.trim();

        let error_reporter: &mut errors::ErrorReporter = &mut errors::ErrorReporter{ had_errors: false };

        match run(command.to_string(), error_reporter) {
            Ok(()) => (),
            Err(e) => return Err(e)
        };
    }
}

fn run(input: String, error_reporter: &mut errors::ErrorReporter) -> Result<(), String> {
    let mut scanner: scanner::Scanner = scanner::Scanner::new(&input, error_reporter);

    let had_errors = scanner.scan_tokens();
    if had_errors {
        return Err(String::from("Had errors parsing."));
    }

    let tokens: Vec<token::Token> = scanner.tokens();

    for token in tokens{
        println!("{:?}", token);
    }

    Ok(())
}
