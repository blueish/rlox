use std::fs;
use std::io;
use std::env;

use std::io::Write;

mod scanner;

fn main() {
    let args: Vec<String> = env::args().collect();
    let res = match args.len() {
        1 => run_prompt(),
        2 => run_file(args.get(1).unwrap().to_string()),
        _ => panic!("Usage: cargo run [script]"),
    };

    match res {
        Ok(()) => return,
        Err(e) => panic!(e),
    }
}

fn run_file(filename: String) -> Result<(), String> {
    let file = fs::read_to_string(filename);
    match file {
        Ok(s) => run(s),
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

        match run(command.to_string()) {
            Ok(()) => (),
            Err(e) => return Err(e)
        };
    }
}

fn run(input: String) -> Result<(), String> {
    let mut scanner: scanner::Scanner = scanner::Scanner::new(input);

    let tokens: Vec<String> = scanner.scan_tokens();

    for token in tokens{
        println!("{:?}", token);
    }

    Ok(())
}
