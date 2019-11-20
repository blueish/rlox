#[cfg(test)]
mod integration {
    use std::fs;
    use std::process::Command;

    const TEST_FILE_DIR: &str = "tests/data";

    #[test]
    fn run_hello() {
        run("hello")
    }

    #[test]
    fn run_vars() {
        run("vars")
    }

    #[test]
    fn run_block() {
        run("block")
    }

    #[test]
    fn run_if() {
        run("if")
    }

    #[test]
    fn run_short_circuit() {
        run("short_circuit")
    }

    #[test]
    fn run_while() {
        run("while")
    }

    #[test]
    fn run_for() {
        run("for")
    }

    #[test]
    fn run_fib() {
        run("fib")
    }

    fn run(file_prefix: &str) {
        assert_eq!(run_file(file_prefix), file_contents(file_prefix));
    }

    fn run_file(file_prefix: &str) -> String {
        String::from_utf8(
            Command::new("./target/debug/interpreter")
                .env("RUST_LOG", "error")
                .arg(format!("{}/{}.lox", TEST_FILE_DIR, file_prefix))
                .output()
                .expect("failed to execute process")
                .stdout
        ).unwrap()
    }

    fn file_contents(file_prefix: &str) -> String {
        fs::read_to_string(format!("{}/{}.out", TEST_FILE_DIR, file_prefix))
            .unwrap()
    }
}
