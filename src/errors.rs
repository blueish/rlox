pub struct ErrorReporter {
    pub had_errors: bool,
}

impl ErrorReporter {
    pub fn report(&mut self, line: usize, wher: String, message: String) {
        println!("[line {}] Error {}: {}", line, wher, message);
        self.had_errors = true;
    }
}
