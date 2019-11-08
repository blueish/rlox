pub struct Scanner {
    source: String,
}

impl Scanner {
    pub fn new(source: String) -> Scanner {
        Scanner {
            source,
        }
    }
    pub fn scan_tokens(&mut self) -> Vec<String> {
        vec!(String::from("noop"))
    }
}
