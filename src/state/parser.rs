use state::scanner::Scanner;
use state::State;

#[derive(Debug)]
pub struct Parser {
    scanner: Scanner,
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            scanner: Scanner::new()
        }
    }
}