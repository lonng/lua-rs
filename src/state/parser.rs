use state::scanner::{Token, Scanner};
use state::State;
use std::io::{Read, BufReader};
use state::Result;
use state::Error;
use state::vm::Chunk;

#[derive(Debug)]
pub struct Parser<R> {
    filename: String,
    scanner: Scanner<R>,
}

impl<R: Read> Parser<R> {
    pub fn new(reader: BufReader<R>, name: String) -> Parser<R> {
        Parser {
            filename: name,
            scanner: Scanner::new(reader)
        }
    }

    pub fn protect_parse(&mut self) -> Result<Box<Chunk>> {
        loop {
            let token = self.scanner.next();
            match token {
                Ok(Token::EOF) => break,
                Ok(t) => println!("{:?}", t),
                Err(e) => println!("{:?}", e)
            }
        }

        Ok(Chunk::new())
    }
}