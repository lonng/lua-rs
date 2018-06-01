use state::scanner::{Token, Scanner};
use state::State;
use std::io::{Read, BufReader};
use state::Result;
use state::Error;
use state::vm::Chunk;

#[derive(Debug)]
pub struct Parser<R> {
    scanner: Scanner<R>,
}

impl<R: Read> Parser<R> {
    pub fn new(reader: BufReader<R>) -> Parser<R> {
        Parser {
            scanner: Scanner::new(reader)
        }
    }

    pub fn protect_parse(&mut self) -> Result<Box<Chunk>> {
        loop {
            let token = self.scanner.next();
            match token {
                Ok(Token::EOF) => break,
                _ => println!("{:?}", token)
            }
        }

        Ok(Chunk::new())
    }
}