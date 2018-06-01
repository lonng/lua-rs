use state::scanner::{Token, Scanner};
use state::State;
use std::io::{Read, BufReader};
use state::Result;
use state::Error;
use state::vm::Chunk;

#[derive(Debug)]
pub struct Parser<T> {
    scanner: Scanner<T>,
}

impl<T: Read> Parser<T> {
    pub fn new(reader: BufReader<T>) -> Parser<T> {
        Parser {
            scanner: Scanner::new(reader)
        }
    }

    pub fn protect_parse(&mut self) -> Result<Box<Chunk>> {
        loop {
            let token = self.scanner.next();
            match token {
                Token::EOS => break,
                _ => println!("{:?}", token)
            }
        }

        Ok(Chunk::new())
    }
}