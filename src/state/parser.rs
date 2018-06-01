use state::scanner::Scanner;
use state::State;
use std::io::{Read, BufReader};
use state::Result;

#[derive(Debug)]
pub struct Chunk {
    prev: Option<Box<Chunk>>
}

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
        Ok(Box::new(Chunk{
            prev: None
        }))
    }
}