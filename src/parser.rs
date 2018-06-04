use scanner::{Token, Scanner};
use state::State;
use std::io::{Read, BufReader};
use state::Result;
use state::Error;
use vm::Chunk;

#[derive(Debug)]
pub struct Parser<'s, R> {
    state: &'s mut State,
    filename: String,
    scanner: Scanner<R>,
}

impl<'s, R: Read> Parser<'s, R> {
    pub fn new(state: &mut State, reader: BufReader<R>, name: String) -> Parser<R> {
        Parser {
            state,
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