use scanner::{Scanner, Token};
use state::State;
use std::io::{BufReader, Read};
use vm::Chunk;
use code::{Kind, Function, ExprDesc};
use ::{Result, Error};
use std::mem;

#[derive(Debug)]
pub struct Parser<'s, R> {
    state: &'s mut State,
    filename: String,
    scanner: Scanner<R>,
    function: Function,
    line_number: i32,
    token: Token,
    ahead_token: Token,
    // entry function
    activity_vars: Vec<isize>,
    pending_gotos: Vec<isize>,
    active_labels: Vec<isize>,
}

impl<'s, R: Read> Parser<'s, R> {
    pub fn new(state: &mut State, reader: BufReader<R>, name: String) -> Parser<R> {
        Parser {
            state,
            filename: name,
            scanner: Scanner::new(reader),
            function: Function::new(),
            line_number: 0,
            token: Token::EOF,
            ahead_token: Token::EOF,
            activity_vars: Vec::new(),
            pending_gotos: Vec::new(),
            active_labels: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> Result<Box<Chunk>> {
        // main function
        self.function.enter_block(false,
                                  self.active_labels.len(),
                                  self.pending_gotos.len(),
                                  self.activity_vars.len());
        self.function.make_upval("_ENV", ExprDesc::new(Kind::Local(0)))?;

        // create closure

        // push to state stack
        loop {
            let token = self.next()?;
            match self.token {
                Token::EOF => break,
                _ => println!("{:?}", self.token),
            }
        }

        Ok(Chunk::new())
    }

    pub fn next(&mut self) -> Result<()>{
        self.line_number = self.scanner.line_number();
        self.token = match self.ahead_token {
            Token::EOF => self.scanner.scan()?,
            _ => {
                let mut ahead = Token::EOF;
                mem::swap(&mut ahead, &mut self.ahead_token);
                ahead
            }
        };
        Ok(())
    }

    pub fn look_ahead(&mut self) -> Result<()> {
        debug_assert!(&self.ahead_token == &Token::EOF);
        self.ahead_token = self.scanner.scan()?;
        Ok(())
    }

    fn statement(&mut self) {

    }
}