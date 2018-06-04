use scanner::{Scanner, Token};
use state::State;
use std::io::{BufReader, Read};
use vm::Chunk;
use code::{Kind, Function, ExprDesc};
use ::{Result, Error};

#[derive(Debug)]
pub struct Parser<'s, R> {
    state: &'s mut State,
    filename: String,
    scanner: Scanner<R>,
    function: Function,
    token: Token,
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
            token: Token::EOF,
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
            let token = self.scanner.next();
            match token {
                Ok(Token::EOF) => break,
                Ok(t) => println!("{:?}", t),
                Err(e) => println!("{:?}", e)
            }
        }

        Ok(Chunk::new())
    }

    fn next(&mut self) -> Result<()>{
        self.token = self.scanner.next()?;
        Ok(())
    }

    fn statement(&mut self) {

    }
}