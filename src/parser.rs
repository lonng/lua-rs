use scanner::{Scanner, Token};
use state::State;
use std::io::{BufReader, Read};
use vm::Chunk;
use code::{Kind, Function, ExprDesc};
use ::{Result, Error, NO_JUMP};
use std::mem;

#[derive(Debug)]
pub struct Parser<'s, R> {
    state: &'s mut State,
    filename: String,
    scanner: Scanner<R>,
    line_number: i32,
    token: Token,
    ahead_token: Token,

    // entry function
    function: Function,
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
        self.next()?;
        let mut is_last = false;
        loop {
            if is_last || self.block_follow(&self.token, true) {
                break;
            }
            is_last = self.statement()?;
            // TODO: assert max stack size
        }
        Ok(Chunk::new())
    }

    fn next(&mut self) -> Result<()> {
        self.line_number = self.scanner.line_number();
        self.token = match self.ahead_token {
            Token::EOF => self.scanner.scan()?,
            _ => {
                let mut ahead = Token::EOF;
                mem::swap(&mut ahead, &mut self.ahead_token);
                ahead
            }
        };
        println!("next token => {:?}", self.token);
        Ok(())
    }

    fn block_follow(&self, t: &Token, with_until: bool) -> bool {
        match *t {
            Token::Else | Token::Elseif | Token::End | Token::EOF => true,
            Token::Until => with_until,
            _ => false,
        }
    }

    fn look_ahead(&mut self) -> Result<()> {
        debug_assert!(&self.ahead_token == &Token::EOF);
        self.ahead_token = self.scanner.scan()?;
        Ok(())
    }

    fn testnext(&mut self, t: &Token) -> Result<bool> {
        if self.token == *t {
            self.next()?;
            return Ok(true);
        }
        Ok(false)
    }

    fn statement(&mut self) -> Result<bool> {
        let line = self.line_number;

        // must be last statement
        if self.token == Token::Return {
            self.retstat()?;
            return Ok(true);
        } else if self.token == Token::Break {
            self.breakstat()?;
            return Ok(true);
        }

        // is not last statement
        match self.token {
            Token::If => self.ifstat(line)?,
            Token::While => self.whilestat(line)?,
            Token::Do => {
                self.next()?;
                self.block()?;
                self.check_match(Token::End, Token::Do, line)?
            }
            Token::For => self.forstat(line)?,
            Token::Repeat => self.repeatstat(line)?,
            Token::Function => self.funcstat(line)?,
            Token::Local => {
                self.next()?;
                if self.testnext(&Token::Function)? {
                    self.localfunc()?;
                } else {
                    self.localstat()?;
                }
            }
            Token::Char(char) if char == ';' => self.next()?,
            _ => self.exprstat()?
        }

        Ok(false)
    }

    fn test_then_block(&mut self, escapes: isize) -> Result<isize> {
        let mut jump_false = 0;
        self.next()?;
        //let mut e = self.expression()?;
        Ok(escapes)
    }

    /// ifstat -> IF cond THEN block {ELSEIF cond THEN block} [ELSE block] END
    fn ifstat(&mut self, line: i32) -> Result<()> {
        let mut escapes = self.test_then_block(NO_JUMP)?;
        loop {
            if self.token != Token::Elseif {
                break;
            }
            escapes = self.test_then_block(escapes)?;
        }
        if self.testnext(&Token::Else)? {
            self.block()?;
        }
        self.check_match(Token::End, Token::If, line)?;
        self.function.patchtohere();
        Ok(())
    }

    fn whilestat(&mut self, line: i32) -> Result<()> { unimplemented!() }
    fn forstat(&mut self, line: i32) -> Result<()> { unimplemented!() }
    fn repeatstat(&mut self, line: i32) -> Result<()> { unimplemented!() }
    fn funcstat(&mut self, line: i32) -> Result<()> { unimplemented!() }
    fn localfunc(&mut self) -> Result<()> { unimplemented!() }
    fn localstat(&mut self) -> Result<()> { unimplemented!() }
    fn exprstat(&mut self) -> Result<()> { unimplemented!() }
    fn breakstat(&mut self) -> Result<()> {
        self.next()?;
        unimplemented!();
        Ok(())
    }

    fn retstat(&mut self) -> Result<()> {
        self.next()?;
        unimplemented!();
        Ok(())
    }

    fn block(&mut self) -> Result<()> { unimplemented!() }

    fn check_match(&mut self, what: Token, who: Token, line: i32) -> Result<()> {
        if !self.testnext(&what)? {
            let err = if line == self.line_number {
                format!("{}:{}: {} expected", self.filename, self.line_number, what.to_string())
            } else {
                format!("{}:{}: {} expected (to close {} at line {}", self.filename, self.line_number,
                        what.to_string(), who.to_string(), line)
            };
            Err(Error::SyntaxError(err))
        } else {
            Ok(())
        }
    }
}