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
    nc_calls: i32,

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
            nc_calls: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Box<Chunk>> {
        // main function
        self.next()?;
        self.statement_list()?;
        self.check(Token::EOF)?;
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

    fn enterlevel(&mut self) {
        self.nc_calls = self.nc_calls + 1;
    }

    fn leavelevel(&mut self) {
        self.nc_calls = self.nc_calls - 1;
    }

    fn block_follow(&self, with_until: bool) -> bool {
        match self.token {
            Token::Else | Token::Elseif | Token::End | Token::EOF => true,
            Token::Until => with_until,
            _ => false,
        }
    }

    fn statement_list(&mut self) -> Result<()> {
        loop {
            if self.block_follow(true) {
                break;
            }

            self.statement()?;
            if self.token == Token::Return {
                break;
            }
        }
        return Ok(());
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

    fn statement(&mut self) -> Result<()> {
        let line = self.line_number;
        self.enterlevel();
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
            Token::DoubleColon => {
                self.next()?;
                self.lebalstat()?;
            },
            Token::Return => {
                self.next()?;
                self.retstat()?;
            },
            Token::Break | Token::Goto => self.gotostat()?,
            Token::Char(char) if char == ';' => self.next()?,
            _ => self.exprstat()?
        }
        // TODO: assert
        //self.function.free_register_count  = self.function.active_var_count;
        self.enterlevel();
        Ok(())
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
    fn lebalstat(&mut self) -> Result<()> { unimplemented!() }
    fn gotostat(&mut self) -> Result<()> {
        unimplemented!();
        Ok(())
    }

    fn retstat(&mut self) -> Result<()> {
        unimplemented!();
        Ok(())
    }

    fn block(&mut self) -> Result<()> { unimplemented!() }

    fn check(&self, expect: Token) -> Result<()> {
        if self.token != expect {
            return Err(Error::SyntaxError(format!("{}:{}: {} expected",
                                                  self.filename, self.line_number, expect.to_string())));
        }
        Ok(())
    }

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