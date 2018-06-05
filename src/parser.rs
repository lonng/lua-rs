use scanner::{Scanner, Token};
use state::State;
use std::io::{BufReader, Read};
use vm::Chunk;
use code::{Kind, Function, ExprDesc};
use ::{Result, Error, NO_JUMP};
use std::mem;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Opr {
    /*binary*/
    Add = 0,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Concat,
    Eq,
    LT,
    LE,
    NE,
    GT,
    GE,
    And,
    Or,
    NoBinary,
    /*unary*/
    Minus,
    Not,
    Length,
    NoUnary,
}

fn unary_op(t: &Token) -> Opr {
    match *t {
        Token::Not => Opr::Not,
        Token::Char(c) if c == '-' => Opr::Minus,
        Token::Char(c) if c == '#' => Opr::Length,
        _ => Opr::NoUnary
    }
}

fn binary_op(t: &Token) -> Opr {
    match *t {
        Token::Char(c) if c == '+' => Opr::Add,
        Token::Char(c) if c == '-' => Opr::Sub,
        Token::Char(c) if c == '*' => Opr::Mul,
        Token::Char(c) if c == '/' => Opr::Div,
        Token::Char(c) if c == '%' => Opr::Mod,
        Token::Char(c) if c == '^' => Opr::Pow,
        Token::Concat => Opr::Concat,
        Token::NE => Opr::NE,
        Token::Eq => Opr::Eq,
        Token::Char(c) if c == '<' => Opr::LT,
        Token::LE => Opr::LE,
        Token::Char(c) if c == '>' => Opr::GT,
        Token::GE => Opr::GE,
        Token::And => Opr::And,
        Token::Or => Opr::Or,
        _ => Opr::NoBinary,
    }
}

static BINARY_PRIORITY: &'static [(u8, u8)/*(left, right)*/; 15] = &[
    (6, 6), (6, 6), (7, 7), (7, 7), (7, 7), // `+' `-' `*' `/' `%'
    (10, 9), (5, 4), // ^, .. (right associative)
    (3, 3), (3, 3), (3, 3), // ==, <, <=
    (3, 3), (3, 3), (3, 3), // ~=, >, >=
    (2, 2), (1, 1), // and, or
];

const UNARY_PRIORITY: u8 = 8;

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

    fn look_ahead(&mut self) -> Result<()> {
        debug_assert!(&self.ahead_token == &Token::EOF);
        self.ahead_token = self.scanner.scan()?;
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
            }
            Token::Return => {
                self.next()?;
                self.retstat()?;
            }
            Token::Break | Token::Goto => self.gotostat()?,
            Token::Char(char) if char == ';' => self.next()?,
            _ => self.exprstat()?
        }
        // TODO: assert
        //self.function.free_register_count  = self.function.active_var_count;
        self.enterlevel();
        Ok(())
    }

    fn simple_expr(&mut self) -> Result<ExprDesc> {
        let e = match self.token {
            Token::Number(n) => ExprDesc::new(Kind::Number(n)),
            Token::String(s) =>
        };
        self.next()?;
        Ok(e)
    }

    fn sub_expression(&mut self, limit: u8) -> Result<(ExprDesc, Opr)> {
        self.enterlevel();
        let op = unary_op(&self.token);
        let mut expr = if op != Opr::NoUnary {
            let line = self.line_number;
            self.next()?;
            let sub = self.sub_expression(UNARY_PRIORITY)?.0;
            self.function.prefix(op, &sub, line)
        } else {
            self.simple_expr()?
        };

        let mut op = binary_op(&self.token);
        loop {
            if op == Opr::NoBinary || BINARY_PRIORITY[op as usize].1 <= limit {
                break;
            }
            let line = self.line_number;
            self.next()?;
            let infix = self.function.infix(op, &expr);
            let sub = self.sub_expression(BINARY_PRIORITY[op as usize].0)?;
            expr = self.function.postfix(op, &infix, &sub.0, line);
            op = sub.1
        }
        self.leavelevel();
        Ok((expr, op))
    }

    fn expression(&mut self) -> Result<ExprDesc> {
        let r = self.sub_expression(0)?;
        Ok(r.0)
    }

    fn test_then_block(&mut self, escapes: isize) -> Result<isize> {
        let mut jump_false = 0;
        self.next()?;
        let mut e = self.expression()?;
        self.check_next(Token::Then)?;


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

    fn check_next(&mut self, expect: Token) -> Result<()> {
        self.check(expect)?;
        self.next()
    }

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