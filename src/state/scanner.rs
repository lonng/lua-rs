use state::State;
use std::io::{Read, BufRead,BufReader};

#[derive(Debug, Clone)]
pub enum Token {
    And,
    Break,
    Do,
    Else,
    Elseif,
    End,
    False,
    For,
    Function,
    Goto,
    If,
    In,
    Local,
    Nil,
    Not,
    Or,
    Repeat,
    Return,
    Then,
    True,
    Until,
    While,
    Concat,
    Dots,
    Eq,
    GE,
    LE,
    NE,
    DoubleColon,
    EOS,
    Number(f64),
    Name(String),
    String(String),
}

#[derive(Debug)]
pub struct Scanner<T> {
    reader: BufReader<T>,
    last_line: i32,
    line_number: i32,
    ahead_token: Token,
}

impl<T: Read> Scanner<T> {
    pub fn new(reader: BufReader<T>) -> Scanner<T> {
        Scanner {
            reader,
            last_line: 0,
            line_number: 0,
            ahead_token: Token::EOS,
        }
    }

    pub fn next(&mut self) -> Token {
        self.line_number = self.last_line;
        match self.ahead_token {
            Token::EOS => self.scan(),
            _ => {
                let ahead = self.ahead_token.clone();
                self.ahead_token = Token::EOS;
                ahead
            }
        }
    }

    fn scan(&mut self) -> Token {
        Token::EOS
    }
}