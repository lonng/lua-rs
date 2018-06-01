use state::{Error, Result, State};
use std::io::{BufRead, BufReader, Read};

/// END_OF_STREAM indicates that scanner has reach the end of stream.
const END_OF_STREAM: u8 = 0xFF;

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
    EOF,
    Number(f64),
    Name(String),
    String(String),
}

fn is_new_line(byte: char) -> bool {
    byte == '\r' || byte == '\n'
}

#[derive(Debug)]
pub struct Scanner<R> {
    current: char,
    reader: BufReader<R>,
    last_line: i32,
    line_number: i32,
    ahead_token: Token,
}

impl<R: Read> Scanner<R> {
    pub fn new(reader: BufReader<R>) -> Scanner<R> {
        Scanner {
            current: '\0',
            reader,
            last_line: 1,
            line_number: 1,
            ahead_token: Token::EOF,
        }
    }

    pub fn next(&mut self) -> Result<Token> {
        self.line_number = self.last_line;
        match self.ahead_token {
            Token::EOF => self.scan(),
            _ => {
                let ahead = self.ahead_token.clone();
                self.ahead_token = Token::EOF;
                Ok(ahead)
            }
        }
    }

    fn scan(&mut self) -> Result<Token> {
        match self.current {
            '\0' => self.advance(),
            '\r' | '\n' => self.incr_line_number(),
            _ => {}
        }
        println!("{:?}", self.current);
        println!("{:?}", END_OF_STREAM);
        Ok(Token::EOF)
    }

    fn advance(&mut self) {
        let byte: u8 = match self.reader.fill_buf() {
            Ok(ref buf) if buf.len() > 0 => buf[0],
            _ => END_OF_STREAM
        };

        if byte != END_OF_STREAM {
            self.reader.consume(1)
        }

        self.current = byte as char;
    }

    fn incr_line_number(&mut self) {
        let old = self.current;
        debug_assert!(is_new_line(old));

        self.advance();
        if is_new_line(self.current) && self.current != old {
            self.advance();
        }

        self.line_number = self.line_number + 1;
        // TODO: check lines too many?
    }
}