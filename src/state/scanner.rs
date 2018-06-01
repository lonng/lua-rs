use state::State;
use std::io::{Read, BufRead,BufReader};

#[derive(Debug)]
enum Token {
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
}

impl<T: Read> Scanner<T> {
    pub fn new(reader: BufReader<T>) -> Scanner<T> {
        Scanner {
            reader
        }
    }

    pub fn peek(&mut self) -> Option<u8> {
        match self.reader.fill_buf() {
            Ok(ref buf) if buf.len() > 0 => Some(buf[0]),
            _ => None
        }
    }
}