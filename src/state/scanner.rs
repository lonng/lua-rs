use state::{Error, Result, State};
use std::io::{BufRead, BufReader, Read};
use bytes::{BufMut, BytesMut};

/// END_OF_STREAM indicates that scanner has reach the end of stream.
const END_OF_STREAM: u8 = 0xFF;
const EOF: char = END_OF_STREAM as char;

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
    Char(char),
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
    buffer: BytesMut,
}

impl<R: Read> Scanner<R> {
    pub fn new(reader: BufReader<R>) -> Scanner<R> {
        Scanner {
            current: '\0',
            reader,
            last_line: 1,
            line_number: 1,
            ahead_token: Token::EOF,
            buffer: BytesMut::new(),
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
        loop {
            println!("{:?}", self.current);
            match self.current {
                EOF => return Ok(Token::EOF),
                '\0' => self.advance(),
                '\r' | '\n' => self.incr_line_number(),
                ' ' | '\u{014}' /*form feed*/ | '\t' | '\u{013}'/*vertical tab character*/ => self.advance(),
                '-' => {
                    self.advance();
                    if self.current != '-' {
                        return Ok(Token::Char('-'));
                    }

                    self.advance();
                    if self.current == '[' {
                        let sep = self.skip_separator();
                        if sep >= 0 {
                            // TODO: read multiple line
                            break
                        }
                        self.buffer.clear();
                    }
                }
                _ => {}
            }
        }
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

    fn save(&mut self, c: char) {
        self.buffer.put(c as u8)
    }

    fn save_and_advance(&mut self) {
        let c = self.current;
        self.save(c);
        self.advance();
    }

    fn advance_and_save(&mut self) {
        self.advance();
        let c = self.current;
        self.save(c);
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

    // TODO is this the right name?
    fn skip_separator(&mut self) -> isize {
        let mut i: isize = 0;
        let c = self.current;
        debug_assert!(c == '[' || c == ']');
        loop {
            self.save_and_advance();
            if self.current != '=' {
                break;
            }
            i = i + 1;
        }
        if self.current == c {
            return i;
        }

        -i - 1
    }

    fn read_multi_line(&mut self, is_comment: bool, sep: isize) -> Result<String> {
        self.save_and_advance();
        if is_new_line(self.current) {
            self.incr_line_number();
        }
        loop {
            match self.current {
                EOF => {
                    if is_comment {
                        return Err(Error::LexicalError("unfinished long comment".to_string()));
                    }else {
                        return Err(Error::LexicalError("unfinished long string".to_string()));
                    }
                },

                ']' => {
                    let sep2 = self.skip_separator();
                    if sep == sep2 {
                        self.save_and_advance();
                        if !is_comment {
                            // TODO
                        }
                    }
                }

                _ => {}
            }
        }
        Ok(String::new())
    }
}