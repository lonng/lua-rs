use state::{Error, Result, State};
use std::io::{BufRead, BufReader, Read};
use bytes::{BufMut, BytesMut};

/// END_OF_STREAM indicates that scanner has reach the end of stream.
const END_OF_STREAM: u8 = 0xFF;
const EOF: char = END_OF_STREAM as char;
const INIT: char = 0x0 as char;

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

impl ToString for Token {
    fn to_string(&self) -> String {
        match *self {
            Token::Number(ff) => format!("{}", ff),
            Token::Name(ref s) => s.clone(),
            Token::String(ref s) => s.clone(),
            Token::Char(ref c) => c.to_string(),
            _ => {
                let s = match *self {
                    Token::And => "and",
                    Token::Break => "break",
                    Token::Do => "do",
                    Token::Else => "else",
                    Token::Elseif => "elseif",
                    Token::End => "end",
                    Token::False => "false",
                    Token::For => "for",
                    Token::Function => "function",
                    Token::Goto => "goto",
                    Token::If => "if",
                    Token::In => "in",
                    Token::Local => "local",
                    Token::Nil => "nil",
                    Token::Not => "not",
                    Token::Or => "or",
                    Token::Repeat => "repeat",
                    Token::Return => "return",
                    Token::Then => "then",
                    Token::True => "true",
                    Token::Until => "until",
                    Token::While => "while",
                    Token::Concat => "..",
                    Token::Dots => "...",
                    Token::Eq => "==",
                    Token::GE => ">=",
                    Token::LE => "<=",
                    Token::NE => "~=",
                    Token::DoubleColon => "::",
                    Token::EOF => "<eof>",
                    _ => unreachable!()
                };
                s.to_string()
            }
        }
    }
}

fn is_new_line(c: char) -> bool {
    c == '\r' || c == '\n'
}

fn is_decimal(c: char) -> bool {
    '0' <= c && c <= '9'
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
            current: INIT,
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
                INIT => self.advance(),
                ' ' => self.advance(),
                '\t' => self.advance(),
                '\u{013}' => self.advance(),/*vertical tab character*/
                '\u{014}' => self.advance(),/*form feed*/
                '\r' | '\n' => self.incr_line_number(),
                '-' => {
                    self.advance();
                    if self.current != '-' {
                        return Ok(Token::Char('-'));
                    }

                    self.advance();
                    if self.current == '[' {
                        let sep = self.skip_separator();
                        if sep >= 0 {
                            self.read_multi_line(true, sep);
                            break
                        }
                        self.buffer.clear();
                    }

                    if is_new_line(self.current) && self.current != EOF {
                        self.advance();
                    }
                }
                '[' => {
                    let sep = self.skip_separator();
                    if sep >= 0 {
                        return Ok(Token::String(self.read_multi_line(false, sep)?));
                    }
                    self.buffer.clear();
                    if sep == -1 {
                        return Ok(Token::Char('['));
                    }
                    return Err(Error::LexicalError("invalid long string delimiter".to_string()));
                }
                '=' => {
                    self.advance();
                    if self.current == '=' {
                        return Ok(Token::Char('='));
                    }
                    self.advance();
                    return Ok(Token::Eq);
                }
                '<' => {
                    self.advance();
                    if self.current == '=' {
                        return Ok(Token::Char('<'));
                    }
                    self.advance();
                    return Ok(Token::LE);
                }
                '>' => {
                    self.advance();
                    if self.current == '=' {
                        return Ok(Token::Char('>'));
                    }
                    self.advance();
                    return Ok(Token::GE);
                }
                '~' => {
                    self.advance();
                    if self.current == '=' {
                        return Ok(Token::Char('~'));
                    }
                    self.advance();
                    return Ok(Token::NE);
                }
                ':' => {
                    self.advance();
                    if self.current == ':' {
                        return Ok(Token::Char(':'));
                    }
                    self.advance();
                    return Ok(Token::DoubleColon);
                }
                '"' | '\'' => return self.read_string(),
                '.' => unimplemented!(),
                _ => {
                    let c = self.current;
                    if c.is_digit(10) {
                        return self.read_number();
                    }else if c == '_' || c.is_alphabetic() {
                        loop {
                            self.save_and_advance();
                            if !self.current.is_alphanumeric(){
                                break;
                            }
                        }
                        return self.reserved_or_name();
                    }
                    self.advance();
                    return Ok(Token::Char(c));
                }
            }
        }
        unreachable!()
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

    fn advance_and_save(&mut self, c: char) {
        self.advance();
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
                    } else {
                        return Err(Error::LexicalError("unfinished long string".to_string()));
                    }
                }
                ']' => {
                    let sep2 = self.skip_separator();
                    let mut ret = String::new();
                    if sep == sep2 {
                        self.save_and_advance();
                        if !is_comment {
                            let buf_len = self.buffer.len();
                            ret = String::from_utf8(self.buffer[(2 + sep) as usize..(buf_len - 2)].to_vec())?;
                        }
                        self.buffer.clear();
                        return Ok(ret);
                    }
                }
                '\r' => self.current = '\n',
                '\n' => {
                    let current = self.current;
                    self.save(current);
                    self.incr_line_number();
                }

                _ => {
                    if !is_comment {
                        let current = self.current;
                        self.save(current);
                    }
                    self.advance();
                }
            }
        }
        Ok(String::new())
    }

    fn read_string(&mut self) -> Result<Token> {
        let delimiter = self.current;
        self.save_and_advance();
        loop {
            if self.current == delimiter {
                break
            }

            match self.current {
                EOF | '\n' | '\r' => return Err(Error::LexicalError("unfinished string".to_string())),
                '\\' => {
                    self.advance();
                    let current = self.current;
                    match current {
                        /// Escape charactors
                        /// \a   U+0007 alert or bell
                        /// \b   U+0008 backspace
                        /// \f   U+000C form feed
                        /// \n   U+000A line feed or newline
                        /// \r   U+000D carriage return
                        /// \t   U+0009 horizontal tab
                        /// \v   U+000b vertical tab
                        /// \\   U+005c backslash
                        /// \'   U+0027 single quote  (valid escape only within rune literals)
                        /// \"   U+0022 double quote  (valid escape only within string literals)
                        'a' => self.advance_and_save('\u{0007}'),
                        'b' => self.advance_and_save('\u{0008}'),
                        'f' => self.advance_and_save('\u{000C}'),
                        'n' => self.advance_and_save('\u{000A}'),
                        'r' => self.advance_and_save('\u{000D}'),
                        't' => self.advance_and_save('\u{0009}'),
                        'v' => self.advance_and_save('\u{000b}'),
                        '\\' => self.advance_and_save('\u{005c}'),
                        '\'' => self.advance_and_save('\u{0027}'),
                        '"' => self.advance_and_save('\u{0022}'),
                        _ if current == EOF => {} // do nothing
                        _ if is_new_line(current) => {
                            self.incr_line_number();
                            self.save('\n');
                        }
                        _ if current == 'x' => {
                            let hex_esc = self.read_hex_escape()?;
                            self.save(hex_esc);
                        }
                        _ if current == 'z' => {
                            self.advance();
                            loop {
                                let c = self.current;
                                if is_new_line(c) {
                                    self.incr_line_number();
                                } else {
                                    self.advance();
                                }
                                if !(c as char).is_whitespace() {
                                    break;
                                }
                            }
                        }
                        _ => {
                            if !is_decimal(current) {
                                return Err(Error::LexicalError("invalid escape sequence".to_string()));
                            }
                            let dec_esc = self.read_decimal_escape()?;
                            self.save(dec_esc);
                        }
                    }
                }
                _ => {
                    self.save_and_advance();
                }
            }
        }

        self.save_and_advance();
        let mut ret = String::new();
        let buf_len = self.buffer.len();
        if buf_len > 0 {
            ret = String::from_utf8(self.buffer[..].to_vec())?;
        }
        self.buffer.clear();
        return Ok(Token::String(ret));
    }

    fn read_hex_escape(&mut self) -> Result<char> {
        unimplemented!()
    }
    fn read_decimal_escape(&mut self) -> Result<char> {
        unimplemented!()
    }

    fn read_number(&mut self) -> Result<Token> {
        unimplemented!()
    }

    fn reserved_or_name(&mut self) -> Result<Token> {
        unimplemented!()
    }
}