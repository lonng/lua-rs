pub mod scanner;
pub mod parser;
pub mod opcode;
pub mod vm;
pub mod config;
pub mod dump;
pub mod undump;

use std::io::{Read, Cursor, BufRead, BufReader};
use std::fs::File;
use std::io;
use std::result;

/// A State is an opaque structure representing per thread Lua state.
#[derive(Debug)]
pub struct State {}

#[derive(Debug)]
pub enum Error {
    IOError(io::Error),
    LexicalError(String),
    SyntaxError(String),
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::IOError(e)
    }
}

type Result<T> = result::Result<T, Error>;

impl State {
    /// Creates a new thread running in a new, independent state.
    ///
    /// # Example
    ///
    /// ```
    /// use lua::state::State;
    ///
    /// let state = State::new();
    /// ```
    pub fn new() -> State {
        State {}
    }

    pub fn load_file(&mut self, path: &str) -> Result<()> {
        let f = File::open(path)?;
        let mut reader = BufReader::new(f);
        self.load(reader)
    }

    pub fn load_string(&mut self, s: &str) -> Result<()> {
        let cursor = Cursor::new(s.as_bytes());
        let mut reader = BufReader::new(cursor);
        self.load(reader)
    }

    /// Load lua chunk from reader
    /// Signature `\033Lua` indicates precompiled lua bytecode
    pub fn load<T: Read>(&mut self, mut reader: BufReader<T>) -> Result<()> {
        let mut magic: u8 = 0;
        {
            let buf = reader.fill_buf()?;
            if buf.len() > 0 {
                magic = buf[0]
            }
        }
        let chunk = if magic == 033 {
            undump::undump(reader)?
        } else {
            let mut parser = parser::Parser::new(reader);
            parser.protect_parse()?
        };

        // TODO: save chunk

        Ok(())
    }
}