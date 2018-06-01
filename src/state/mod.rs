pub mod scanner;
pub mod parser;
pub mod opcode;
pub mod vm;
pub mod config;
pub mod dump;
pub mod undump;

use std::io::{Read, Cursor, BufReader};
use std::fs::File;
use std::io;
use std::result;

/// `SIGNATURE` is the mark for precompiled code ('<esc>Lua').
const SIGNATURE: &'static str = r"\033Lua";

/// A State is an opaque structure representing per thread Lua state.
#[derive(Debug)]
pub struct State {
}

#[derive(Debug)]
pub enum Error {
    IOError(io::Error)
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
        State{}
    }

    pub fn load_file(&mut self, path: &str)->Result<()> {
        let f = File::open(path)?;
        let reader = BufReader::new(f);
        self.load(reader)
    }

    pub fn load_string(&mut self, s: &str) -> Result<()> {
        let cursor = Cursor::new(s.as_bytes());
        let reader = BufReader::new(cursor);
        self.load(reader)
    }

    pub fn load<T: Read>(&mut self, reader: BufReader<T>) -> Result<()> {
        let parser = parser::Parser::new(reader);
        Ok(())
    }
}