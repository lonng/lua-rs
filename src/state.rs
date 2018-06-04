use std::io::{Read, Cursor, BufRead, BufReader};
use std::fs::File;
use std::io;
use std::result;
use std::string;
use {undump, parser};

/// A State is an opaque structure representing per thread Lua state.
#[derive(Debug)]
pub struct State {}

#[derive(Debug)]
pub enum Error {
    IOError(io::Error),
    LexicalError(String),
    SyntaxError(String),
    Utf8Error,
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::IOError(e)
    }
}

impl From<string::FromUtf8Error> for Error {
    fn from(_: string::FromUtf8Error) -> Self {
        Error::Utf8Error
    }
}

pub type Result<T> = result::Result<T, Error>;

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
        self.load(reader, path.to_string())
    }

    pub fn load_string(&mut self, s: &str) -> Result<()> {
        let cursor = Cursor::new(s.as_bytes());
        let mut reader = BufReader::new(cursor);
        self.load(reader, s.to_string())
    }

    /// Load lua chunk from reader
    /// Signature `\033Lua` indicates precompiled lua bytecode
    pub fn load<T: Read>(&mut self, mut reader: BufReader<T>, name: String) -> Result<()> {
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
            let mut parser = parser::Parser::new(self, reader, name);
            parser.protect_parse()?
        };

        // TODO: save chunk

        Ok(())
    }
}