extern crate bytes;

pub mod state;
pub mod scanner;
pub mod parser;
pub mod vm;
pub mod config;
pub mod dump;
pub mod undump;
pub mod instruction;
pub mod ast;
pub mod compiler;
mod value;

use std::io;
use std::string;
use std::result;

const NO_JUMP: isize = -1;

#[derive(Debug)]
pub enum Error {
    IOError(io::Error),
    LexicalError(String),
    SyntaxError(String),
    Utf8Error,
    CompileError(String),
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