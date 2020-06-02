use crate::{Result};
use crate::compiler::FunctionProto;
use std::io::{BufReader, Read};

pub fn undump<T: Read>(_reader: BufReader<T>) -> Result<Box<FunctionProto>> {
    unimplemented!()
}