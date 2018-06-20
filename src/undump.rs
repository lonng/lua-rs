use ::{Error, Result};
use compiler::FunctionProto;
use std::io::{BufRead, BufReader, Read};

pub fn undump<T: Read>(reader: BufReader<T>) -> Result<Box<FunctionProto>> {
    unimplemented!()
}