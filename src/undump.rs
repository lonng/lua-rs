#![allow(unused_variables)]

use ::Result;
use compiler::FunctionProto;
use std::io::{BufReader, Read};

pub fn undump<T: Read>(reader: BufReader<T>) -> Result<Box<FunctionProto>> {
    unimplemented!()
}
