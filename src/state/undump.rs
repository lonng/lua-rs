use std::io::{Read, BufRead, BufReader};
use state::Result;
use state::vm::Chunk;

pub fn undump<T: Read>(reader: BufReader<T>) -> Result<Box<Chunk>> {
    Ok(Chunk::new())
}