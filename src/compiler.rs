use ast::*;
use vm::Chunk;
use ::Result;

pub fn compile(stmts: Vec<StmtNode>, name: String) -> Result<Box<Chunk>> {
    Ok(Chunk::new())
}