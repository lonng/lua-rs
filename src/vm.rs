use ast::StmtNode;


#[derive(Debug)]
pub struct Chunk {
    stmts: Vec<StmtNode>
}

impl Chunk {
    pub fn new() -> Box<Chunk> {
        Box::new(Chunk{
            stmts: vec![]
        })
    }
}