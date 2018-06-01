
#[derive(Debug)]
pub struct Chunk {
    prev: Option<Box<Chunk>>
}

impl Chunk {
    pub fn new() -> Box<Chunk> {
        Box::new(Chunk{
            prev: None
        })
    }
}