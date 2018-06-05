pub trait Node {
    fn line(&self) -> isize;
    fn set_line(&mut self, line: isize);
    fn last_line(&self) -> isize;
    fn set_last_line(&mut self, line: isize);
}

pub struct Position {
    line: isize,
    last_line: isize,
}

impl Node for Position {
    fn line(&self) -> isize { self.line }
    fn set_line(&mut self, line: isize) { self.line = line }
    fn last_line(&self) -> isize { self.last_line }
    fn set_last_line(&mut self, line: isize) { self.last_line = line }
}

pub trait Localizable {
    fn position(&self) -> &mut Position;
}

impl<T: Localizable> Node for T {
    fn line(&self) -> isize { self.position().line() }
    fn set_line(&mut self, line: isize) { self.position().set_line(line) }
    fn last_line(&self) -> isize { self.position().last_line() }
    fn set_last_line(&mut self, line: isize) { self.position().set_last_line(line) }
}

pub trait Expr {}

pub trait ConstExpr {}

impl<T: ConstExpr> Expr for T {}

pub trait Stmt {}