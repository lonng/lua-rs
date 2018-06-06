pub enum Expr {
    True,
    False,
    Nil,
    Number(f64),
    String(String),
    Comma3,
    Ident(String),

    /// AttrGet(Object, Key)
    AttrGet(Box<Expr>, Box<Expr>),
    Table(Vec<Field>),
    FuncCall(Box<FuncCall>),

    /// LogicalOp(Operator, Lhs, Rhs)
    LogicalOp(String, Box<Expr>, Box<Expr>),

    /// StringConcatOp(Lhs, Rhs)
    StringConcatOp(Box<Expr>, Box<Expr>),

    /// ArithmeticOp(Operator, Lhs, Rhs)
    ArithmeticOp(String, Box<Expr>, Box<Expr>),
    UnaryMinusOp(Box<Expr>),
    UnaryNotOp(Box<Expr>),
    UnaryLenOp(Box<Expr>),

    /// Function(ParList, Stmts)
    Function(Vec<ParList>, Vec<Stmt>),
}

pub struct Field {
    key: Expr,
    value: Expr,
}

pub struct ParList {
    has_vargs: bool,
    names: Vec<String>,
}

pub struct FuncName {
    func: Expr,
    receiver: Expr,
    method: String,
}

pub struct FuncCall {
    func: Expr,
    receiver: Expr,
    method: String,
    args: Vec<Expr>,
    adjust_ret: bool,
}

pub struct ExprNode {
    position: Position,
    expr: Expr,
}

pub enum Stmt {
    /// Assign(Lhs, Rhs)
    Assign(Vec<Expr>, Vec<Expr>),

    /// LocalAssign(Names, Exprs)
    LocalAssign(Vec<String>, Vec<Expr>),
    FuncCall(Expr),
    DoBlock(Vec<Stmt>),

    /// While(Condition, Stmts)
    While(Expr, Vec<Stmt>),

    /// If(Condition, Then, Else)
    If(Expr, Vec<Stmt>, Vec<Stmt>),

    /// NumberFor(Name, Init, Limit, Step, Stmts)
    NumberFor(String, Expr, Expr, Expr, Vec<Stmt>),

    /// GenericFor(Names, Exprs, Stmts)
    GenericFor(Vec<String>, Vec<Expr>, Vec<Stmt>),

    /// FuncDef(Name, Func)
    FuncDef(Box<FuncName>, Expr),
    Return(Vec<Expr>),
    Break,
}

pub struct StmtNode {
    position: Position,
    stmt: Stmt,
}

/// Node represents a node in abstract syntax tree
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

impl Node for ExprNode {
    fn line(&self) -> isize { self.position.line }
    fn set_line(&mut self, line: isize) { self.position.line = line }
    fn last_line(&self) -> isize { self.position.last_line }
    fn set_last_line(&mut self, line: isize) { self.position.last_line = line }
}

impl Node for StmtNode {
    fn line(&self) -> isize { self.position.line }
    fn set_line(&mut self, line: isize) { self.position.line = line }
    fn last_line(&self) -> isize { self.position.last_line }
    fn set_last_line(&mut self, line: isize) { self.position.last_line = line }
}