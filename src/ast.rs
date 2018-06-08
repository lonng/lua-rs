
/// Node represents a node in abstract syntax tree
#[derive(Debug)]
pub struct Node<T> {
    line: i32,
    last_line: i32,
    inner: T,
}

impl<T> Node<T> {
    pub fn new(inner: T) -> Node<T> {
        Node {
            line: 0,
            last_line: 0,
            inner,
        }
    }

    pub fn line(&self) -> i32 { self.line }
    pub fn set_line(&mut self, line: i32) { self.line = line }
    pub fn last_line(&self) -> i32 { self.last_line }
    pub fn set_last_line(&mut self, line: i32) { self.last_line = line }
}


#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOpr {
    Add = 0,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Concat,
    Eq,
    LT,
    LE,
    NE,
    GT,
    GE,
    And,
    Or,
    NoBinary,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnaryOpr{
    Minus,
    Not,
    Length,
    NoUnary,
}

#[derive(Debug)]
pub enum Expr {
    True,
    False,
    Nil,
    Number(f64),
    String(String),
    Comma3,
    Ident(String),

    /// AttrGet(Object, Key)
    AttrGet(Box<ExprNode>, Box<ExprNode>),
    Table(Vec<Field>),
    FuncCall(Box<FuncCall>),

    /// BinaryOp(Operator, Lhs, Rhs)
    BinaryOp(BinaryOpr, Box<ExprNode>, Box<ExprNode>),

    /// UnaryOp(Operator, expr)
    UnaryOp(UnaryOpr, Box<ExprNode>),

    /// Function(ParList, Stmts)
    Function(Vec<ParList>, Vec<StmtNode>),
}

#[derive(Debug)]
pub struct Field {
    key: ExprNode,
    value: ExprNode,
}

#[derive(Debug)]
pub struct ParList {
    has_vargs: bool,
    names: Vec<String>,
}

#[derive(Debug)]
pub struct FuncName {
    func: ExprNode,
    receiver: ExprNode,
    method: String,
}

#[derive(Debug)]
pub struct FuncCall {
    func: ExprNode,
    receiver: ExprNode,
    method: String,
    args: Vec<ExprNode>,
    adjust_ret: bool,
}

#[derive(Debug)]
pub enum Stmt {
    /// Assign(Lhs, Rhs)
    Assign(Vec<ExprNode>, Vec<ExprNode>),

    /// LocalAssign(Names, Exprs)
    LocalAssign(Vec<String>, Vec<ExprNode>),
    FuncCall(ExprNode),
    DoBlock(Vec<StmtNode>),

    /// While(Condition, Stmts)
    While(ExprNode, Vec<StmtNode>),

    /// If(Condition, Then, Else)
    If(ExprNode, Vec<StmtNode>, Vec<StmtNode>),

    /// NumberFor(Name, Init, Limit, Step, Stmts)
    NumberFor(String, ExprNode, ExprNode, ExprNode, Vec<StmtNode>),

    /// GenericFor(Names, Exprs, Stmts)
    GenericFor(Vec<String>, Vec<ExprNode>, Vec<StmtNode>),

    /// FuncDef(Name, Func)
    FuncDef(Box<FuncName>, ExprNode),
    Return(Vec<ExprNode>),
    Break,
}

pub type StmtNode = Node<Stmt>;
pub type ExprNode = Node<Expr>;