
/// Node represents a node in abstract syntax tree
pub struct Node<T> {
    line: isize,
    last_line: isize,
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

    fn line(&self) -> isize { self.line }
    fn set_line(&mut self, line: isize) { self.line = line }
    fn last_line(&self) -> isize { self.last_line }
    fn set_last_line(&mut self, line: isize) { self.last_line = line }
}

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

    /// LogicalOp(Operator, Lhs, Rhs)
    LogicalOp(String, Box<ExprNode>, Box<ExprNode>),

    /// StringConcatOp(Lhs, Rhs)
    StringConcatOp(Box<ExprNode>, Box<ExprNode>),

    /// ArithmeticOp(Operator, Lhs, Rhs)
    ArithmeticOp(String, Box<ExprNode>, Box<ExprNode>),
    UnaryMinusOp(Box<ExprNode>),
    UnaryNotOp(Box<ExprNode>),
    UnaryLenOp(Box<ExprNode>),

    /// Function(ParList, Stmts)
    Function(Vec<ParList>, Vec<StmtNode>),
}

pub struct Field {
    key: ExprNode,
    value: ExprNode,
}

pub struct ParList {
    has_vargs: bool,
    names: Vec<String>,
}

pub struct FuncName {
    func: ExprNode,
    receiver: ExprNode,
    method: String,
}

pub struct FuncCall {
    func: ExprNode,
    receiver: ExprNode,
    method: String,
    args: Vec<ExprNode>,
    adjust_ret: bool,
}

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

type StmtNode = Node<Stmt>;
type ExprNode = Node<Expr>;