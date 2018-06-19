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

    pub fn inner(&self) -> &T {
        &self.inner
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
pub enum UnaryOpr {
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
    Dots,
    Ident(String),

    /// AttrGet(Object, Key)
    AttrGet(Box<ExprNode>, Box<ExprNode>),
    Table(Vec<Field>),
    FuncCall(Box<FuncCall>),
    MethodCall(Box<MethodCall>),

    /// BinaryOp(Operator, Lhs, Rhs)
    BinaryOp(BinaryOpr, Box<ExprNode>, Box<ExprNode>),

    /// UnaryOp(Operator, expr)
    UnaryOp(UnaryOpr, Box<ExprNode>),

    /// Function(ParList, Stmts)
    Function(ParList, Vec<StmtNode>),
}

#[derive(Debug)]
pub struct Field {
    pub key: Option<ExprNode>,
    pub val: ExprNode,
}

impl Field {
    pub fn new(key: Option<ExprNode>, val: ExprNode) -> Field { Field { key, val } }
}

#[derive(Debug)]
pub struct ParList {
    vargs: bool,
    names: Vec<String>,
}

impl ParList {
    pub fn new() -> ParList {
        ParList {
            vargs: false,
            names: Vec::new(),
        }
    }

    pub fn set_vargs(&mut self, vargs: bool) {
        self.vargs = vargs;
    }

    pub fn set_names(&mut self, names: Vec<String>) {
        self.names = names;
    }

    pub fn push_param(&mut self, param: String) {
        self.names.push(param)
    }
}

#[derive(Debug)]
pub struct FuncName {
    func: ExprNode,
}

impl FuncName {
    pub fn new(func: ExprNode) -> FuncName {
        FuncName {
            func
        }
    }
}

#[derive(Debug)]
pub struct MethodName {
    receiver: ExprNode,
    method: String,
}

impl MethodName {
    pub fn new(receiver: ExprNode, method: String) -> MethodName {
        MethodName {
            receiver,
            method,
        }
    }
}

#[derive(Debug)]
pub struct MethodCall {
    receiver: ExprNode,
    method: String,
    args: Vec<ExprNode>,
}

impl MethodCall {
    pub fn new(receiver: ExprNode, method: String, args: Vec<ExprNode>) -> MethodCall {
        MethodCall {
            receiver,
            method,
            args,
        }
    }
}

#[derive(Debug)]
pub struct FuncCall {
    func: ExprNode,
    args: Vec<ExprNode>,
}

impl FuncCall {
    pub fn new(func: ExprNode, args: Vec<ExprNode>) -> FuncCall {
        FuncCall {
            func,
            args,
        }
    }
}

#[derive(Debug)]
pub struct IfThenElse {
    condition: ExprNode,
    then: Vec<StmtNode>,
    els: Vec<StmtNode>,
}

impl IfThenElse {
    pub fn new(condition: ExprNode, then: Vec<StmtNode>, els: Vec<StmtNode>) -> IfThenElse {
        IfThenElse {
            condition,
            then,
            els,
        }
    }

    pub fn set_els(&mut self, els: Vec<StmtNode>) {
        self.els = els;
    }
}

#[derive(Debug)]
pub enum Stmt {
    /// Assign(Lhs, Rhs)
    Assign(Vec<ExprNode>, Vec<ExprNode>),

    /// LocalAssign(Names, Exprs)
    LocalAssign(Vec<String>, Vec<ExprNode>),
    FuncCall(ExprNode),
    MethodCall(ExprNode),

    DoBlock(Vec<StmtNode>),

    /// While(Condition, Stmts)
    While(ExprNode, Vec<StmtNode>),

    /// Repeat(Condition, Stmts)
    Repeat(ExprNode, Vec<StmtNode>),

    /// If(Condition, Then, Else)
    If(IfThenElse),

    /// NumberFor(Name, Init, Limit, Step, Stmts)
    NumberFor(String, ExprNode, ExprNode, ExprNode, Vec<StmtNode>),

    /// GenericFor(Names, Exprs, Stmts)
    GenericFor(Vec<String>, Vec<ExprNode>, Vec<StmtNode>),

    /// FuncDef(Name, Func)
    FuncDef(FuncName, ExprNode),

    /// MethodDef(Name, Func)
    MethodDef(MethodName, ExprNode),
    Return(Vec<ExprNode>),
    Break,
}

pub type StmtNode = Node<Stmt>;
pub type ExprNode = Node<Expr>;