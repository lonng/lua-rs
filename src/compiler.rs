use ::{Error, Result};
use ast::*;
use instruction::*;
use value::*;
use vm::Chunk;

const MAX_REGISTERS: i32 = 200;
const REG_UNDEFINED: i32 = OPCODE_MAXA;
const LABEL_NO_JUMP: i32 = 0;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum ExprContextType {
    Global,
    Upval,
    Local,
    Table,
    Vararg,
    Method,
    None,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
struct ExprContext {
    typ: ExprContextType,
    reg: i32,
    /// opt >= 0: wants varargopt+1 results, i.e  a = func()
    /// opt = -1: ignore results             i.e  func()
    /// opt = -2: receive all results        i.e  a = {func()}
    opt: i32,
}

impl ExprContext {
    pub fn new(typ: ExprContextType, reg: i32, opt: i32) -> ExprContext {
        ExprContext {
            typ,
            reg,
            opt,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
struct AssignContext {
    expr_ctx: ExprContext,
    keyrk: i32,
    valrk: i32,
    keyks: i32,
    /// need move
    nmove: bool,
}

type ConstValue = Node<Value>;

struct Lblabels {
    t: int,
    f: int,
    e: int,
    d: bool,
}

fn expr_ctx_none(opt: int) -> ExprContext {
    ExprContext::new(ExprContextType::None, REG_UNDEFINED, 0)
}

fn start_line<T>(p: Node<T>) -> i32 {
    p.line()
}

fn end_line<T>(p: Node<T>) -> i32 {
    p.last_line()
}

fn save_reg(ctx: &ExprContext, reg: i32) -> i32 {
    if ctx.typ != ExprContextType::Local || ctx.reg == REG_UNDEFINED {
        reg
    } else {
        ctx.reg
    }
}

fn is_vararg(expr: Expr) -> bool {
    expr == Expr::Dots
}

struct CodeStore {
    codes: Vec<u32>,
    lines: Vec<i32>,
    pc: i32,
}

impl CodeStore {
    pub fn new() -> CodeStore {
        CodeStore {
            codes: Vec::new(),
            lines: Vec::new(),
            pc: 0,
        }
    }
}

pub fn compile(stmts: Vec<StmtNode>, name: String) -> Result<Box<Chunk>> {
    Ok(Chunk::new())
}