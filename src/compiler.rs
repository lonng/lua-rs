#![allow(non_snake_case)]

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
    t: i32,
    f: i32,
    e: i32,
    d: bool,
}

fn expr_ctx_none(opt: i32) -> ExprContext {
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
    if let Expr::Dots = expr {
        true
    } else {
        false
    }
}

struct CodeStore {
    codes: Vec<u32>,
    lines: Vec<i32>,
    pc: usize,
}

impl CodeStore {
    pub fn new() -> CodeStore {
        CodeStore {
            codes: Vec::new(),
            lines: Vec::new(),
            pc: 0,
        }
    }

    pub fn add(&mut self, inst: Instruction, line: i32) {
        let len = self.codes.len();
        if len <= 0 || self.pc == len {
            self.codes.push(inst);
            self.lines.push(line);
        } else {
            let pc = self.pc;
            self.codes[pc] = inst;
            self.lines[pc] = line;
        }
        self.pc += 1;
    }

    pub fn add_ABC(&mut self, op: OpCode, a: i32, b: i32, c: i32, line: i32) {
        self.add(ABC(op, a, b, c), line);
    }

    pub fn add_ABx(&mut self, op: OpCode, a: i32, bx: i32, line: i32) {
        self.add(ABx(op, a, bx), line)
    }

    pub fn add_ASBx(&mut self, op: OpCode, a: i32, sbx: i32, line: i32) {
        self.add(ASBx(op, a, sbx), line)
    }

    pub fn set_opcode(&mut self, pc: usize, op: OpCode) {
        set_opcode(&mut self.codes[pc], op)
    }

    pub fn set_arga(&mut self, pc: usize, a: i32) {
        set_arga(&mut self.codes[pc], a)
    }

    pub fn set_argb(&mut self, pc: usize, b: i32) {
        set_argb(&mut self.codes[pc], b)
    }

    pub fn set_argc(&mut self, pc: usize, c: i32) {
        set_argc(&mut self.codes[pc], c)
    }

    pub fn set_argbx(&mut self, pc: usize, bx: i32) {
        set_argbx(&mut self.codes[pc], bx)
    }

    pub fn set_argsbx(&mut self, pc: usize, sbx: i32) {
        set_argsbx(&mut self.codes[pc], sbx)
    }

    pub fn at(&self, pc: usize) -> Instruction {
        self.codes[pc]
    }

    pub fn list(&self) -> &[Instruction] {
        let pc = self.pc;
        &self.codes[..pc]
    }

    pub fn line_list(&self) -> &[i32] {
        let pc = self.pc;
        &self.lines[..pc]
    }

    pub fn last_pc(&self) -> Instruction {
        if self.pc == 0 {
            INVALID_INSTRUCTION
        } else {
            self.codes[self.pc - 1]
        }
    }

    pub fn pop(&mut self) {
        self.pc -= 1
    }
}

struct Variable {
    index: usize,
    name: String,
}

impl Variable {
    pub fn new(index: usize, name: String) -> Variable {
        Variable {
            index,
            name,
        }
    }
}

struct VariableTable {
    names: Vec<String>,
    offset: usize,
}

impl VariableTable {
    pub fn new(offset: usize) -> VariableTable {
        VariableTable {
            names: Vec::new(),
            offset,
        }
    }

    pub fn names(&self) -> &[String] {
        &self.names
    }

    pub fn list(&self) -> Vec<Variable> {
        self.names.
            iter().
            enumerate().
            map(|(index, name)|
                Variable::new(index + self.offset, name.clone())).
            collect()
    }

    pub fn last_index(&self) -> usize {
        self.offset + self.names.len()
    }

    pub fn find(&self, name: &String) -> Option<usize> {
        match self.names.
            iter().
            enumerate().
            find(|x| x.1 == name) {
            Some((index, _)) => Some(self.offset + index),
            None => None
        }
    }

    pub fn register_unique(&mut self, name: String) -> usize {
        match self.find(&name) {
            Some(index) => index,
            None => self.register(name),
        }
    }

    pub fn register(&mut self, name: String) -> usize {
        self.names.push(name);
        self.names.len() - 1 + self.offset
    }
}


pub fn compile(stmts: Vec<StmtNode>, name: String) -> Result<Box<Chunk>> {
    Ok(Chunk::new())
}