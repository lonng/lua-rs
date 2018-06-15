#![allow(non_snake_case)]

use ::{Error, Result};
use ast::*;
use instruction::*;
use std::collections::HashMap;
use std::mem::swap;
use std::rc::Rc;
use value::*;
use vm::Chunk;

const MAX_REGISTERS: i32 = 200;
const REG_UNDEFINED: i32 = OPCODE_MAXA;
const LABEL_NO_JUMP: usize = 0;

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

    pub fn last_pc(&self) -> usize {
        self.pc - 1
    }

    pub fn last(&self) -> Instruction {
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

struct CodeBlock {
    locals: VariableTable,
    break_label: usize,
    parent: Option<Box<CodeBlock>>,
    ref_upval: bool,
    start_line: i32,
    end_line: i32,
}

impl CodeBlock {
    pub fn new(locals: VariableTable, break_label: usize, start_line: i32, end_line: i32) -> Box<CodeBlock> {
        Box::new(CodeBlock {
            locals,
            break_label,
            parent: None,
            ref_upval: false,
            start_line,
            end_line,
        })
    }

    pub fn set_parent(&mut self, parent: Option<Box<CodeBlock>>) {
        self.parent = parent;
    }
}

const VARARG_HAS: u8 = 1;
const VARARG_IS: u8 = 2;
const VARARG_NEED: u8 = 4;

struct DebugLocalInfo {
    name: String,
    /// start pc
    spc: usize,
    /// end pc
    epc: usize,
}

impl DebugLocalInfo {
    pub fn new(name: String, spc: usize, epc: usize) -> Box<DebugLocalInfo> {
        Box::new(DebugLocalInfo {
            name,
            spc,
            epc,
        })
    }
}

struct DebugCall {
    name: String,
    pc: usize,
}

impl DebugCall {
    pub fn new(name: String, pc: usize) -> DebugCall {
        DebugCall {
            name,
            pc,
        }
    }
}

struct FunctionProto {
    source: String,
    define_line: i32,
    last_define_line: i32,
    upval_count: u8,
    param_count: u8,
    is_vararg: u8,
    used_registers: u8,
    code: Vec<Instruction>,
    constants: Vec<Rc<Value>>,
    prototypes: Vec<Box<FunctionProto>>,

    debug_pos: Vec<i32>,
    debug_locals: Vec<Box<DebugLocalInfo>>,
    debug_calls: Vec<DebugCall>,
    debug_upval: Vec<String>,

    str_constants: Vec<String>,
}

impl FunctionProto {
    pub fn new(source: String) -> Box<FunctionProto> {
        Box::new(FunctionProto {
            source,
            define_line: 0,
            last_define_line: 0,
            upval_count: 0,
            param_count: 0,
            is_vararg: 0,
            used_registers: 2,
            code: Vec::with_capacity(128),
            constants: Vec::with_capacity(32),
            prototypes: Vec::with_capacity(16),
            debug_pos: Vec::with_capacity(128),
            debug_locals: Vec::with_capacity(16),
            debug_calls: Vec::with_capacity(128),
            debug_upval: Vec::with_capacity(16),
            str_constants: Vec::with_capacity(32),
        })
    }
}

struct FunctionContext {
    proto: Box<FunctionProto>,
    code: CodeStore,
    parent: Option<Box<FunctionContext>>,
    upval: VariableTable,
    block: Box<CodeBlock>,

    reg_top: usize,
    label_id: i32,
    label_pc: HashMap<i32, usize>,
}

impl FunctionContext {
    pub fn new(source: String, parent: Option<Box<FunctionContext>>) -> Box<FunctionContext> {
        let mut ctx = FunctionContext {
            proto: FunctionProto::new(source),
            code: CodeStore::new(),
            parent,
            upval: VariableTable::new(0),
            block: CodeBlock::new(VariableTable::new(0), LABEL_NO_JUMP, 0, 0),
            reg_top: 0,
            label_id: 1,
            label_pc: HashMap::new(),
        };
        Box::new(ctx)
    }

    pub fn new_lable(&mut self) -> i32 {
        let r = self.label_id;
        self.label_id += 1;
        r
    }

    pub fn set_label_pc(&mut self, label: i32, pc: usize) {
        self.label_pc.insert(label, pc);
    }

    pub fn get_label_pc(&self, label: i32) -> usize {
        self.label_pc[&label]
    }

    pub fn const_index(&mut self, value: &Rc<Value>) -> usize {
        let v = self.proto.constants
            .iter()
            .enumerate()
            .find(|x| x.1 == value)
            .map(|x| x.0);

        match v {
            Some(v) => v,
            None => {
                self.proto.constants.push(value.clone());
                let len = self.proto.constants.len() - 1;
                if len > (OPCODE_MAXBx as usize) {
                    panic!("{}:{} to many constants", self.proto.source, self.proto.define_line)
                } else {
                    len
                }
            }
        }
    }

    pub fn reg_top(&self) -> usize {
        self.reg_top
    }

    pub fn set_reg_top(&mut self, top: usize) {
        self.reg_top = top;
    }

    pub fn register_local_var(&mut self, name: String) -> usize {
        let ret = self.block.locals.register(name.clone());
        self.proto.debug_locals.push(DebugLocalInfo::new(name, self.code.last_pc() + 1, 0));
        let top = self.reg_top();
        self.set_reg_top(top);
        ret
    }

    pub fn find_local_var_and_block(&self, name: &String) -> Option<(usize, &Box<CodeBlock>)> {
        let mut blk = &self.block;
        loop {
            let r = blk.locals.find(name);
            match r {
                Some(i) => return Some((i, blk)),
                None => match blk.parent {
                    Some(ref parent) => blk = parent,
                    None => break
                }
            }
        }
        None
    }

    pub fn find_local_var(&self, name: &String) -> Option<usize> {
        self.find_local_var_and_block(name).map(|x| x.0)
    }

    pub fn enter_block(&mut self, blabel: usize, start_line: i32, end_line: i32) {
        let vtb = VariableTable::new(self.reg_top());
        let mut blk = CodeBlock::new(vtb, blabel, start_line, end_line);
        swap(&mut blk, &mut self.block);
        self.block.set_parent(Some(blk));
    }

    pub fn close_upval(&mut self) -> Option<usize> {
        if self.block.ref_upval {
            match self.block.parent {
                Some(ref p) => {
                    let x = p.locals.last_index();
                    self.code.add_ABC(OP_CLOSE, x as i32, 0, 0, self.block.end_line);
                    Some(x)
                }
                None => None,
            }
        } else {
            None
        }
    }

    pub fn end_scope(&mut self) {
        let last_pc = self.code.last_pc();
        for vr in self.block.locals.list().iter() {
            self.proto.debug_locals[vr.index].epc = last_pc;
        }
    }

    pub fn leave_block(&mut self) -> Option<usize> {
        let closed = self.close_upval();
        self.end_scope();
        let mut parent: Option<Box<CodeBlock>> = None; // swap replacement
        swap(&mut self.block.parent, &mut parent);
        self.block = parent.unwrap();
        let top = self.block.locals.last_index();
        self.set_reg_top(top);
        closed
    }
}

pub fn compile(stmts: Vec<StmtNode>, name: String) -> Result<Box<Chunk>> {
    Ok(Chunk::new())
}