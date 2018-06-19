#![allow(non_snake_case)]

use ::{Error, Result};
use ast::*;
use instruction::*;
use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::mem::{replace, swap};
use std::rc::Rc;
use value::*;
use vm::Chunk;

const MAX_REGISTERS: i32 = 200;
const REG_UNDEFINED: usize = OPCODE_MAXA as usize;
const LABEL_NO_JUMP: usize = 0;
const FIELDS_PER_FLUSH: i32 = 50;

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
    reg: usize,
    /// opt >= 0: wants varargopt+1 results, i.e  a = func()
    /// opt = -1: ignore results             i.e  func()
    /// opt = -2: receive all results        i.e  a = {func()}
    opt: i32,
}

impl ExprContext {
    pub fn new(typ: ExprContextType, reg: usize, opt: i32) -> ExprContext {
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
    keyrk: usize,
    valrk: i32,
    keyks: bool,
    /// need move
    nmove: bool,
}

impl AssignContext {
    pub fn new(expr_ctx: ExprContext, keyrk: usize, valrk: i32, keyks: bool, nmove: bool) -> AssignContext {
        AssignContext {
            expr_ctx,
            keyrk,
            valrk,
            keyks,
            nmove,
        }
    }
}

type ConstValue = Node<Value>;

#[derive(Debug)]
struct Lblabels {
    t: i32,
    f: i32,
    e: i32,
    b: bool,
}

impl Lblabels {
    pub fn new(t: i32, f: i32, e: i32, b: bool) -> Lblabels {
        Lblabels {
            t,
            f,
            e,
            b: b,
        }
    }
}

fn expr_ctx_none(opt: i32) -> ExprContext {
    ExprContext::new(ExprContextType::None, REG_UNDEFINED, 0)
}

fn start_line<T>(p: &Node<T>) -> i32 {
    p.line()
}

fn end_line<T>(p: &Node<T>) -> i32 {
    p.last_line()
}

fn save_reg(ctx: &ExprContext, reg: usize) -> usize {
    if ctx.typ != ExprContextType::Local || ctx.reg == REG_UNDEFINED {
        reg
    } else {
        ctx.reg
    }
}

fn is_vararg(expr: &Expr) -> bool {
    if let &Expr::Dots = expr {
        true
    } else {
        false
    }
}

fn int2fb(val: i32) -> i32 {
    let mut e = 0;
    let mut x = val;
    while x >= 16 {
        x = (x + 1) >> 1;
        e += 1;
    }
    if x < 8 {
        return x;
    }
    ((e + 1) << 3) | (x - 8)
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

    pub fn propagate_KMV(&mut self, top: usize, save: &mut usize, reg: &mut usize, inc: usize, loadk: bool) {
        let lastinst = self.last();
        if get_arga(lastinst) >= (top as i32) {
            match get_opcode(lastinst) {
                OP_LOADK => {
                    // if check `LOADK`
                    if loadk {
                        let cindex = get_argbx(lastinst);
                        if cindex <= opMaxIndexRk {
                            self.pop();
                            *save = rk_ask(cindex) as usize;
                            return;
                        }
                    }
                }
                OP_MOVE => {
                    self.pop();
                    *save = get_argb(lastinst) as usize;
                    return;
                }
                _ => {}
            }
        }
        *save = *reg;
        *reg += inc;
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

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
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

    pub fn variable_block(&mut self, name: &String) -> Option<&mut CodeBlock> {
        match self.locals.find(name) {
            Some(_) => Some(self),
            None => match self.parent {
                Some(ref mut parent) => parent.variable_block(name),
                None => None
            }
        }
    }

    pub fn set_parent(&mut self, parent: Option<Box<CodeBlock>>) {
        self.parent = parent;
    }

    pub fn set_ref_upval(&mut self, b: bool) {
        self.ref_upval = b;
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

struct FunctionContext<'p> {
    proto: Box<FunctionProto>,
    code: CodeStore,
    parent: Option<&'p FunctionContext<'p>>,
    upval: VariableTable,
    block: Box<CodeBlock>,

    reg_top: usize,
    label_id: i32,
    label_pc: HashMap<i32, usize>,
}

impl<'p> FunctionContext<'p> {
    pub fn new(source: String, parent: Option<&'p FunctionContext>) -> Box<FunctionContext<'p>> {
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

    pub fn const_index(&mut self, value: Rc<Value>) -> usize {
        let v = self.proto.constants
            .iter()
            .enumerate()
            .find(|x| x.1 == &value)
            .map(|x| x.0);

        match v {
            Some(v) => v,
            None => {
                self.proto.constants.push(value);
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

    pub fn find_local_var(&self, name: &String) -> Option<usize> {
        let mut blk = &self.block;
        loop {
            let r = blk.locals.find(name);
            match r {
                Some(i) => return Some(i),
                None => match blk.parent {
                    Some(ref parent) => blk = parent,
                    None => break
                }
            }
        }
        None
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

fn compile_chunk(ctx: &mut FunctionContext, chunk: &Vec<StmtNode>) {
    for stmt in chunk.iter() {
        compile_stmt(ctx, stmt)
    }
}

fn compile_block(ctx: &mut FunctionContext, block: &Vec<StmtNode>) {
    if block.len() < 1 {
        return;
    }
    let start_line = start_line(&block[0]);
    let end_line = end_line(&block[block.len() - 1]);
    ctx.enter_block(LABEL_NO_JUMP, start_line, end_line);
    for stmt in block.iter() {
        compile_stmt(ctx, stmt);
    }
    ctx.leave_block();
}

fn get_ident_reftype(ctx: &FunctionContext, name: &String) -> ExprContextType {
    // local variable
    match ctx.find_local_var(name) {
        Some(_) => ExprContextType::Local,
        None => {
            // upvalue or global variable
            let t = match ctx.parent {
                Some(ref pctx) => get_ident_reftype(pctx, name),
                None => ExprContextType::Global,
            };

            if t == ExprContextType::Local {
                ExprContextType::Upval
            } else {
                ExprContextType::Global
            }
        }
    }
}

fn get_expr_name(expr: &Expr) -> String {
    match expr {
        Expr::Ident(ref s) => s.clone(),
        Expr::AttrGet(ref key, _) => {
            match key.inner() {
                Expr::String(ref s) => s.clone(),
                _ => "?".to_string()
            }
        }
        _ => "?".to_string()
    }
}

fn load_rk(ctx: &mut FunctionContext, reg: &mut usize, expr: &ExprNode, cnst: Rc<Value>) -> i32 {
    let mut cindex = ctx.const_index(cnst) as i32;
    if cindex < opMaxIndexRk {
        rk_ask(cindex)
    } else {
        let mut ret = *reg;
        *reg += 1;
        ctx.code.add_ABx(OP_LOADK, ret as i32, cindex, start_line(expr));
        ret as i32
    }
}

fn compile_table_expr(ctx: &mut FunctionContext, mut reg: usize, table: &ExprNode, expr_ctx: &ExprContext) {
    let tablereg = reg;
    reg += 1;
    ctx.code.add_ABC(OP_NEWTABLE, tablereg as i32, 0, 0, start_line(table));
    let tablepc = ctx.code.last_pc();
    let regbase = reg;
    if let Expr::Table(ref fields) = table.inner() {
        let mut array_count = 0;
        let mut lastvarargs = false;
        let fieldlen = fields.len();
        for (i, field) in fields.iter().enumerate() {
            let islast = i == fieldlen - 1;
            match field.key {
                None => {
                    if islast && is_vararg(field.val.inner()) {
                        lastvarargs = true;
                        reg += compile_expr(ctx, reg, &field.val, &expr_ctx_none(-2));
                    } else {
                        array_count += 1;
                        reg += compile_expr(ctx, reg, &field.val, &expr_ctx_none(0))
                    }
                }
                Some(ref expr) => {
                    let regorg = reg;
                    let mut b = reg;
                    compile_expr_with_KMV_propagation(ctx, expr, &mut reg, &mut b);
                    let mut c = reg;
                    compile_expr_with_MV_propagation(ctx, expr, &mut reg, &mut c);
                    let opcode = if let Expr::String(_) = expr.inner() { OP_SETTABLEKS } else { OP_SETTABLE };
                    ctx.code.add_ABC(opcode, tablereg as i32, b as i32, c as i32, start_line(expr));
                    reg = regorg;
                }
            }
            let flush = array_count % FIELDS_PER_FLUSH;
            if (array_count != 0 && (flush == 0 || islast)) || lastvarargs {
                reg = regbase;
                let num = if flush == 0 { FIELDS_PER_FLUSH } else { flush };
                let mut c = (array_count - 1) / FIELDS_PER_FLUSH + 1;
                let b = if islast && is_vararg(field.val.inner()) { 0 } else { num };
                let line = match field.key {
                    Some(ref expr) => start_line(expr),
                    None => start_line(&field.val),
                };
                if c > 511 {
                    c = 0;
                }
                ctx.code.add_ABC(OP_SETLIST, tablereg as i32, b as i32, c as i32, line);
                if c == 0 {
                    ctx.code.add(0, line);
                }
            }
        }

        ctx.code.set_argb(tablepc, int2fb(tablereg as i32));
        ctx.code.set_argc(tablepc, int2fb((fieldlen - tablereg) as i32));
        if expr_ctx.typ == ExprContextType::Local && expr_ctx.reg != tablereg {
            ctx.code.add_ABC(OP_MOVE, expr_ctx.reg as i32, tablereg as i32, 0, start_line(table))
        }
    } else {
        unreachable!()
    }
}

fn compile_fncall_expr(ctx: &mut FunctionContext, mut reg: usize, expr: &ExprNode, expr_ctx: &ExprContext) -> usize {
    if expr_ctx.typ == ExprContextType::Local && expr_ctx.reg == ctx.proto.param_count as usize - 1 {
        reg = expr_ctx.reg
    }
    let funcreg = reg;
    let mut islastvargs = false;

    let (name, argc) = match expr.inner() {
        Expr::FuncCall(ref func) => {
            reg += compile_expr(ctx, reg, &func.func, expr_ctx);
            let len = func.args.len();
            for (i, arg) in func.args.iter().enumerate() {
                islastvargs = (i == len - 1) && is_vararg(arg.inner());
                if islastvargs {
                    compile_expr(ctx, reg, arg, &expr_ctx_none(-2));
                } else {
                    reg += compile_expr(ctx, reg, arg, &expr_ctx_none(0));
                }
            }
            (get_expr_name(&func.func.inner()), len)
        }
        Expr::MethodCall(ref method) => {
            let mut b = reg;
            compile_expr_with_MV_propagation(ctx, &method.receiver, &mut reg, &mut b);
            let c = load_rk(ctx, &mut reg, expr, Rc::new(Value::String(method.method.clone())));
            ctx.code.add_ABC(OP_SELF, funcreg as i32, b as i32, c, start_line(expr));
            reg = b + 1;
            let reg2 = funcreg + 2;
            if reg2 > reg {
                reg = reg2
            }
            let len = method.args.len();
            for (i, arg) in method.args.iter().enumerate() {
                islastvargs = (i == len - 1) && is_vararg(arg.inner());
                if islastvargs {
                    compile_expr(ctx, reg, arg, &expr_ctx_none(-2));
                } else {
                    reg += compile_expr(ctx, reg, arg, &expr_ctx_none(0));
                }
            }
            (method.method.clone(), len + 1)
        }
        _ => unreachable!()
    };

    let b = if islastvargs { 0 } else { argc + 1 };
    ctx.code.add_ABC(OP_CALL, funcreg as i32, b as i32, expr_ctx.opt + 2, start_line(expr));
    ctx.proto.debug_calls.push(DebugCall::new(name, ctx.code.last_pc()));

    if expr_ctx.opt == 2 && expr_ctx.typ == ExprContextType::Local && funcreg != expr_ctx.reg {
        ctx.code.add_ABC(OP_MOVE, expr_ctx.reg as i32, funcreg as i32, 0, start_line(expr));
        return 1;
    }

    if ctx.reg_top() > (funcreg + 2 + expr_ctx.opt as usize) || expr_ctx.opt < -1 {
        return 0;
    }

    (expr_ctx.opt + 1) as usize
}

fn compile_binary_arith_expr(ctx: &mut FunctionContext, mut reg: usize,
                             opr: BinaryOpr, lhs: &ExprNode, rhs: &ExprNode,
                             expr_ctx: &ExprContext, line: i32) {
    let a = save_reg(expr_ctx, reg);
    let mut b = reg;
    compile_expr_with_KMV_propagation(ctx, lhs, &mut reg, &mut b);
    let mut c = reg;
    compile_expr_with_MV_propagation(ctx, rhs, &mut reg, &mut c);

    let opcode = match opr {
        BinaryOpr::Add => OP_ADD,
        BinaryOpr::Sub => OP_SUB,
        BinaryOpr::Mul => OP_MUL,
        BinaryOpr::Div => OP_DIV,
        BinaryOpr::Mod => OP_MOD,
        BinaryOpr::Pow => OP_POW,
        _ => unreachable!()
    };
    ctx.code.add_ABC(opcode, a as i32, b as i32, c as i32, line);
}

fn compile_binary_rel_expr_aux(ctx: &mut FunctionContext, mut reg: usize,
                               opr: BinaryOpr, lhs: &ExprNode, rhs: &ExprNode,
                               expr_ctx: &ExprContext, flip: i32, jumplabel: i32, line: i32) {
    let mut b = reg;
    compile_expr_with_KMV_propagation(ctx, lhs, &mut reg, &mut b);
    let mut c = reg;
    compile_expr_with_MV_propagation(ctx, rhs, &mut reg, &mut c);

    let inst = match opr {
        BinaryOpr::Eq => ABC(OP_EQ, 0 ^ flip, b as i32, c as i32),
        BinaryOpr::NE => ABC(OP_EQ, 1 ^ flip, b as i32, c as i32),
        BinaryOpr::LT => ABC(OP_LT, 0 ^ flip, b as i32, c as i32),
        BinaryOpr::GT => ABC(OP_LT, 0 ^ flip, c as i32, b as i32),
        BinaryOpr::LE => ABC(OP_LE, 0 ^ flip, b as i32, c as i32),
        BinaryOpr::GE => ABC(OP_LE, 0 ^ flip, c as i32, b as i32),
        _ => unreachable!()
    };
    ctx.code.add(inst, line);
    ctx.code.add_ASBx(OP_JMP, 0, jumplabel, line);
}

fn compile_binary_rel_expr(ctx: &mut FunctionContext, mut reg: usize,
                           opr: BinaryOpr, lhs: &ExprNode, rhs: &ExprNode,
                           expr_ctx: &ExprContext, line: i32) {
    let a = save_reg(expr_ctx, reg);
    let jumplabel = ctx.new_lable();
    compile_binary_rel_expr_aux(ctx, reg, opr, lhs, rhs, expr_ctx, 1, jumplabel, line);
    ctx.code.add_ABC(OP_LOADBOOL, a as i32, 0, 1, line);
    let lastpc = ctx.code.last_pc();
    ctx.set_label_pc(jumplabel, lastpc);
    ctx.code.add_ABC(OP_LOADBOOL, a as i32, 1, 0, line);
}

fn compile_binary_log_expr_aux(ctx: &mut FunctionContext, mut reg: usize,
                               expr: &ExprNode, expr_ctx: &ExprContext,
                               thenlabel: i32, elselabel: i32, hasnextcond: bool, lb: &mut Lblabels) {
    let mut flip = 0;
    let mut jumplabel = elselabel;
    if hasnextcond {
        flip - 1;
        jumplabel = thenlabel;
    }

    match expr.inner() {
        &Expr::False => {
            if elselabel == lb.e {
                ctx.code.add_ASBx(OP_JMP, 0, lb.f, start_line(expr));
                lb.b = true;
            } else {
                ctx.code.add_ASBx(OP_JMP, 0, elselabel, start_line(expr));
            }
        }
        &Expr::True => {
            if elselabel == lb.e {
                ctx.code.add_ASBx(OP_JMP, 0, lb.t, start_line(expr));
                lb.b = true;
            } else {
                ctx.code.add_ASBx(OP_JMP, 0, thenlabel, start_line(expr));
            }
        }
        &Expr::Nil => {
            if elselabel == lb.e {
                compile_expr(ctx, reg, expr, expr_ctx);
                ctx.code.add_ASBx(OP_JMP, 0, lb.e, start_line(expr));
            } else {
                ctx.code.add_ASBx(OP_JMP, 0, elselabel, start_line(expr));
            }
        }
        &Expr::String(_) | &Expr::Number(_) => {
            if thenlabel == lb.e {
                compile_expr(ctx, reg, expr, expr_ctx);
                ctx.code.add_ASBx(OP_JMP, 0, lb.e, start_line(expr));
            } else {
                ctx.code.add_ASBx(OP_JMP, 0, thenlabel, start_line(expr));
            }
        }
        &Expr::BinaryOp(BinaryOpr::And, ref lhs, ref rhs) => {
            let nextcondlabel = ctx.new_lable();
            compile_binary_log_expr_aux(ctx, reg, lhs, expr_ctx, nextcondlabel, elselabel, false, lb);
            let lastpc = ctx.code.last_pc();
            ctx.set_label_pc(nextcondlabel, lastpc);
            compile_binary_log_expr_aux(ctx, reg, rhs, expr_ctx, thenlabel, elselabel, hasnextcond, lb);
        }
        &Expr::BinaryOp(BinaryOpr::Or, ref lhs, ref rhs) => {
            let nextcondlabel = ctx.new_lable();
            compile_binary_log_expr_aux(ctx, reg, lhs, expr_ctx, thenlabel, nextcondlabel, true, lb);
            let lastpc = ctx.code.last_pc();
            ctx.set_label_pc(nextcondlabel, lastpc);
            compile_binary_log_expr_aux(ctx, reg, rhs, expr_ctx, thenlabel, elselabel, hasnextcond, lb);
        }
        Expr::BinaryOp(ref opr, ref lhs, ref rhs)
        if opr == &BinaryOpr::Eq ||
            opr == &BinaryOpr::LT ||
            opr == &BinaryOpr::LE ||
            opr == &BinaryOpr::NE ||
            opr == &BinaryOpr::GT ||
            opr == &BinaryOpr::GE => {
            if thenlabel == elselabel {
                flip ^= 1;
                jumplabel = lb.t;
                lb.b = true;
            } else if thenlabel == lb.e {
                jumplabel = lb.t;
                lb.b = true;
            } else if elselabel == lb.e {
                jumplabel = lb.f;
                lb.b = true;
            }
            compile_binary_rel_expr_aux(ctx, reg, *opr, lhs, rhs, expr_ctx, flip, jumplabel, start_line(expr));
        }
        _ => {
            if !hasnextcond && thenlabel == elselabel {
                reg += compile_expr(ctx, reg, expr, expr_ctx);
            } else {
                let a = reg;
                let sreg = save_reg(expr_ctx, a);
                reg += compile_expr(ctx, reg, expr, &expr_ctx_none(0));
                if sreg == a {
                    ctx.code.add_ABC(OP_TEST, a as i32, 0, 0 ^ flip, start_line(expr));
                } else {
                    ctx.code.add_ABC(OP_TESTSET, sreg as i32, a as i32, 0 ^ flip, start_line(expr));
                }
            }
            ctx.code.add_ASBx(OP_JMP, 0, jumplabel, start_line(expr))
        }
    }
}

fn compile_binary_log_expr(ctx: &mut FunctionContext, mut reg: usize,
                           opr: BinaryOpr, lhs: &ExprNode, rhs: &ExprNode,
                           expr_ctx: &ExprContext, line: i32) {
    let a = save_reg(expr_ctx, reg);
    let endlabel = ctx.new_lable();
    let mut lb = Lblabels::new(ctx.new_lable(), ctx.new_lable(), endlabel, false);
    let nextcondlabel = ctx.new_lable();
    match opr {
        BinaryOpr::And => {
            compile_binary_log_expr_aux(ctx, reg, lhs, expr_ctx, nextcondlabel, endlabel, false, &mut lb);
            let lastpc = ctx.code.last_pc();
            ctx.set_label_pc(nextcondlabel, lastpc);
            compile_binary_log_expr_aux(ctx, reg, rhs, expr_ctx, endlabel, endlabel, false, &mut lb);
        }
        BinaryOpr::Or => {
            compile_binary_log_expr_aux(ctx, reg, lhs, expr_ctx, endlabel, nextcondlabel, true, &mut lb);
            let lastpc = ctx.code.last_pc();
            ctx.set_label_pc(nextcondlabel, lastpc);
            compile_binary_log_expr_aux(ctx, reg, rhs, expr_ctx, endlabel, endlabel, false, &mut lb);
        }
        _ => unreachable!()
    }

    if lb.b {
        let lastpc = ctx.code.last_pc();
        ctx.set_label_pc(lb.f, lastpc);
        ctx.code.add_ABC(OP_LOADBOOL, a as i32, 0, 1, line);
        let lastpc = ctx.code.last_pc();
        ctx.set_label_pc(lb.t, lastpc);
        ctx.code.add_ABC(OP_LOADBOOL, a as i32, 1, 0, line);
    }

    let lastinst = ctx.code.last();
    if get_opcode(lastinst) == OP_JMP && get_argsbx(lastinst) == endlabel {
        ctx.code.pop();
    }
    let lastpc = ctx.code.last_pc();
    ctx.set_label_pc(endlabel, lastpc);
}

fn compile_binaryop_expr(ctx: &mut FunctionContext, mut reg: usize, expr: &ExprNode, expr_ctx: &ExprContext) {
    match expr.inner() {
        Expr::BinaryOp(ref opr, ref lhs, ref rhs)
        if opr == &BinaryOpr::Add ||
            opr == &BinaryOpr::Sub ||
            opr == &BinaryOpr::Mul ||
            opr == &BinaryOpr::Div ||
            opr == &BinaryOpr::Mod ||
            opr == &BinaryOpr::Pow => {
            compile_binary_arith_expr(ctx, reg, *opr, lhs, rhs, expr_ctx, start_line(expr));
        }
        Expr::BinaryOp(BinaryOpr::Concat, ref lhs, ref rhs) => {
            let mut crange = 1;
            let mut current = rhs;
            loop {
                match current.inner() {
                    Expr::BinaryOp(BinaryOpr::Concat, ref sublhs, ref subrhs) => {
                        crange += 1;
                        current = subrhs;
                    }
                    _ => break
                }
            }
            let a = save_reg(expr_ctx, reg);
            let basereg = reg;
            reg += compile_expr(ctx, reg, lhs, &expr_ctx_none(0));
            reg += compile_expr(ctx, reg, rhs, &expr_ctx_none(0));
            let mut pc = ctx.code.last_pc();
            while pc != 0 && get_opcode(ctx.code.at(pc)) == OP_CONCAT {
                ctx.code.pop();
                pc -= 1;
            }
            ctx.code.add_ABC(OP_CONCAT, a as i32, basereg as i32, (basereg as i32 + crange), start_line(expr));
        }
        Expr::BinaryOp(ref opr, ref lhs, ref rhs)
        if opr == &BinaryOpr::Eq ||
            opr == &BinaryOpr::LT ||
            opr == &BinaryOpr::LE ||
            opr == &BinaryOpr::NE ||
            opr == &BinaryOpr::GT ||
            opr == &BinaryOpr::GE => {
            compile_binary_rel_expr(ctx, reg, *opr, lhs, rhs, expr_ctx, start_line(expr));
        }
        Expr::BinaryOp(opr, ref lhs, ref rhs) if opr == &BinaryOpr::And || opr == &BinaryOpr::Or => {
            compile_binary_log_expr(ctx, reg, *opr, lhs, rhs, expr_ctx, start_line(expr));
        }
        _ => unreachable!()
    }
}

fn compile_unaryop_expr(ctx: &mut FunctionContext, mut reg: usize, expr: &ExprNode, expr_ctx: &ExprContext) {
    let (opcode, operand) = match expr.inner() {
        Expr::UnaryOp(UnaryOpr::Not, ref subexpr) => {
            match subexpr.inner() {
                &Expr::True => {
                    ctx.code.add_ABC(OP_LOADBOOL, save_reg(expr_ctx, reg) as i32, 0, 0, start_line(expr));
                    return;
                }
                &Expr::False | &Expr::Nil => {
                    ctx.code.add_ABC(OP_LOADBOOL, save_reg(expr_ctx, reg) as i32, 1, 0, start_line(expr));
                    return;
                }
                _ => (OP_NOT, subexpr)
            }
        }
        Expr::UnaryOp(UnaryOpr::Length, ref subexpr) => (OP_LEN, subexpr),
        Expr::UnaryOp(UnaryOpr::Minus, ref subexpr) => (OP_UNM, subexpr),
        _ => unreachable!()
    };

    let a = save_reg(expr_ctx, reg);
    let mut b = reg;
    compile_expr_with_MV_propagation(ctx, operand, &mut reg, &mut b);
    ctx.code.add_ABC(opcode, a as i32, b as i32, 0, start_line(expr));
}

fn compile_expr(ctx: &mut FunctionContext, mut reg: usize, expr: &ExprNode, expr_ctx: &ExprContext) -> usize {
    let sreg = save_reg(expr_ctx, reg);
    let mut sused: usize = if sreg < reg { 0 } else { 1 };
    let svreg = sreg as i32;

    // TODO: const value
    match expr.inner() {
        &Expr::True => ctx.code.add_ABC(OP_LOADBOOL, 1, 0, 0, start_line(expr)),
        &Expr::False => ctx.code.add_ABC(OP_LOADBOOL, 0, 0, 0, start_line(expr)),
        &Expr::Nil => ctx.code.add_ABC(OP_LOADNIL, svreg, svreg, 0, start_line(expr)),
        &Expr::Number(f) => {
            let num = ctx.const_index(Rc::new(Value::Number(f)));
            ctx.code.add_ABx(OP_LOADBOOL, svreg, num as i32, start_line(expr))
        }
        &Expr::String(ref s) => {
            let index = ctx.const_index(Rc::new(Value::String(s.clone())));
            ctx.code.add_ABx(OP_LOADK, svreg, index as i32, start_line(expr))
        }
        &Expr::Dots => {
            if ctx.proto.is_vararg == 0 {
                panic!("cannot use '...' outside a vararg function")
            }
            ctx.proto.is_vararg &= !VARARG_NEED;
            ctx.code.add_ABC(OP_VARARG, svreg, expr_ctx.opt + 2, 0, start_line(expr));
            sused = if ctx.reg_top() > (expr_ctx.opt + 2) as usize || expr_ctx.opt < -1 {
                0
            } else {
                (svreg + 1 + expr_ctx.opt) as usize - reg
            }
        }
        &Expr::Ident(ref s) => {
            let identtype = get_ident_reftype(ctx, s);
            match identtype {
                ExprContextType::Global => {
                    let index = ctx.const_index(Rc::new(Value::String(s.clone())));
                    ctx.code.add_ABx(OP_LOADK, svreg, index as i32, start_line(expr))
                }
                ExprContextType::Upval => {
                    let index = ctx.upval.register_unique(s.clone());
                    ctx.code.add_ABC(OP_GETUPVAL, svreg, index as i32, 0, start_line(expr));
                }
                ExprContextType::Local => {
                    let index = match ctx.find_local_var(s) {
                        Some(i) => i as i32,
                        None => -1
                    };
                    ctx.code.add_ABC(OP_MOVE, svreg, index, 0, start_line(expr));
                }
                _ => unreachable!()
            }
        }
        &Expr::AttrGet(ref obj, ref key) => {
            let a = svreg;
            let mut b = reg.clone();
            compile_expr_with_MV_propagation(ctx, obj, &mut reg, &mut b);
            let mut c = reg.clone();
            compile_expr_with_KMV_propagation(ctx, key, &mut reg, &mut c);
            let opcode = if let &Expr::String(_) = key.inner() { OP_GETTABLEKS } else { OP_GETTABLE };
            ctx.code.add_ABC(opcode, a, b as i32, c as i32, start_line(expr));
        }
        &Expr::Table(_) => compile_table_expr(ctx, reg, expr, expr_ctx),
        &Expr::FuncCall(_) | &Expr::MethodCall(_) => sused = compile_fncall_expr(ctx, reg, expr, expr_ctx),
        &Expr::BinaryOp(_, _, _) => compile_binaryop_expr(ctx, reg, expr, expr_ctx),
        &Expr::UnaryOp(_, _) => compile_unaryop_expr(ctx, reg, expr, expr_ctx),
        &Expr::Function(ref params, ref stmts) => {
            let (proto, upvals) = {
                let mut childctx = FunctionContext::new(ctx.proto.source.clone(), Some(ctx));
                compile_func_expr(childctx.borrow_mut(), params, stmts, expr_ctx);
                let mut upval = VariableTable::new(0);
                swap(&mut childctx.upval, &mut upval);
                (childctx.proto, upval)
            };

            let protono = ctx.proto.prototypes.len();
            ctx.proto.prototypes.push(proto);
            ctx.code.add_ABx(OP_CLOSURE, svreg, protono as i32, start_line(expr));
            for upv in upvals.names() {
                let (op, mut index) = match ctx.block.variable_block(upv) {
                    Some(blk) => {
                        blk.ref_upval = true;
                        (OP_MOVE, blk.locals.find(upv).unwrap() as i32)
                    }
                    None => {
                        match ctx.upval.find(upv) {
                            Some(i) => (OP_GETUPVAL, i as i32),
                            None => (OP_GETUPVAL, -1)
                        }
                    }
                };
                if index == -1 {
                    index = ctx.upval.register_unique(upv.clone()) as i32;
                }
                ctx.code.add_ABC(op, 0, index, 0, start_line(expr));
            }
        }
    };
    sused
}

fn compile_expr_with_propagation(ctx: &mut FunctionContext, expr: &ExprNode, reg: &mut usize, save: &mut usize, loadk: bool) {
    let incr = compile_expr(ctx, *reg, expr, &expr_ctx_none(0));
    match expr.inner() {
        Expr::BinaryOp(BinaryOpr::And, _, _) | Expr::BinaryOp(BinaryOpr::Or, _, _) => {
            *save = *reg;
            *reg += incr;
        }
        _ => {
            let top = ctx.reg_top();
            ctx.code.propagate_KMV(top, save, reg, incr, loadk)
        }
    }
}

fn compile_expr_with_KMV_propagation(ctx: &mut FunctionContext, expr: &ExprNode, reg: &mut usize, save: &mut usize) {
    compile_expr_with_propagation(ctx, expr, reg, save, true)
}

fn compile_expr_with_MV_propagation(ctx: &mut FunctionContext, expr: &ExprNode, reg: &mut usize, save: &mut usize) {
    compile_expr_with_propagation(ctx, expr, reg, save, false)
}

fn compile_assign_stmt_left(ctx: &mut FunctionContext, lhs: &Vec<ExprNode>) -> (usize, Vec<AssignContext>) {
    let mut reg = ctx.reg_top();
    let len = lhs.len();
    let mut acs = Vec::<AssignContext>::with_capacity(len);
    for (i, expr) in lhs.iter().enumerate() {
        let islast = i == len - 1;
        match expr.inner() {
            &Expr::Ident(ref s) => {
                let identtype = get_ident_reftype(ctx, s);
                let mut expr_ctx = ExprContext::new(identtype, REG_UNDEFINED, 0);
                match identtype {
                    ExprContextType::Global => {
                        ctx.const_index(Rc::new(Value::String(s.clone())));
                    }
                    ExprContextType::Upval => {
                        ctx.upval.register_unique(s.clone());
                    }
                    ExprContextType::Local => {
                        if islast {
                            // TODO: check
                            expr_ctx.reg = ctx.find_local_var(s).unwrap();
                        }
                    }
                    _ => unreachable!("invalid lhs identity type")
                };
                acs.push(AssignContext::new(expr_ctx, 0, 0, false, false))
            }
            &Expr::AttrGet(ref obj, ref key) => {
                let mut expr_ctx = ExprContext::new(ExprContextType::Table, REG_UNDEFINED, 0);
                compile_expr_with_KMV_propagation(ctx, obj, &mut reg, &mut expr_ctx.reg);
                reg += compile_expr(ctx, reg, key, &expr_ctx_none(0));
                let keyks = if let Expr::String(_) = key.inner() { true } else { false };
                let assi_ctx = AssignContext::new(expr_ctx, reg, 0, keyks, false);
                acs.push(assi_ctx);
            }
            _ => unreachable!("invalid left expression")
        }
    };

    (reg, acs)
}

fn compile_assign_stmt(ctx: &mut FunctionContext, lhs: &Vec<ExprNode>, rhs: &Vec<ExprNode>) {
    let lhslen = lhs.len();
    let (reg, acs) = compile_assign_stmt_left(ctx, lhs);
}

fn compile_stmt(ctx: &mut FunctionContext, stmt: &StmtNode) {
    match stmt.inner() {
        &Stmt::Assign(ref lhs, ref rhs) => compile_assign_stmt(ctx, lhs, rhs),
        &Stmt::LocalAssign(ref names, ref values) => unimplemented!(),
        &Stmt::FuncCall(ref call) => unimplemented!(),
        &Stmt::MethodCall(ref call) => unimplemented!(),
        &Stmt::DoBlock(ref stmts) => unimplemented!(),
        &Stmt::While(ref cond, ref stmts) => unimplemented!(),
        &Stmt::Repeat(ref cond, ref stmts) => unimplemented!(),
        &Stmt::If(ref ifthenelse) => unimplemented!(),
        &Stmt::NumberFor(ref name, ref init, ref limit, ref step, ref stmts) => unimplemented!(),
        &Stmt::GenericFor(ref names, ref exprs, ref stmts) => unimplemented!(),
        &Stmt::FuncDef(ref name, ref func) => unimplemented!(),
        &Stmt::MethodDef(ref name, ref method) => unimplemented!(),
        &Stmt::Return(ref exprs) => unimplemented!(),
        &Stmt::Break => unimplemented!(),
    }
}

fn compile_func_expr(ctx: &mut FunctionContext, params: &ParList, stmts: &Vec<StmtNode>, expr_ctx: &ExprContext) {}

pub fn compile(stmts: Vec<StmtNode>, name: String) -> Result<Box<Chunk>> {
    Ok(Chunk::new())
}