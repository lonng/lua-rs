#![allow(non_snake_case)]

use ::{Error, Result};
use ast::*;
use instruction::*;
use std::collections::HashMap;
use std::mem::{swap, replace};
use std::rc::Rc;
use value::*;
use vm::Chunk;
use std::borrow::BorrowMut;

const MAX_REGISTERS: i32 = 200;
const REG_UNDEFINED: usize = OPCODE_MAXA as usize;
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

struct Lblabels {
    t: i32,
    f: i32,
    e: i32,
    d: bool,
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

    pub fn ref_local_var_or_upval(&mut self, name: &String) -> Option<usize> {
        let mut blk = &self.block;
        let mut slf = true;
        let mut res = blk.locals.find(name);
        let mut idx: Option<usize> = None;
        loop {
            match res {
                Some(i) => {
                    idx = Some(i);
                    break;
                }
                None => match blk.parent {
                    Some(ref parent) => {
                        res = parent.locals.find(name);
                        slf = false;
                        blk = parent;
                    }
                    None => break
                }
            }
        }
        if !slf {
            // TODO
            //blk.set_ref_upval(true);
            unimplemented!()
        }
        idx
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

fn compile_table_expr(ctx: &mut FunctionContext, reg: &mut usize, fields: &Vec<Field>, expr_ctx: &ExprContext) {}

fn compile_fncall_expr(ctx: &mut FunctionContext, reg: &mut usize, fncall: &FuncCall, expr_ctx: &ExprContext) {}

fn compile_mcall_expr(ctx: &mut FunctionContext, reg: &mut usize, mcall: &MethodCall, expr_ctx: &ExprContext) {}

fn compile_binaryop_expr(ctx: &mut FunctionContext, reg: &mut usize, opr: &BinaryOpr, lhs: &ExprNode, rhs: &ExprNode, expr_ctx: &ExprContext) {}

fn compile_unaryop_expr(ctx: &mut FunctionContext, reg: &mut usize, opr: &UnaryOpr, expr: &ExprNode, expr_ctx: &ExprContext) {}


fn compile_expr(ctx: &mut FunctionContext, reg: &mut usize, expr: &ExprNode, expr_ctx: &ExprContext) -> usize {
    let sreg = save_reg(expr_ctx, *reg);
    let mut sused: usize = if sreg < *reg { 0 } else { 1 };
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
                (svreg + 1 + expr_ctx.opt) as usize - *reg
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
            compile_expr_with_MV_propagation(ctx, obj, reg, &mut b);
            let mut c = reg.clone();
            compile_expr_with_KMV_propagation(ctx, key, reg, &mut c);
            let opcode = if let &Expr::String(_) = key.inner() { OP_GETTABLEKS } else { OP_GETTABLE };
            ctx.code.add_ABC(opcode, a, b as i32, c as i32, start_line(expr));
        }
        &Expr::Table(ref fields) => compile_table_expr(ctx, reg, fields, expr_ctx),
        &Expr::FuncCall(ref call) => compile_fncall_expr(ctx, reg, call, expr_ctx),
        &Expr::MethodCall(ref call) => compile_mcall_expr(ctx, reg, call, expr_ctx),
        &Expr::BinaryOp(ref opr, ref lhs, ref rhs) => compile_binaryop_expr(ctx, reg, opr, lhs, rhs, expr_ctx),
        &Expr::UnaryOp(ref opr, ref exp) => compile_unaryop_expr(ctx, reg, opr, exp, expr_ctx),
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
                let (op, mut index) = match ctx.ref_local_var_or_upval(upv) {
                    Some(index) => {
                        (OP_MOVE, index as i32)
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
    let incr = compile_expr(ctx, reg, expr, &expr_ctx_none(0));
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
                reg += compile_expr(ctx, &mut reg, key, &expr_ctx_none(0));
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