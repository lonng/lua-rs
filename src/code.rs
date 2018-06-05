use std::collections::HashMap;
use std::mem;
use ::{Result, Error};
use parser::Opr;

/// Kind represent expression kind
///
/// ```
/// Constant       // info = index of constant
/// Number         // value = numerical value
/// NonRelocatable // info = result register
/// Local          // info = local register
/// UpValue        // info = index of upvalue
/// Indexed        // table = table register/upvalue, index = register/constant index
/// Jump           // info = instruction pc
/// Relocatable    // info = instruction pc
/// Call           // info = instruction pc
/// VarArg         // info = instruction pc
/// ```
pub enum Kind {
    Void,
    Nil,
    True,
    False,
    Constant(usize),
    Number(f64),
    NonRelocatable(usize),
    Local(usize),
    UpValue(usize),
    Indexed(usize, usize),
    Jump(usize),
    Relocatable(usize),
    Call(usize),
    VarArg(usize),
}

type Instruction = u32;

pub struct ExprDesc {
    kind: Kind,
    table_type: i32,
    t: i32,
    f: i32,
}

impl ExprDesc {
    pub fn new(kind: Kind) -> ExprDesc {
        ExprDesc {
            kind,
            table_type: 0,
            t: 0,
            f: 0,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum Value {}

#[derive(Debug)]
pub struct Block {
    prev: Option<Box<Block>>,
    first_label: usize,
    first_goto: usize,
    active_var_count: usize,
    has_upval: bool,
    is_loop: bool,
}

impl Block {
    pub fn new(prev: Option<Box<Block>>,
               first_label: usize,
               first_goto: usize,
               active_var_count: usize,
               is_loop: bool) -> Box<Block> {
        Box::new(Block {
            prev,
            first_label,
            first_goto,
            active_var_count,
            has_upval: false,
            is_loop,
        })
    }
}

#[derive(Debug)]
pub struct LocalVariable {
    name: String,
    start_pc: isize,
    end_pc: isize,
}

#[derive(Debug)]
pub struct UpValueDesc {
    name: String,
    is_local: bool,
    index: isize,
}

#[derive(Debug)]
pub struct Prototype {
    constants: Vec<Value>,
    code: Vec<Instruction>,
    prototypes: Vec<Prototype>,
    line_info: Vec<i32>,
    local_variables: Vec<LocalVariable>,
    up_values: Vec<UpValueDesc>,
    source: String,
    line_defined: isize,
    last_line_defined: isize,
    parameter_count: isize,
    max_stack_size: isize,
    is_varrag: bool,
    // TODO: cache lua closure
}

impl Prototype {
    pub fn new() -> Prototype {
        Prototype {
            constants: Vec::new(),
            code: Vec::new(),
            prototypes: Vec::new(),
            line_info: Vec::new(),
            local_variables: Vec::new(),
            up_values: Vec::new(),
            source: String::new(),
            line_defined: 0,
            last_line_defined: 0,
            parameter_count: 0,
            max_stack_size: 0,
            is_varrag: false,
        }
    }
}

#[derive(Debug)]
pub struct Function {
    const_lookup: HashMap<Value, isize>,
    prev: Option<Box<Function>>,
    block: Option<Box<Block>>,
    f: Box<Prototype>,
    jump_pc: isize,
    last_target: isize,
    free_register_count: isize,
    active_var_count: isize,
    first_local: isize,
}

impl Function {
    pub fn new() -> Function {
        Function {
            const_lookup: HashMap::new(),
            prev: None,
            block: None,
            f: Box::new(Prototype::new()),
            jump_pc: 0,
            last_target: 0,
            free_register_count: 0,
            active_var_count: 0,
            first_local: 0,
        }
    }

    pub fn enter_block(&mut self, is_loop: bool,
                       active_labels: usize,
                       pending_gotos: usize,
                       activity_vars: usize) {
        debug_assert!(self.free_register_count == self.active_var_count);
        let mut prev_block: Option<Box<Block>> = None;
        mem::swap(&mut self.block, &mut prev_block);
        self.block = Some(Block::new(prev_block,
                                     active_labels,
                                     pending_gotos,
                                     activity_vars,
                                     is_loop));
    }

    pub fn make_upval(&mut self, name: &str, expr: ExprDesc) -> Result<usize> {
        Ok(0)
    }

    pub fn patchtohere(&mut self) {
        unimplemented!()
    }

    pub fn prefix(&mut self, op: Opr, sub: &ExprDesc, line: i32) -> ExprDesc {
        unimplemented!()
    }

    pub fn infix(&mut self, op: Opr, infix: &ExprDesc) -> ExprDesc {
        unimplemented!()
    }

    pub fn postfix(&mut self, op: Opr, expr: &ExprDesc, sub: &ExprDesc, line: i32) -> ExprDesc {
        unimplemented!()
    }
}