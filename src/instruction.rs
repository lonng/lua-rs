pub type Instruction = u32;

type OpCodeSize = i32;

const OPCODE_SIZE: OpCodeSize = 6;
const OPCODE_SIZEA: OpCodeSize = 9;
const OPCODE_SIZEB: OpCodeSize = 9;
const OPCODE_SIZEC: OpCodeSize = 9;
const OPCODE_SIZEBx: OpCodeSize = 18;
const OPCODE_SIZESBx: OpCodeSize = 18;

const OPCODE_MAXA: OpCodeSize = 1 << OPCODE_SIZEA - 1;
const OPCODE_MAXB: OpCodeSize = 1 << OPCODE_SIZEB - 1;
const OPCODE_MAXC: OpCodeSize = 1 << OPCODE_SIZEC - 1;
const OPCODE_MAXBx: OpCodeSize = 1 << OPCODE_SIZEBx - 1;
const OPCODE_MAXSBx: OpCodeSize = OPCODE_MAXBx >> 1;

/// Lua 5.1.4 opcodes layout:
///
/// instruction = 32bit(fixed length)
///
/// +---------------------------------------------+
/// |0-5(6bits)|6-13(8bit)|14-22(9bit)|23-31(9bit)|
/// |==========+==========+===========+===========|
/// |  opcode  |    A     |     C     |    B      |
/// |----------+----------+-----------+-----------|
/// |  opcode  |    A     |      Bx(unsigned)     |
/// |----------+----------+-----------+-----------|
/// |  opcode  |    A     |      sBx(signed)      |
/// +---------------------------------------------+
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum OpCode {
    /// A B     R(A) := R(B)
    MOVE,
    /// A B     R(A) := R(B); followed by R(C) MOVE ops
    MOVEN,
    /// A Bx    R(A) := Kst(Bx)
    LOADK,
    /// A B C   R(A) := (Bool)B; if (C) pc++
    LOADBOOL,
    /// A B     R(A) := ... := R(B) := nil
    LOADNIL,
    /// A B     R(A) := UpValue[B]
    GETUPVAL,
    /// A Bx    R(A) := Gbl[Kst(Bx)]
    GETGLOBAL,
    /// A B C   R(A) := R(B)[RK(C)]
    GETTABLE,
    /// A B C   R(A) := R(B)[RK(C)] ; RK(C) is constant string
    GETTABLEKS,
    /// A Bx    Gbl[Kst(Bx)] := R(A)
    SETGLOBAL,
    /// A B     UpValue[B] := R(A)
    SETUPVAL,
    /// A B C   R(A)[RK(B)] := RK(C)
    SETTABLE,
    /// A B C   R(A)[RK(B)] := RK(C) ; RK(B) is constant string
    SETTABLEKS,
    /// A B C   R(A) := {} (size = BC)
    NEWTABLE,
    /// A B C   R(A+1) := R(B); R(A) := R(B)[RK(C)]
    SELF,
    /// A B C   R(A) := RK(B) + RK(C)
    ADD,
    /// A B C   R(A) := RK(B) - RK(C)
    SUB,
    /// A B C   R(A) := RK(B) * RK(C)
    MUL,
    /// A B C   R(A) := RK(B) / RK(C)
    DIV,
    /// A B C   R(A) := RK(B) % RK(C)
    MOD,
    /// A B C   R(A) := RK(B) ^ RK(C)
    POW,
    /// A B     R(A) := -R(B)
    UNM,
    /// A B     R(A) := not R(B)
    NOT,
    /// A B     R(A) := length of R(B)
    LEN,
    /// A B C   R(A) := R(B).. ... ..R(C)
    CONCAT,
    /// sBx     pc+=sBx
    JMP,
    /// A B C   if ((RK(B) == RK(C)) ~= A) then pc++
    EQ,
    /// A B C   if ((RK(B) <  RK(C)) ~= A) then pc++
    LT,
    /// A B C   if ((RK(B) <= RK(C)) ~= A) then pc++
    LE,
    /// A C     if not (R(A) <=> C) then pc++
    TEST,
    /// A B C   if (R(B) <=> C) then R(A) := R(B) else pc++
    TESTSET,
    /// A B C   R(A) ... R(A+C-2) := R(A)(R(A+1) ... R(A+B-1))
    CALL,
    /// A B C   return R(A)(R(A+1) ... R(A+B-1))
    TAILCALL,
    /// A B     return R(A) ... R(A+B-2)      (see note)
    RETURN,
    /// A sBx   R(A)+=R(A+2);
    ///         if R(A) <?= R(A+1) then { pc+=sBx; R(A+3)=R(A) }
    FORLOOP,
    /// A sBx   R(A)-=R(A+2); pc+=sBx
    FORPREP,
    /// A C     R(A+3) ... R(A+3+C) := R(A)(R(A+1) R(A+2));
    ///         if R(A+3) ~= nil then { pc++; R(A+2)=R(A+3); }
    TFORLOOP,
    /// A B C   R(A)[(C-1)*FPF+i] := R(A+i) 1 <= i <= B
    SETLIST,
    /// A       close all variables in the stack up to (>=) R(A)
    CLOSE,
    /// A Bx    R(A) := closure(KPROTO[Bx] R(A) ... R(A+n))
    CLOSURE,
    /// A B     R(A) R(A+1) ... R(A+B-1) = vararg
    VARARG,
    /// NOP
    NOP,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum OpArgMode {
    N,
    U,
    R,
    K,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum OpType {
    ABC,
    ABx,
    ASbx,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct OpProp {
    name: &'static str,
    is_test: bool,
    set_reg_a: bool,
    mode_arg_b: OpArgMode,
    mode_arg_c: OpArgMode,
    typ: OpType,
}

static OP_NAMES: &'static [OpProp; (OpCode::NOP as usize + 1)] = &[
    OpProp { name: "MOVE", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::R, mode_arg_c: OpArgMode::N, typ: OpType::ABC },
    OpProp { name: "MOVEN", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::R, mode_arg_c: OpArgMode::N, typ: OpType::ABC },
    OpProp { name: "LOADK", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::K, mode_arg_c: OpArgMode::N, typ: OpType::ABx },
    OpProp { name: "LOADBOOL", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::U, mode_arg_c: OpArgMode::U, typ: OpType::ABC },
    OpProp { name: "LOADNIL", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::R, mode_arg_c: OpArgMode::N, typ: OpType::ABC },
    OpProp { name: "GETUPVAL", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::U, mode_arg_c: OpArgMode::N, typ: OpType::ABC },
    OpProp { name: "GETGLOBAL", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::K, mode_arg_c: OpArgMode::N, typ: OpType::ABx },
    OpProp { name: "GETTABLE", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::R, mode_arg_c: OpArgMode::K, typ: OpType::ABC },
    OpProp { name: "GETTABLEKS", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::R, mode_arg_c: OpArgMode::K, typ: OpType::ABC },
    OpProp { name: "SETGLOBAL", is_test: false, set_reg_a: false, mode_arg_b: OpArgMode::K, mode_arg_c: OpArgMode::N, typ: OpType::ABx },
    OpProp { name: "SETUPVAL", is_test: false, set_reg_a: false, mode_arg_b: OpArgMode::U, mode_arg_c: OpArgMode::N, typ: OpType::ABC },
    OpProp { name: "SETTABLE", is_test: false, set_reg_a: false, mode_arg_b: OpArgMode::K, mode_arg_c: OpArgMode::K, typ: OpType::ABC },
    OpProp { name: "SETTABLEKS", is_test: false, set_reg_a: false, mode_arg_b: OpArgMode::K, mode_arg_c: OpArgMode::K, typ: OpType::ABC },
    OpProp { name: "NEWTABLE", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::U, mode_arg_c: OpArgMode::U, typ: OpType::ABC },
    OpProp { name: "SELF", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::R, mode_arg_c: OpArgMode::K, typ: OpType::ABC },
    OpProp { name: "ADD", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::K, mode_arg_c: OpArgMode::K, typ: OpType::ABC },
    OpProp { name: "SUB", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::K, mode_arg_c: OpArgMode::K, typ: OpType::ABC },
    OpProp { name: "MUL", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::K, mode_arg_c: OpArgMode::K, typ: OpType::ABC },
    OpProp { name: "DIV", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::K, mode_arg_c: OpArgMode::K, typ: OpType::ABC },
    OpProp { name: "MOD", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::K, mode_arg_c: OpArgMode::K, typ: OpType::ABC },
    OpProp { name: "POW", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::K, mode_arg_c: OpArgMode::K, typ: OpType::ABC },
    OpProp { name: "UNM", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::R, mode_arg_c: OpArgMode::N, typ: OpType::ABC },
    OpProp { name: "NOT", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::R, mode_arg_c: OpArgMode::N, typ: OpType::ABC },
    OpProp { name: "LEN", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::R, mode_arg_c: OpArgMode::N, typ: OpType::ABC },
    OpProp { name: "CONCAT", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::R, mode_arg_c: OpArgMode::R, typ: OpType::ABC },
    OpProp { name: "JMP", is_test: false, set_reg_a: false, mode_arg_b: OpArgMode::R, mode_arg_c: OpArgMode::N, typ: OpType::ASbx },
    OpProp { name: "EQ", is_test: true, set_reg_a: false, mode_arg_b: OpArgMode::K, mode_arg_c: OpArgMode::K, typ: OpType::ABC },
    OpProp { name: "LT", is_test: true, set_reg_a: false, mode_arg_b: OpArgMode::K, mode_arg_c: OpArgMode::K, typ: OpType::ABC },
    OpProp { name: "LE", is_test: true, set_reg_a: false, mode_arg_b: OpArgMode::K, mode_arg_c: OpArgMode::K, typ: OpType::ABC },
    OpProp { name: "TEST", is_test: true, set_reg_a: true, mode_arg_b: OpArgMode::R, mode_arg_c: OpArgMode::U, typ: OpType::ABC },
    OpProp { name: "TESTSET", is_test: true, set_reg_a: true, mode_arg_b: OpArgMode::R, mode_arg_c: OpArgMode::U, typ: OpType::ABC },
    OpProp { name: "CALL", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::U, mode_arg_c: OpArgMode::U, typ: OpType::ABC },
    OpProp { name: "TAILCALL", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::U, mode_arg_c: OpArgMode::U, typ: OpType::ABC },
    OpProp { name: "RETURN", is_test: false, set_reg_a: false, mode_arg_b: OpArgMode::U, mode_arg_c: OpArgMode::N, typ: OpType::ABC },
    OpProp { name: "FORLOOP", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::R, mode_arg_c: OpArgMode::N, typ: OpType::ASbx },
    OpProp { name: "FORPREP", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::R, mode_arg_c: OpArgMode::N, typ: OpType::ASbx },
    OpProp { name: "TFORLOOP", is_test: true, set_reg_a: false, mode_arg_b: OpArgMode::N, mode_arg_c: OpArgMode::U, typ: OpType::ABC },
    OpProp { name: "SETLIST", is_test: false, set_reg_a: false, mode_arg_b: OpArgMode::U, mode_arg_c: OpArgMode::U, typ: OpType::ABC },
    OpProp { name: "CLOSE", is_test: false, set_reg_a: false, mode_arg_b: OpArgMode::N, mode_arg_c: OpArgMode::N, typ: OpType::ABC },
    OpProp { name: "CLOSURE", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::U, mode_arg_c: OpArgMode::N, typ: OpType::ABx },
    OpProp { name: "VARARG", is_test: false, set_reg_a: true, mode_arg_b: OpArgMode::U, mode_arg_c: OpArgMode::N, typ: OpType::ABC },
    OpProp { name: "NOP", is_test: false, set_reg_a: false, mode_arg_b: OpArgMode::R, mode_arg_c: OpArgMode::N, typ: OpType::ASbx },
];

impl ToString for OpCode {
    fn to_string(&self) -> String {
        OP_NAMES[*self as usize].name.to_string()
    }
}

impl Instruction {
    #[inline]
    pub fn with_ABC(op: OpCode, a: i32, b: i32) -> Instruction {
        let mut inst: Instruction = 0;

    }

    pub fn set_opcode(&mut self, op: OpCode) {

    }
}

pub fn op_to_string(op: Instruction) -> String {}