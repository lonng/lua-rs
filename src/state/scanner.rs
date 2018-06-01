use state::State;

#[derive(Debug)]
enum Token {
    And,
    Break,
    Do,
    Else,
    Elseif,
    End,
    False,
    For,
    Function,
    Goto,
    If,
    In,
    Local,
    Nil,
    Not,
    Or,
    Repeat,
    Return,
    Then,
    True,
    Until,
    While,
    Concat,
    Dots,
    Eq,
    GE,
    LE,
    NE,
    DoubleColon,
    EOS,
    Number(f64),
    Name(String),
    String(String),
}

#[derive(Debug)]
pub struct Scanner {
}

impl Scanner {
    pub fn new() -> Scanner {
        Scanner {
        }
    }
}