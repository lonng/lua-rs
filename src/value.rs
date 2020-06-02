#[derive(Debug, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Function,
    UserData,
    Thread,
    Table,
}
