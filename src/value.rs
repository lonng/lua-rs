pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(s),
    Function,
    UserData,
    Thread,
    Table,
}