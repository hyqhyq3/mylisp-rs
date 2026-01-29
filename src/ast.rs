use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(f64),
    Symbol(String),
    String(String),
    Bool(bool),
    List(Vec<Expr>),
    Nil,
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Number(n) => write!(f, "{}", n),
            Expr::Symbol(s) => write!(f, "{}", s),
            Expr::String(s) => write!(f, "\"{}\"", s),
            Expr::Bool(b) => write!(f, "{}", b),
            Expr::Nil => write!(f, "nil"),
            Expr::List(exprs) => {
                let items: Vec<String> = exprs.iter().map(|e| e.to_string()).collect();
                write!(f, "({})", items.join(" "))
            }
        }
    }
}
