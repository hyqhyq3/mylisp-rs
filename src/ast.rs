use crate::env::Env;
use std::cell::RefCell;
use std::rc::Rc;
use std::fmt;

#[derive(Debug, Clone)]
pub enum ThunkState {
    Unevaluated { expr: Expr, env: Env },
    Evaluated(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(f64),
    Symbol(String),
    String(String),
    Bool(bool),
    List(Vec<Expr>),
    Lambda {
        params: Vec<Expr>,
        body: Vec<Expr>,
        env: Env,
    },
    Thunk(Rc<RefCell<ThunkState>>),
    Nil,
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expr::Number(a), Expr::Number(b)) => a == b,
            (Expr::Symbol(a), Expr::Symbol(b)) => a == b,
            (Expr::String(a), Expr::String(b)) => a == b,
            (Expr::Bool(a), Expr::Bool(b)) => a == b,
            (Expr::List(a), Expr::List(b)) => a == b,
            (
                Expr::Lambda { params: a_params, body: a_body, .. },
                Expr::Lambda { params: b_params, body: b_body, .. },
            ) => a_params == b_params && a_body == b_body,
            (Expr::Thunk(a), Expr::Thunk(b)) => Rc::ptr_eq(a, b),
            (Expr::Nil, Expr::Nil) => true,
            _ => false,
        }
    }
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
            Expr::Lambda { params, body, .. } => {
                let mut items = Vec::new();
                let params_expr = Expr::List(params.clone());
                items.push("lambda".to_string());
                items.push(params_expr.to_string());
                items.extend(body.iter().map(|e| e.to_string()));
                write!(f, "({})", items.join(" "))
            }
            Expr::Thunk(_) => write!(f, "<thunk>"),
        }
    }
}
