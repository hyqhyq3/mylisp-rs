//! 支持 TCO 的环境实现
//!
//! 这个实现使用 Rc<RefCell<>> 来共享环境链，避免克隆

use crate::ast::Expr;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Clone)]
pub struct Env {
    data: Rc<RefCell<HashMap<String, Expr>>>,
    parent: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            data: Rc::new(RefCell::new(HashMap::new())),
            parent: None,
        }
    }

    pub fn with_parent(parent: &Env) -> Self {
        Env {
            data: Rc::new(RefCell::new(HashMap::new())),
            parent: Some(Rc::new(RefCell::new(parent.clone()))),
        }
    }

    pub fn define(&mut self, key: String, value: Expr) {
        self.data.borrow_mut().insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<Expr> {
        if let Some(value) = self.data.borrow().get(key) {
            Some(value.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().get(key)
        } else {
            None
        }
    }

    pub fn set(&mut self, key: &str, value: Expr) -> Result<(), String> {
        if self.data.borrow().contains_key(key) {
            self.data.borrow_mut().insert(key.to_string(), value);
            Ok(())
        } else if let Some(parent) = &self.parent {
            parent.borrow_mut().set(key, value)
        } else {
            Err(format!("Undefined symbol: {}", key))
        }
    }

    /// 重新绑定参数（用于 TCO）
    pub fn rebind(&mut self, params: Vec<Expr>, args: Vec<Expr>) {
        self.data.borrow_mut().clear();
        for (param, arg) in params.iter().zip(args.iter()) {
            if let Expr::Symbol(name) = param {
                self.data.borrow_mut().insert(name.clone(), arg.clone());
            }
        }
    }
}
