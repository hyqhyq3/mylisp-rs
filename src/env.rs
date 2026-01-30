use crate::ast::Expr;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Env {
    pub data: HashMap<String, Expr>,
    pub parent: Option<Box<Env>>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            data: HashMap::new(),
            parent: None,
        }
    }

    pub fn with_parent(parent: Env) -> Self {
        Env {
            data: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn define(&mut self, key: String, value: Expr) {
        self.data.insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<Expr> {
        if let Some(value) = self.data.get(key) {
            Some(value.clone())
        } else if let Some(parent) = &self.parent {
            parent.get(key)
        } else {
            None
        }
    }

    pub fn set(&mut self, key: &str, value: Expr) -> Result<(), String> {
        if self.data.contains_key(key) {
            self.data.insert(key.to_string(), value);
            Ok(())
        } else if let Some(parent) = &mut self.parent {
            parent.set(key, value)
        } else {
            Err(format!("Undefined symbol: {}", key))
        }
    }

    pub fn flatten_with_parent(&self, parent: Env) -> Env {
        let mut data = HashMap::new();
        let mut current = Some(self);

        while let Some(env) = current {
            for (key, value) in env.data.iter() {
                if !data.contains_key(key) {
                    data.insert(key.clone(), value.clone());
                }
            }
            current = env.parent.as_deref();
        }

        Env {
            data,
            parent: Some(Box::new(parent)),
        }
    }
}
