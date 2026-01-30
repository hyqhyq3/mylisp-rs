//! 基于 Trampoline 的尾调用优化实现
//!
//! 关键改进：TCO 内部不递归调用求值器，而是返回 Thunk

use crate::ast::Expr;
use crate::env::Env;
use std::cell::RefCell;
use std::rc::Rc;

/// Trampoline 的中间状态
pub enum Thunk {
    /// 最终结果
    Done(Expr),
    /// 需要继续执行的尾调用
    TailCall {
        func: Expr,
        args: Vec<Expr>,
        env_snapshot: Env,  // 保存环境快照
    },
}

impl Thunk {
    /// 执行 trampoline 循环直到得到最终结果
    ///
    /// # 参数
    /// - `base_env`: 基础环境
    /// - `eval_fn`: 外部求值函数，用于求值非尾位置的表达式
    pub fn trampoline<F>(
        mut self,
        base_env: &mut Env,
        eval_fn: F,
    ) -> Result<Expr, String>
    where
        F: Fn(Expr, &mut Env) -> Result<Expr, String>,
    {
        loop {
            match self {
                Thunk::Done(result) => return Ok(result),
                Thunk::TailCall { func, args, mut env_snapshot } => {
                    // 执行一次尾调用，得到下一个 Thunk
                    self = Self::eval_tail_call_iterative(
                        func,
                        args,
                        &mut env_snapshot,
                        base_env,
                        &eval_fn,
                    )?;
                }
            }
        }
    }

    /// 迭代式地评估一次尾调用（完全非递归版本）
    fn eval_tail_call_iterative<F>(
        func: Expr,
        args: Vec<Expr>,
        env: &mut Env,
        base_env: &Env,
        eval_fn: &F,
    ) -> Result<Thunk, String>
    where
        F: Fn(Expr, &mut Env) -> Result<Expr, String>,
    {
        // 提取 lambda 信息
        let (params, body_exprs, captured_env) = match &func {
            Expr::Lambda { params, body, env: func_env } => {
                (params.clone(), body.clone(), Some(func_env.clone()))
            }
            Expr::List(list) if !list.is_empty() => {
                match &list[0] {
                    Expr::Symbol(s) if s == "lambda" || s == "fn" => {
                        match &list[1] {
                            Expr::List(params) => (params.clone(), list[2..].to_vec(), None),
                            _ => return Err("Invalid lambda parameter list".to_string()),
                        }
                    }
                    _ => return Err("Cannot call non-function".to_string()),
                }
            }
            _ => return Err("Cannot call non-function".to_string()),
        };

        // 检查参数数量
        if params.len() != args.len() {
            return Err(format!(
                "Arity mismatch: expected {}, got {}",
                params.len(),
                args.len()
            ));
        }

        // 绑定参数到环境
        env.data.clear();
        let parent_env = captured_env
            .as_ref()
            .map(|captured| captured.flatten_with_parent(base_env.clone()))
            .unwrap_or_else(|| base_env.clone());
        env.parent = Some(Box::new(parent_env));
        for (param, arg) in params.iter().zip(args.iter()) {
            if let Expr::Symbol(name) = param {
                env.data.insert(name.clone(), arg.clone());
            }
        }

        // 求值 body（最后一个表达式）- 保持尾位置语义
        if let Some(last_expr) = body_exprs.last() {
            return Self::eval_tail_position_expr(last_expr, env, base_env, eval_fn);
        }

        Ok(Thunk::Done(Expr::Nil))
    }

    /// 在尾位置求值 if 表达式
    fn eval_if_in_tail_position<F>(
        args: &[Expr],
        env: &mut Env,
        base_env: &Env,
        eval_fn: &F,
    ) -> Result<Thunk, String>
    where
        F: Fn(Expr, &mut Env) -> Result<Expr, String>,
    {
        if args.len() < 2 || args.len() > 3 {
            return Err("if requires 2 or 3 arguments".to_string());
        }

        // 求值条件
        let condition = Self::eval_simple_with_builtins(&args[0], env, base_env, eval_fn)?;
        let is_true = match condition {
            Expr::Bool(b) => b,
            Expr::Nil => false,
            _ => true,
        };

        if is_true {
            return Self::eval_tail_position_expr(&args[1], env, base_env, eval_fn);
        } else if args.len() == 3 {
            return Self::eval_tail_position_expr(&args[2], env, base_env, eval_fn);
        } else {
            Ok(Thunk::Done(Expr::Nil))
        }
    }

    fn make_thunk(expr: Expr, env: &Env) -> Expr {
        Expr::Thunk(Rc::new(RefCell::new(crate::ast::ThunkState::Unevaluated {
            expr,
            env: env.clone(),
        })))
    }

    fn force_expr<F>(expr: Expr, eval_fn: &F) -> Result<Expr, String>
    where
        F: Fn(Expr, &mut Env) -> Result<Expr, String>,
    {
        let mut current = expr;
        loop {
            match current {
                Expr::Thunk(thunk) => {
                    let mut state = thunk.borrow_mut();
                    match &*state {
                        crate::ast::ThunkState::Evaluated(value) => {
                            current = value.clone();
                            continue;
                        }
                        crate::ast::ThunkState::Unevaluated { expr, env } => {
                            let expr = expr.clone();
                            let mut eval_env = env.clone();
                            drop(state);
                            let value = eval_fn(expr, &mut eval_env)?;
                            let mut state = thunk.borrow_mut();
                            *state = crate::ast::ThunkState::Evaluated(value.clone());
                            current = value;
                            continue;
                        }
                    }
                }
                _ => return Ok(current),
            }
        }
    }

    /// 在尾位置求值任意表达式
    fn eval_tail_position_expr<F>(
        expr: &Expr,
        env: &mut Env,
        base_env: &Env,
        eval_fn: &F,
    ) -> Result<Thunk, String>
    where
        F: Fn(Expr, &mut Env) -> Result<Expr, String>,
    {
        match expr {
            Expr::List(call_list) if !call_list.is_empty() => {
                if let Expr::Symbol(op) = &call_list[0] {
                    match op.as_str() {
                        "if" => {
                            return Self::eval_if_in_tail_position(&call_list[1..], env, base_env, eval_fn);
                        }
                        "+" | "-" | "*" | "/" | "mod" |
                        "=" | "<" | ">" | "<=" | ">=" |
                        "not" | "and" | "or" |
                        "list" | "head" | "car" | "tail" | "cdr" |
                        "cons" | "append" | "length" | "reverse" |
                        "null?" | "symbol?" | "list?" | "number?" | "string?" |
                        "display" | "newline" => {
                            return Ok(Thunk::Done(
                                Self::apply_builtin(expr, env, base_env, eval_fn)?
                            ));
                        }
                        _ => {}
                    }
                }

                let func_candidate = Self::eval_simple_with_builtins(&call_list[0], env, base_env, eval_fn)?;
                let mut evaluated_args = Vec::new();
                for arg in &call_list[1..] {
                    evaluated_args.push(Self::make_thunk(arg.clone(), env));
                }

                match &func_candidate {
                    Expr::Lambda { env: func_env, .. } => {
                        return Ok(Thunk::TailCall {
                            func: func_candidate.clone(),
                            args: evaluated_args,
                            env_snapshot: func_env.clone(),
                        });
                    }
                    Expr::List(inner_list) if !inner_list.is_empty() => {
                        if matches!(&inner_list[0], Expr::Symbol(s) if s == "lambda" || s == "fn") {
                            return Ok(Thunk::TailCall {
                                func: func_candidate,
                                args: evaluated_args,
                                env_snapshot: env.clone(),
                            });
                        }
                    }
                    _ => {}
                }

                Ok(Thunk::Done(
                    Self::apply_non_tail_function(&func_candidate, &evaluated_args, env, base_env, eval_fn)?
                ))
            }
            _ => Ok(Thunk::Done(Self::eval_simple_with_builtins(expr, env, base_env, eval_fn)?)),
        }
    }

    /// 简化的求值函数：只处理字面量、符号查找和内置运算
    fn eval_simple_with_builtins<F>(
        expr: &Expr,
        env: &mut Env,
        base_env: &Env,
        eval_fn: &F,
    ) -> Result<Expr, String>
    where
        F: Fn(Expr, &mut Env) -> Result<Expr, String>,
    {
        match expr {
            Expr::Number(_) | Expr::Bool(_) | Expr::Nil | Expr::String(_) | Expr::Lambda { .. } => {
                Ok(expr.clone())
            }
            Expr::Thunk(_) => Self::force_expr(expr.clone(), eval_fn),

            Expr::Symbol(sym) => {
                // 先在当前环境查找
                if let Some(val) = env.get(sym) {
                    return Self::force_expr(val, eval_fn);
                }
                // 再在基础环境查找
                if let Some(val) = base_env.get(sym) {
                    return Self::force_expr(val, eval_fn);
                }
                Err(format!("Undefined symbol: {}", sym))
            }

            Expr::List(list) if !list.is_empty() => {
                // 检查是否是简单的二元运算
                if list.len() == 3 {
                    if let Expr::Symbol(op) = &list[0] {
                        match op.as_str() {
                            "+" | "-" | "*" | "/" | "mod" |
                            "=" | "<" | ">" | "<=" | ">=" => {
                                let left = Self::eval_simple_with_builtins(&list[1], env, base_env, eval_fn)?;
                                let right = Self::eval_simple_with_builtins(&list[2], env, base_env, eval_fn)?;
                                return Self::apply_binary_op(op, &left, &right);
                            }
                            _ => {}
                        }
                    }
                }
                // 对于复杂表达式，使用外部求值器
                eval_fn(expr.clone(), env)
            }

            Expr::List(_) => Ok(Expr::List(vec![])),
        }
    }

    /// 应用二元运算符
    fn apply_binary_op(op: &str, left: &Expr, right: &Expr) -> Result<Expr, String> {
        let l = match left {
            Expr::Number(n) => *n,
            _ => return Err(format!("{} requires numbers", op)),
        };
        let r = match right {
            Expr::Number(n) => *n,
            _ => return Err(format!("{} requires numbers", op)),
        };

        let result = match op {
            "+" => l + r,
            "-" => l - r,
            "*" => l * r,
            "/" => l / r,
            "mod" => l % r,
            "=" => {
                return Ok(Expr::Bool((l - r).abs() < 1e-10));
            }
            "<" => {
                return Ok(Expr::Bool(l < r));
            }
            ">" => {
                return Ok(Expr::Bool(l > r));
            }
            "<=" => {
                return Ok(Expr::Bool(l <= r));
            }
            ">=" => {
                return Ok(Expr::Bool(l >= r));
            }
            _ => return Err(format!("Unknown operator: {}", op)),
        };

        Ok(Expr::Number(result))
    }

    /// 应用内置函数
    fn apply_builtin<F>(
        call_list: &Expr,
        env: &mut Env,
        base_env: &Env,
        eval_fn: &F,
    ) -> Result<Expr, String>
    where
        F: Fn(Expr, &mut Env) -> Result<Expr, String>,
    {
        if let Expr::List(list) = call_list {
            if let Expr::Symbol(op) = &list[0] {
                match op.as_str() {
                    "+" | "-" | "*" | "/" | "mod" |
                    "=" | "<" | ">" | "<=" | ">=" => {
                        let mut evaluated = Vec::new();
                        for arg in &list[1..] {
                            evaluated.push(Self::eval_simple_with_builtins(arg, env, base_env, eval_fn)?);
                        }
                        return Self::apply_builtin_arith(op, &evaluated);
                    }
                    "not" => {
                        if list.len() != 2 {
                            return Err("not requires 1 argument".to_string());
                        }
                        let arg = Self::eval_simple_with_builtins(&list[1], env, base_env, eval_fn)?;
                        return Ok(Expr::Bool(!Self::is_truthy(&arg)));
                    }
                    _ => {}
                }
            }
        }
        eval_fn(call_list.clone(), env)
    }

    /// 应用内置算术/比较函数
    fn apply_builtin_arith(op: &str, args: &[Expr]) -> Result<Expr, String> {
        match op {
            "+" => {
                let sum: f64 = args.iter().map(|a| match a {
                    Expr::Number(n) => Ok(*n),
                    _ => Err("+ expects numbers".to_string()),
                }).collect::<Result<Vec<_>, _>>()?.iter().sum();
                Ok(Expr::Number(sum))
            }
            "-" => {
                if args.is_empty() {
                    return Err("- requires at least 1 argument".to_string());
                }
                let first = match &args[0] {
                    Expr::Number(n) => *n,
                    _ => return Err("- expects numbers".to_string()),
                };
                let rest: f64 = args[1..].iter().map(|a| match a {
                    Expr::Number(n) => Ok(*n),
                    _ => Err("- expects numbers".to_string()),
                }).collect::<Result<Vec<_>, _>>()?.iter().sum();
                Ok(Expr::Number(first - rest))
            }
            "*" => {
                let product: f64 = args.iter().map(|a| match a {
                    Expr::Number(n) => Ok(*n),
                    _ => Err("* expects numbers".to_string()),
                }).collect::<Result<Vec<_>, _>>()?.iter().product();
                Ok(Expr::Number(product))
            }
            "/" => {
                if args.is_empty() {
                    return Err("/ requires at least 1 argument".to_string());
                }
                let first = match &args[0] {
                    Expr::Number(n) => *n,
                    _ => return Err("/ expects numbers".to_string()),
                };
                if args.len() == 1 {
                    return Ok(Expr::Number(1.0 / first));
                }
                let rest: f64 = args[1..].iter().map(|a| match a {
                    Expr::Number(n) => Ok(*n),
                    _ => Err("/ expects numbers".to_string()),
                }).collect::<Result<Vec<_>, _>>()?.iter().product();
                Ok(Expr::Number(first / rest))
            }
            "=" => {
                if args.len() < 2 {
                    return Ok(Expr::Bool(true));
                }
                let first = match &args[0] {
                    Expr::Number(n) => *n,
                    _ => return Err("= expects numbers".to_string()),
                };
                for arg in &args[1..] {
                    match arg {
                        Expr::Number(n) if (*n - first).abs() < 1e-10 => continue,
                        Expr::Number(_) => return Ok(Expr::Bool(false)),
                        _ => return Err("= expects numbers".to_string()),
                    }
                }
                Ok(Expr::Bool(true))
            }
            _ => Err(format!("Operator {} not implemented", op)),
        }
    }

    /// 判断表达式是否为真
    fn is_truthy(expr: &Expr) -> bool {
        match expr {
            Expr::Bool(false) | Expr::Nil => false,
            _ => true,
        }
    }

    /// 应用非尾调用函数
    fn apply_non_tail_function<F>(
        func: &Expr,
        args: &[Expr],
        env: &mut Env,
        base_env: &Env,
        eval_fn: &F,
    ) -> Result<Expr, String>
    where
        F: Fn(Expr, &mut Env) -> Result<Expr, String>,
    {
        match func {
            Expr::Lambda { params, body, env: func_env } => {
                if params.len() != args.len() {
                    return Err(format!(
                        "Arity mismatch: expected {}, got {}",
                        params.len(),
                        args.len()
                    ));
                }

                let merged_env = func_env.flatten_with_parent(base_env.clone());
                let mut call_env = Env::with_parent(merged_env);
                for (param, arg) in params.iter().zip(args.iter()) {
                    if let Expr::Symbol(name) = param {
                        call_env.data.insert(name.clone(), arg.clone());
                    }
                }

                let mut result = Ok(Expr::Nil);
                for expr in body {
                    result = Self::eval_simple_with_builtins(expr, &mut call_env, base_env, eval_fn);
                }

                result
            }
            Expr::List(list) => {
                if list.len() >= 3
                    && matches!(&list[0], Expr::Symbol(s) if s == "lambda" || s == "fn")
                {
                    match &list[1] {
                        Expr::List(params) => {
                            if params.len() != args.len() {
                                return Err(format!(
                                    "Arity mismatch: expected {}, got {}",
                                    params.len(),
                                    args.len()
                                ));
                            }

                            // 创建新的函数环境
                            let mut func_env = Env::with_parent(base_env.clone());
                            func_env.data = env.data.clone();

                            // 绑定参数
                            for (param, arg) in params.iter().zip(args.iter()) {
                                if let Expr::Symbol(name) = param {
                                    func_env.data.insert(name.clone(), arg.clone());
                                }
                            }

                            // 求值 body
                            let mut result = Ok(Expr::Nil);
                            for expr in &list[2..] {
                                result = Self::eval_simple_with_builtins(expr, &mut func_env, base_env, eval_fn);
                            }

                            result
                        }
                        _ => Err("Invalid lambda parameter list".to_string()),
                    }
                } else {
                    Err("Cannot call non-function".to_string())
                }
            }
            _ => Err("Cannot call non-function".to_string()),
        }
    }
}

// TCO 单元测试
#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval_impl::Evaluator;

    /// 辅助函数：创建数字表达式
    fn num(n: f64) -> Expr {
        Expr::Number(n)
    }

    /// 辅助函数：创建符号表达式
    fn sym(s: &str) -> Expr {
        Expr::Symbol(s.to_string())
    }

    /// 辅助函数：创建列表表达式
    fn list(items: Vec<Expr>) -> Expr {
        Expr::List(items)
    }

    /// 辅助函数：创建 lambda 表达式
    fn lambda(params: Vec<Expr>, body: Vec<Expr>) -> Expr {
        let mut lambda_list = vec![sym("lambda"), list(params)];
        lambda_list.extend(body);
        list(lambda_list)
    }

    #[test]
    fn test_thunk_done_creation() {
        let expr = num(42.0);
        let thunk = Thunk::Done(expr.clone());
        match thunk {
            Thunk::Done(e) => assert_eq!(e, num(42.0)),
            _ => panic!("Expected Thunk::Done"),
        }
    }

    #[test]
    fn test_simple_tail_call_trampoline() {
        // 测试简单的尾调用: (lambda (x) x) 42
        let mut env = Env::new();

        let func = lambda(vec![sym("x")], vec![sym("x")]);
        let args = vec![num(42.0)];

        let thunk = Thunk::TailCall {
            func,
            args,
            env_snapshot: env.clone(),
        };

        let result = thunk.trampoline(&mut env, |expr, env| {
            Evaluator::eval(expr, env)
        });

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), num(42.0));
    }

    #[test]
    fn test_tail_recursion_factorial() {
        // 测试尾递归阶乘
        let mut env = Env::new();

        let fact_tail = lambda(
            vec![sym("n"), sym("acc")],
            vec![
                list(vec![
                    sym("if"),
                    list(vec![sym("="), sym("n"), num(0.0)]),
                    sym("acc"),
                    list(vec![
                        sym("fact-tail"),
                        list(vec![sym("-"), sym("n"), num(1.0)]),
                        list(vec![sym("*"), sym("n"), sym("acc")]),
                    ]),
                ]),
            ],
        );

        env.define("fact-tail".to_string(), fact_tail);

        let thunk = Thunk::TailCall {
            func: env.get("fact-tail").unwrap(),
            args: vec![num(5.0), num(1.0)],
            env_snapshot: env.clone(),
        };

        let result = thunk.trampoline(&mut env, |expr, env| {
            Evaluator::eval(expr, env)
        });

        if let Err(ref e) = result {
            eprintln!("Error in factorial test: {}", e);
        }
        assert!(result.is_ok(), "Factorial test failed: {:?}", result);
    }

    #[test]
    fn test_deep_tail_recursion() {
        // 测试深度尾递归（1000 次）
        let mut env = Env::new();

        let sum_tail = lambda(
            vec![sym("n"), sym("acc")],
            vec![
                list(vec![
                    sym("if"),
                    list(vec![sym("="), sym("n"), num(0.0)]),
                    sym("acc"),
                    list(vec![
                        sym("sum-tail"),
                        list(vec![sym("-"), sym("n"), num(1.0)]),
                        list(vec![sym("+"), sym("acc"), sym("n")]),
                    ]),
                ]),
            ],
        );

        env.define("sum-tail".to_string(), sum_tail);

        let thunk = Thunk::TailCall {
            func: env.get("sum-tail").unwrap(),
            args: vec![num(1000.0), num(0.0)],
            env_snapshot: env.clone(),
        };

        let result = thunk.trampoline(&mut env, |expr, env| {
            Evaluator::eval(expr, env)
        });

        if let Err(ref e) = result {
            eprintln!("Error in deep recursion test: {}", e);
        }
        assert!(result.is_ok(), "Deep recursion test failed: {:?}", result);
    }

    #[test]
    fn test_very_deep_tail_recursion() {
        // 测试超深度尾递归（10000 次）
        let mut env = Env::new();

        let sum_tail = lambda(
            vec![sym("n"), sym("acc")],
            vec![
                list(vec![
                    sym("if"),
                    list(vec![sym("="), sym("n"), num(0.0)]),
                    sym("acc"),
                    list(vec![
                        sym("sum-tail"),
                        list(vec![sym("-"), sym("n"), num(1.0)]),
                        list(vec![sym("+"), sym("acc"), sym("n")]),
                    ]),
                ]),
            ],
        );

        env.define("sum-tail".to_string(), sum_tail);

        let thunk = Thunk::TailCall {
            func: env.get("sum-tail").unwrap(),
            args: vec![num(10000.0), num(0.0)],
            env_snapshot: env.clone(),
        };

        let result = thunk.trampoline(&mut env, |expr, env| {
            Evaluator::eval(expr, env)
        });

        if let Err(ref e) = result {
            eprintln!("Error in very deep recursion test: {}", e);
        }
        assert!(result.is_ok(), "Very deep recursion test failed: {:?}", result);
    }
}
