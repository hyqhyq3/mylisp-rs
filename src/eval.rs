use crate::ast::Expr;
use crate::env::Env;
use std::rc::Rc;
use std::cell::RefCell;

pub struct Evaluator;

// 用户定义的函数类型
type UserFunction = Rc<RefCell<dyn FnMut(Vec<Expr>, &mut Env) -> Result<Expr, String>>>;

// 全局用户函数注册表
thread_local! {
    static USER_FUNCTIONS: RefCell<std::collections::HashMap<String, UserFunction>> =
        RefCell::new(std::collections::HashMap::new());
}

pub fn register_user_function(name: String, func: UserFunction) {
    USER_FUNCTIONS.with(|f| f.borrow_mut().insert(name, func));
}

pub fn call_user_function(name: &str, args: Vec<Expr>, env: &mut Env) -> Result<Expr, String> {
    USER_FUNCTIONS.with(|f| {
        let mut functions = f.borrow_mut();
        if let Some(func) = functions.get_mut(name) {
            func.borrow_mut()(args, env)
        } else {
            Err(format!("User function '{}' not found", name))
        }
    })
}

impl Evaluator {
    pub fn eval(expr: Expr, env: &mut Env) -> Result<Expr, String> {
        match expr {
            Expr::Number(_) | Expr::Bool(_) | Expr::Nil | Expr::String(_) => Ok(expr),

            Expr::Symbol(ref sym) => {
                env.get(sym).ok_or_else(|| format!("Undefined symbol: {}", sym))
            }

            Expr::List(list) => {
                if list.is_empty() {
                    Ok(Expr::List(list))
                } else {
                    let first = &list[0];
                    match first {
                        Expr::Symbol(op) => match op.as_str() {
                            "define" => Self::eval_define(&list[1..], env),
                            "set!" => Self::eval_set(&list[1..], env),
                            "if" => Self::eval_if(&list[1..], env),
                            "lambda" | "fn" => Self::eval_lambda(&list[1..], env),
                            "let" => Self::eval_let(&list[1..], env),
                            "quote" => Self::eval_quote(&list[1..]),
                            "eval" => Self::eval_eval(&list[1..], env),
                            "load" => Self::eval_load(&list[1..], env),
                            _ => Self::eval_function_call(list, env),
                        },
                        _ => Self::eval_function_call(list, env),
                    }
                }
            }
        }
    }

    fn eval_define(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
        if args.len() != 2 {
            return Err("define requires exactly 2 arguments".to_string());
        }

        match &args[0] {
            Expr::Symbol(name) => {
                let value = Self::eval(args[1].clone(), env)?;
                env.define(name.clone(), value);
                Ok(Expr::Nil)
            }
            Expr::List(list) if !list.is_empty() => {
                // 支持 (define (f x y) body) 语法
                match &list[0] {
                    Expr::Symbol(name) => {
                        let params = list[1..].to_vec();
                        let body = args[1].clone();
                        let lambda = Expr::List(vec![
                            Expr::Symbol("lambda".to_string()),
                            Expr::List(params),
                            body,
                        ]);
                        let value = Self::eval(lambda, env)?;
                        env.define(name.clone(), value);
                        Ok(Expr::Nil)
                    }
                    _ => Err("define: function name must be a symbol".to_string()),
                }
            }
            _ => Err("define: first argument must be a symbol or list".to_string()),
        }
    }

    fn eval_set(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
        if args.len() != 2 {
            return Err("set! requires exactly 2 arguments".to_string());
        }

        match &args[0] {
            Expr::Symbol(name) => {
                let value = Self::eval(args[1].clone(), env)?;
                env.set(name, value)?;
                Ok(Expr::Nil)
            }
            _ => Err("set!: first argument must be a symbol".to_string()),
        }
    }

    fn eval_if(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
        if args.len() < 2 || args.len() > 3 {
            return Err("if requires 2 or 3 arguments".to_string());
        }

        let condition = Self::eval(args[0].clone(), env)?;
        let is_true = match condition {
            Expr::Bool(b) => b,
            Expr::Nil => false,
            _ => true,
        };

        if is_true {
            Self::eval(args[1].clone(), env)
        } else if args.len() == 3 {
            Self::eval(args[2].clone(), env)
        } else {
            Ok(Expr::Nil)
        }
    }

    fn eval_lambda(args: &[Expr], _env: &mut Env) -> Result<Expr, String> {
        if args.len() < 2 {
            return Err("lambda requires at least 2 arguments".to_string());
        }

        match &args[0] {
            Expr::List(params) => {
                // lambda 表达式: (lambda (params) body1 body2 ...)
                // 存储为: [lambda, params, body1, body2, ...]
                let mut lambda_expr = vec![
                    Expr::Symbol("lambda".to_string()),
                    Expr::List(params.clone()),
                ];
                lambda_expr.extend_from_slice(&args[1..]);
                Ok(Expr::List(lambda_expr))
            }
            _ => Err("lambda: first argument must be a parameter list".to_string()),
        }
    }

    fn eval_let(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
        if args.len() < 2 {
            return Err("let requires at least 2 arguments".to_string());
        }

        match &args[0] {
            Expr::List(bindings) => {
                let mut let_env = Env::with_parent(env.clone());

                for binding in bindings {
                    match binding {
                        Expr::List(pair) if pair.len() == 2 => {
                            match &pair[0] {
                                Expr::Symbol(name) => {
                                    let value = Self::eval(pair[1].clone(), env)?;
                                    let_env.define(name.clone(), value);
                                }
                                _ => {
                                    return Err("let: binding names must be symbols".to_string())
                                }
                            }
                        }
                        _ => return Err("let: bindings must be (name value) pairs".to_string()),
                    }
                }

                let mut result = Ok(Expr::Nil);
                for expr in &args[1..] {
                    result = Self::eval(expr.clone(), &mut let_env);
                }
                result
            }
            _ => Err("let: first argument must be a list of bindings".to_string()),
        }
    }

    fn eval_quote(args: &[Expr]) -> Result<Expr, String> {
        if args.len() != 1 {
            return Err("quote requires exactly 1 argument".to_string());
        }
        Ok(args[0].clone())
    }

    fn eval_eval(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
        if args.len() != 1 {
            return Err("eval requires exactly 1 argument".to_string());
        }
        let evaluated = Self::eval(args[0].clone(), env)?;
        Self::eval(evaluated, env)
    }

    fn eval_load(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
        if args.len() != 1 {
            return Err("load requires exactly 1 argument".to_string());
        }

        match &args[0] {
            Expr::Symbol(filename) => {
                use crate::lexer::Lexer;
                use crate::parser::Parser;

                let content = std::fs::read_to_string(filename)
                    .map_err(|e| format!("Failed to read file '{}': {}", filename, e))?;

                let mut result = Ok(Expr::Nil);
                for line in content.lines() {
                    let line = line.trim();
                    if !line.is_empty() && !line.starts_with(';') {
                        let lexer = Lexer::new(line);
                        let mut parser = Parser::new(lexer);
                        if let Ok(expr) = parser.parse() {
                            result = Self::eval(expr, env);
                        }
                    }
                }
                result
            }
            _ => Err("load: filename must be a string/symbol".to_string()),
        }
    }

    fn eval_function_call(args: Vec<Expr>, env: &mut Env) -> Result<Expr, String> {
        let evaluated_args: Result<Vec<Expr>, String> =
            args.iter().skip(1).map(|a| Self::eval(a.clone(), env)).collect();

        let evaluated_args = evaluated_args?;

        // Handle built-in operations
        match &args[0] {
            Expr::Symbol(op) => match op.as_str() {
                "+" => Self::apply_add(&evaluated_args),
                "-" => Self::apply_sub(&evaluated_args),
                "*" => Self::apply_mul(&evaluated_args),
                "/" => Self::apply_div(&evaluated_args),
                ">" => Self::apply_gt(&evaluated_args),
                ">=" => Self::apply_ge(&evaluated_args),
                "<" => Self::apply_lt(&evaluated_args),
                "<=" => Self::apply_le(&evaluated_args),
                "=" => Self::apply_eq(&evaluated_args),
                "not" => Self::apply_not(&evaluated_args),
                "and" => Self::apply_and(&evaluated_args, env),
                "or" => Self::apply_or(&evaluated_args, env),
                "list" => Ok(Expr::List(evaluated_args)),
                "head" | "car" => Self::apply_head(&evaluated_args),
                "tail" | "cdr" => Self::apply_tail(&evaluated_args),
                "cons" => Self::apply_cons(&evaluated_args),
                "append" => Self::apply_append(&evaluated_args),
                "eq?" => Self::apply_eq(&evaluated_args),
                "null?" => Self::apply_null(&evaluated_args),
                "symbol?" => Self::apply_symbol(&evaluated_args),
                "list?" => Self::apply_list(&evaluated_args),
                "number?" => Self::apply_number(&evaluated_args),
                "string?" => Self::apply_string(&evaluated_args),
                "display" => Self::apply_display(&evaluated_args),
                "newline" => Self::apply_newline(),
                _ => {
                    let func = Self::eval(args[0].clone(), env)?;
                    Self::apply_user_function(func, evaluated_args, env)
                }
            },
            _ => {
                let func = Self::eval(args[0].clone(), env)?;
                Self::apply_user_function(func, evaluated_args, env)
            }
        }
    }

    fn apply_user_function(func: Expr, args: Vec<Expr>, env: &mut Env) -> Result<Expr, String> {
        match func {
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

                            let mut func_env = Env::with_parent(env.clone());

                            for (param, arg) in params.iter().zip(args.iter()) {
                                match param {
                                    Expr::Symbol(name) => {
                                        func_env.define(name.clone(), arg.clone());
                                    }
                                    _ => {}
                                }
                            }

                            let mut result = Ok(Expr::Nil);
                            for expr in &list[2..] {
                                result = Self::eval(expr.clone(), &mut func_env);
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

    fn apply_add(args: &[Expr]) -> Result<Expr, String> {
        let result: f64 = args
            .iter()
            .map(|a| match a {
                Expr::Number(n) => Ok(*n),
                _ => Err("+ expects numbers".to_string()),
            })
            .collect::<Result<Vec<f64>, String>>()?
            .iter()
            .sum();

        Ok(Expr::Number(result))
    }

    fn apply_sub(args: &[Expr]) -> Result<Expr, String> {
        if args.is_empty() {
            return Err("- requires at least 1 argument".to_string());
        }

        let first = match &args[0] {
            Expr::Number(n) => *n,
            _ => return Err("- expects numbers".to_string()),
        };

        let result: f64 = args[1..]
            .iter()
            .map(|a| match a {
                Expr::Number(n) => Ok(*n),
                _ => Err("- expects numbers".to_string()),
            })
            .collect::<Result<Vec<f64>, String>>()?
            .iter()
            .fold(first, |acc, x| acc - x);

        Ok(Expr::Number(result))
    }

    fn apply_mul(args: &[Expr]) -> Result<Expr, String> {
        let result: f64 = args
            .iter()
            .map(|a| match a {
                Expr::Number(n) => Ok(*n),
                _ => Err("* expects numbers".to_string()),
            })
            .collect::<Result<Vec<f64>, String>>()?
            .iter()
            .product();

        Ok(Expr::Number(result))
    }

    fn apply_div(args: &[Expr]) -> Result<Expr, String> {
        if args.is_empty() {
            return Err("/ requires at least 1 argument".to_string());
        }

        let first = match &args[0] {
            Expr::Number(n) => *n,
            _ => return Err("/ expects numbers".to_string()),
        };

        let result: f64 = args[1..]
            .iter()
            .map(|a| match a {
                Expr::Number(n) if *n != 0.0 => Ok(*n),
                Expr::Number(_) => Err("Division by zero".to_string()),
                _ => Err("/ expects numbers".to_string()),
            })
            .collect::<Result<Vec<f64>, String>>()?
            .iter()
            .fold(first, |acc, x| acc / x);

        Ok(Expr::Number(result))
    }

    fn apply_gt(args: &[Expr]) -> Result<Expr, String> {
        if args.len() != 2 {
            return Err("> requires exactly 2 arguments".to_string());
        }

        match (&args[0], &args[1]) {
            (Expr::Number(a), Expr::Number(b)) => Ok(Expr::Bool(a > b)),
            _ => Err("> expects numbers".to_string()),
        }
    }

    fn apply_lt(args: &[Expr]) -> Result<Expr, String> {
        if args.len() != 2 {
            return Err("< requires exactly 2 arguments".to_string());
        }

        match (&args[0], &args[1]) {
            (Expr::Number(a), Expr::Number(b)) => Ok(Expr::Bool(a < b)),
            _ => Err("< expects numbers".to_string()),
        }
    }

    fn apply_le(args: &[Expr]) -> Result<Expr, String> {
        if args.len() != 2 {
            return Err("<= requires exactly 2 arguments".to_string());
        }

        match (&args[0], &args[1]) {
            (Expr::Number(a), Expr::Number(b)) => Ok(Expr::Bool(a <= b)),
            _ => Err("<= expects numbers".to_string()),
        }
    }

    fn apply_ge(args: &[Expr]) -> Result<Expr, String> {
        if args.len() != 2 {
            return Err(">= requires exactly 2 arguments".to_string());
        }

        match (&args[0], &args[1]) {
            (Expr::Number(a), Expr::Number(b)) => Ok(Expr::Bool(a >= b)),
            _ => Err(">= expects numbers".to_string()),
        }
    }

    fn apply_eq(args: &[Expr]) -> Result<Expr, String> {
        if args.len() != 2 {
            return Err("= requires exactly 2 arguments".to_string());
        }

        let result = match (&args[0], &args[1]) {
            (Expr::Number(a), Expr::Number(b)) => a == b,
            (Expr::Bool(a), Expr::Bool(b)) => a == b,
            (Expr::Nil, Expr::Nil) => true,
            (Expr::Symbol(a), Expr::Symbol(b)) => a == b,
            _ => false,
        };

        Ok(Expr::Bool(result))
    }

    fn apply_not(args: &[Expr]) -> Result<Expr, String> {
        if args.len() != 1 {
            return Err("not requires exactly 1 argument".to_string());
        }

        let result = match &args[0] {
            Expr::Bool(b) => !b,
            Expr::Nil => true,
            _ => false,
        };

        Ok(Expr::Bool(result))
    }

    fn apply_and(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
        for arg in args {
            let result = Self::eval(arg.clone(), env)?;
            match result {
                Expr::Bool(false) => return Ok(Expr::Bool(false)),
                Expr::Nil => return Ok(Expr::Bool(false)),
                _ => continue,
            }
        }
        Ok(Expr::Bool(true))
    }

    fn apply_or(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
        for arg in args {
            let result = Self::eval(arg.clone(), env)?;
            match result {
                Expr::Bool(true) => return Ok(Expr::Bool(true)),
                Expr::Nil => continue,
                _ => return Ok(Expr::Bool(true)),
            }
        }
        Ok(Expr::Bool(false))
    }

    fn apply_head(args: &[Expr]) -> Result<Expr, String> {
        if args.len() != 1 {
            return Err("head requires exactly 1 argument".to_string());
        }

        match &args[0] {
            Expr::List(list) if !list.is_empty() => Ok(list[0].clone()),
            Expr::List(_) => Err("head: empty list".to_string()),
            _ => Err("head expects a list".to_string()),
        }
    }

    fn apply_tail(args: &[Expr]) -> Result<Expr, String> {
        if args.len() != 1 {
            return Err("tail requires exactly 1 argument".to_string());
        }

        match &args[0] {
            Expr::List(list) if !list.is_empty() => Ok(Expr::List(list[1..].to_vec())),
            Expr::List(_) => Err("tail: empty list".to_string()),
            _ => Err("tail expects a list".to_string()),
        }
    }

    fn apply_cons(args: &[Expr]) -> Result<Expr, String> {
        if args.len() != 2 {
            return Err("cons requires exactly 2 arguments".to_string());
        }

        match &args[1] {
            Expr::List(list) => {
                let mut new_list = vec![args[0].clone()];
                new_list.extend(list.clone());
                Ok(Expr::List(new_list))
            }
            Expr::Nil => Ok(Expr::List(vec![args[0].clone()])),
            _ => Err("cons: second argument must be a list".to_string()),
        }
    }

    fn apply_append(args: &[Expr]) -> Result<Expr, String> {
        let mut result = Vec::new();

        for arg in args {
            match arg {
                Expr::List(list) => result.extend(list.clone()),
                _ => return Err("append expects lists".to_string()),
            }
        }

        Ok(Expr::List(result))
    }

    fn apply_null(args: &[Expr]) -> Result<Expr, String> {
        if args.len() != 1 {
            return Err("null? requires exactly 1 argument".to_string());
        }
        match &args[0] {
            Expr::Nil => Ok(Expr::Bool(true)),
            Expr::List(list) if list.is_empty() => Ok(Expr::Bool(true)),
            _ => Ok(Expr::Bool(false)),
        }
    }

    fn apply_symbol(args: &[Expr]) -> Result<Expr, String> {
        if args.len() != 1 {
            return Err("symbol? requires exactly 1 argument".to_string());
        }
        match &args[0] {
            Expr::Symbol(_) => Ok(Expr::Bool(true)),
            _ => Ok(Expr::Bool(false)),
        }
    }

    fn apply_list(args: &[Expr]) -> Result<Expr, String> {
        if args.len() != 1 {
            return Err("list? requires exactly 1 argument".to_string());
        }
        match &args[0] {
            Expr::List(_) => Ok(Expr::Bool(true)),
            _ => Ok(Expr::Bool(false)),
        }
    }

    fn apply_number(args: &[Expr]) -> Result<Expr, String> {
        if args.len() != 1 {
            return Err("number? requires exactly 1 argument".to_string());
        }
        match &args[0] {
            Expr::Number(_) => Ok(Expr::Bool(true)),
            _ => Ok(Expr::Bool(false)),
        }
    }

    fn apply_string(args: &[Expr]) -> Result<Expr, String> {
        if args.len() != 1 {
            return Err("string? requires exactly 1 argument".to_string());
        }
        match &args[0] {
            Expr::String(_) => Ok(Expr::Bool(true)),
            _ => Ok(Expr::Bool(false)),
        }
    }

    fn apply_display(args: &[Expr]) -> Result<Expr, String> {
        for arg in args {
            match arg {
                Expr::String(s) => print!("{}", s),
                _ => print!("{}", arg),
            }
        }
        Ok(Expr::Nil)
    }

    fn apply_newline() -> Result<Expr, String> {
        println!();
        Ok(Expr::Nil)
    }
}
