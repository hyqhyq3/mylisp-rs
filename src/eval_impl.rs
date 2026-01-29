use crate::ast::Expr;
use crate::env::Env;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

pub struct Evaluator;

// 尾调用上下文:用于尾递归优化
enum TailContext {
    // 非尾位置
    NonTail,
    // 尾位置:需要优化的尾调用
    Tail {
        func: Expr,
        args: Vec<Expr>,
        env: Env,
    },
}

// 宏定义:使用 syntax-rules
#[derive(Debug, Clone)]
struct Macro {
    name: String,
    rules: Vec<MacroRule>,
}

#[derive(Debug, Clone)]
struct MacroRule {
    pattern: Expr,     // 模式模式
    template: Expr,    // 输出模板
    literals: Vec<String>,  // 字面量标识符
}

// 模式变量绑定:支持多个值的列表(用于 ...)
#[derive(Debug, Clone)]
enum PatternBinding {
    Single(Expr),
    Multiple(Vec<Expr>),
}

// 用户定义的函数类型
type UserFunction = Rc<RefCell<dyn FnMut(Vec<Expr>, &mut Env) -> Result<Expr, String>>>;

// 全局用户函数注册表
thread_local! {
    static USER_FUNCTIONS: RefCell<std::collections::HashMap<String, UserFunction>> =
        RefCell::new(std::collections::HashMap::new());

    // 全局宏注册表
    static MACROS: RefCell<HashMap<String, Macro>> =
        RefCell::new(HashMap::new());
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

// 注册宏
fn register_macro(name: String, macro_def: Macro) {
    MACROS.with(|m| m.borrow_mut().insert(name, macro_def));
}

// 查找宏
fn lookup_macro(name: &str) -> Option<Macro> {
    MACROS.with(|m| m.borrow().get(name).cloned())
}

impl Evaluator {
    // 主求值函数:支持尾调用优化
    pub fn eval(expr: Expr, env: &mut Env) -> Result<Expr, String> {
        Self::eval_with_tail_context(expr, env, false)
    }

    // 带尾调用上下文的求值函数
    fn eval_with_tail_context(expr: Expr, env: &mut Env, is_tail: bool) -> Result<Expr, String> {
        // 首先展开宏
        let expr = Self::expand_macros(expr)?;

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
                            // 基础特殊形式 - 使用新的 SpecialForms 模块
                            "define" => Self::eval_define_wrapper(&list[1..], env),
                            "set!" => Self::eval_set_wrapper(&list[1..], env),
                            "if" => Self::eval_if_wrapper(&list[1..], env, is_tail),
                            "quote" => Self::eval_quote_wrapper(&list[1..]),

                            // 复杂特殊形式 - 使用新的 SpecialForms 模块
                            "lambda" | "fn" => Self::eval_lambda_wrapper(&list[1..]),
                            "let" => Self::eval_let_wrapper(&list[1..], env, is_tail),
                            "cond" => Self::eval_cond_wrapper(&list[1..], env, is_tail),
                            "begin" => Self::eval_begin_wrapper(&list[1..], env, is_tail),

                            // I/O 特殊形式 - 使用新的 SpecialForms 模块
                            "eval" => Self::eval_eval_wrapper(&list[1..], env),
                            "load" => Self::eval_load_wrapper(&list[1..], env),

                            // 宏系统 - 保留在原处（后续拆分）
                            "define-syntax" => Self::eval_define_syntax(&list[1..], env),

                            // 函数调用
                            _ => Self::eval_function_call(list, env, is_tail),
                        },
                        _ => Self::eval_function_call(list, env, is_tail),
                    }
                }
            }
        }
    }

    // ========== SpecialForms 包装函数 ==========
    // 这些函数将 Evaluator::eval 转换为符合 SpecialForms API 的闭包

    fn eval_define_wrapper(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
        crate::eval::special_forms::SpecialForms::eval_define(args, env, |expr, env| {
            Self::eval(expr, env).map_err(|e| crate::eval::error::MyLispError::runtime(e))
        })
        .map_err(|e| e.to_string())
    }

    fn eval_set_wrapper(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
        crate::eval::special_forms::SpecialForms::eval_set(args, env, |expr, env| {
            Self::eval(expr, env).map_err(|e| crate::eval::error::MyLispError::runtime(e))
        })
        .map_err(|e| e.to_string())
    }

    fn eval_if_wrapper(args: &[Expr], env: &mut Env, is_tail: bool) -> Result<Expr, String> {
        crate::eval::special_forms::SpecialForms::eval_if(args, env, |expr, env, is_tail| {
            Self::eval_with_tail_context(expr, env, is_tail).map_err(|e| crate::eval::error::MyLispError::runtime(e))
        }, is_tail)
        .map_err(|e| e.to_string())
    }

    fn eval_quote_wrapper(args: &[Expr]) -> Result<Expr, String> {
        crate::eval::special_forms::SpecialForms::eval_quote(args)
            .map_err(|e| e.to_string())
    }

    fn eval_lambda_wrapper(args: &[Expr]) -> Result<Expr, String> {
        crate::eval::special_forms::SpecialForms::eval_lambda(args)
            .map_err(|e| e.to_string())
    }

    fn eval_let_wrapper(args: &[Expr], env: &mut Env, is_tail: bool) -> Result<Expr, String> {
        crate::eval::special_forms::SpecialForms::eval_let(args, env, |expr, env, is_tail| {
            Self::eval_with_tail_context(expr, env, is_tail).map_err(|e| crate::eval::error::MyLispError::runtime(e))
        }, is_tail)
        .map_err(|e| e.to_string())
    }

    fn eval_cond_wrapper(args: &[Expr], env: &mut Env, is_tail: bool) -> Result<Expr, String> {
        crate::eval::special_forms::SpecialForms::eval_cond(args, env, |expr, env, is_tail| {
            Self::eval_with_tail_context(expr, env, is_tail).map_err(|e| crate::eval::error::MyLispError::runtime(e))
        }, is_tail)
        .map_err(|e| e.to_string())
    }

    fn eval_begin_wrapper(args: &[Expr], env: &mut Env, is_tail: bool) -> Result<Expr, String> {
        crate::eval::special_forms::SpecialForms::eval_begin(args, env, |expr, env, is_tail| {
            Self::eval_with_tail_context(expr, env, is_tail).map_err(|e| crate::eval::error::MyLispError::runtime(e))
        }, is_tail)
        .map_err(|e| e.to_string())
    }

    fn eval_eval_wrapper(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
        crate::eval::special_forms::SpecialForms::eval_eval(args, env, |expr, env| {
            Self::eval(expr, env).map_err(|e| crate::eval::error::MyLispError::runtime(e))
        })
        .map_err(|e| e.to_string())
    }

    fn eval_load_wrapper(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
        crate::eval::special_forms::SpecialForms::eval_load(args, env, |expr, env| {
            Self::eval(expr, env).map_err(|e| crate::eval::error::MyLispError::runtime(e))
        })
        .map_err(|e| e.to_string())
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

    fn eval_if(args: &[Expr], env: &mut Env, is_tail: bool) -> Result<Expr, String> {
        if args.len() < 2 || args.len() > 3 {
            return Err("if requires 2 or 3 arguments".to_string());
        }

        let condition = Self::eval_with_tail_context(args[0].clone(), env, false)?;
        let is_true = match condition {
            Expr::Bool(b) => b,
            Expr::Nil => false,
            _ => true,
        };

        if is_true {
            Self::eval_with_tail_context(args[1].clone(), env, is_tail)
        } else if args.len() == 3 {
            Self::eval_with_tail_context(args[2].clone(), env, is_tail)
        } else {
            Ok(Expr::Nil)
        }
    }

    fn eval_cond(args: &[Expr], env: &mut Env, is_tail: bool) -> Result<Expr, String> {
        if args.is_empty() {
            return Err("cond requires at least one clause".to_string());
        }

        for clause in args {
            match clause {
                Expr::List(clause_list) if !clause_list.is_empty() => {
                    let test = &clause_list[0];

                    // 检查是否是 else 子句
                    let is_else = match test {
                        Expr::Symbol(s) if s == "else" => true,
                        _ => false,
                    };

                    // 求值测试条件
                    let test_result = if is_else {
                        Ok(Expr::Bool(true))
                    } else {
                        Self::eval_with_tail_context(test.clone(), env, false)
                    }?;

                    let is_true = match test_result {
                        Expr::Bool(b) => b,
                        Expr::Nil => false,
                        _ => true,
                    };

                    if is_true {
                        // 执行该子句的主体
                        if clause_list.len() == 1 {
                            // 只有测试条件,没有主体,返回测试值
                            return Ok(test_result);
                        } else if clause_list.len() == 2 {
                            // 单个表达式
                            return Self::eval_with_tail_context(clause_list[1].clone(), env, is_tail);
                        } else {
                            // 多个表达式,用 begin 语义
                            return Self::eval_begin(&clause_list[1..], env, is_tail);
                        }
                    }
                }
                Expr::Symbol(s) if s == "else" => {
                    return Err("cond: else clause must be a list".to_string());
                }
                _ => return Err("cond: each clause must be a list".to_string()),
            }
        }

        // 没有子句匹配,返回 nil
        Ok(Expr::Nil)
    }

    fn eval_begin(args: &[Expr], env: &mut Env, is_tail: bool) -> Result<Expr, String> {
        if args.is_empty() {
            return Ok(Expr::Nil);
        }

        let mut result = Ok(Expr::Nil);
        let len = args.len();

        for (i, expr) in args.iter().enumerate() {
            let is_last = i == len - 1;
            result = Self::eval_with_tail_context(expr.clone(), env, is_tail && is_last);
        }

        result
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

    fn eval_let(args: &[Expr], env: &mut Env, is_tail: bool) -> Result<Expr, String> {
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
                                    let value = Self::eval_with_tail_context(pair[1].clone(), env, false)?;
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

                // 只有一个表达式时,它是尾位置
                if args.len() == 2 {
                    Self::eval_with_tail_context(args[1].clone(), &mut let_env, is_tail)
                } else {
                    // 多个表达式时,只有最后一个是尾位置
                    let mut result = Ok(Expr::Nil);
                    for (i, expr) in args[1..].iter().enumerate() {
                        let is_last = i == args.len() - 2;
                        result = Self::eval_with_tail_context(expr.clone(), &mut let_env, is_tail && is_last);
                    }
                    result
                }
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

    fn eval_function_call(args: Vec<Expr>, env: &mut Env, is_tail: bool) -> Result<Expr, String> {
        // 先求值操作数(不求值操作符)
        let evaluated_args: Result<Vec<Expr>, String> =
            args.iter().skip(1).map(|a| Self::eval_with_tail_context(a.clone(), env, false)).collect();
        let evaluated_args = evaluated_args?;

        // 检查操作符是否是内置函数(符号)
        match &args[0] {
            Expr::Symbol(op) => match op.as_str() {
                // 算术运算 - 使用新的 Builtins 模块
                "+" => crate::eval::builtins::Builtins::apply_add(&evaluated_args)
                    .map_err(|e| e.to_string()),
                "-" => crate::eval::builtins::Builtins::apply_sub(&evaluated_args)
                    .map_err(|e| e.to_string()),
                "*" => crate::eval::builtins::Builtins::apply_mul(&evaluated_args)
                    .map_err(|e| e.to_string()),
                "/" => crate::eval::builtins::Builtins::apply_div(&evaluated_args)
                    .map_err(|e| e.to_string()),
                "mod" => crate::eval::builtins::Builtins::apply_mod(&evaluated_args)
                    .map_err(|e| e.to_string()),

                // 比较运算 - 使用新的 Builtins 模块
                ">" => crate::eval::builtins::Builtins::apply_gt(&evaluated_args)
                    .map_err(|e| e.to_string()),
                ">=" => crate::eval::builtins::Builtins::apply_ge(&evaluated_args)
                    .map_err(|e| e.to_string()),
                "<" => crate::eval::builtins::Builtins::apply_lt(&evaluated_args)
                    .map_err(|e| e.to_string()),
                "<=" => crate::eval::builtins::Builtins::apply_le(&evaluated_args)
                    .map_err(|e| e.to_string()),
                "=" => crate::eval::builtins::Builtins::apply_eq(&evaluated_args)
                    .map_err(|e| e.to_string()),

                // 逻辑运算 - and 和 or 需要保留在原处（依赖 eval）
                "not" => crate::eval::builtins::Builtins::apply_not(&evaluated_args)
                    .map_err(|e| e.to_string()),
                "and" => Self::apply_and(&evaluated_args, env),
                "or" => Self::apply_or(&evaluated_args, env),

                // 列表操作 - 使用新的 Builtins 模块
                "list" => crate::eval::builtins::Builtins::apply_list(&evaluated_args)
                    .map_err(|e| e.to_string()),
                "head" | "car" => crate::eval::builtins::Builtins::apply_head(&evaluated_args)
                    .map_err(|e| e.to_string()),
                "tail" | "cdr" => crate::eval::builtins::Builtins::apply_tail(&evaluated_args)
                    .map_err(|e| e.to_string()),
                "cons" => crate::eval::builtins::Builtins::apply_cons(&evaluated_args)
                    .map_err(|e| e.to_string()),
                "append" => crate::eval::builtins::Builtins::apply_append(&evaluated_args)
                    .map_err(|e| e.to_string()),
                "length" => crate::eval::builtins::Builtins::apply_length(&evaluated_args)
                    .map_err(|e| e.to_string()),
                "reverse" => crate::eval::builtins::Builtins::apply_reverse(&evaluated_args)
                    .map_err(|e| e.to_string()),

                // 谓词函数 - 使用新的 Builtins 模块
                "eq?" => crate::eval::builtins::Builtins::apply_eq(&evaluated_args)
                    .map_err(|e| e.to_string()),
                "null?" => crate::eval::builtins::Builtins::apply_null(&evaluated_args)
                    .map_err(|e| e.to_string()),
                "symbol?" => crate::eval::builtins::Builtins::apply_symbol_predicate(&evaluated_args)
                    .map_err(|e| e.to_string()),
                "list?" => crate::eval::builtins::Builtins::apply_list_predicate(&evaluated_args)
                    .map_err(|e| e.to_string()),
                "number?" => crate::eval::builtins::Builtins::apply_number_predicate(&evaluated_args)
                    .map_err(|e| e.to_string()),
                "string?" => crate::eval::builtins::Builtins::apply_string_predicate(&evaluated_args)
                    .map_err(|e| e.to_string()),

                // 高阶函数 - 保留在原处（依赖 eval）
                "map" => Self::apply_map(&evaluated_args, env),
                "filter" => Self::apply_filter(&evaluated_args, env),
                "fold" => Self::apply_fold(&evaluated_args, env),

                // I/O 操作 - 使用新的 Builtins 模块
                "display" => crate::eval::builtins::Builtins::apply_display(&evaluated_args)
                    .map_err(|e| e.to_string()),
                "newline" => crate::eval::builtins::Builtins::apply_newline()
                    .map_err(|e| e.to_string()),

                _ => {
                    // 不是内置函数,求值操作符并调用
                    let func = Self::eval_with_tail_context(args[0].clone(), env, false)?;
                    if is_tail {
                        if let Expr::List(ref list) = func {
                            if !list.is_empty()
                                && matches!(&list[0], Expr::Symbol(s) if s == "lambda" || s == "fn")
                            {
                                // 尾调用优化
                                return Self::apply_user_function_tail(func, evaluated_args, env);
                            }
                        }
                    }
                    Self::apply_user_function(func, evaluated_args, env)
                }
            },
            _ => {
                // 操作符不是符号,求值并调用
                let func = Self::eval_with_tail_context(args[0].clone(), env, false)?;
                if is_tail {
                    if let Expr::List(ref list) = func {
                        if !list.is_empty()
                            && matches!(&list[0], Expr::Symbol(s) if s == "lambda" || s == "fn")
                        {
                            return Self::apply_user_function_tail(func, evaluated_args, env);
                        }
                    }
                }
                Self::apply_user_function(func, evaluated_args, env)
            }
        }
    }

    // 尾调用优化版本的函数应用:使用循环代替递归
    fn apply_user_function_tail(func: Expr, args: Vec<Expr>, env: &mut Env) -> Result<Expr, String> {
        let mut current_func = func;
        let mut current_args = args;
        let mut current_env = env.clone();

        loop {
            // 提取 lambda 信息
            let (params, body_exprs) = match &current_func {
                Expr::List(list) if !list.is_empty() => {
                    match &list[0] {
                        Expr::Symbol(s) if s == "lambda" || s == "fn" => {
                            match &list[1] {
                                Expr::List(params) => (params.clone(), list[2..].to_vec()),
                                _ => return Err("Invalid lambda parameter list".to_string()),
                            }
                        }
                        _ => return Err("Cannot call non-function".to_string()),
                    }
                }
                _ => return Err("Cannot call non-function".to_string()),
            };

            // 检查参数数量
            if params.len() != current_args.len() {
                return Err(format!(
                    "Arity mismatch: expected {}, got {}",
                    params.len(),
                    current_args.len()
                ));
            }

            // 创建新环境
            let mut func_env = Env::with_parent(current_env);

            // 绑定参数
            for (param, arg) in params.iter().zip(current_args.iter()) {
                match param {
                    Expr::Symbol(name) => {
                        func_env.define(name.clone(), arg.clone());
                    }
                    _ => {}
                }
            }

            // 按顺序求值 body
            let mut result = Ok(Expr::Nil);
            let mut next_tail_call = None;

            for (i, expr) in body_exprs.iter().enumerate() {
                let is_last = i == body_exprs.len() - 1;

                if is_last {
                    // 最后一个表达式:检查是否是尾调用
                    match expr {
                        Expr::List(call_list) if !call_list.is_empty() => {
                            // 求值操作符
                            let func = Self::eval_with_tail_context(
                                call_list[0].clone(),
                                &mut func_env,
                                false
                            )?;

                            // 求值参数
                            let evaluated_args: Result<Vec<Expr>, String> = call_list[1..]
                                .iter()
                                .map(|a| Self::eval_with_tail_context(a.clone(), &mut func_env, false))
                                .collect();
                            let evaluated_args = evaluated_args?;

                            // 检查是否是尾调用 lambda
                            match func {
                                Expr::List(ref inner_list) if !inner_list.is_empty() => {
                                    match &inner_list[0] {
                                        Expr::Symbol(s) if s == "lambda" || s == "fn" => {
                                            // 尾调用:保存信息并继续循环
                                            next_tail_call = Some((func, evaluated_args, func_env));
                                            break;
                                        }
                                        _ => {
                                            // 不是 lambda,正常求值并返回
                                            result = Self::apply_user_function(func, evaluated_args, &mut func_env);
                                            break;
                                        }
                                    }
                                }
                                _ => {
                                    // 不是列表,正常求值并返回
                                    result = Self::apply_user_function(func, evaluated_args, &mut func_env);
                                    break;
                                }
                            }
                        }
                        _ => {
                            // 最后一个表达式不是函数调用
                            result = Self::eval_with_tail_context(expr.clone(), &mut func_env, true);
                        }
                    }
                } else {
                    // 不是最后一个表达式
                    result = Self::eval_with_tail_context(expr.clone(), &mut func_env, false);
                }
            }

            // 如果有尾调用,继续循环
            if let Some((func, args, new_env)) = next_tail_call {
                current_func = func;
                current_args = args;
                current_env = new_env;
                continue;
            }

            // 没有尾调用,返回结果
            return result;
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

    fn apply_mod(args: &[Expr]) -> Result<Expr, String> {
        if args.len() != 2 {
            return Err("mod requires exactly 2 arguments".to_string());
        }

        match (&args[0], &args[1]) {
            (Expr::Number(a), Expr::Number(b)) => {
                if *b == 0.0 {
                    return Err("Division by zero".to_string());
                }
                Ok(Expr::Number(a % b))
            }
            _ => Err("mod expects numbers".to_string()),
        }
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

    // ========== 宏系统实现 ==========

    // 定义宏: (define-syntax name (syntax-rules (literal ...) (pattern template) ...))
    fn eval_define_syntax(args: &[Expr], _env: &mut Env) -> Result<Expr, String> {
        if args.len() != 2 {
            return Err("define-syntax requires exactly 2 arguments".to_string());
        }

        let name = match &args[0] {
            Expr::Symbol(s) => s.clone(),
            _ => return Err("define-syntax: name must be a symbol".to_string()),
        };

        let syntax_rules = match &args[1] {
            Expr::List(list) if !list.is_empty() => {
                match &list[0] {
                    Expr::Symbol(s) if s == "syntax-rules" => &list[1..],
                    _ => return Err("define-syntax: must use syntax-rules".to_string()),
                }
            }
            _ => return Err("define-syntax: second argument must be a list".to_string()),
        };

        if syntax_rules.is_empty() {
            return Err("syntax-rules requires at least literals and one rule".to_string());
        }

        // 提取字面量
        let literals: Vec<String> = match &syntax_rules[0] {
            Expr::List(lits) => {
                lits.iter().filter_map(|e| {
                    if let Expr::Symbol(s) = e { Some(s.clone()) } else { None }
                }).collect()
            }
            _ => return Err("syntax-rules: literals must be a list of symbols".to_string()),
        };

        // 提取规则
        let mut rules = Vec::new();
        for rule in &syntax_rules[1..] {
            match rule {
                Expr::List(r) if r.len() == 2 => {
                    rules.push(MacroRule {
                        pattern: r[0].clone(),
                        template: r[1].clone(),
                        literals: literals.clone(),
                    });
                }
                _ => return Err("syntax-rules: each rule must be (pattern template)".to_string()),
            }
        }

        register_macro(name.clone(), Macro { name: name.clone(), rules });
        Ok(Expr::Nil)
    }

    // 宏展开:递归展开所有宏
    fn expand_macros(expr: Expr) -> Result<Expr, String> {
        match expr {
            Expr::List(list) if !list.is_empty() => {
                // 检查是否是宏调用
                match &list[0] {
                    Expr::Symbol(sym) => {
                        if let Some(macro_def) = lookup_macro(sym) {
                            // 展开宏
                            let expanded = Self::apply_macro(&macro_def, &list)?;
                            // 递归展开结果
                            return Self::expand_macros(expanded);
                        }
                    }
                    _ => {}
                }

                // 递归展开列表元素
                let expanded: Result<Vec<Expr>, String> = list
                    .into_iter()
                    .map(|e| Self::expand_macros(e))
                    .collect();
                Ok(Expr::List(expanded?))
            }
            _ => Ok(expr),
        }
    }

    // 应用宏:模式匹配和模板展开
    fn apply_macro(macro_def: &Macro, args: &[Expr]) -> Result<Expr, String> {
        for rule in &macro_def.rules {
            if let Some(binding) = Self::match_pattern(&rule.pattern, args, &rule.literals) {
                return Self::expand_template(&rule.template, &binding);
            }
        }
        Err(format!("Macro '{}' pattern match failed", macro_def.name))
    }

    // 模式匹配:返回模式变量绑定(支持 ...)
    fn match_pattern(
        pattern: &Expr,
        expr: &[Expr],
        literals: &[String]
    ) -> Option<HashMap<String, PatternBinding>> {
        if expr.is_empty() {
            return None;
        }

        // 检查操作符是否匹配
        let pattern_list = match pattern {
            Expr::List(l) => l.as_slice(),
            _ => return None,
        };

        if pattern_list.is_empty() {
            return None;
        }

        let pattern_op = match &pattern_list[0] {
            Expr::Symbol(s) => s.clone(),
            _ => return None,
        };

        let expr_op = match &expr[0] {
            Expr::Symbol(s) => s.clone(),
            _ => return None,
        };

        if pattern_op != expr_op {
            return None;
        }

        let mut bindings = HashMap::new();

        // 匹配参数(支持 ...)
        Self::match_pattern_list(&pattern_list[1..], &expr[1..], literals, &mut bindings)
            .then_some(bindings)
    }

    // 匹配模式列表,支持省略号
    fn match_pattern_list(
        pattern: &[Expr],
        expr: &[Expr],
        literals: &[String],
        bindings: &mut HashMap<String, PatternBinding>
    ) -> bool {
        let mut p_idx = 0;
        let mut e_idx = 0;

        while p_idx < pattern.len() && e_idx < expr.len() {
            // 检查是否是 ... 模式
            let has_ellipsis = p_idx + 1 < pattern.len()
                && matches!(&pattern[p_idx + 1], Expr::Symbol(s) if s == "...");

            if has_ellipsis {
                // 处理可变参数模式
                match &pattern[p_idx] {
                    Expr::Symbol(sym) if !literals.contains(sym) && sym != "..." => {
                        // 收集剩余所有表达式
                        let mut collected = Vec::new();
                        while e_idx < expr.len() {
                            collected.push(expr[e_idx].clone());
                            e_idx += 1;
                        }
                        bindings.insert(sym.clone(), PatternBinding::Multiple(collected));
                        p_idx += 2; // 跳过模式和 ...
                        break;
                    }
                    _ => return false,
                }
            } else {
                // 正常匹配单个元素
                if !Self::match_pattern_single(&pattern[p_idx], &expr[e_idx], literals, bindings) {
                    return false;
                }
                p_idx += 1;
                e_idx += 1;
            }
        }

        // 检查是否所有模式都匹配
        while p_idx < pattern.len() {
            let has_ellipsis = p_idx + 1 < pattern.len()
                && matches!(&pattern[p_idx + 1], Expr::Symbol(s) if s == "...");

            if has_ellipsis {
                // 空列表匹配
                match &pattern[p_idx] {
                    Expr::Symbol(sym) if !literals.contains(sym) && sym != "..." => {
                        bindings.insert(sym.clone(), PatternBinding::Multiple(vec![]));
                        p_idx += 2;
                    }
                    _ => return false,
                }
            } else {
                return false;
            }
        }

        e_idx == expr.len()
    }

    // 单个模式元素匹配
    fn match_pattern_single(
        pattern: &Expr,
        expr: &Expr,
        literals: &[String],
        bindings: &mut HashMap<String, PatternBinding>
    ) -> bool {
        match pattern {
            Expr::Symbol(sym) => {
                // 检查是否是字面量
                if literals.contains(sym) {
                    match expr {
                        Expr::Symbol(s) if s == sym => true,
                        _ => false,
                    }
                } else if sym == "..." {
                    // 省略号不应该在这里单独出现
                    false
                } else {
                    // 模式变量
                    bindings.insert(sym.clone(), PatternBinding::Single(expr.clone()));
                    true
                }
            }
            Expr::List(pattern_list) => {
                match expr {
                    Expr::List(expr_list) => {
                        Self::match_pattern_list(pattern_list, expr_list, literals, bindings)
                    }
                    _ => false,
                }
            }
            _ => pattern == expr,
        }
    }

    // 模板展开(支持 ... 重复)
    fn expand_template(template: &Expr, bindings: &HashMap<String, PatternBinding>) -> Result<Expr, String> {
        match template {
            Expr::Symbol(sym) => {
                if let Some(binding) = bindings.get(sym) {
                    match binding {
                        PatternBinding::Single(expr) => Ok(expr.clone()),
                        PatternBinding::Multiple(list) => Ok(Expr::List(list.clone())),
                    }
                } else {
                    Ok(Expr::Symbol(sym.clone()))
                }
            }
            Expr::List(list) => {
                // 检查是否有省略号模式
                Self::expand_template_with_ellipsis(list, bindings)
            }
            _ => Ok(template.clone()),
        }
    }

    // 展开模板列表,处理省略号重复
    fn expand_template_with_ellipsis(
        template: &[Expr],
        bindings: &HashMap<String, PatternBinding>
    ) -> Result<Expr, String> {
        let mut result = Vec::new();
        let mut i = 0;

        while i < template.len() {
            // 检查是否后面跟着省略号
            let has_ellipsis = i + 1 < template.len()
                && matches!(&template[i + 1], Expr::Symbol(s) if s == "...");

            if has_ellipsis {
                // 处理重复模式
                match &template[i] {
                    Expr::Symbol(sym) => {
                        if let Some(binding) = bindings.get(sym) {
                            match binding {
                                PatternBinding::Single(expr) => {
                                    result.push(expr.clone());
                                }
                                PatternBinding::Multiple(list) => {
                                    // 重复展开列表中的每个元素
                                    for item in list {
                                        result.push(item.clone());
                                    }
                                }
                            }
                        } else {
                            // 未绑定的变量,保持原样
                            result.push(template[i].clone());
                        }
                    }
                    Expr::List(sub_template) => {
                        // 检查是否需要嵌套展开
                        if let Some(repeated_bindings) = Self::extract_repeated_bindings(sub_template, bindings) {
                            // 为每个重复值展开模板
                            for single_bindings in repeated_bindings {
                                let expanded = Self::expand_template_list(sub_template, &single_bindings)?;
                                result.extend(expanded);
                            }
                        } else {
                            // 正常展开
                            let expanded = Self::expand_template_list(sub_template, bindings)?;
                            result.extend(expanded);
                        }
                    }
                    _ => {
                        result.push(template[i].clone());
                    }
                }
                i += 2; // 跳过模式和 ...
            } else {
                // 正常展开单个元素
                let expanded = Self::expand_template(&template[i], bindings)?;
                result.push(expanded);
                i += 1;
            }
        }

        Ok(Expr::List(result))
    }

    // 展开模板列表(不处理省略号)
    fn expand_template_list(
        template: &[Expr],
        bindings: &HashMap<String, PatternBinding>
    ) -> Result<Vec<Expr>, String> {
        template.iter()
            .map(|e| Self::expand_template(e, bindings))
            .collect()
    }

    // 提取重复绑定(用于嵌套模板)
    fn extract_repeated_bindings(
        template: &[Expr],
        bindings: &HashMap<String, PatternBinding>
    ) -> Option<Vec<HashMap<String, PatternBinding>>> {
        // 找出所有 Multiple 绑定
        let mut repeated_keys = Vec::new();
        for expr in template {
            if let Expr::Symbol(sym) = expr {
                if let Some(PatternBinding::Multiple(_)) = bindings.get(sym) {
                    repeated_keys.push(sym.clone());
                }
            }
        }

        if repeated_keys.is_empty() {
            return None;
        }

        // 获取第一个 Multiple 绑定的长度
        let first_len = match bindings.get(&repeated_keys[0]) {
            Some(PatternBinding::Multiple(list)) => list.len(),
            _ => return None,
        };

        // 为每个位置创建单独的绑定
        let mut result = Vec::new();
        for i in 0..first_len {
            let mut single_bindings = HashMap::new();

            for key in &repeated_keys {
                if let Some(PatternBinding::Multiple(list)) = bindings.get(key) {
                    if i < list.len() {
                        single_bindings.insert(key.clone(), PatternBinding::Single(list[i].clone()));
                    }
                }
            }

            // 复制其他 Single 绑定
            for (k, v) in bindings {
                if !repeated_keys.contains(k) {
                    single_bindings.insert(k.clone(), v.clone());
                }
            }

            result.push(single_bindings);
        }

        Some(result)
    }

    // ========== 高阶列表函数 ==========

    fn apply_map(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
        if args.len() != 2 {
            return Err("map requires exactly 2 arguments".to_string());
        }

        let func = &args[0];
        let list = match &args[1] {
            Expr::List(l) => l,
            _ => return Err("map: second argument must be a list".to_string()),
        };

        let mut result = Vec::new();
        for item in list {
            let call = Expr::List(vec![func.clone(), item.clone()]);
            let mapped = Self::eval_with_tail_context(call, env, false)?;
            result.push(mapped);
        }

        Ok(Expr::List(result))
    }

    fn apply_filter(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
        if args.len() != 2 {
            return Err("filter requires exactly 2 arguments".to_string());
        }

        let func = &args[0];
        let list = match &args[1] {
            Expr::List(l) => l,
            _ => return Err("filter: second argument must be a list".to_string()),
        };

        let mut result = Vec::new();
        for item in list {
            let call = Expr::List(vec![func.clone(), item.clone()]);
            let predicate_result = Self::eval_with_tail_context(call, env, false)?;

            let keep = match predicate_result {
                Expr::Bool(b) => b,
                Expr::Nil => false,
                _ => true,
            };

            if keep {
                result.push(item.clone());
            }
        }

        Ok(Expr::List(result))
    }

    fn apply_fold(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
        if args.len() != 3 {
            return Err("fold requires exactly 3 arguments".to_string());
        }

        let func = &args[0];
        let initial = &args[1];
        let list = match &args[2] {
            Expr::List(l) => l,
            _ => return Err("fold: third argument must be a list".to_string()),
        };

        let mut acc = initial.clone();
        for item in list {
            let call = Expr::List(vec![func.clone(), acc, item.clone()]);
            acc = Self::eval_with_tail_context(call, env, false)?;
        }

        Ok(acc)
    }

    fn apply_length(args: &[Expr]) -> Result<Expr, String> {
        if args.len() != 1 {
            return Err("length requires exactly 1 argument".to_string());
        }

        let len = match &args[0] {
            Expr::List(l) => l.len() as f64,
            Expr::Nil => 0.0,
            _ => return Err("length: argument must be a list".to_string()),
        };

        Ok(Expr::Number(len))
    }

    fn apply_reverse(args: &[Expr]) -> Result<Expr, String> {
        if args.len() != 1 {
            return Err("reverse requires exactly 1 argument".to_string());
        }

        match &args[0] {
            Expr::List(l) => {
                let reversed: Vec<Expr> = l.iter().rev().cloned().collect();
                Ok(Expr::List(reversed))
            }
            Expr::Nil => Ok(Expr::List(vec![])),
            _ => Err("reverse: argument must be a list".to_string()),
        }
    }
}
