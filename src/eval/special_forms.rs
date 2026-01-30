//! 特殊形式模块
//!
//! 包含所有特殊形式的实现：define, set!, if, lambda, let, quote, eval, load, cond, begin

use crate::ast::Expr;
use crate::env::Env;
use crate::eval::error::MyLispError;
use crate::utils::{check_arity, check_min_arity, expect_symbol, expect_list};

/// 特殊形式应用器
pub struct SpecialForms;

impl SpecialForms {
    // ========== 基础特殊形式 ==========

    /// 变量定义: (define name value) 或 (define (f x y) body)
    pub fn eval_define<F>(
        args: &[Expr],
        env: &mut Env,
        eval_fn: F,
    ) -> Result<Expr, MyLispError>
    where
        F: Fn(Expr, &mut Env) -> Result<Expr, MyLispError>,
    {
        check_arity(args, 2, "define")?;

        match &args[0] {
            Expr::Symbol(name) => {
                let value = eval_fn(args[1].clone(), env)?;
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
                        let value = eval_fn(lambda, env)?;
                        env.define(name.clone(), value);
                        Ok(Expr::Nil)
                    }
                    _ => Err(MyLispError::runtime("define: function name must be a symbol")),
                }
            }
            _ => Err(MyLispError::type_error(
                "symbol or list",
                crate::utils::get_type_name(&args[0]),
                "define",
            )),
        }
    }

    /// 变量赋值: (set! name value)
    pub fn eval_set<F>(
        args: &[Expr],
        env: &mut Env,
        eval_fn: F,
    ) -> Result<Expr, MyLispError>
    where
        F: Fn(Expr, &mut Env) -> Result<Expr, MyLispError>,
    {
        check_arity(args, 2, "set!")?;

        let name = expect_symbol(&args[0], "set!")?;
        let value = eval_fn(args[1].clone(), env)?;

        env.set(&name, value)?;
        Ok(Expr::Nil)
    }

    /// 条件表达式: (if condition true_branch [false_branch])
    pub fn eval_if<F>(
        args: &[Expr],
        env: &mut Env,
        eval_fn: F,
        is_tail: bool,
    ) -> Result<Expr, MyLispError>
    where
        F: Fn(Expr, &mut Env, bool) -> Result<Expr, MyLispError>,
    {
        if args.len() < 2 || args.len() > 3 {
            return Err(MyLispError::arity_error("if", "2 or 3", args.len()));
        }

        let condition = eval_fn(args[0].clone(), env, false)?;
        let is_true = Self::is_truthy(&condition);

        if is_true {
            eval_fn(args[1].clone(), env, is_tail)
        } else if args.len() == 3 {
            eval_fn(args[2].clone(), env, is_tail)
        } else {
            Ok(Expr::Nil)
        }
    }

    /// 引用: (quote expr)
    pub fn eval_quote(args: &[Expr]) -> Result<Expr, MyLispError> {
        check_arity(args, 1, "quote")?;
        Ok(args[0].clone())
    }

    // ========== 复杂特殊形式 ==========

    /// Lambda 函数: (lambda (params) body1 body2 ...)
    pub fn eval_lambda(args: &[Expr], env: &Env) -> Result<Expr, MyLispError> {
        check_min_arity(args, 2, "lambda")?;

        match &args[0] {
            Expr::List(params) => {
                Ok(Expr::Lambda {
                    params: params.clone(),
                    body: args[1..].to_vec(),
                    env: env.clone(),
                })
            }
            _ => Err(MyLispError::type_error(
                "parameter list",
                crate::utils::get_type_name(&args[0]),
                "lambda",
            )),
        }
    }

    /// Let 绑定: (let ((x 1) (y 2)) body)
    pub fn eval_let<F>(
        args: &[Expr],
        env: &mut Env,
        eval_fn: F,
        is_tail: bool,
    ) -> Result<Expr, MyLispError>
    where
        F: Fn(Expr, &mut Env, bool) -> Result<Expr, MyLispError>,
    {
        check_min_arity(args, 2, "let")?;

        let bindings = expect_list(&args[0], "let")?;
        let mut let_env = Env::with_parent(env.clone());

        for binding in &bindings {
            match binding {
                Expr::List(pair) if pair.len() == 2 => {
                    let name = expect_symbol(&pair[0], "let")?;
                    let value = eval_fn(pair[1].clone(), env, false)?;
                    let_env.define(name, value);
                }
                _ => {
                    return Err(MyLispError::runtime(
                        "let: bindings must be (name value) pairs",
                    ))
                }
            }
        }

        // 只有一个表达式时,它是尾位置
        if args.len() == 2 {
            eval_fn(args[1].clone(), &mut let_env, is_tail)
        } else {
            // 多个表达式时,只有最后一个是尾位置
            let mut result = Ok(Expr::Nil);
            for (i, expr) in args[1..].iter().enumerate() {
                let is_last = i == args.len() - 2;
                result = eval_fn(expr.clone(), &mut let_env, is_tail && is_last);
            }
            result
        }
    }

    /// Cond 条件分支: (cond (test1 body1) (test2 body2) ... (else bodyN))
    pub fn eval_cond<F>(
        args: &[Expr],
        env: &mut Env,
        eval_fn: F,
        is_tail: bool,
    ) -> Result<Expr, MyLispError>
    where
        F: Fn(Expr, &mut Env, bool) -> Result<Expr, MyLispError> + Copy,
    {
        if args.is_empty() {
            return Err(MyLispError::runtime("cond requires at least one clause"));
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
                        eval_fn(test.clone(), env, false)
                    }?;

                    let is_true = Self::is_truthy(&test_result);

                    if is_true {
                        // 执行该子句的主体
                        if clause_list.len() == 1 {
                            // 只有测试条件,没有主体,返回测试值
                            return Ok(test_result);
                        } else if clause_list.len() == 2 {
                            // 单个表达式
                            return eval_fn(clause_list[1].clone(), env, is_tail);
                        } else {
                            // 多个表达式,用 begin 语义
                            return Self::eval_begin(&clause_list[1..], env, eval_fn, is_tail);
                        }
                    }
                }
                Expr::Symbol(s) if s == "else" => {
                    return Err(MyLispError::runtime("cond: else clause must be a list"));
                }
                _ => return Err(MyLispError::runtime("cond: each clause must be a list")),
            }
        }

        // 没有子句匹配,返回 nil
        Ok(Expr::Nil)
    }

    /// Begin 序列: (begin expr1 expr2 ...)
    pub fn eval_begin<F>(
        args: &[Expr],
        env: &mut Env,
        eval_fn: F,
        is_tail: bool,
    ) -> Result<Expr, MyLispError>
    where
        F: Fn(Expr, &mut Env, bool) -> Result<Expr, MyLispError> + Copy,
    {
        if args.is_empty() {
            return Ok(Expr::Nil);
        }

        let mut result = Ok(Expr::Nil);
        let len = args.len();

        for (i, expr) in args.iter().enumerate() {
            let is_last = i == len - 1;
            result = eval_fn(expr.clone(), env, is_tail && is_last);
        }

        result
    }

    // ========== I/O 特殊形式 ==========

    /// 动态求值: (eval expr)
    pub fn eval_eval<F>(
        args: &[Expr],
        env: &mut Env,
        eval_fn: F,
    ) -> Result<Expr, MyLispError>
    where
        F: Fn(Expr, &mut Env) -> Result<Expr, MyLispError> + Copy,
    {
        check_arity(args, 1, "eval")?;
        let evaluated = eval_fn(args[0].clone(), env)?;
        eval_fn(evaluated, env)
    }

    /// 加载文件: (load filename)
    pub fn eval_load<F>(
        args: &[Expr],
        env: &mut Env,
        eval_fn: F,
    ) -> Result<Expr, MyLispError>
    where
        F: Fn(Expr, &mut Env) -> Result<Expr, MyLispError> + Copy,
    {
        check_arity(args, 1, "load")?;

        let filename = expect_symbol(&args[0], "load")?;

        use crate::lexer::Lexer;
        use crate::parser::Parser;

        let content = std::fs::read_to_string(&filename)
            .map_err(|e| MyLispError::runtime(format!("Failed to read file '{}': {}", filename, e)))?;

        // 解析整个文件内容，支持跨行表达式
        let mut result = Ok(Expr::Nil);
        let lexer = Lexer::new(&content);
        let mut parser = Parser::new(lexer);

        loop {
            match parser.parse() {
                Ok(expr) => {
                    result = eval_fn(expr, env);
                }
                Err(e) => {
                    // 如果是 EOF 错误，说明正常结束
                    if e.contains("EOF") {
                        break;
                    }
                    return Err(MyLispError::runtime(format!("Parse error in '{}': {}", filename, e)));
                }
            }
        }

        result
    }

    // ========== 辅助函数 ==========

    /// 判断表达式是否为真
    ///
    /// 在 Lisp 中,只有 #f 和 nil 是假,其他都是真
    fn is_truthy(expr: &Expr) -> bool {
        match expr {
            Expr::Bool(false) => false,
            Expr::Nil => false,
            _ => true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::env::Env;

    #[test]
    fn test_quote() {
        let args = vec![Expr::Symbol("x".to_string())];
        assert_eq!(SpecialForms::eval_quote(&args).unwrap(), Expr::Symbol("x".to_string()));
    }

    #[test]
    fn test_lambda() {
        let args = vec![
            Expr::List(vec![Expr::Symbol("x".to_string())]),
            Expr::Symbol("x".to_string()),
        ];
        let env = Env::new();
        let result = SpecialForms::eval_lambda(&args, &env).unwrap();
        match result {
            Expr::List(list) => {
                assert_eq!(list[0], Expr::Symbol("lambda".to_string()));
                assert_eq!(list[1], Expr::List(vec![Expr::Symbol("x".to_string())]));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn test_is_truthy() {
        assert!(!SpecialForms::is_truthy(&Expr::Bool(false)));
        assert!(!SpecialForms::is_truthy(&Expr::Nil));
        assert!(SpecialForms::is_truthy(&Expr::Bool(true)));
        assert!(SpecialForms::is_truthy(&Expr::Number(0.0)));
        assert!(SpecialForms::is_truthy(&Expr::String("".to_string())));
    }
}
