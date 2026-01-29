//! 内置函数模块
//!
//! 包含所有内置函数的实现：算术、比较、逻辑、列表操作、谓词和 I/O

use crate::ast::Expr;
use crate::env::Env;
use crate::eval::error::MyLispError;
use crate::utils::{check_arity, check_min_arity, expect_number, is_number, is_nil, is_list};

/// 内置函数应用器
pub struct Builtins;

impl Builtins {
    // ========== 算术运算 ==========

    /// 加法: (+ arg1 arg2 ...)
    pub fn apply_add(args: &[Expr]) -> Result<Expr, MyLispError> {
        let nums: Vec<f64> = args
            .iter()
            .map(|a| expect_number(a, "+"))
            .collect::<Result<Vec<_>, _>>()?;

        let result: f64 = nums.iter().sum();
        Ok(Expr::Number(result))
    }

    /// 减法: (- arg1 arg2 ...)
    pub fn apply_sub(args: &[Expr]) -> Result<Expr, MyLispError> {
        check_min_arity(args, 1, "-")?;

        let first = expect_number(&args[0], "-")?;
        let rest_nums: Vec<f64> = args[1..]
            .iter()
            .map(|a| expect_number(a, "-"))
            .collect::<Result<Vec<_>, _>>()?;

        let result = rest_nums.iter().fold(first, |acc, x| acc - x);
        Ok(Expr::Number(result))
    }

    /// 乘法: (* arg1 arg2 ...)
    pub fn apply_mul(args: &[Expr]) -> Result<Expr, MyLispError> {
        let nums: Vec<f64> = args
            .iter()
            .map(|a| expect_number(a, "*"))
            .collect::<Result<Vec<_>, _>>()?;

        let result: f64 = nums.iter().product();
        Ok(Expr::Number(result))
    }

    /// 除法: (/ arg1 arg2 ...)
    pub fn apply_div(args: &[Expr]) -> Result<Expr, MyLispError> {
        check_min_arity(args, 1, "/")?;

        let first = expect_number(&args[0], "/")?;
        let rest_nums: Vec<f64> = args[1..]
            .iter()
            .map(|a| {
                let n = expect_number(a, "/")?;
                if n == 0.0 {
                    return Err(MyLispError::runtime("Division by zero"));
                }
                Ok(n)
            })
            .collect::<Result<Vec<_>, _>>()?;

        let result = rest_nums.iter().fold(first, |acc, x| acc / x);
        Ok(Expr::Number(result))
    }

    /// 取模: (mod a b)
    pub fn apply_mod(args: &[Expr]) -> Result<Expr, MyLispError> {
        check_arity(args, 2, "mod")?;

        let a = expect_number(&args[0], "mod")?;
        let b = expect_number(&args[1], "mod")?;

        if b == 0.0 {
            return Err(MyLispError::runtime("Division by zero"));
        }

        Ok(Expr::Number(a % b))
    }

    // ========== 比较运算 ==========

    /// 大于: (> a b)
    pub fn apply_gt(args: &[Expr]) -> Result<Expr, MyLispError> {
        check_arity(args, 2, ">")?;

        let a = expect_number(&args[0], ">")?;
        let b = expect_number(&args[1], ">")?;

        Ok(Expr::Bool(a > b))
    }

    /// 小于: (< a b)
    pub fn apply_lt(args: &[Expr]) -> Result<Expr, MyLispError> {
        check_arity(args, 2, "<")?;

        let a = expect_number(&args[0], "<")?;
        let b = expect_number(&args[1], "<")?;

        Ok(Expr::Bool(a < b))
    }

    /// 小于等于: (<= a b)
    pub fn apply_le(args: &[Expr]) -> Result<Expr, MyLispError> {
        check_arity(args, 2, "<=")?;

        let a = expect_number(&args[0], "<=")?;
        let b = expect_number(&args[1], "<=")?;

        Ok(Expr::Bool(a <= b))
    }

    /// 大于等于: (>= a b)
    pub fn apply_ge(args: &[Expr]) -> Result<Expr, MyLispError> {
        check_arity(args, 2, ">=")?;

        let a = expect_number(&args[0], ">=")?;
        let b = expect_number(&args[1], ">=")?;

        Ok(Expr::Bool(a >= b))
    }

    /// 相等: (= a b)
    pub fn apply_eq(args: &[Expr]) -> Result<Expr, MyLispError> {
        check_arity(args, 2, "=")?;

        let result = match (&args[0], &args[1]) {
            (Expr::Number(a), Expr::Number(b)) => a == b,
            (Expr::Bool(a), Expr::Bool(b)) => a == b,
            (Expr::Nil, Expr::Nil) => true,
            (Expr::Symbol(a), Expr::Symbol(b)) => a == b,
            _ => false,
        };

        Ok(Expr::Bool(result))
    }

    // ========== 逻辑运算 ==========

    /// 逻辑非: (not arg)
    pub fn apply_not(args: &[Expr]) -> Result<Expr, MyLispError> {
        check_arity(args, 1, "not")?;

        let result = match &args[0] {
            Expr::Bool(b) => !b,
            Expr::Nil => true,
            _ => false,
        };

        Ok(Expr::Bool(result))
    }

    // ========== 列表操作 ==========

    /// 获取列表第一个元素: (head list)
    pub fn apply_head(args: &[Expr]) -> Result<Expr, MyLispError> {
        check_arity(args, 1, "head")?;

        match &args[0] {
            Expr::List(list) if !list.is_empty() => Ok(list[0].clone()),
            Expr::List(_) => Err(MyLispError::runtime("head: empty list")),
            _ => Err(MyLispError::type_error("list", crate::utils::get_type_name(&args[0]), "head")),
        }
    }

    /// 获取列表除第一个元素外的部分: (tail list)
    pub fn apply_tail(args: &[Expr]) -> Result<Expr, MyLispError> {
        check_arity(args, 1, "tail")?;

        match &args[0] {
            Expr::List(list) if !list.is_empty() => Ok(Expr::List(list[1..].to_vec())),
            Expr::List(_) => Err(MyLispError::runtime("tail: empty list")),
            _ => Err(MyLispError::type_error("list", crate::utils::get_type_name(&args[0]), "tail")),
        }
    }

    /// 构造列表: (cons elem list)
    pub fn apply_cons(args: &[Expr]) -> Result<Expr, MyLispError> {
        check_arity(args, 2, "cons")?;

        match &args[1] {
            Expr::List(list) => {
                let mut new_list = vec![args[0].clone()];
                new_list.extend(list.clone());
                Ok(Expr::List(new_list))
            }
            Expr::Nil => Ok(Expr::List(vec![args[0].clone()])),
            _ => Err(MyLispError::runtime("cons: second argument must be a list")),
        }
    }

    /// 连接列表: (append list1 list2 ...)
    pub fn apply_append(args: &[Expr]) -> Result<Expr, MyLispError> {
        let mut result = Vec::new();

        for arg in args {
            match arg {
                Expr::List(list) => result.extend(list.clone()),
                _ => return Err(MyLispError::type_error("list", crate::utils::get_type_name(arg), "append")),
            }
        }

        Ok(Expr::List(result))
    }

    /// 创建列表: (list elem1 elem2 ...)
    pub fn apply_list(args: &[Expr]) -> Result<Expr, MyLispError> {
        Ok(Expr::List(args.to_vec()))
    }

    /// 列表长度: (length list)
    pub fn apply_length(args: &[Expr]) -> Result<Expr, MyLispError> {
        check_arity(args, 1, "length")?;

        let len = match &args[0] {
            Expr::List(l) => l.len() as f64,
            Expr::Nil => 0.0,
            _ => return Err(MyLispError::type_error("list or nil", crate::utils::get_type_name(&args[0]), "length")),
        };

        Ok(Expr::Number(len))
    }

    // ========== 谓词函数 ==========

    /// 检查是否为空: (null? arg)
    pub fn apply_null(args: &[Expr]) -> Result<Expr, MyLispError> {
        check_arity(args, 1, "null?")?;

        let result = is_nil(&args[0]);
        Ok(Expr::Bool(result))
    }

    /// 检查是否为符号: (symbol? arg)
    pub fn apply_symbol_predicate(args: &[Expr]) -> Result<Expr, MyLispError> {
        check_arity(args, 1, "symbol?")?;

        let result = matches!(args[0], Expr::Symbol(_));
        Ok(Expr::Bool(result))
    }

    /// 检查是否为列表: (list? arg)
    pub fn apply_list_predicate(args: &[Expr]) -> Result<Expr, MyLispError> {
        check_arity(args, 1, "list?")?;

        let result = is_list(&args[0]);
        Ok(Expr::Bool(result))
    }

    /// 检查是否为数字: (number? arg)
    pub fn apply_number_predicate(args: &[Expr]) -> Result<Expr, MyLispError> {
        check_arity(args, 1, "number?")?;

        let result = is_number(&args[0]);
        Ok(Expr::Bool(result))
    }

    /// 检查是否为字符串: (string? arg)
    pub fn apply_string_predicate(args: &[Expr]) -> Result<Expr, MyLispError> {
        check_arity(args, 1, "string?")?;

        let result = matches!(args[0], Expr::String(_));
        Ok(Expr::Bool(result))
    }

    // ========== I/O 操作 ==========

    /// 显示输出: (display arg1 arg2 ...)
    pub fn apply_display(args: &[Expr]) -> Result<Expr, MyLispError> {
        for arg in args {
            match arg {
                Expr::String(s) => print!("{}", s),
                _ => print!("{}", arg),
            }
        }
        Ok(Expr::Nil)
    }

    /// 输出换行: (newline)
    pub fn apply_newline() -> Result<Expr, MyLispError> {
        println!();
        Ok(Expr::Nil)
    }

    /// 反转列表: (reverse list)
    pub fn apply_reverse(args: &[Expr]) -> Result<Expr, MyLispError> {
        check_arity(args, 1, "reverse")?;

        match &args[0] {
            Expr::List(l) => {
                let reversed: Vec<Expr> = l.iter().rev().cloned().collect();
                Ok(Expr::List(reversed))
            }
            Expr::Nil => Ok(Expr::List(vec![])),
            _ => Err(MyLispError::type_error("list", crate::utils::get_type_name(&args[0]), "reverse")),
        }
    }
}

/// 高阶函数应用器
pub struct HigherOrder;

impl HigherOrder {
    /// 映射函数: (map func list)
    pub fn apply_map(args: &[Expr], env: &mut Env, eval_fn: impl Fn(Expr, &mut Env, bool) -> Result<Expr, MyLispError>) -> Result<Expr, MyLispError> {
        check_arity(args, 2, "map")?;

        let func = &args[0];
        let list = match &args[1] {
            Expr::List(l) => l,
            _ => return Err(MyLispError::type_error("list", crate::utils::get_type_name(&args[1]), "map")),
        };

        let mut result = Vec::new();
        for item in list {
            let call = Expr::List(vec![func.clone(), item.clone()]);
            let mapped = eval_fn(call, env, false)?;
            result.push(mapped);
        }

        Ok(Expr::List(result))
    }

    /// 过滤列表: (filter predicate list)
    pub fn apply_filter(args: &[Expr], env: &mut Env, eval_fn: impl Fn(Expr, &mut Env, bool) -> Result<Expr, MyLispError>) -> Result<Expr, MyLispError> {
        check_arity(args, 2, "filter")?;

        let func = &args[0];
        let list = match &args[1] {
            Expr::List(l) => l,
            _ => return Err(MyLispError::type_error("list", crate::utils::get_type_name(&args[1]), "filter")),
        };

        let mut result = Vec::new();
        for item in list {
            let call = Expr::List(vec![func.clone(), item.clone()]);
            let predicate_result = eval_fn(call, env, false)?;

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

    /// 折叠列表: (fold func initial list)
    pub fn apply_fold(args: &[Expr], env: &mut Env, eval_fn: impl Fn(Expr, &mut Env, bool) -> Result<Expr, MyLispError>) -> Result<Expr, MyLispError> {
        check_arity(args, 3, "fold")?;

        let func = &args[0];
        let initial = &args[1];
        let list = match &args[2] {
            Expr::List(l) => l,
            _ => return Err(MyLispError::type_error("list", crate::utils::get_type_name(&args[2]), "fold")),
        };

        let mut acc = initial.clone();
        for item in list {
            let call = Expr::List(vec![func.clone(), acc, item.clone()]);
            acc = eval_fn(call, env, false)?;
        }

        Ok(acc)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arithmetic() {
        let args = vec![Expr::Number(1.0), Expr::Number(2.0), Expr::Number(3.0)];

        assert_eq!(Builtins::apply_add(&args).unwrap(), Expr::Number(6.0));
        assert_eq!(Builtins::apply_sub(&args).unwrap(), Expr::Number(-4.0));
        assert_eq!(Builtins::apply_mul(&args).unwrap(), Expr::Number(6.0));
    }

    #[test]
    fn test_comparison() {
        let args1 = vec![Expr::Number(5.0), Expr::Number(3.0)];
        let args2 = vec![Expr::Number(3.0), Expr::Number(5.0)];

        assert_eq!(Builtins::apply_gt(&args1).unwrap(), Expr::Bool(true));
        assert_eq!(Builtins::apply_lt(&args2).unwrap(), Expr::Bool(true));
        assert_eq!(Builtins::apply_le(&args2).unwrap(), Expr::Bool(true));
        assert_eq!(Builtins::apply_ge(&args1).unwrap(), Expr::Bool(true));
    }

    #[test]
    fn test_list_operations() {
        let list = Expr::List(vec![Expr::Number(1.0), Expr::Number(2.0), Expr::Number(3.0)]);
        let args = vec![list.clone()];

        assert_eq!(Builtins::apply_head(&args).unwrap(), Expr::Number(1.0));
        assert_eq!(Builtins::apply_tail(&args).unwrap(), Expr::List(vec![Expr::Number(2.0), Expr::Number(3.0)]));
        assert_eq!(Builtins::apply_length(&args).unwrap(), Expr::Number(3.0));
    }

    #[test]
    fn test_predicates() {
        let num = vec![Expr::Number(42.0)];
        let sym = vec![Expr::Symbol("x".to_string())];
        let list = vec![Expr::List(vec![])];

        assert_eq!(Builtins::apply_number_predicate(&num).unwrap(), Expr::Bool(true));
        assert_eq!(Builtins::apply_symbol_predicate(&sym).unwrap(), Expr::Bool(true));
        assert_eq!(Builtins::apply_list_predicate(&list).unwrap(), Expr::Bool(true));
        assert_eq!(Builtins::apply_null(&list).unwrap(), Expr::Bool(true));
    }
}
