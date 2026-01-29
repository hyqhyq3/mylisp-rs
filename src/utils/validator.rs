//! 参数验证工具
//!
//! 提供统一的参数验证逻辑，减少代码重复

use crate::ast::Expr;
use crate::eval::error::MyLispError;

/// 检查参数数量
///
/// # 参数
/// - `args`: 参数列表
/// - `expected`: 期望的参数数量
/// - `function_name`: 函数名称（用于错误消息）
///
/// # 示例
/// ```
/// check_arity(&args, 2, "add")?;
/// ```
pub fn check_arity(args: &[Expr], expected: usize, function_name: &str) -> Result<(), MyLispError> {
    if args.len() != expected {
        return Err(MyLispError::arity_error(
            function_name,
            format!("{}", expected),
            args.len(),
        ));
    }
    Ok(())
}

/// 检查最少参数数量
///
/// # 参数
/// - `args`: 参数列表
/// - `min`: 最少参数数量
/// - `function_name`: 函数名称（用于错误消息）
///
/// # 示例
/// ```
/// check_min_arity(&args, 1, "+")?;
/// ```
pub fn check_min_arity(args: &[Expr], min: usize, function_name: &str) -> Result<(), MyLispError> {
    if args.len() < min {
        return Err(MyLispError::arity_error(
            function_name,
            format!("at least {}", min),
            args.len(),
        ));
    }
    Ok(())
}

/// 检查参数范围
///
/// # 参数
/// - `args`: 参数列表
/// - `min`: 最少参数数量
/// - `max`: 最多参数数量
/// - `function_name`: 函数名称（用于错误消息）
pub fn check_arity_range(
    args: &[Expr],
    min: usize,
    max: usize,
    function_name: &str,
) -> Result<(), MyLispError> {
    if args.len() < min || args.len() > max {
        return Err(MyLispError::arity_error(
            function_name,
            format!("{} to {}", min, max),
            args.len(),
        ));
    }
    Ok(())
}

/// 验证表达式是符号
///
/// # 参数
/// - `expr`: 待验证的表达式
/// - `context`: 上下文描述（用于错误消息）
pub fn expect_symbol(expr: &Expr, context: &str) -> Result<String, MyLispError> {
    match expr {
        Expr::Symbol(name) => Ok(name.clone()),
        _ => Err(MyLispError::type_error(
            "symbol",
            get_type_name(expr),
            context,
        )),
    }
}

/// 验证表达式是列表
///
/// # 参数
/// - `expr`: 待验证的表达式
/// - `context`: 上下文描述（用于错误消息）
pub fn expect_list(expr: &Expr, context: &str) -> Result<Vec<Expr>, MyLispError> {
    match expr {
        Expr::List(list) => Ok(list.clone()),
        _ => Err(MyLispError::type_error("list", get_type_name(expr), context)),
    }
}

/// 验证表达式不是空列表
///
/// # 参数
/// - `expr`: 待验证的表达式
/// - `context`: 上下文描述（用于错误消息）
pub fn expect_non_empty_list(expr: &Expr, context: &str) -> Result<Vec<Expr>, MyLispError> {
    match expr {
        Expr::List(list) if !list.is_empty() => Ok(list.clone()),
        Expr::List(_) => Err(MyLispError::runtime(format!(
            "{}: list cannot be empty",
            context
        ))),
        _ => Err(MyLispError::type_error("list", get_type_name(expr), context)),
    }
}

/// 验证参数至少有一个
pub fn expect_non_empty(args: &[Expr], function_name: &str) -> Result<(), MyLispError> {
    if args.is_empty() {
        return Err(MyLispError::arity_error(function_name, "at least 1", 0));
    }
    Ok(())
}

/// 获取表达式的类型名称
pub fn get_type_name(expr: &Expr) -> String {
    match expr {
        Expr::Number(_) => "number".to_string(),
        Expr::Symbol(_) => "symbol".to_string(),
        Expr::String(_) => "string".to_string(),
        Expr::Bool(_) => "boolean".to_string(),
        Expr::List(_) => "list".to_string(),
        Expr::Nil => "nil".to_string(),
    }
}

/// 验证字符串参数是否为有效标识符
pub fn is_valid_identifier(name: &str) -> bool {
    if name.is_empty() {
        return false;
    }

    // 第一个字符必须是字母或特殊符号
    let first = name.chars().next().unwrap();
    if !first.is_alphabetic() && !matches!(first, '+' | '-' | '*' | '/' | '=' | '<' | '>' | '!' | '?' | '%' | '&' | '|' | '^' | '~' | '$' | '_') {
        return false;
    }

    // 其余字符可以是字母、数字或特殊符号
    name.chars().all(|c| {
        c.is_alphanumeric() || matches!(c, '+' | '-' | '*' | '/' | '=' | '<' | '>' | '!' | '?' | '%' | '&' | '|' | '^' | '~' | '$' | '_' | '.' | ':')
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_check_arity() {
        let args = vec![Expr::Number(1.0), Expr::Number(2.0)];
        assert!(check_arity(&args, 2, "test").is_ok());
        assert!(check_arity(&args, 3, "test").is_err());
    }

    #[test]
    fn test_check_min_arity() {
        let args = vec![Expr::Number(1.0)];
        assert!(check_min_arity(&args, 1, "test").is_ok());
        assert!(check_min_arity(&args, 2, "test").is_err());
    }

    #[test]
    fn test_check_arity_range() {
        let args = vec![Expr::Number(1.0), Expr::Number(2.0)];
        assert!(check_arity_range(&args, 1, 3, "test").is_ok());
        assert!(check_arity_range(&args, 3, 5, "test").is_err());
    }

    #[test]
    fn test_expect_symbol() {
        let sym = Expr::Symbol("x".to_string());
        assert_eq!(expect_symbol(&sym, "test").unwrap(), "x");

        let num = Expr::Number(1.0);
        assert!(expect_symbol(&num, "test").is_err());
    }

    #[test]
    fn test_expect_list() {
        let list = Expr::List(vec![Expr::Number(1.0)]);
        assert!(expect_list(&list, "test").is_ok());

        let num = Expr::Number(1.0);
        assert!(expect_list(&num, "test").is_err());
    }

    #[test]
    fn test_expect_non_empty_list() {
        let list = Expr::List(vec![Expr::Number(1.0)]);
        assert!(expect_non_empty_list(&list, "test").is_ok());

        let empty = Expr::List(vec![]);
        assert!(expect_non_empty_list(&empty, "test").is_err());
    }

    #[test]
    fn test_get_type_name() {
        assert_eq!(get_type_name(&Expr::Number(1.0)), "number");
        assert_eq!(get_type_name(&Expr::Symbol("x".to_string())), "symbol");
        assert_eq!(get_type_name(&Expr::String("hi".to_string())), "string");
        assert_eq!(get_type_name(&Expr::Bool(true)), "boolean");
        assert_eq!(get_type_name(&Expr::List(vec![])), "list");
        assert_eq!(get_type_name(&Expr::Nil), "nil");
    }

    #[test]
    fn test_is_valid_identifier() {
        assert!(is_valid_identifier("foo"));
        assert!(is_valid_identifier("foo-bar"));
        assert!(is_valid_identifier("foo?"));
        assert!(is_valid_identifier("+"));
        assert!(is_valid_identifier("*"));
        assert!(is_valid_identifier("set!"));
        assert!(!is_valid_identifier(""));
        assert!(!is_valid_identifier("123abc"));
    }
}
