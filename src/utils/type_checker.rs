//! 类型检查工具
//!
//! 提供统一的类型检查逻辑，减少代码重复

use crate::ast::Expr;
use crate::eval::error::MyLispError;

/// 验证表达式是数字
///
/// # 参数
/// - `expr`: 待验证的表达式
/// - `context`: 上下文描述（用于错误消息）
///
/// # 返回
/// 返回数字值
pub fn expect_number(expr: &Expr, context: &str) -> Result<f64, MyLispError> {
    match expr {
        Expr::Number(n) => Ok(*n),
        _ => Err(MyLispError::type_error(
            "number",
            get_type_name(expr),
            context,
        )),
    }
}

/// 验证表达式是字符串
///
/// # 参数
/// - `expr`: 待验证的表达式
/// - `context`: 上下文描述（用于错误消息）
///
/// # 返回
/// 返回字符串值
pub fn expect_string(expr: &Expr, context: &str) -> Result<String, MyLispError> {
    match expr {
        Expr::String(s) => Ok(s.clone()),
        _ => Err(MyLispError::type_error(
            "string",
            get_type_name(expr),
            context,
        )),
    }
}

/// 验证表达式是布尔值
///
/// # 参数
/// - `expr`: 待验证的表达式
/// - `context`: 上下文描述（用于错误消息）
///
/// # 返回
/// 返回布尔值
pub fn expect_bool(expr: &Expr, context: &str) -> Result<bool, MyLispError> {
    match expr {
        Expr::Bool(b) => Ok(*b),
        _ => Err(MyLispError::type_error(
            "boolean",
            get_type_name(expr),
            context,
        )),
    }
}

/// 检查表达式是否为数字
pub fn is_number(expr: &Expr) -> bool {
    matches!(expr, Expr::Number(_))
}

/// 检查表达式是否为字符串
pub fn is_string(expr: &Expr) -> bool {
    matches!(expr, Expr::String(_))
}

/// 检查表达式是否为布尔值
pub fn is_bool(expr: &Expr) -> bool {
    matches!(expr, Expr::Bool(_))
}

/// 检查表达式是否为符号
pub fn is_symbol(expr: &Expr) -> bool {
    matches!(expr, Expr::Symbol(_))
}

/// 检查表达式是否为列表
pub fn is_list(expr: &Expr) -> bool {
    matches!(expr, Expr::List(_))
}

/// 检查表达式是否为空列表或 nil
pub fn is_nil(expr: &Expr) -> bool {
    match expr {
        Expr::Nil => true,
        Expr::List(list) => list.is_empty(),
        _ => false,
    }
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

/// 批量验证多个表达式为数字
///
/// # 参数
/// - `args`: 表达式列表
/// - `context`: 上下文描述
///
/// # 返回
/// 返回数字向量
pub fn expect_numbers(args: &[Expr], context: &str) -> Result<Vec<f64>, MyLispError> {
    args.iter()
        .map(|arg| expect_number(arg, context))
        .collect()
}

/// 验证列表表达式中的元素是否都是数字
///
/// # 参数
/// - `list_expr`: 列表表达式
/// - `context`: 上下文描述
///
/// # 返回
/// 返回数字向量
pub fn expect_number_list(list_expr: &Expr, context: &str) -> Result<Vec<f64>, MyLispError> {
    match list_expr {
        Expr::List(items) => expect_numbers(items, context),
        _ => Err(MyLispError::type_error(
            "list",
            get_type_name(list_expr),
            context,
        )),
    }
}

/// 从参数列表中提取可选的数字参数
///
/// # 参数
/// - `args`: 参数列表
/// - `index`: 参数索引
/// - `default`: 默认值
///
/// # 返回
/// 返回数字值（如果存在且为数字）或默认值
pub fn get_optional_number(args: &[Expr], index: usize, default: f64) -> f64 {
    args.get(index)
        .and_then(|expr| match expr {
            Expr::Number(n) => Some(*n),
            _ => None,
        })
        .unwrap_or(default)
}

/// 检查两个表达式的类型是否相同
pub fn same_type(expr1: &Expr, expr2: &Expr) -> bool {
    std::mem::discriminant(expr1) == std::mem::discriminant(expr2)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expect_number() {
        let num = Expr::Number(42.0);
        assert_eq!(expect_number(&num, "test").unwrap(), 42.0);

        let sym = Expr::Symbol("x".to_string());
        assert!(expect_number(&sym, "test").is_err());
    }

    #[test]
    fn test_expect_string() {
        let s = Expr::String("hello".to_string());
        assert_eq!(expect_string(&s, "test").unwrap(), "hello");

        let num = Expr::Number(1.0);
        assert!(expect_string(&num, "test").is_err());
    }

    #[test]
    fn test_expect_bool() {
        let b = Expr::Bool(true);
        assert_eq!(expect_bool(&b, "test").unwrap(), true);

        let num = Expr::Number(1.0);
        assert!(expect_bool(&num, "test").is_err());
    }

    #[test]
    fn test_type_predicates() {
        assert!(is_number(&Expr::Number(1.0)));
        assert!(!is_number(&Expr::Symbol("x".to_string())));

        assert!(is_string(&Expr::String("hi".to_string())));
        assert!(!is_string(&Expr::Number(1.0)));

        assert!(is_bool(&Expr::Bool(true)));
        assert!(!is_bool(&Expr::Number(1.0)));

        assert!(is_symbol(&Expr::Symbol("x".to_string())));
        assert!(!is_symbol(&Expr::Number(1.0)));

        assert!(is_list(&Expr::List(vec![])));
        assert!(!is_list(&Expr::Number(1.0)));

        assert!(is_nil(&Expr::Nil));
        assert!(is_nil(&Expr::List(vec![])));
        assert!(!is_nil(&Expr::Number(1.0)));
    }

    #[test]
    fn test_expect_numbers() {
        let args = vec![
            Expr::Number(1.0),
            Expr::Number(2.0),
            Expr::Number(3.0),
        ];
        assert_eq!(expect_numbers(&args, "test").unwrap(), vec![1.0, 2.0, 3.0]);

        let args2 = vec![Expr::Number(1.0), Expr::Symbol("x".to_string())];
        assert!(expect_numbers(&args2, "test").is_err());
    }

    #[test]
    fn test_expect_number_list() {
        let list = Expr::List(vec![Expr::Number(1.0), Expr::Number(2.0)]);
        assert_eq!(expect_number_list(&list, "test").unwrap(), vec![1.0, 2.0]);

        let not_list = Expr::Number(1.0);
        assert!(expect_number_list(&not_list, "test").is_err());
    }

    #[test]
    fn test_get_optional_number() {
        let args = vec![Expr::Number(1.0), Expr::Number(2.0)];
        assert_eq!(get_optional_number(&args, 0, 99.0), 1.0);
        assert_eq!(get_optional_number(&args, 1, 99.0), 2.0);
        assert_eq!(get_optional_number(&args, 2, 99.0), 99.0);

        let args2 = vec![Expr::Symbol("x".to_string())];
        assert_eq!(get_optional_number(&args2, 0, 99.0), 99.0);
    }

    #[test]
    fn test_same_type() {
        assert!(same_type(&Expr::Number(1.0), &Expr::Number(2.0)));
        assert!(same_type(&Expr::Symbol("x".to_string()), &Expr::Symbol("y".to_string())));
        assert!(!same_type(&Expr::Number(1.0), &Expr::Symbol("x".to_string())));
    }
}
