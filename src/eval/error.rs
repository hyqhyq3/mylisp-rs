//! 统一的错误类型系统
//!
//! 提供结构化的错误类型，替代简单的 String 错误消息

use std::fmt;

/// MyLisp 统一错误类型
#[derive(Debug, Clone, PartialEq)]
pub enum MyLispError {
    /// 语法错误：括号不匹配、非法表达式等
    SyntaxError(String),

    /// 类型错误：期望的类型与实际类型不符
    TypeError {
        expected: String,
        found: String,
        context: String,
    },

    /// 运行时错误：未定义变量、除零等
    RuntimeError(String),

    /// 参数数量错误：函数调用参数数量不匹配
    ArityError {
        function: String,
        expected: String,
        found: usize,
    },

    /// 未定义错误：变量、函数或宏未定义
    UndefinedError(String),
}

impl MyLispError {
    /// 创建语法错误
    pub fn syntax(msg: impl Into<String>) -> Self {
        MyLispError::SyntaxError(msg.into())
    }

    /// 创建类型错误
    pub fn type_error(expected: impl Into<String>, found: impl Into<String>, context: impl Into<String>) -> Self {
        MyLispError::TypeError {
            expected: expected.into(),
            found: found.into(),
            context: context.into(),
        }
    }

    /// 创建运行时错误
    pub fn runtime(msg: impl Into<String>) -> Self {
        MyLispError::RuntimeError(msg.into())
    }

    /// 创建参数数量错误
    pub fn arity_error(function: impl Into<String>, expected: impl Into<String>, found: usize) -> Self {
        MyLispError::ArityError {
            function: function.into(),
            expected: expected.into(),
            found,
        }
    }

    /// 创建未定义错误
    pub fn undefined(msg: impl Into<String>) -> Self {
        MyLispError::UndefinedError(msg.into())
    }
}

impl fmt::Display for MyLispError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MyLispError::SyntaxError(msg) => write!(f, "Syntax error: {}", msg),
            MyLispError::TypeError { expected, found, context } => {
                write!(f, "Type error in {}: expected {}, found {}", context, expected, found)
            }
            MyLispError::RuntimeError(msg) => write!(f, "Runtime error: {}", msg),
            MyLispError::ArityError { function, expected, found } => {
                write!(f, "Arity error: '{}' expects {} arguments, found {}", function, expected, found)
            }
            MyLispError::UndefinedError(msg) => write!(f, "Undefined: {}", msg),
        }
    }
}

impl std::error::Error for MyLispError {}

/// 向后兼容：将 MyLispError 转换为 String
impl From<MyLispError> for String {
    fn from(err: MyLispError) -> Self {
        err.to_string()
    }
}

/// 转换 Result<Expr, MyLispError> 为 Result<Expr, String>
pub fn into_string_result<T>(result: Result<T, MyLispError>) -> Result<T, String> {
    result.map_err(|e| e.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        assert_eq!(
            MyLispError::syntax("unexpected token").to_string(),
            "Syntax error: unexpected token"
        );

        assert_eq!(
            MyLispError::type_error("number", "string", "addition").to_string(),
            "Type error in addition: expected number, found string"
        );

        assert_eq!(
            MyLispError::runtime("division by zero").to_string(),
            "Runtime error: division by zero"
        );

        assert_eq!(
            MyLispError::arity_error("add", "2", 3).to_string(),
            "Arity error: 'add' expects 2 arguments, found 3"
        );

        assert_eq!(
            MyLispError::undefined("x").to_string(),
            "Undefined: x"
        );
    }

    #[test]
    fn test_error_conversion() {
        let err: String = MyLispError::syntax("test").into();
        assert_eq!(err, "Syntax error: test");
    }

    #[test]
    fn test_into_string_result() {
        let result: Result<i32, String> = into_string_result(Ok(42));
        assert_eq!(result, Ok(42));

        let result: Result<i32, String> = into_string_result(Err(MyLispError::undefined("x")));
        assert_eq!(result, Err("Undefined: x".to_string()));
    }
}
