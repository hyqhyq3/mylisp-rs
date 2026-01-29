//! 工具模块
//!
//! 提供参数验证、类型检查等通用功能

mod validator;
mod type_checker;

// 从 validator 导出
pub use validator::{
    check_arity,
    check_min_arity,
    check_arity_range,
    expect_symbol,
    expect_list,
    expect_non_empty_list,
    expect_non_empty,
    is_valid_identifier,
};

// 从 type_checker 导出（避免重导出 get_type_name，它已在 validator 中定义）
pub use type_checker::{
    expect_number,
    expect_string,
    expect_bool,
    is_number,
    is_string,
    is_bool,
    is_symbol,
    is_list,
    is_nil,
    expect_numbers,
    expect_number_list,
    get_optional_number,
    same_type,
};
