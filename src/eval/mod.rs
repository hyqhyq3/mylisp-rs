//! 求值器模块
//!
//! 这是 MyLisp 解释器的核心模块，负责表达式的求值。
//! 当前这个模块包含了所有的求值逻辑，将会在后续的重构中拆分为多个子模块。

pub mod error;
pub mod builtins;

// 为了向后兼容，暂时导出 error 模块的内容
pub use error::MyLispError;

// 包含原始的 eval.rs 实现
include!("../eval_impl.rs");
