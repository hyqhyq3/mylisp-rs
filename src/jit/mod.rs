//! MyLisp JIT 编译模块
//!
//! 此模块实现了 MyLisp 的即时编译（JIT）功能，包括：
//! - 字节码设计和编码
//! - AST 到字节码的编译器
//! - 字节码虚拟机
//! - Cranelift JIT 后端集成
//!
//! # 架构
//!
//! ```text
//! AST Source → Bytecode Compiler → Bytecode VM → Result
//!                                 ↓
//!                            Cranelift JIT → Native Code
//! ```
//!
//! # 使用示例
//!
//! ```rust
//! use mylisp::jit::{BytecodeCompiler, BytecodeVM};
//!
//! // 编译为字节码
//! let mut compiler = BytecodeCompiler::new();
//! let bytecode = compiler.compile("(+ 1 2 3)").unwrap();
//!
//! // 执行字节码
//! let mut vm = BytecodeVM::new(bytecode);
//! let result = vm.run().unwrap();
//! ```

pub mod bytecode;
pub mod compiler;
pub mod vm;

// 重新导出主要类型
pub use bytecode::{OpCode, Instruction, Chunk};
pub use compiler::BytecodeCompiler;
pub use vm::BytecodeVM;
