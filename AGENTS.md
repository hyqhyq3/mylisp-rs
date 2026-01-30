# AGENTS.md

本文档为在本仓库工作的智能体提供说明与约定，参考 CLAUDE.md 内容并结合当前实现做了更新。

## 项目概览

MyLisp 是一个用 Rust 编写的 Lisp 解释器，重点展示**元循环求值（bootstrapping）**的思想，概念来自 SICP 第四章。

## 构建与运行

### 构建
```bash
# 安装 Rust toolchain（如需要）
source ~/.cargo/env

# 构建 release 版本
cargo build --release
```

### 运行
```bash
# 运行 Lisp 文件
./target/release/mylisp <file.lisp>

# 启动 REPL
./target/release/mylisp
```

### 测试/演示
```bash
# 运行自举演示
./target/release/mylisp final_bootstrap.lisp

# 综合功能测试
./target/release/mylisp test_all.lisp

# TCO 测试
./target/release/mylisp test_tco.lisp
./target/release/mylisp test_tco_stress.lisp
```

## 架构与模块

### 1) 词法分析器 `src/lexer.rs`
- 将输入字符串拆分成 token（数字、符号、字符串、布尔、括号等）
- 支持字符串转义（`\n`、`\t`、`\"` 等）
- 支持 `;` 注释

### 2) 语法分析器 `src/parser.rs`
- 将 token 流构造为 AST
- 支持 S 表达式嵌套
- 支持跨行表达式

### 3) AST `src/ast.rs`
- 表达式类型：`Number`、`Symbol`、`String`、`Bool`、`List`、`Nil`、`Lambda`
- `Lambda` 携带参数、函数体与捕获环境（闭包）

### 4) 环境 `src/env.rs`
- 基于父子链的词法作用域
- `define`/`get`/`set`
- `flatten_with_parent` 用于在闭包与调用环境间合并可见性

### 5) 求值器 `src/eval/`
- 核心求值逻辑在 `src/eval_impl.rs`（通过 `src/eval/mod.rs` include）
- 特殊形式在 `src/eval/special_forms.rs`
- 内置函数在 `src/eval/builtins.rs`
- 宏系统在 `src/eval/macros.rs`
- TCO 逻辑在 `src/eval/tco.rs`

### 6) 入口 `src/main.rs`
- 文件模式：解析并执行整个文件
- REPL 模式：交互式执行

## 关键实现说明

### Lambda / 闭包
- `lambda` 会构造 `Expr::Lambda`，捕获定义时的环境。
- 调用时会合并捕获环境与调用时环境，保证闭包语义与全局可见性。

### 多行表达式
- `eval_program` 以完整 token 流解析，允许跨行表达式。

### 函数定义语法糖
- 同时支持：
  - `(define name (lambda (x) body))`
  - `(define (name x) body)`

### TCO
- 尾调用优化使用 trampoline（`src/eval/tco.rs`）。
- 尾位置由求值器传递；函数体最后一个表达式使用尾上下文求值。

## 常用文件

- `final_bootstrap.lisp`：自举演示
- `test_all.lisp`：综合功能测试
- `test_tco*.lisp`：尾递归测试
- `BOOTSTRAP.md`：自举原理说明

## 已知限制（仅列出当前仍需确认/改进项）

- 依赖于 Lisp 文件的错误打印仍比较粗糙（运行时错误文本在 REPL/文件模式中一致性一般）。
- 仍有部分历史遗留函数在 `eval_impl.rs` 中未被调用（编译期会有 unused 警告）。

如需添加新特性，请优先在 `src/eval/special_forms.rs` 或 `src/eval/builtins.rs` 中扩展，并在 `src/eval_impl.rs` 分发调用。
