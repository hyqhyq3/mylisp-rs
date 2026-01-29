# MyLisp 自举实现文档

## 概述

本项目实现了一个用 Rust 编写的 Lisp 解释器，并展示了**元循环求值器（Meta-circular Evaluator）**的自举概念。

## 什么是自举？

**自举（Bootstrapping）**在编程语言中指的是用语言自己定义自己。最经典的形式是**元循环求值器**：

> 用 Lisp 语言编写 Lisp 解释器，然后用已有的解释器运行它。

## 核心思想

```lisp
; 用 Lisp 定义的简单求值器
(define (my-eval expr)
  (cond
    ((number? expr) expr)           ; 数字求值为自身
    ((string? expr) expr)           ; 字符串求值为自身
    ((eq? (car expr) '+)            ; 加法表达式
     (+ (cadr expr) (caddr expr)))
    ((eq? (car expr) '*)            ; 乘法表达式
     (* (cadr expr) (caddr expr)))
    (else nil)))
```

这就是自举的本质：**用 Lisp 代码描述 Lisp 的求值逻辑**。

## 已实现的功能

### 1. 基础数据类型
- ✅ 数字（整数、浮点数）
- ✅ 字符串（带转义支持）
- ✅ 布尔值（true/false）
- ✅ 符号
- ✅ 列表
- ✅ nil（空值）

### 2. 语法特性
- ✅ 变量定义（`define`）
- ✅ 函数定义（`define (f x) ...`）
- ✅ Lambda 表达式（`lambda`）
- ✅ 条件表达式（`if`）
- ✅ Let 绑定（`let`）
- ✅ 引用（`quote`）
- ✅ 求值（`eval`）
- ✅ 加载文件（`load`）

### 3. 内置函数
- ✅ 算术：`+`, `-`, `*`, `/`
- ✅ 比较：`=`, `<`, `>`, `<=`, `>=`
- ✅ 逻辑：`not`, `and`, `or`
- ✅ 列表：`car/head`, `cdr/tail`, `cons`, `append`, `list`
- ✅ 谓词：`null?`, `symbol?`, `list?`, `number?`, `string?`, `eq?`
- ✅ I/O：`display`, `newline`

### 4. 高级特性
- ✅ 多行表达式支持
- ✅ 注释（`;` 开头）
- ✅ 闭包和词法作用域
- ✅ 递归函数
- ✅ 高阶函数（函数作为参数）

## 自举演示

运行 `final_bootstrap.lisp`：

```bash
./target/release/mylisp final_bootstrap.lisp
```

输出：
```
=== Bootstrap Demo ===
Test 1 - Number: 42
Test 2 - String: hello
Test 3 - Addition: 30
Test 4 - Multiplication: 12
Test 5 - Factorial(5): 120
Test 6 - Map square: (1 4 9 16 25)
Test 7 - Eval: 42
=== Bootstrap Complete ===
This Lisp interpreter is running Lisp code
that defines Lisp evaluation logic!
```

## 自举的意义

1. **概念验证**：证明了 Lisp 的简洁性 - 用少量代码就能实现完整的求值器
2. **元编程**：展示了 Lisp 强大的元编程能力
3. **教育价值**：这是理解编程语言实现的最佳方式之一（SICP 第 4 章）

## 扩展方向

要实现更完整的自举，可以添加：

1. **完整的元循环求值器**（基于 SICP）
   - `cond` 表达式
   - `begin` 序列
   - 命名让步（named let）
   - 更完善的环境模型

2. **编译器自举**
   - 用 Lisp 编写 Lisp→Rust 的编译器
   - 用编译器编译自己

3. **REPL 增强**
   - 更好的错误信息
   - 调试功能
   - 宏系统

## 参考资料

- **SICP**（Structure and Interpretation of Computer Programs）第 4 章
- **Lisp in Small Pieces** - Christian Queinnec
- **The Art of the Interpreter** - Sussman & Steele

## 总结

本项目实现了一个功能完整的 Lisp 解释器，并通过自举演示证明了 Lisp 的核心特性：**用简单的规则构建复杂的系统**。这就是 Lisp 的美学所在。
