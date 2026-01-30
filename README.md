# MyLisp

一个用 Rust 实现的 Lisp 解释器，展示了**元循环求值**（Meta-circular Evaluation）——即使用 Lisp 自身来实现 Lisp 解释器。这是基于 SICP（计算机程序的构造和解释）第4章概念的教育项目。

## 特性

### 核心功能
- ✅ **完整的 Lisp 语法**：支持 S-expressions、符号、字符串、数字、布尔值
- ✅ **词法作用域**：实现环境链，支持变量遮蔽
- ✅ **Lambda 闭包**：高阶函数与词法闭包
- ✅ **惰性求值**：延迟求值与强制求值机制
- ✅ **尾递归优化**（TCO）：防止栈溢出
- ✅ **宏系统**：支持 `...` 省略号宏展开
- ✅ **元循环求值**：可用 Lisp 实现 Lisp 求值器

### 特殊形式
- `define` - 变量绑定与函数定义
- `set!` - 变量赋值
- `if` - 条件分支
- `lambda` - 匿名函数
- `let` - 局部作用域
- `quote` - 引用（简写 `'`）
- `eval` - 元求值
- `load` - 加载文件
- `cond` - 多分支条件
- `begin` - 顺序执行
- `...` - 宏省略号模式匹配

### 内置函数
- **算术**: `+`, `-`, `*`, `/`
- **比较**: `=`, `<`, `>`, `<=`, `>=`
- **逻辑**: `not`, `and`, `or`
- **列表**: `car`/`head`, `cdr`/`tail`, `cons`, `append`, `list`
- **谓词**: `null?`, `symbol?`, `list?`, `number?`, `string?`, `eq?`
- **I/O**: `display`, `newline`

### FFI 外部函数调用
- `ffi-call` - 调用 C 动态库函数
- `syscall` - 直接系统调用

## 快速开始

### 构建

```bash
# 安装 Rust 工具链（如果需要）
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# 构建发布版本
cargo build --release
```

### 运行

```bash
# 运行 Lisp 文件
./target/release/mylisp program.lisp

# 启动 REPL（交互模式）
./target/release/mylisp
```

### 示例

#### 基础运算
```lisp
; 算术运算
(+ 1 2 3)        ; => 6
(* 2 3)          ; => 6
(/ 10 2)         ; => 5

; 列表操作
(define lst (list 1 2 3 4 5))
(car lst)        ; => 1
(cdr lst)        ; => (2 3 4 5)
(append lst lst) ; => (1 2 3 4 5 1 2 3 4 5)
```

#### Lambda 与高阶函数
```lisp
; Lambda 表达式
(define square
  (lambda (x) (* x x)))

(square 5)       ; => 25

; 高阶函数
(define map
  (lambda (fn lst)
    (if (null? lst)
        '()
        (cons (fn (car lst))
              (map fn (cdr lst))))))

(map square '(1 2 3 4 5))  ; => (1 4 9 16 25)
```

#### 宏系统
```lisp
; 使用省略号定义宏
(define-macro when
  (lambda (condition . body)
    `(if ,condition
         (begin ,@body)
         #f)))

(when (= 2 2)
  (display "条件成立!")
  (newline))  ; 输出: 条件成立!
```

#### 惰性求值
```lisp
; 创建惰性序列
(define naturals
  (lambda (n)
    (cons n (delay (naturals (+ n 1))))))

; 无限流取前 10 个
(define take
  (lambda (n stream)
    (if (= n 0)
        '()
        (cons (car stream)
              (take (- n 1) (force (cdr stream)))))))

(take 10 (naturals 1))  ; => (1 2 3 4 5 6 7 8 9 10)
```

## 元循环求值

MyLisp 的核心特色是**元循环求值**——使用 Lisp 自身来实现 Lisp 求值器：

```lisp
; 运行元循环演示
./target/release/mylisp final_bootstrap.lisp
```

`final_bootstrap.lisp` 实现了一个简单的 `my-eval` 函数，展示了如何用 Lisp 定义 Lisp：

```lisp
(define my-eval
  (lambda (exp env)
    (cond
      ; 自求值表达式
      ((number? exp) exp)
      ((string? exp) exp)
      ((null? exp) '())
      ((boolean? exp) exp)

      ; 变量引用
      ((symbol? exp) (lookup exp env))

      ; 特殊形式...
      )))

; 使用 my-eval 求值表达式
(my-eval '(+ 1 2) global-env)  ; => 3
```

这展示了编程语言的一个深刻概念：**语言可以定义自身**。

## 架构

```
src/
├── main.rs           # 入口：文件执行与 REPL
├── lexer.rs          # 词法分析器：Token 化
├── parser.rs         # 语法分析器：AST 构建
├── ast.rs            # 抽象语法树定义
├── env.rs            # 环境：词法作用域链
└── eval/
    ├── mod.rs        # 求值器模块
    ├── impl.rs       # 核心求值逻辑
    ├── error.rs      # 错误类型
    ├── builtins.rs   # 内置函数
    ├── special_forms.rs  # 特殊形式
    ├── macros.rs     # 宏系统
    ├── tco.rs        # 尾递归优化
    └── ffi.rs        # FFI 外部调用
```

### 求值流程

```
输入字符串
    ↓
Lexer (词法分析)
    ↓
Token 流
    ↓
Parser (语法分析)
    ↓
AST (抽象语法树)
    ↓
Evaluator (求值) ← Environment (环境)
    ↓
结果
```

## 测试

运行完整功能测试：

```bash
./target/release/mylisp test_all.lisp
```

测试覆盖：
- ✅ 算术运算
- ✅ 列表操作
- ✅ Lambda 闭包
- ✅ 递归与 TCO
- ✅ 宏展开
- ✅ 惰性求值
- ✅ 元循环求值

## 文档

- [CLAUDE.md](CLAUDE.md) - 开发指南
- [BOOTSTRAP.md](BOOTSTRAP.md) - 元循环求值详解
- [FEATURES.md](FEATURES.md) - 完整功能列表
- [TCO.md](TCO.md) - 尾递归优化实现
- [MACRO.md](MACRO.md) - 宏系统文档

## 设计原则

1. **简洁性**：最小化核心，用组合实现复杂功能
2. **教育性**：清晰的代码结构，便于学习 PL 原理
3. **可扩展**：模块化设计，易于添加新特性
4. **正确性**：词法作用域、闭包语义符合 Scheme 标准

## 已知限制

1. **闭包突变**：`set!` 无法修改外层作用域变量
2. **CPS 未实现**：不支持 `call/cc`（延续传递风格）
3. **类型系统**：动态类型，无静态检查
4. **宏卫生**：宏展开不保证符号卫生性

## 未来方向

- [ ] 添加更多内置函数（字符串处理、数学函数）
- [ ] 实现 `call/cc` 和完整 CPS
- [ ] 支持编译为字节码
- [ ] 添加模块系统
- [ ] 实现卫生宏
- [ ] 优化性能（字节码解释、JIT）

## 参考资料

- [SICP 第4章：元循环求值器](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html)
- [Rust 编程语言](https://doc.rust-lang.org/book/)
- [Scheme 语言标准](https://schemers.org/)

## 许可证

MIT License

---

**用 Lisp 定义 Lisp，理解编程语言的本质。**
