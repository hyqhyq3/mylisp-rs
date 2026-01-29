# MyLisp 宏系统文档

## 概述

MyLisp 现在支持强大的**卫生宏(Hygienic Macros)**系统,使用类似 Scheme 的 `syntax-rules` 语法。宏在编译时展开,允许你扩展语言的语法。

## 基本语法

```lisp
(define-syntax macro-name
  (syntax-rules (literal1 literal2 ...)
    ((pattern-template) template-to-expand)
    ...))
```

### 参数说明

- `macro-name`: 宏的名称
- `literal1 literal2 ...`: 字面量符号(在模式中必须精确匹配)
- `pattern-template`: 输入模式(使用模式变量)
- `template-to-expand`: 输出模板

## 示例

### 示例1: 简单宏 - `square`

```lisp
(define-syntax square
  (syntax-rules ()
    ((square x)
     (* x x))))

(square 5)  ; 展开为 (* 5 5),结果: 25
```

### 示例2: 条件宏 - `when`

```lisp
(define-syntax when
  (syntax-rules ()
    ((when test body)
     (if test body nil))))

(when #t
  (display "Hello"))
; 展开为 (if #t (display "Hello") nil)
```

### 示例3: 带字面量的宏 - `incf`

```lisp
(define-syntax incf
  (syntax-rules ()
    ((incf x)
     (set! x (+ x 1)))))

(define counter 0)
(incf counter)  ; counter 变为 1
```

## 模式变量

模式中的符号(非字面量)被视为**模式变量**,会匹配任意表达式:

```lisp
(define-syntax swap
  (syntax-rules ()
    ((swap a b)
     (let ((temp a))
       (set! a b)
       (set! b temp)))))

; 使用
(define x 1)
(define y 2)
(swap x y)  ; x=2, y=1
```

## 字面量

字面量是在模式中必须**精确匹配**的符号:

```lisp
(define-syntax my-if
  (syntax-rules (then else)
    ((my-if test then consequent else alternative)
     (if test consequent alternative))))

(my-if #t then 1 else 2)  ; ✅ 匹配
(my-if #t 1 2 2)          ; ❌ 不匹配(缺少 then/else)
```

## 宏展开过程

1. **读取**: 解析器读取 `(macro-name args ...)`
2. **查找**: 在全局宏表中查找 `macro-name`
3. **匹配**: 将参数与宏的规则模式进行匹配
4. **绑定**: 捕获模式变量的绑定
5. **展开**: 使用绑定的值填充模板
6. **递归**: 对展开结果再次进行宏展开

## 当前限制

### ❌ 不支持的功能

1. **省略号模式** (`...`):
   ```lisp
   ; 不支持
   (define-syntax when
     (syntax-rules ()
       ((when test body ...)  ; ❌ ... 未实现
        (if test (begin body ...)))))
   ```

2. **多表达式模式**:
   ```lisp
   ; 当前只能匹配固定数量的参数
   ((macro a b c) ...)  ; ✅ 固定3个参数
   ((macro a b ...) ...) ; ❌ 可变参数
   ```

3. **卫生宏的变量捕获保护**:
   - 当前实现是简单的文本替换
   - 可能会发生意外的变量捕获

### ✅ 支持的功能

1. **基本模式匹配**: 固定参数数量
2. **嵌套模式**: 支持嵌套列表匹配
3. **递归宏展开**: 宏可以调用其他宏
4. **字面量匹配**: 精确匹配关键字

## 测试

运行测试:
```bash
./target/release/mylisp test_macro_simple.lisp
```

预期输出:
```
=== Simple Macro Test ===
Test 1 - when macro:  when executed!
Test 2 - square macro:  square of 5 = 25
Test 3 - double-add macro:  double-add 3 4 = 14
=== Tests completed! ===
```

## 实现细节

### 核心数据结构

```rust
struct Macro {
    name: String,
    rules: Vec<MacroRule>,
}

struct MacroRule {
    pattern: Expr,     // 模式
    template: Expr,    // 模板
    literals: Vec<String>,  // 字面量
}
```

### 关键函数

1. **`eval_define_syntax`**: 定义宏
2. **`expand_macros`**: 递归展开所有宏
3. **`apply_macro`**: 应用单条宏规则
4. **`match_pattern`**: 模式匹配
5. **`expand_template`**: 模板展开

## 最佳实践

### ✅ 推荐

1. **使用宏简化常见模式**:
   ```lisp
   (define-syntax inc!
     (syntax-rules ()
       ((inc! x) (set! x (+ x 1)))))
   ```

2. **使用字面量提高可读性**:
   ```lisp
   (define-syntax case
     (syntax-rules (else)
       ((case (else body ...))
        (begin body ...))
       ...))
   ```

### ❌ 避免

1. **过度使用宏**: 函数通常更清晰
2. **复杂的模式匹配**: 当前实现有限制
3. **依赖副作用**: 宏展开时期望纯函数式

## 未来改进

1. **支持 `...` 省略号模式**:
   - 实现可变参数模式
   - 模式变量重复

2. **真正的卫生宏**:
   - 变量作用域隔离
   - 防止意外捕获

3. **模式匹配错误**:
   - 更好的错误消息
   - 调试支持

4. **宏展开调试**:
   - `macro-expand` 函数
   - 显示展开步骤

## 参考资料

- [Scheme R5RS: Macro System](https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.3)
- [The Scheme Programming Language: Macros](https://www.scheme.com/tspl4/syntax.html)
- [Writing Macros in Scheme](https://www.youtube.com/watch?v=cH8SYaPHkA0)
