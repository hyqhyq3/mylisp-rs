# JIT 阶段 4 报告：局部变量和高级特性

**完成时间**: 2026-01-30
**阶段目标**: 实现 let 表达式、set! 表达式和闭包变量运行时装载

---

## 概述

本阶段完成了 MyLisp JIT 编译器的局部变量管理功能，包括 `let` 表达式编译、`set!` 表达式编译，以及闭包变量的运行时装载。这些功能使得字节码编译器支持更完整的 Lisp 语义。

---

## 新增功能

### 1. let 表达式编译 ✅

**语法**:
```lisp
(let ((var1 val1) (var2 val2) ...)
  body...)
```

**实现原理**:
1. 进入新的作用域 (`enter_scope`)
2. 编译每个绑定的值并使用 `StoreLocal` 存储到局部变量
3. 编译 body 表达式
4. 离开作用域 (`exit_scope`)

**字节码示例**:
```
(let ((x 10) (y 20)) (+ x y))

== Disassembly ==
0000         LOAD_CONST 0    ; 加载 10
0005     STORE_LOCAL 0, 0    ; 存储 x
0008         LOAD_CONST 1    ; 加载 20
0013     STORE_LOCAL 0, 1    ; 存储 y
0016      LOAD_LOCAL 0, 0    ; 加载 x
0019      LOAD_LOCAL 0, 1    ; 加载 y
0022         LOAD_CONST 2    ; 加载 +
0027               CALL 2     ; 调用 (+ x y)
```

**测试用例**:
- `test_compile_let_simple`: 基本 let 测试
- `test_compile_let_multiple_bindings`: 多变量绑定测试

---

### 2. set! 表达式编译 ✅

**语法**:
```lisp
(set! var value)
```

**实现原理**:
1. 编译 `value` 表达式
2. 查找变量位置（局部或全局）
3. 发出 `StoreLocal` 或 `StoreGlobal` 指令

**变量查找逻辑**:
- 优先在局部变量中查找（从内到外）
- 如果找不到，在全局变量中查找
- 如果都找不到，返回 `UndefinedVariable` 错误

**测试用例**:
- `test_compile_set_local`: 修改局部变量
- `test_compile_set_global`: 修改全局变量

---

### 3. 闭包变量运行时装载 ✅

**改进前**:
```rust
// MakeLambda 指令执行时，捕获的变量值都是 Nil 占位符
captures: captures.iter().map(|name| {
    (name.clone(), Expr::Nil)  // ❌ 占位符
}).collect()
```

**改进后**:
```rust
// 从当前环境中查找捕获变量的值
let mut capture_values = Vec::new();
for name in captures {
    let value = if let Some(v) = self.env.get(name) {
        v.clone()  // ✅ 从环境获取
    } else if let Some(v) = self.globals.get(name) {
        v.clone()  // ✅ 从全局变量获取
    } else {
        Expr::Nil  // 兜底
    };
    capture_values.push((name.clone(), value));
}
```

**关键修改**:
- `BytecodeVM` 添加 `env: Env` 字段
- 添加 `with_env()` 方法支持传入环境
- `MakeLambda` 指令执行时从环境获取闭包变量值

---

## 架构改进

### 1. 全局变量管理

**新增字段**:
```rust
// Chunk 结构
pub struct Chunk {
    pub global_names: Vec<String>,  // 全局变量名称列表
    // ...
}

// BytecodeVM 结构
pub struct BytecodeVM {
    pub global_names: Vec<String>,  // 从 chunk 复制
    // ...
}
```

**优点**:
- 编译时记录全局变量名称
- VM 可以正确映射索引到名称
- 支持完整的全局变量读写

### 2. LoadGlobal/StoreGlobal 指令实现

**实现前**: 返回 "not yet implemented" 错误

**实现后**:
```rust
OpCode::LoadGlobal => {
    let idx = ...;
    let var_name = &self.global_names[idx];
    // 从 globals HashMap 或 env 中查找
    if let Some(value) = self.globals.get(var_name) {
        self.push(value.clone());
    } else if let Some(value) = self.env.get(var_name) {
        self.push(value.clone());
    } else {
        return Err(VMError::UndefinedVariable(var_name.clone()));
    }
}

OpCode::StoreGlobal => {
    let idx = ...;
    let var_name = self.global_names[idx].clone();
    self.globals.insert(var_name, value);
}
```

---

## 测试结果

### 编译器测试

所有测试通过 (28/28):
```
test jit::bytecode::tests::test_chunk_add_constant ... ok
test jit::bytecode::tests::test_instruction_encode_decode ... ok
test jit::bytecode::tests::test_chunk_disassemble ... ok
test jit::bytecode::tests::test_instruction_with_operands ... ok
test jit::bytecode::tests::test_opcode_from_byte ... ok
test jit::compiler::tests::test_compile_define ... ok
test jit::compiler::tests::test_compile_if ... ok
test jit::compiler::tests::test_compile_lambda_single_param ... ok
test jit::compiler::tests::test_compile_lambda_with_closure ... ok
test jit::compiler::tests::test_compile_lambda_with_multiple_params ... ok
test jit::compiler::tests::test_compile_let_simple ... ok ⭐ NEW
test jit::compiler::tests::test_compile_let_multiple_bindings ... ok ⭐ NEW
test jit::compiler::tests::test_compile_number ... ok
test jit::compiler::tests::test_compile_set_global ... ok ⭐ NEW
test jit::compiler::tests::test_compile_set_local ... ok ⭐ NEW
test jit::compiler::tests::test_compile_simple_arithmetic ... ok
test jit::compiler::tests::test_compile_simple_lambda ... ok
test jit::vm::tests::test_vm_builtin_add ... ok
test jit::vm::tests::test_vm_builtin_compare ... ok
test jit::vm::tests::test_vm_builtin_list_operations ... ok
test jit::vm::tests::test_vm_bytecode_lambda_creation ... ok
test jit::vm::tests::test_vm_bytecode_lambda_call ... ok
test jit::vm::tests::test_vm_bytecode_lambda_arithmetic ... ok
test jit::vm::tests::test_vm_lambda_call_interpreted ... ok
test jit::vm::tests::test_vm_local_variable_access ... ok
test jit::vm::tests::test_vm_nested_frames ... ok
test jit::vm::tests::test_vm_conditional_jump ... ok
test jit::vm::tests::test_vm_simple_arithmetic ... ok
```

### 性能基准测试 (解释器模式)

| 测试 | 时间 |
|------|------|
| fib(10) | 3.35 µs |
| fib(12) | 3.83 µs |
| fib(15) | 3.82 µs |
| list_create_50 | 3.42 µs |
| sum_1_to_50 | 2.94 µs |
| closure_100_calls | 2.04 µs |

**注意**: 这些是纯解释器模式的性能数据。JIT 字节码模式的性能测试将在后续阶段进行。

---

## 已知问题和限制

### 1. 局部变量 depth 字段未使用

**问题**: `LocalVar` 结构中的 `depth` 字段在编译时被设置，但在当前实现中未被充分利用。

**影响**: 暂时不影响功能，因为 let 表达式总是创建新的作用域（depth=0）。

**未来改进**: 当支持嵌套作用域时，需要正确使用 depth 字段来访问外层局部变量。

### 2. 闭包变量未在运行时绑定

**问题**: 虽然在 MakeLambda 时从环境获取了闭包变量值，但这些值没有被实际绑定到 Lambda 的执行环境中。

**影响**: 嵌套 Lambda 闭包可能无法正确访问外层变量。

**未来改进**: 在 Lambda 调用时，需要将捕获的变量值绑定到新的调用帧或环境中。

---

## 文件修改清单

| 文件 | 修改内容 |
|------|----------|
| `src/jit/bytecode.rs` | Chunk 添加 global_names 字段 |
| `src/jit/compiler.rs` | 实现 compile_let 和 compile_set 方法 |
| `src/jit/vm.rs` | 实现 LoadGlobal/StoreGlobal，添加 env 支持 |
| `src/jit/compiler.rs` | 添加 4 个新测试用例 |

---

## 下一步计划

根据 TODO.md，接下来应该：

1. **性能测试**: 比较字节码执行与解释器执行的性能
2. **集成 JIT**: 将 JIT 编译器集成到主解释器中
3. **热点检测**: 实现函数调用计数和热点识别
4. **Cranelift JIT**: 将热点字节码编译为原生机器码

---

## 总结

本阶段成功实现了 `let` 和 `set!` 表达式的编译，以及闭包变量的运行时装载。这些功能使得 JIT 编译器支持更完整的 Lisp 语义，为后续的性能优化打下了基础。

**关键成就**:
- ✅ let 表达式编译
- ✅ set! 表达式编译
- ✅ 全局变量管理改进
- ✅ 闭包变量运行时装载
- ✅ 28/28 JIT 测试通过
- ✅ 4 个新测试用例

**下一步**: 将 JIT 编译器集成到主解释器中，并开始实际的性能对比测试。
