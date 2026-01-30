# MyLisp 性能基准测试

本目录包含 MyLisp 解释器的性能基准测试套件。

## 运行基准测试

### 运行所有基准测试（生成 HTML 报告）

```bash
cargo bench
```

结果将保存在 `target/criterion/` 目录中，打开以下文件查看详细报告：

```bash
# Linux
xdg-open target/criterion/report/index.html

# macOS
open target/criterion/report/index.html
```

### 生成火焰图

```bash
cargo bench --bench flamegraph
```

火焰图将保存在 `target/flamegraph/` 目录中。

### 运行特定基准测试

```bash
# 只测试斐波那契性能
cargo bench --bench benchmark -- fibonacci

# 只测试列表操作
cargo bench --bench benchmark -- list_operations

# 只测试闭包性能
cargo bench --bench benchmark -- closures
```

## 基准测试内容

### 1. 斐波那契数列 (`fibonacci`)
- **目的**：测试递归调用性能
- **规模**：fib(15), fib(20), fib(25)
- **关键指标**：递归深度、函数调用开销

### 2. 算术运算 (`arithmetic`)
- **目的**：测试基本数学运算性能
- **测试**：1-100 求和、10 的阶乘
- **关键指标**：算术操作开销、条件分支

### 3. 列表操作 (`list_operations`)
- **目的**：测试数据结构操作性能
- **测试**：创建列表、map 映射、reduce 归约
- **关键指标**：内存分配、垃圾回收开销

### 4. Lambda 闭包 (`closures`)
- **目的**：测试闭包创建和调用性能
- **测试**：adder 闭包、Y 组合子
- **关键指标**：闭包捕获、函数调用开销

### 5. 宏展开 (`macros`)
- **目的**：测试宏定义和展开性能
- **测试**：when 宏、重复展开
- **关键指标**：宏展开时间、编译时开销

### 6. 文件加载 (`file_loading`)
- **目的**：测试完整程序的加载性能
- **测试**：fib.lisp, primes.lisp, list_ops.lisp, closure.lisp, macros.lisp
- **关键指标**：解析时间、整体执行时间

### 7. 尾调用优化 (`tail_call_optimization`)
- **目的**：验证 TCO 性能和正确性
- **测试**：尾递归求和 (10000)、尾递归阶乘 (1000)
- **关键指标**：栈使用、调用优化效果

### 8. 惰性求值 (`lazy_evaluation`)
- **目的**：测试 Thunk 创建和强制求值性能
- **测试**：创建 100 个 thunk 并求和
- **关键指标**：闭包创建、延迟执行开销

## 性能基线

初始基线数据将在首次运行后记录在此处：

### 基准测试结果（初始）

运行以下命令获取基线：

```bash
cargo bench -- --save-baseline main
```

后续对比：

```bash
cargo bench -- --baseline main
```

## 分析瓶颈

1. **查看火焰图**：识别热点函数
2. **检查 Criterion 报告**：查看详细统计数据
3. **比较不同优化级别**：

```bash
# Debug 模式（未优化）
cargo bench

# Release 模式（优化）
cargo bench --release
```

## JIT 优化目标

在实现 JIT 编译后，期望的性能提升：

| 基准测试 | 解释器 | JIT 优化后 | 提升比例 |
|---------|--------|-----------|---------|
| fib(25) | ~X ms | ~Y ms | 3-5x |
| map/reduce 100 | ~X ms | ~Y ms | 2-3x |
| closure 1000 calls | ~X ms | ~Y ms | 2-4x |
| TCO 10000 | ~X ms | ~Y ms | 1.5-2x |
| 宏展开 100x | ~X ms | ~Y ms | 1.2-1.5x |

## 添加新基准测试

1. 在 `benches/data/` 中创建新的 `.lisp` 测试文件
2. 在 `benches/benchmark.rs` 中添加新的基准函数
3. 使用 `criterion_group!` 宏注册新测试
4. 运行并验证结果

## 参考文档

- [Criterion.rs 文档](https://bheisler.github.io/criterion.rs/book/)
- [pprof 文档](https://docs.rs/pprof/)
- [火焰图解释](http://www.brendangregg.com/flamegraphs.html)
