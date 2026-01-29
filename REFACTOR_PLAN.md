# MyLisp 可维护性重构计划

## 一、重构目标

1. **降低模块复杂度**: 将 1395 行的 `eval.rs` 拆分为多个职责单一的模块
2. **提高代码复用**: 提取公共的验证、类型检查和错误处理逻辑
3. **改善可测试性**: 为每个模块添加单元测试
4. **增强可扩展性**: 通过 trait 抽象使添加新功能更容易
5. **保持向后兼容**: 确保所有现有功能和测试正常工作

## 二、重构风险评估

### 高风险区域
- **TCO (尾递归优化) 逻辑**: `eval_function_call`, `apply_user_function_tail`
- **宏展开系统**: `expand_macros` 及相关函数
- **全局状态管理**: `thread_local!` 的 `USER_FUNCTIONS` 和 `MACROS`

### 中风险区域
- **特殊形式求值**: `eval_define`, `eval_set`, `eval_if` 等
- **内置函数**: 算术、比较、列表操作等

### 低风险区域
- **错误消息**: 只影响字符串格式
- **工具函数**: 独立的辅助函数

## 三、新模块结构设计

### 3.1 目录结构
```
src/
├── ast.rs              # 保持不变 (AST 定义)
├── env.rs              # 保持不变 (环境管理)
├── lexer.rs            # 保持不变 (词法分析)
├── parser.rs           # 保持不变 (语法分析)
├── main.rs             # 保持不变 (入口点)
├── lib.rs              # 新增: 库根文件
│
├── eval/               # 新增: 求值器模块
│   ├── mod.rs          # 求值器入口和核心逻辑
│   ├── special_forms.rs # 特殊形式 (define, set!, if, lambda, let, quote)
│   ├── builtins.rs     # 内置函数 (算术, 比较, 列表操作)
│   ├── macros.rs       # 宏系统 (define-syntax, pattern matching)
│   ├── tco.rs          # 尾递归优化
│   └── error.rs        # 错误类型和处理
│
└── utils/              # 新增: 工具模块
    ├── mod.rs          # 工具模块入口
    ├── validator.rs    # 参数验证工具
    └── type_checker.rs # 类型检查工具
```

### 3.2 模块职责划分

#### `eval/mod.rs` (核心求值器, ~200 行)
- `pub fn eval()` - 主求值入口
- `fn eval_with_tail_context()` - 带尾调用上下文的求值
- `fn eval_function_call()` - 函数调用分发
- 导入其他子模块的功能

#### `eval/special_forms.rs` (特殊形式, ~300 行)
- `fn eval_define()` - 变量定义
- `fn eval_set()` - 变量赋值
- `fn eval_if()` - 条件表达式
- `fn eval_cond()` - 多条件分支
- `fn eval_begin()` - 序列求值
- `fn eval_lambda()` - Lambda 函数
- `fn eval_let()` - Let 绑定
- `fn eval_quote()` - 引用
- `fn eval_eval()` - 动态求值
- `fn eval_load()` - 文件加载

#### `eval/builtins.rs` (内置函数, ~350 行)
- 算术运算: `apply_add`, `apply_sub`, `apply_mul`, `apply_div`, `apply_mod`
- 比较运算: `apply_gt`, `apply_lt`, `apply_le`, `apply_ge`, `apply_eq`
- 逻辑运算: `apply_not`, `apply_and`, `apply_or`
- 列表操作: `apply_head`, `apply_tail`, `apply_cons`, `apply_append`, `apply_list`
- 谓词函数: `apply_null`, `apply_symbol`, `apply_number`, `apply_string`
- I/O 操作: `apply_display`, `apply_newline`

#### `eval/macros.rs` (宏系统, ~350 行)
- `struct Macro`, `struct MacroRule` - 宏定义
- `fn eval_define_syntax()` - 定义宏
- `fn expand_macros()` - 宏展开入口
- `fn apply_macro()` - 应用宏
- `fn match_pattern()` - 模式匹配
- `fn match_pattern_list()` - 列表模式匹配
- `fn match_pattern_single()` - 单个模式匹配
- `fn expand_template()` - 模板展开
- `fn expand_template_with_ellipsis()` - 省略号展开
- `fn extract_repeated_bindings()` - 提取重复绑定
- 全局宏注册表管理

#### `eval/tco.rs` (尾递归优化, ~150 行)
- `enum TailContext` - 尾调用上下文
- `fn apply_user_function_tail()` - 尾调用优化
- `fn apply_user_function()` - 普通函数应用
- 全局用户函数注册表管理

#### `eval/error.rs` (错误处理, ~100 行)
- `enum MyLispError` - 统一错误类型
  - `SyntaxError` - 语法错误
  - `TypeError` - 类型错误
  - `RuntimeError` - 运行时错误
  - `ArityError` - 参数数量错误
  - `UndefinedError` - 未定义错误
- `impl From<MyLispError> for String` - 向后兼容转换
- 错误构造辅助函数

#### `utils/validator.rs` (参数验证, ~150 行)
- `trait ArgValidator` - 验证 trait
- `fn check_arity()` - 检查参数数量
- `fn check_min_arity()` - 检查最少参数
- `fn expect_symbol()` - 期望符号
- `fn expect_list()` - 期望列表
- 宏来生成验证逻辑

#### `utils/type_checker.rs` (类型检查, ~100 行)
- `fn expect_number()` - 期望数字
- `fn expect_string()` - 期望字符串
- `fn expect_bool()` - 期望布尔值
- `fn get_type_name()` - 获取类型名称
- 类型检查宏

## 四、重构步骤

### 阶段 1: 基础设施 (低风险)
1. 创建 `eval/error.rs` - 统一错误类型
2. 创建 `utils/validator.rs` - 参数验证工具
3. 创建 `utils/type_checker.rs` - 类型检查工具
4. 创建 `src/lib.rs` - 库结构
5. 运行测试确保基础设施正常

### 阶段 2: 内置函数提取 (低风险)
1. 创建 `eval/builtins.rs`
2. 移动所有 `apply_*` 函数
3. 更新 `eval_function_call` 中的调用
4. 运行内置函数测试

### 阶段 3: 特殊形式提取 (中风险)
1. 创建 `eval/special_forms.rs`
2. 移动所有 `eval_*` 特殊形式函数
3. 更新 `eval_with_tail_context` 中的分发逻辑
4. 运行特殊形式测试

### 阶段 4: 宏系统提取 (中风险)
1. 创建 `eval/macros.rs`
2. 移动宏相关结构体和函数
3. 移动全局宏注册表
4. 更新宏展开调用
5. 运行宏系统测试

### 阶段 5: 尾递归优化提取 (高风险)
1. 创建 `eval/tco.rs`
2. 移动 `TailContext` 枚举
3. 移动 TCO 相关函数
4. 移动全局用户函数注册表
5. 更新函数调用逻辑
6. 运行 TCO 测试

### 阶段 6: 最终整合 (低风险)
1. 更新 `eval/mod.rs` 为新的模块结构
2. 清理 `eval.rs` (重命名为 `eval/mod.rs`)
3. 更新所有 `use` 语句
4. 完整测试套件验证

### 阶段 7: 测试完善 (低风险)
1. 为每个模块添加单元测试
2. 添加集成测试
3. 验证测试覆盖率
4. 性能基准测试

## 五、向后兼容性保证

### 5.1 公共 API
- `Evaluator::eval()` - 保持不变
- `register_user_function()` - 保持不变
- `call_user_function()` - 保持不变
- 返回类型 `Result<Expr, String>` - 保持不变

### 5.2 内部重构
- 所有重构在内部进行
- 不改变 AST、Env 等核心类型
- 不改变 Lexer、Parser 接口

### 5.3 渐进式迁移
- 每个阶段独立运行测试
- 失败时可以快速回滚
- 保持 git 历史清晰

## 六、测试策略

### 6.1 现有测试
- 所有 `.lisp` 测试文件必须通过
- 共 20+ 个测试文件

### 6.2 新增测试
- 每个新模块添加 Rust 单元测试
- 测试错误处理路径
- 测试边界条件

### 6.3 测试优先级
1. **高优先级**: TCO 测试 (最复杂)
2. **中优先级**: 宏系统测试
3. **低优先级**: 基础功能测试

## 七、预期收益

### 7.1 可维护性提升
- 单个文件从 1395 行降到 <300 行
- 模块职责单一,易于理解
- 代码复用度提高

### 7.2 可扩展性提升
- 添加新的内置函数只需修改 `builtins.rs`
- 添加新的特殊形式只需修改 `special_forms.rs`
- 通过 trait 可以轻松扩展功能

### 7.3 可测试性提升
- 每个模块可独立测试
- 更容易编写单元测试
- 测试覆盖率显著提高

## 八、时间估算

- 阶段 1: 2-3 小时
- 阶段 2: 1-2 小时
- 阶段 3: 2-3 小时
- 阶段 4: 3-4 小时
- 阶段 5: 4-5 小时
- 阶段 6: 1-2 小时
- 阶段 7: 2-3 小时

**总计**: 15-22 小时

## 九、回滚策略

每个阶段完成后创建 git commit:
- `feat: phase-1-infrastructure`
- `feat: phase-2-builtins`
- `feat: phase-3-special-forms`
- 等等...

如果某阶段失败:
1. 保留失败的代码供学习
2. `git reset --hard HEAD~1` 回滚
3. 重新设计并实施

## 十、成功标准

1. ✅ 所有现有测试通过
2. ✅ 每个模块 <400 行
3. ✅ 没有代码重复 (DRY 原则)
4. ✅ 添加至少 50% 的单元测试覆盖率
5. ✅ 性能无明显下降
6. ✅ 代码可读性显著提升
