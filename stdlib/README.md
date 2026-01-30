# MyLisp 标准库

MyLisp 标准库提供了一系列可重用的函数和模块，方便开发者快速构建应用。

## 目录结构

```
stdlib/
├── README.md        # 本文档
└── time.lisp        # 时间处理模块
```

## 加载标准库

使用 `(load)` 函数加载标准库模块：

```lisp
;; 加载时间模块
(load stdlib/time.lisp)

;; 现在可以使用时间函数
(display (time-now))
```

## 模块列表

### time.lisp - 时间处理模块 ✅

提供时间相关的函数，包括时间戳获取、转换、计算等。**完全可用。**

### math.lisp - 数学模块 ⚠️

提供数学运算相关的函数。**部分可用** - 由于递归深度限制，某些函数使用简化实现。

可用函数：
- `abs` - 绝对值
- `expt` - 幂运算（小的整数幂）
- `max`, `min` - 最大/最小值
- `sqrt` - 平方根（简化版，只处理完全平方数）
- `cbrt` - 立方根（简化版，只处理完全立方数）

### net.lisp - 网络模块 ⚠️

提供网络相关的函数和常量。**概念演示** - 需要 FFI 支持才能实现完整功能。

提供内容：
- 网络常量（地址族、Socket 类型、协议等）
- Socket 操作函数（概念实现）
- 端口验证函数
- 常用端口号定义

#### 常量

- `TIME_NTP_EPOCH` - NTP 时间戳 epoch (1900-01-01 00:00:00 UTC) = 2208988800 秒

#### 基本时间函数

##### `(time-now)` → number

获取当前 Unix 时间戳（秒）。

```lisp
(define now (time-now))
;; => 1769744898
```

##### `(unix-to-ntp unix-time)` → number

将 Unix 时间戳转换为 NTP 时间戳。

```lisp
(define ntp-time (unix-to-ntp 1704067200))
;; => 3913056000
```

##### `(ntp-to-unix ntp-time)` → number

将 NTP 时间戳转换为 Unix 时间戳。

```lisp
(define unix-time (ntp-to-unix 3913056000))
;; => 1704067200
```

##### `(ntp-now)` → number

获取当前 NTP 时间戳。

```lisp
(define now-ntp (ntp-now))
;; => 3978733698
```

#### 时间计算

##### `(time-add-sec timestamp seconds)` → number

时间戳加秒数。

```lisp
(define future (time-add-sec (time-now) 3600))
;; => 当前时间 + 1小时
```

##### `(time-sub-sec timestamp seconds)` → number

时间戳减秒数。

```lisp
(define past (time-sub-sec (time-now) 3600))
;; => 当前时间 - 1小时
```

##### `(time-diff timestamp1 timestamp2)` → number

计算两个时间戳之间的差值（秒）。

```lisp
(define diff (time-diff future past))
;; => 7200
```

#### 时间格式化

##### `(time-format-simple unix-time)` → nil

以简单格式显示时间。

```lisp
(time-format-simple (time-now))
;; => Unix timestamp: 1769744898
```

##### `(ntp-format ntp-time)` → nil

以简单格式显示 NTP 时间戳。

```lisp
(ntp-format (ntp-now))
;; => NTP timestamp: 3978733698
```

#### NTP 辅助函数

##### `(ntp-create-seconds unix-time)` → number

创建 NTP 时间戳（秒部分）。

```lisp
(define ntp-sec (ntp-create-seconds (time-now)))
```

##### `(ntp-get-seconds ntp-time)` → number

获取 NTP 时间戳的秒部分。

```lisp
(define seconds (ntp-get-seconds (ntp-now)))
```

#### 睡眠功能

##### `(sleep-sec seconds)` → nil

睡眠指定秒数。

```lisp
(sleep-sec 1)  ; 睡眠 1 秒
```

#### 演示函数

##### `(time-show-current)` → nil

显示当前时间的详细信息。

```lisp
(time-show-current)
;; ======================================
;;            Current Time Information
;; ======================================
;; Unix timestamp: 1769744898
;; NTP timestamp: 3978733698
;; NTP epoch: 2208988800 seconds
;; ======================================
```

##### `(time-test)` → nil

运行时间库测试套件。

```lisp
(time-test)
;; ======================================
;;        MyLisp Time Library Test
;; ======================================
;;
;; Test 1: Current Time
;; Test 2: Time Conversion
;; Test 3: Time Arithmetic
;;
;; All tests passed!
```

#### 数学函数

##### `(expt base power)` → number

计算 base 的 power 次幂。

```lisp
(display (expt 2 10))
;; => 1024
```

##### `(ash value count)` → number

算术左移操作。

```lisp
(display (ash 1 3))
;; => 8
```

## 示例程序

标准库包含多个示例程序，展示如何使用标准库：

### NTP 服务器演示

```bash
./target/release/mylisp examples/ntp_server.lisp
```

演示如何使用时间库构建 NTP 服务器，包括：
- 时间戳获取和转换
- NTP 协议常量定义
- NTP 响应包构建
- 完整性验证

### 时间库测试

```bash
./target/release/mylisp examples/test_time.lisp
```

运行时间库的完整测试套件。

## 扩展标准库

要创建新的标准库模块：

1. 在 `stdlib/` 目录下创建新的 `.lisp` 文件
2. 使用 `(define)` 定义函数和常量
3. 确保函数体内的局部绑定使用 `let` 而不是 `define`
4. 编写测试文件到 `examples/` 目录
5. 更新本 README 文档

## 注意事项

1. **函数定义**: 使用 `(define (name args) body)` 语法定义函数
2. **局部变量**: 函数体内的局部变量必须使用 `let` 绑定，不能使用 `define`
3. **递归**: 支持递归函数，但要避免过深的递归导致栈溢出
4. **作用域**: 加载的库函数在全局作用域中可用
5. **路径**: `load` 的参数是符号，不是字符串，如 `(load stdlib/time.lisp)`

## 未来计划

- [ ] 添加字符串处理模块 (string.lisp)
- [ ] 添加文件操作模块 (file.lisp)
- [ ] 添加网络操作模块 (net.lisp)
- [ ] 添加数学运算模块 (math.lisp)
- [ ] 添加数据结构模块 (data.lisp)

## 贡献

欢迎为 MyLisp 标准库贡献新的模块和函数！
