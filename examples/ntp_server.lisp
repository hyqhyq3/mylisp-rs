;; MyLisp NTP 服务器演示（使用标准库）
;;
;; 这是一个使用 MyLisp 编写的 NTP (Network Time Protocol) 服务器概念实现
;; 使用 stdlib/time.lisp 标准库
;;
;; 运行方式：
;;   ./target/release/mylisp examples/ntp_server.lisp

;; 加载时间标准库
(load stdlib/time.lisp)

(display "========================================")
(newline)
(display "       MyLisp NTP 服务器 v2.0")
(newline)
(display "       (使用标准库)")
(newline)
(display "========================================")
(newline)
(newline)

;; ========== NTP 协议常量 ==========

(define NTP_VERSION 4)
(define NTP_MODE_SERVER 4)
(define NTP_STRATUM 2)

;; ========== 辅助函数 ==========

;; 移位运算
(define (ash value count)
  (* value (expt 2 count)))

;; 计算 NTP 头部字节
(define (build-ntp-header)
  (let ((li-vn-mode (+ (ash 0 6) (ash NTP_VERSION 3) NTP_MODE_SERVER)))
    li-vn-mode))

;; ========== 服务器信息展示 ==========

(display "NTP 服务器配置:")
(newline)
(display "----------------------------------------")
(newline)

(display "协议版本: NTP v")
(display NTP_VERSION)
(newline)

(display "服务器模式: ")
(display NTP_MODE_SERVER)
(display " (服务器)")
(newline)

(display "层级: Stratum ")
(display NTP_STRATUM)
(newline)

(display "NTP Epoch: ")
(display TIME_NTP_EPOCH)
(display " 秒 (1900-01-01 00:00:00 UTC)")
(newline)
(newline)

;; ========== NTP 响应包构建演示 ==========

(display "构建 NTP 响应包:")
(newline)
(display "----------------------------------------")
(newline)

(define now-unix (time-now))
(define now-ntp (unix-to-ntp now-unix))
(define ntp-header (build-ntp-header))

(display "当前 Unix 时间戳: ")
(display now-unix)
(newline)

(display "当前 NTP 时间戳: ")
(display now-ntp)
(newline)

(display "LI-VN-Mode 字节: ")
(display ntp-header)
(display " (LI=0, VN=")
(display NTP_VERSION)
(display ", Mode=")
(display NTP_MODE_SERVER)
(display ")")
(newline)

(display "Stratum: ")
(display NTP_STRATUM)
(newline)

;; ========== 时间转换演示 ==========

(newline)
(display "时间转换演示:")
(newline)
(display "----------------------------------------")
(newline)

(define test-time 1704067200)  ; 2024-01-01 00:00:00 UTC

(display "测试时间戳: ")
(display test-time)
(display " (2024-01-01 00:00:00 UTC)")
(newline)

(display "转换为 NTP: ")
(display (unix-to-ntp test-time))
(newline)

(display "转回 Unix: ")
(display (ntp-to-unix (unix-to-ntp test-time)))
(newline)

;; ========== 完整性验证 ==========

(newline)
(display "完整性验证:")
(newline)
(display "----------------------------------------")
(newline)

(display "双向转换正确性: ")
(if (= (ntp-to-unix (unix-to-ntp test-time)) test-time)
    (display "✓ 通过")
    (display "✗ 失败"))
(newline)

(display "时间戳计算正确性: ")
(define future (time-add-sec now-unix 3600))
(define past (time-sub-sec now-unix 3600))
(define diff (time-diff future past))

(if (= diff 7200)
    (display "✓ 通过")
    (display "✗ 失败"))
(newline)

(display "预期差值: 7200 秒, 实际: ")
(display diff)
(display " 秒")
(newline)

;; ========== 说明 ==========

(newline)
(display "========================================")
(newline)
(display "说明:")
(newline)
(display "========================================")
(newline)
(newline)

(display "这是一个使用 MyLisp 标准库实现的")
(newline)
(display "NTP 服务器概念演示。")
(newline)
(newline)

(display "已实现的功能:")
(newline)
(display "  ✓ 时间戳获取 (time-now)")
(newline)
(display "  ✓ Unix/NTP 时间转换")
(newline)
(display "  ✓ 时间计算 (加减、差值)")
(newline)
(display "  ✓ NTP 包头部构建")
(newline)
(newline)

(display "完整 NTP 服务器还需要:")
(newline)
(display "  1. UDP Socket (ffi-call socket)")
(newline)
(display "  2. 网络 I/O (bind, recvfrom, sendto)")
(newline)
(display "  3. 二进制数据包序列化")
(newline)
(display "  4. sockaddr_in 结构体处理")
(newline)
(newline)

(display "MyLisp 已支持 FFI，可以扩展实现！")
(newline)
(newline)

(display "========================================")
(newline)
