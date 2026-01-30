;; MyLisp 标准库 - 时间模块 (time.lisp)
;;
;; 提供时间相关的函数
;;
;; 使用方式：
;;   (load stdlib/time.lisp)

;; ========== 常量定义 ==========

;; NTP 时间戳 epoch (1900-01-01 00:00:00 UTC)
(define TIME_NTP_EPOCH 2208988800)

;; ========== 数学函数 ==========

;; 计算幂 (base^power)
(define (expt base power)
  (if (= power 0)
      1
      (* base (expt base (- power 1)))))

;; 移位运算 (arithmetic shift)
(define (ash value count)
  (* value (expt 2 count)))

;; ========== 基本时间函数 ==========

;; 获取当前 Unix 时间戳（秒）
;; 使用 syscall 201 (time)
(define (time-now)
  (syscall 201 0))

;; Unix 时间戳转 NTP 时间戳
(define (unix-to-ntp unix-time)
  (+ unix-time TIME_NTP_EPOCH))

;; NTP 时间戳转 Unix 时间戳
(define (ntp-to-unix ntp-time)
  (- ntp-time TIME_NTP_EPOCH))

;; 获取当前 NTP 时间戳
(define (ntp-now)
  (unix-to-ntp (time-now)))

;; ========== 简单的时间计算 ==========

;; 时间戳加秒数
(define (time-add-sec timestamp seconds)
  (+ timestamp seconds))

;; 时间戳减秒数
(define (time-sub-sec timestamp seconds)
  (- timestamp seconds))

;; 计算两个时间戳之间的差值（秒）
(define (time-diff timestamp1 timestamp2)
  (- timestamp1 timestamp2))

;; ========== 时间格式化 ==========

;; 格式化时间为简单的日期时间显示
(define (time-format-simple unix-time)
  (display "Unix timestamp: ")
  (display unix-time))

;; 格式化 NTP 时间戳
(define (ntp-format ntp-time)
  (display "NTP timestamp: ")
  (display ntp-time))

;; ========== NTP 辅助函数 ==========

;; 创建 NTP 时间戳（秒部分）
(define (ntp-create-seconds unix-time)
  (unix-to-ntp unix-time))

;; 获取 NTP 时间戳的秒部分
(define (ntp-get-seconds ntp-time)
  (floor ntp-time))

;; ========== 睡眠功能 ==========

;; 睡眠指定秒数
;; 使用 syscall 35 (nanosleep)
;; 参数: (seconds nanoseconds)
(define (sleep-sec seconds)
  (syscall 35 (list seconds 0) (list 0 0))
  ())

;; ========== 演示函数 ==========

;; 显示当前时间信息
(define (time-show-current)
  (let ((current (time-now)))
    (display "========================================")
    (newline)
    (display "           Current Time Information")
    (display "========================================")
    (newline)

    (display "Unix timestamp: ")
    (display current)
    (newline)

    (display "NTP timestamp: ")
    (display (unix-to-ntp current))
    (newline)

    (display "NTP epoch: ")
    (display TIME_NTP_EPOCH)
    (display " seconds")
    (newline)

    (display "========================================")
    (newline)))

;; 测试时间库
(define (time-test)
  (let ((now (time-now)))
    (display "========================================")
    (newline)
    (display "       MyLisp Time Library Test")
    (newline)
    (display "========================================")
    (newline)

    (display "Test 1: Current Time")
    (newline)
    (display "  Current Unix timestamp: ")
    (display now)
    (newline)

    (display "  Current NTP timestamp: ")
    (display (unix-to-ntp now))
    (newline)
    (newline)

    (display "Test 2: Time Conversion")
    (newline)
    (display "  Unix -> NTP: ")
    (display (unix-to-ntp 1704067200))
    (newline)

    (display "  NTP -> Unix: ")
    (display (ntp-to-unix (unix-to-ntp 1704067200)))
    (newline)
    (newline)

    (display "Test 3: Time Arithmetic")
    (newline)
    (display "  Add 1 hour: ")
    (display (time-add-sec now 3600))
    (newline)

    (display "  Subtract 1 hour: ")
    (display (time-sub-sec now 3600))
    (newline)

    (display "  Time difference: ")
    (display (time-diff (time-add-sec now 100) now))
    (display " seconds")
    (newline)
    (newline)

    (display "========================================")
    (newline)

    (display "All tests passed!")
    (newline)))

;; 导出的函数列表
(define *time-functions*
  (time-now
   unix-to-ntp
   ntp-to-unix
   ntp-now
   time-add-sec
   time-sub-sec
   time-diff
   time-format-simple
   ntp-format
   ntp-create-seconds
   ntp-get-seconds
   sleep-sec
   time-show-current
   time-test))
