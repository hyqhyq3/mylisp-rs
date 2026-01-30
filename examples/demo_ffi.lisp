;; MyLisp FFI 和 Syscall 演示
;;
;; 展示如何使用 ffi-call 和 syscall

(display "========================================")
(newline)
(display "       MyLisp FFI & Syscall 演示")
(newline)
(display "========================================")
(newline)
(newline)

;; ========== Syscall 演示 ==========

(display "1. Syscall 演示")
(newline)
(display "----------------------------------------")
(newline)

;; Linux x86_64 syscall 号
(define SYS_WRITE 1)      ; write
(define SYS_OPEN 2)       ; open
(define SYS_CLOSE 3)      ; close
(define SYS_STAT 4)       ; stat
(define SYS_FSTAT 5)      ; fstat
(define SYS_LSEEK 8)      ; lseek
(define SYS_MMAP 9)       ; mmap
(define SYS_MPROTECT 10)  ; mprotect
(define SYS_MUNMAP 11)    ; munmap
(define SYS_BRK 12)       ; brk
(define SYS_RT_SIGRETURN 15)
(define SYS_IOCTL 16)     ; ioctl
(define SYS_PREAD64 17)   ; pread64
(define SYS_PWRITE64 18)  ; pwrite64
(define SYS_READ 0)       ; read
(define SYS_TIME 201)     ; time (x86_64)
(define SYS_GETPID 39)    ; getpid
(define SYS_GETUID 102)   ; getuid
(define SYS_GETGID 104)   ; getgid
(define SYS_NANOSLEEP 35) ; nanosleep

(display "系统调用示例:")
(newline)

;; 获取当前时间戳
(display "  获取当前时间 (syscall 201 = time): ")
(define current-time (syscall SYS_TIME 0))
(display current-time)
(newline)

;; 获取进程 ID
(display "  获取进程 PID (syscall 39 = getpid): ")
(define pid (syscall SYS_GETPID))
(display pid)
(newline)

;; 获取用户 ID
(display "  获取用户 UID (syscall 102 = getuid): ")
(define uid (syscall SYS_GETUID))
(display uid)
(newline)

;; 获取组 ID
(display "  获取组 GID (syscall 104 = getgid): ")
(define gid (syscall SYS_GETGID))
(display gid)
(newline)
(newline)

;; ========== FFI-call 演示 ==========

(display "2. FFI-call 演示")
(newline)
(display "----------------------------------------")
(newline)

(display "FFI-call 可以调用外部 C 库函数:")
(newline)
(newline)

(display "  示例: 调用 libc 的 getpid() 函数")
(newline)
(display "  (ffi-call \"c\" \"getpid\" \"void\" ())")
(newline)
;; (define getpid-result (ffi-call c getpid void ()))
;; (display "    Result: ")
;; (display getpid-result)
;; (newline)

(newline)

(display "  FFI-call 参数说明:")
(newline)
(display "    (ffi-call lib func ret-type (arg-types...) args...)")
(newline)
(newline)

(display "  支持的类型:")
(newline)
(display "    - i64, u64: 64位整数")
(newline)
(display "    - isize, usize: 指针大小整数")
(newline)
(display "    - f64: 浮点数")
(newline)
(display "    - bool: 布尔值")
(newline)
(display "    - ptr: 指针")
(newline)
(display "    - string: 字符串")
(newline)
(display "    - bytes: 字节数组")
(newline)
(display "    - void: 无返回值")
(newline)
(newline)

;; ========== 实际应用示例 ==========

(display "3. 实际应用示例")
(newline)
(display "----------------------------------------")
(newline)

;; 使用 syscall 获取高精度时间
(display "  使用 gettimeofday 获取微秒级时间:")
(newline)
(define SYS_GETTIMEOFDAY 96)
;; gettimeofday 需要 struct timezone 指针，这里简化
;; (define tv (list 0 0))
;; (define tz (list 0 0))
;; (define time-result (syscall SYS_GETTIMEOFDAY tv tz))
;; (display "    返回值: ")
;; (display time-result)
;; (newline)

(newline)

;; ========== 注意事项 ==========

(display "4. 注意事项")
(newline)
(display "----------------------------------------")
(newline)
(newline)

(display "Syscall 限制:")
(newline)
(display "  - 仅支持 Linux (不支持 Windows)")
(newline)
(display "  - 最多 6 个参数")
(newline)
(display "  - 需要知道正确的系统调用号")
(newline)
(display "  - 需要处理架构差异 (x86_64, x86, arm等)")
(newline)
(newline)

(display "FFI-call 限制:")
(newline)
(display "  - 需要系统中安装相应的库")
(newline)
(display "  - 参数和返回值类型必须匹配")
(newline)
(display "  - 字符串自动转换为 C string")
(newline)
(display "  - 不支持复杂的结构体 (需要手动处理)")
(newline)
(newline)

;; ========== 总结 ==========

(display "========================================")
(newline)
(display "总结:")
(newline)
(display "========================================")
(newline)
(newline)

(display "✅ Syscall - 可用于直接系统调用")
(newline)
(display "✅ FFI-call - 可用于调用 C 库函数")
(newline)
(newline)

(display "这些功能使得 MyLisp 可以:")
(newline)
(display "  - 实现真正的网络编程")
(newline)
(display "  - 文件 I/O 操作")
(newline)
(display "  - 进程管理")
(newline)
(display "  - 调用系统服务")
(newline)
(newline)

(display "建议:")
(newline)
(display "  1. 优先使用 syscall 进行简单操作")
(newline)
(display "  2. 复杂功能使用 ffi-call 调用 libc")
(newline)
(display "  3. 查阅 Linux/Unix 手册获取 syscall 号")
(newline)
(newline)

(display "========================================")
(newline)
