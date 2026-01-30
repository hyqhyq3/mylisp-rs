;; MyLisp 真正的网络函数 - 使用 syscall
;;
;; 使用 syscall 实现真实的网络操作

;; ========== Socket 相关 Syscall ==========

(define SYS_SOCKET 41)     ; socket(domain, type, protocol)
(define SYS_BIND 49)       ; bind(fd, addr, addr_len)
(define SYS_CONNECT 42)    ; connect(fd, addr, addr_len)
(define SYS_LISTEN 50)     ; listen(fd, backlog)
(define SYS_ACCEPT 43)     ; accept(fd, addr, addr_len)
(define SYS_SENDTO 44)     ; sendto(fd, buf, len, flags, addr, addr_len)
(define SYS_RECVFROM 45)   ; recvfrom(fd, buf, len, flags, addr, addr_len)
(define SYS_SEND 46)       ; send(fd, buf, len, flags)
(define SYS_RECV 47)       ; recv(fd, buf, len, flags)
(define SYS_SHUTDOWN 48)   ; shutdown(fd, how)
(define SYS_CLOSE 3)       ; close(fd)

;; 地址族和类型
(define AF_INET 2)
(define SOCK_STREAM 1)
(define SOCK_DGRAM 2)

;; ========== 真实的 Socket 创建 ==========

;; 创建 Socket
(define (socket-create-real domain type protocol)
  (display "Creating socket (syscall)...")
  (newline)
  (let ((fd (syscall SYS_SOCKET domain type protocol)))
    (if (< fd 0)
        (begin
          (display "Error: socket creation failed")
          (newline)
          -1)
        (begin
          (display "Socket created: fd=")
          (display fd)
          (newline)
          fd))))

;; 关闭 Socket
(define (socket-close-real fd)
  (display "Closing socket fd=")
  (display fd)
  (newline)
  (syscall SYS_CLOSE fd))

;; 测试
(display "========================================")
(newline)
(display "    真实网络功能测试")
(newline)
(display "========================================")
(newline)
(newline)

(display "1. 创建真实的 Socket:")
(newline)
(define tcp-fd (socket-create-real AF_INET SOCK_STREAM 6))
(newline)

(display "2. 关闭 Socket:")
(newline)
(socket-close-real tcp-fd)
(newline)

(display "3. 使用 Syscall 获取时间:")
(newline)
(define SYS_TIME 201)
(define current-time (syscall SYS_TIME 0))
(display "   Current time: ")
(display current-time)
(newline)
(newline)

(display "========================================")
(newline)
(display "结论: 有了 syscall，可以实现:")
(newline)
(display "  ✓ 真实的 Socket 创建")
(newline)
(display "  ✓ 网络连接和数据传输")
(newline)
(display "  ✓ 服务器和客户端功能")
(newline)
(display "  ✓ 系统级编程")
(newline)
(newline)

(display "建议下一步:")
(newline)
(display "  1. 实现 sockaddr_in 结构体打包")
(newline)
(display "  2. 实现 bind, connect, listen")
(newline)
(display "  3. 实现 send/recv 数据传输")
(newline)
(display "  4. 创建完整的 HTTP/NTP 服务器")
(newline)
(newline)

(display "========================================")
(newline)
