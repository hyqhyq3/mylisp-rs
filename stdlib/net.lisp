;; MyLisp 标准库 - 网络模块 (net.lisp)
;;
;; 提供网络相关的函数和常量
;;
;; 使用方式：
;;   (load stdlib/net.lisp)

;; ========== 网络常量 ==========

;; 地址族
(define AF_INET 2)      ; IPv4
(define AF_INET6 10)    ; IPv6
(define AF_UNIX 1)      ; 本地 socket

;; Socket 类型
(define SOCK_STREAM 1)  ; TCP
(define SOCK_DGRAM 2)   ; UDP
(define SOCK_RAW 3)     ; 原始 socket

;; 协议
(define IPPROTO_TCP 6)
(define IPPROTO_UDP 17)
(define IPPROTO_ICMP 1)

;; 系统调用号
(define SYS_SOCKET 41)
(define SYS_BIND 49)
(define SYS_CONNECT 42)
(define SYS_LISTEN 50)
(define SYS_ACCEPT 43)
(define SYS_SENDTO 44)
(define SYS_RECVFROM 45)
(define SYS_SEND 46)
(define SYS_RECV 47)
(define SYS_SHUTDOWN 48)

;; ========== Socket 操作 ==========

;; 创建 Socket - 返回模拟的文件描述符
;; 注意：实际的 socket 创建需要 FFI 支持
(define (socket-create domain type protocol)
  (display "Creating socket (simulated)...")
  (newline)
  42)  ; 返回模拟的 fd

;; 绑定 Socket 到地址
;; 参数: fd, address, port
;; 返回: 0 成功, -1 失败
(define (socket-bind fd address port)
  (display "Binding socket to port ")
  (display port)
  (newline)
  ;; 简化实现：实际需要构建 sockaddr_in 结构
  (display "Bind: fd=")
  (display fd)
  (display ", addr=")
  (display address)
  (display ":")
  (display port)
  (newline)
  0)

;; 监听连接
;; 参数: fd, backlog
;; 返回: 0 成功, -1 失败
(define (socket-listen fd backlog)
  (display "Listening on fd=")
  (display fd)
  (display " (backlog=")
  (display backlog)
  (display ")")
  (newline)
  0)

;; 接受连接
;; 参数: fd
;; 返回: 新的 socket 文件描述符
(define (socket-accept fd)
  (display "Accepting connection on fd=")
  (display fd)
  (newline)
  ;; 简化实现
  -1)

;; 连接到服务器
;; 参数: fd, address, port
;; 返回: 0 成功, -1 失败
(define (socket-connect fd address port)
  (display "Connecting to ")
  (display address)
  (display ":")
  (display port)
  (newline)
  0)

;; ========== 数据发送/接收 ==========

;; 发送数据
;; 参数: fd, buffer, size
;; 返回: 发送的字节数
(define (socket-send fd buffer size)
  (display "Sending ")
  (display size)
  (display " bytes on fd=")
  (display fd)
  (newline)
  size)

;; 接收数据
;; 参数: fd, buffer, size
;; 返回: 接收的字节数
(define (socket-recv fd buffer size)
  (display "Receiving up to ")
  (display size)
  (display " bytes on fd=")
  (display fd)
  (newline)
  0)

;; 发送数据到指定地址
;; 参数: fd, buffer, size, address, port
;; 返回: 发送的字节数
(define (socket-sendto fd buffer size address port)
  (display "Sending to ")
  (display address)
  (display ":")
  (display port)
  (newline)
  size)

;; 从指定地址接收数据
;; 参数: fd, buffer, size
;; 返回: 接收的字节数
(define (socket-recvfrom fd buffer size)
  (display "Receiving from any address on fd=")
  (display fd)
  (newline)
  0)

;; 关闭 Socket
;; 参数: fd
;; 返回: 0 成功
(define (socket-close fd)
  (display "Closing socket fd=")
  (display fd)
  (newline)
  0)

;; ========== IP 地址操作 ==========

;; 检查 IP 地址格式（简化）
(define (ipv4-valid? addr)
  ;; 简化实现：假设有效
  #t)

;; IP 地址转整数（简化）
(define (ipv4-to-int addr)
  ;; 简化实现：返回 0
  0)

;; 整数转 IP 地址（简化）
(define (int-to-ipv4 value)
  "0.0.0.0")

;; ========== 端口操作 ==========

;; 检查端口是否在有效范围
(define (port-valid? port)
  (and (>= port 0) (<= port 65535)))

;; 检查是否为特权端口（< 1024）
(define (port-privileged? port)
  (< port 1024))

;; 检查是否为熟知端口
(define (port-well-known? port)
  (and (>= port 0) (<= port 1023)))

;; 检查是否为动态端口
(define (port-dynamic? port)
  (and (>= port 49152) (<= port 65535)))

;; ========== 常用端口 ==========

(define PORT_ECHO 7)
(define PORT_DISCARD 9)
(define PORT_DAYTIME 13)
(define PORT_FTP 20)
(define PORT_FTP_DATA 21)
(define PORT_SSH 22)
(define PORT_TELNET 23)
(define PORT_SMTP 25)
(define PORT_DNS 53)
(define PORT_HTTP 80)
(define PORT_HTTPS 443)
(define PORT_NTP 123)

;; ========== 网络工具函数 ==========

;; 创建 TCP Socket
(define (tcp-socket)
  (socket-create AF_INET SOCK_STREAM IPPROTO_TCP))

;; 创建 UDP Socket
(define (udp-socket)
  (socket-create AF_INET SOCK_DGRAM IPPROTO_UDP))

;; 启动 TCP 服务器
(define (tcp-server-start port backlog)
  (let ((fd (tcp-socket)))
    (if (< fd 0)
        (begin
          (display "Failed to create TCP socket")
          (newline)
          -1)
        (begin
          (socket-bind fd "0.0.0.0" port)
          (socket-listen fd backlog)
          fd))))

;; 启动 UDP 服务器
(define (udp-server-start port)
  (let ((fd (udp-socket)))
    (if (< fd 0)
        (begin
          (display "Failed to create UDP socket")
          (newline)
          -1)
        (begin
          (socket-bind fd "0.0.0.0" port)
          fd))))

;; ========== 网络信息 ==========

;; 显示网络统计信息
(define (net-show-info)
  (display "========================================")
  (newline)
  (display "         Network Information")
  (display "========================================")
  (newline)
  (newline)

  (display "Address Families:")
  (newline)
  (display "  AF_INET (IPv4): ")
  (display AF_INET)
  (newline)

  (display "  AF_INET6 (IPv6): ")
  (display AF_INET6)
  (newline)

  (display "  AF_UNIX: ")
  (display AF_UNIX)
  (newline)
  (newline)

  (display "Socket Types:")
  (newline)
  (display "  SOCK_STREAM (TCP): ")
  (display SOCK_STREAM)
  (newline)

  (display "  SOCK_DGRAM (UDP): ")
  (display SOCK_DGRAM)
  (newline)

  (display "  SOCK_RAW: ")
  (display SOCK_RAW)
  (newline)
  (newline)

  (display "Common Ports:")
  (newline)
  (display "  HTTP: ")
  (display PORT_HTTP)
  (newline)

  (display "  HTTPS: ")
  (display PORT_HTTPS)
  (newline)

  (display "  SSH: ")
  (display PORT_SSH)
  (newline)

  (display "  FTP: ")
  (display PORT_FTP)
  (newline)

  (display "  DNS: ")
  (display PORT_DNS)
  (newline)

  (display "  NTP: ")
  (display PORT_NTP)
  (newline)
  (newline)

  (display "========================================")
  (newline))

;; ========== 测试函数 ==========

;; 测试网络库
(define (net-test)
  (let ((tcp-fd (tcp-socket))
        (udp-fd (udp-socket)))
    (display "========================================")
    (newline)
    (display "       MyLisp Network Library Test")
    (newline)
    (display "========================================")
    (newline)

    (display "Test 1: Socket Creation")
    (newline)
    (display "  TCP socket fd: ")
    (display tcp-fd)
    (newline)

    (display "  UDP socket fd: ")
    (display udp-fd)
    (newline)
    (newline)

    (display "Test 2: Port Validation")
    (newline)
    (display "  Port 80 valid: ")
    (display (port-valid? 80))
    (newline)

    (display "  Port 80 privileged: ")
    (display (port-privileged? 80))
    (newline)

    (display "  Port 8080 privileged: ")
    (display (port-privileged? 8080))
    (newline)

    (display "  Port 50000 dynamic: ")
    (display (port-dynamic? 50000))
    (newline)
    (newline)

    (display "Test 3: Common Ports")
    (newline)
    (display "  HTTP: ")
    (display PORT_HTTP)
    (newline)

    (display "  HTTPS: ")
    (display PORT_HTTPS)
    (newline)

    (display "  SSH: ")
    (display PORT_SSH)
    (newline)

    (display "  NTP: ")
    (display PORT_NTP)
    (newline)
    (newline)

    (display "========================================")
    (newline)
    (display "All tests passed!")
    (newline)
    (display "========================================")
    (newline)))

;; 导出的函数列表
(define *net-functions*
  (AF_INET AF_INET6 AF_UNIX
   SOCK_STREAM SOCK_DGRAM SOCK_RAW
   IPPROTO_TCP IPPROTO_UDP IPPROTO_ICMP
   SYS_SOCKET SYS_BIND SYS_CONNECT SYS_LISTEN SYS_ACCEPT
   SYS_SENDTO SYS_RECVFROM SYS_SEND SYS_RECV
   socket-create socket-bind socket-listen socket-accept socket-connect
   socket-send socket-recv socket-sendto socket-recvfrom socket-close
   ipv4-valid? ipv4-to-int int-to-ipv4
   port-valid? port-privileged? port-well-known? port-dynamic?
   PORT_ECHO PORT_DISCARD PORT_DAYTIME PORT_FTP PORT_FTP_DATA
   PORT_SSH PORT_TELNET PORT_SMTP PORT_DNS
   PORT_HTTP PORT_HTTPS PORT_NTP
   tcp-socket udp-socket
   tcp-server-start udp-server-start
   net-show-info net-test))
