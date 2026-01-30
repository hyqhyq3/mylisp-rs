;; JIT vs 解释器性能基准测试

(display "=== JIT vs Interpreter Performance Benchmark ===")
(newline)
(newline)

;; 辅助函数：测量表达式执行时间
(define measure-time
  (lambda (name thunk iterations)
    (display name)
    (display " ... ")
    (let ((start 0))
      (set! start (use-jit (syscall 1)))  ; 获取当前时间（纳秒）
      (let loop ((i 0))
        (if (< i iterations)
            (begin
              (thunk)
              (loop (+ i 1)))
            (let ((end 0))
              (set! end (use-jit (syscall 1)))
              (let ((elapsed (/ (- end start) 1000000.0)))  ; 转换为毫秒
                (display elapsed)
                (display " ms")
                (newline))))))))

;; 测试 1: 简单算术运算
(newline)
(display "Test 1: Simple arithmetic")
(newline)
(measure-time
  "  Interpreter: (+ 1 2 3)"
  (lambda () (+ 1 2 3))
  10000)
(measure-time
  "  JIT:        (+ 1 2 3)"
  (lambda () (use-jit (+ 1 2 3)))
  10000)

;; 测试 2: 复杂算术运算
(newline)
(display "Test 2: Complex arithmetic")
(newline)
(measure-time
  "  Interpreter: (+ (* 2 3) (/ 10 2))"
  (lambda () (+ (* 2 3) (/ 10 2)))
  10000)
(measure-time
  "  JIT:        (+ (* 2 3) (/ 10 2))"
  (lambda () (use-jit (+ (* 2 3) (/ 10 2))))
  10000)

;; 测试 3: Lambda 调用
(newline)
(display "Test 3: Lambda call")
(newline)
(measure-time
  "  Interpreter: ((lambda (x) (* x x)) 5)"
  (lambda () ((lambda (x) (* x x)) 5))
  10000)
(measure-time
  "  JIT:        ((lambda (x) (* x x)) 5)"
  (lambda () (use-jit ((lambda (x) (* x x)) 5)))
  10000)

;; 测试 4: Let 表达式
(newline)
(display "Test 4: Let expression")
(newline)
(measure-time
  "  Interpreter: (let ((x 10) (y 20)) (+ x y))"
  (lambda () (let ((x 10) (y 20)) (+ x y)))
  10000)
(measure-time
  "  JIT:        (let ((x 10) (y 20)) (+ x y))"
  (lambda () (use-jit (let ((x 10) (y 20)) (+ x y))))
  10000)

;; 测试 5: 条件表达式
(newline)
(display "Test 5: Conditional expression")
(newline)
(measure-time
  "  Interpreter: (if (> 5 3) 10 20)"
  (lambda () (if (> 5 3) 10 20))
  10000)
(measure-time
  "  JIT:        (if (> 5 3) 10 20)"
  (lambda () (use-jit (if (> 5 3) 10 20)))
  10000)

;; 测试 6: 列表操作
(newline)
(display "Test 6: List operations")
(newline)
(measure-time
  "  Interpreter: (head (cons 1 (cons 2 nil)))"
  (lambda () (head (cons 1 (cons 2 nil))))
  10000)
(measure-time
  "  JIT:        (head (cons 1 (cons 2 nil)))"
  (lambda () (use-jit (head (cons 1 (cons 2 nil)))))
  10000)

(newline)
(display "=== Benchmark completed ===")
(newline)
