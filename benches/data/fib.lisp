; 斐波那契数列 - 递归性能测试
; 测试尾调用优化和递归性能

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

; 测试不同规模的计算
(display "Fibonacci(10): ")
(display (fib 10))
(newline)

(display "Fibonacci(20): ")
(display (fib 20))
(newline)

(display "Fibonacci(25): ")
(display (fib 25))
(newline)

; 性能测试目标：计算 fib(25)
(fib 25)
