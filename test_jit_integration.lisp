;; JIT 集成测试文件

;; 测试 1: 简单算术运算
(display "Test 1: Simple arithmetic")
(newline)
(display (use-jit (+ 1 2 3)))
(newline)
(newline)

;; 测试 2: Lambda 创建
(display "Test 2: Lambda creation")
(newline)
(use-jit (define square (lambda (x) (* x x))))
(display (square 5))
(newline)
(newline)

;; 测试 3: Let 表达式
(display "Test 3: Let expression")
(newline)
(display (use-jit (let ((x 10) (y 20)) (+ x y))))
(newline)
(newline)

;; 测试 4: 条件表达式
(display "Test 4: Conditional")
(newline)
(display (use-jit (if (> 5 3) 10 20)))
(newline)
(newline)

;; 测试 5: 函数调用
(display "Test 5: Function call")
(newline)
(use-jit (define add (lambda (a b) (+ a b))))
(display (add 3 4))
(newline)
(newline)

;; 测试 6: 列表操作
(display "Test 6: List operations")
(newline)
(display (use-jit (list 1 2 3)))
(newline)
(display (use-jit (head (list 1 2 3))))
(newline)
(display (use-jit (tail (list 1 2 3))))
(newline)
(newline)

;; 测试 7: 复杂表达式（斐波那契）
(display "Test 7: Fibonacci function")
(newline)
(use-jit (define fib
  (lambda (n)
    (if (<= n 1)
        n
        (+ (fib (- n 1)) (fib (- n 2)))))))
(display "fib(10) = ")
(display (fib 10))
(newline)
(newline)

(display "All JIT integration tests completed!")
(newline)
