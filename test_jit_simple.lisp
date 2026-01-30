;; 简单的 JIT 集成测试

;; 测试 1: 简单算术运算
(display "Test 1: Simple arithmetic")
(newline)
(display (use-jit (+ 1 2 3)))
(newline)
(newline)

;; 测试 2: 比较运算
(display "Test 2: Comparison")
(newline)
(display (use-jit (> 5 3)))
(newline)
(display (use-jit (< 5 3)))
(newline)
(newline)

;; 测试 3: 条件表达式
(display "Test 3: Conditional")
(newline)
(display (use-jit (if (> 5 3) 10 20)))
(newline)
(display (use-jit (if (< 5 3) 10 20)))
(newline)
(newline)

;; 测试 4: Lambda 表达式（不使用 define）
(display "Test 4: Lambda expression")
(newline)
(display (use-jit ((lambda (x) (* x x)) 5)))
(newline)
(newline)

;; 测试 5: Let 表达式
(display "Test 5: Let expression")
(newline)
(display (use-jit (let ((x 10) (y 20)) (+ x y))))
(newline)
(newline)

;; 测试 6: 列表操作（使用 cons 而不是 list）
(display "Test 6: List operations")
(newline)
(display (use-jit (cons 1 (cons 2 (cons 3 nil)))))
(newline)
(display (use-jit (head (cons 1 (cons 2 nil)))))
(newline)
(display (use-jit (tail (cons 1 (cons 2 nil)))))
(newline)
(newline)

(display "Simple JIT tests completed!")
(newline)
