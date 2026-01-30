;; 测试闭包变量捕获

(display "=== Testing Closure Variable Capture ===")
(newline)
(newline)

;; 测试 1: 简单闭包
(display "Test 1: Simple closure")
(newline)
(display "  ((lambda (x) ((lambda (y) (+ x y)) 3)) 10) = ")
(display (use-jit ((lambda (x) ((lambda (y) (+ x y)) 3)) 10)))
(newline)
(newline)

;; 测试 2: 嵌套闭包
(display "Test 2: Nested closure")
(newline)
(display "  ((lambda (x) ((lambda (y) ((lambda (z) (+ x (+ y z))) 5)) 3)) 10) = ")
(display (use-jit ((lambda (x) ((lambda (y) ((lambda (z) (+ x (+ y z))) 5)) 3)) 10)))
(newline)
(newline)

;; 测试 3: 闭包修改外层变量（只读）
(display "Test 3: Closure reading outer variable")
(newline)
(display "  ((lambda (x) ((lambda () (* x x)) )) 5) = ")
(display (use-jit ((lambda (x) ((lambda () (* x x)))) 5)))
(newline)
(newline)

(display "=== Tests completed ===")
(newline)
