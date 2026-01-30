;; 测试嵌套 let 表达式

(display "=== Testing Nested Let Expressions ===")
(newline)
(newline)

;; 测试 1: 两层嵌套 let
(display "Test 1: Two-level nested let")
(newline)
(display "  (let ((x 10)) (let ((y 20)) (+ x y))) = ")
(display (use-jit (let ((x 10)) (let ((y 20)) (+ x y)))))
(newline)
(newline)

;; 测试 2: 三层嵌套 let
(display "Test 2: Three-level nested let")
(newline)
(display "  (let ((x 10)) (let ((y 20)) (let ((z 30)) (+ x (+ y z))))) = ")
(display (use-jit (let ((x 10)) (let ((y 20)) (let ((z 30)) (+ x (+ y z)))))))
(newline)
(newline)

;; 测试 3: 嵌套 let 使用外层变量
(display "Test 3: Nested let using outer variable")
(newline)
(display "  (let ((x 5)) (let ((y (* x 2))) (+ x y))) = ")
(display (use-jit (let ((x 5)) (let ((y (* x 2))) (+ x y)))))
(newline)
(newline)

;; 测试 4: 同名变量遮蔽
(display "Test 4: Variable shadowing")
(newline)
(display "  (let ((x 10)) (let ((x 20)) x)) = ")
(display (use-jit (let ((x 10)) (let ((x 20)) x))))
(newline)
(newline)

(display "=== Tests completed ===")
(newline)
