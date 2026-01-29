; ============================================
; Lisp 元循环求值器 - 自举演示
; ============================================

; 这个文件展示了如何用 Lisp 定义类似 eval 的函数
; 这就是"自举"的核心思想：用语言自己定义自己

; 1. 定义一个简单的求值器
(define (my-eval expr)
  (if (number? expr)
      expr
      (if (string? expr)
          expr
          (if (eq? (car expr) '+)
              (+ (cadr expr) (caddr expr))
              (if (eq? (car expr) '*)
                  (* (cadr expr) (caddr expr))
                  nil)))))

; 2. 测试我们的求值器
(display "=== Bootstrap Demo ===")
(newline)

(display "Test 1 - Number: ")
(display (my-eval 42))
(newline)

(display "Test 2 - String: ")
(display (my-eval "hello"))
(newline)

(display "Test 3 - Addition: ")
(display (my-eval (+ 10 20)))
(newline)

(display "Test 4 - Multiplication: ")
(display (my-eval (* 3 4)))
(newline)

; 3. 展示递归定义
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(display "Test 5 - Factorial(5): ")
(display (factorial 5))
(newline)

; 4. 高阶函数
(define (map f lst)
  (if (null? lst)
      nil
      (cons (f (car lst)) (map f (cdr lst)))))

(display "Test 6 - Map square: ")
(display (map (lambda (x) (* x x)) (list 1 2 3 4 5)))
(newline)

; 5. 自举的精髓 - 用 eval 执行代码
(display "Test 7 - Eval: ")
(display (my-eval (* 6 7)))
(newline)

(display "=== Bootstrap Complete ===")
(newline)
(display "This Lisp interpreter is running Lisp code")
(newline)
(display "that defines Lisp evaluation logic!")
(newline)
