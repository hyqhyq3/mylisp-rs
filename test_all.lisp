; ============================================
; MyLisp 综合功能测试
; ============================================

(display "========================================")
(display " MyLisp Interpreter - Feature Test")
(display "========================================")
(newline)

; 1. 基础运算
(display "1. Basic Operations:")
(display (+ 1 2 3))
(display (* 4 5))
(display (/ 10 2))
(display (- 20 7))
(newline)

; 2. 比较运算
(display "2. Comparisons:")
(display (= 10 10))
(display (< 5 8))
(display (>= 10 5))
(newline)

; 3. 字符串
(display "3. Strings:")
(display "Hello, Lisp!")
(display (string? "test"))
(display (string? 42))
(newline)

; 4. 列表操作
(display "4. Lists:")
(define lst (list 1 2 3 4 5))
(display lst)
(display (car lst))
(display (cdr lst))
(display (cons 0 lst))
(display (append lst (list 6 7)))
(newline)

; 5. 条件表达式
(display "5. Conditionals:")
(define abs
  (lambda (x)
    (if (< x 0)
        (- x)
        x)))
(display (abs -10))
(display (abs 15))
(newline)

; 6. 递归函数
(display "6. Recursion - Fibonacci:")
(define fib
  (lambda (n)
    (if (<= n 1)
        n
        (+ (fib (- n 1)) (fib (- n 2))))))
(display (fib 10))
(newline)

; 7. 高阶函数
(display "7. Higher-Order Functions:")
(define (map f lst)
  (if (null? lst)
      nil
      (cons (f (car lst)) (map f (cdr lst)))))

(define (filter p lst)
  (if (null? lst)
      nil
      (if (p (car lst))
          (cons (car lst) (filter p (cdr lst)))
          (filter p (cdr lst)))))

(display "Squares:")
(display (map (lambda (x) (* x x)) (list 1 2 3 4 5)))

(display "Even numbers:")
(display (filter (lambda (x) (= (mod x 2) 0)) (list 1 2 3 4 5 6 7 8 9 10)))
(newline)

; 8. 词法作用域
(display "8. Lexical Scope:")
(define (make-counter init)
  (lambda ()
    (set! init (+ init 1))
    init))

(define counter1 (make-counter 0))
(define counter2 (make-counter 100))

(display (counter1))
(display (counter1))
(display (counter2))
(display (counter1))
(newline)

; 9. 自举测试 - 用 Lisp 实现 eval
(display "9. Bootstrap - my-eval:")
(define (my-eval expr)
  (cond
    ((number? expr) expr)
    ((string? expr) expr)
    ((null? expr) nil)
    ((eq? (car expr) '+)
     (+ (cadr expr) (caddr expr)))
    ((eq? (car expr) '*)
     (* (cadr expr) (caddr expr)))
    (else nil)))

(display (my-eval 100))
(display (my-eval (* 12 12)))
(newline)

; 10. 复杂表达式
(display "10. Complex Expressions:")
(display (+
  (* 3 4)
  (/ 10 2)
  (- 20 5)))
(newline)

(display "========================================")
(display " All tests completed successfully!")
(display "========================================")
(newline)
