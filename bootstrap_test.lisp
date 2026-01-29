; 简单自举测试

; 用 Lisp 实现的简单 eval 函数
(define (simple-eval expr)
  (cond
    ((number? expr) expr)
    ((eq? (car expr) '+)
     (+ (simple-eval (cadr expr)) (simple-eval (caddr expr))))
    ((eq? (car expr) '*)
     (* (simple-eval (cadr expr)) (simple-eval (caddr expr))))
    (else nil)))

; 测试
(display "Bootstrap test:")
(newline)
(display (simple-eval 42))
(newline)
(display (simple-eval (+ 1 2)))
(newline)
(display "Success!")
(newline)
