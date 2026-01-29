; 自举测试 - 用 Lisp 实现 Lisp 解释器

; 基础自求值表达式测试
(display "Test 1: Self-evaluating")
(newline)
(display 42)
(newline)
(display "hello")
(newline)

; 简单的 eval 函数
(define (simple-eval expr)
  (cond
    ((number? expr) expr)
    ((string? expr) expr)
    ((eq? (car expr) '+) (+ (cadr expr) (caddr expr)))
    ((eq? (car expr) '*) (* (cadr expr) (caddr expr)))
    (else nil)))

; 测试 simple-eval
(display "Test 2: simple-eval")
(newline)
(display (simple-eval 42))
(newline)
(display (simple-eval (+ 10 20)))
(newline)

; 显示成功信息
(display "Bootstrap test complete!")
(newline)
