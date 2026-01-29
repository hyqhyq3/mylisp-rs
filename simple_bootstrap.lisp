; 最简单的自举测试

; 定义一个简单的 eval 函数
(define (simple-eval expr)
  (cond
    ((number? expr) expr)
    ((symbol? expr) (error "Undefined symbol"))
    ((eq? (car expr) '+)
     (+ (cadr expr) (caddr expr)))
    ((eq? (car expr) '*)
     (* (cadr expr) (caddr expr)))
    (else (error "Unknown expression"))))

; 测试
(display "Testing simple-eval:")
(newline)
(display (simple-eval 42))
(newline)
(display (simple-eval (+ 1 2)))
(newline)
(display "Bootstrap successful!")
(newline)
