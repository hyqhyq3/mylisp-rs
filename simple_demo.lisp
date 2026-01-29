; 简单的自举演示
; 这个文件展示如何用 Lisp 定义类似 eval 的函数

; 定义一个简单的求值函数
(define my-eval
  (lambda (expr)
    (cond
      ((number? expr) expr)
      ((symbol? expr) (error "undefined"))
      ((eq? (car expr) '+) (+ (cadr expr) (caddr expr)))
      ((eq? (car expr) '*) (* (cadr expr) (caddr expr)))
      (else (error "unknown")))))

; 测试
(display 42)
(newline)
(display (+ 10 20))
(newline)
(display (* 3 4))
(newline)
