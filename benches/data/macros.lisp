; 宏展开性能测试
; 测试宏定义和展开的性能

; 简单宏
(define-syntax unless
  (syntax-rules ()
    ((unless condition body)
     (if (not condition) body))))

; 递归宏
(define-syntax inc!
  (syntax-rules ()
    ((inc! x) (set! x (+ x 1)))))

; 复杂宏：when
(define-syntax when
  (syntax-rules ()
    ((when test body1 body2 ...)
     (if test
         (begin body1 body2 ...)))))

; 模式匹配宏
(define-syntax match
  (syntax-rules ()
    ((match x ((pattern) body) ...)
     (cond ((equal? x 'pattern) body) ...))))

; 测试宏展开
(define x 10)
(unless (= x 0) (display "x is not zero") (newline))

(when (= x 10)
      (display "x is 10")
      (newline))

; 重复展开测试
(define (test-macro-expand)
  (define a 0)
  (define b 0)
  (define c 0)
  (inc! a)
  (inc! b)
  (inc! c)
  (+ a b c))

; 性能测试：多次宏展开
(define (macro-stress n)
  (if (= n 0)
      0
      (begin
        (test-macro-expand)
        (macro-stress (- n 1)))))

(macro-stress 100)
