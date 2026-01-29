; 简单的宏系统测试

(display "=== Simple Macro Test ===")
(newline)

; 测试1: 简单宏 - when (单表达式)
(display "Test 1 - when macro:")
(define-syntax when
  (syntax-rules ()
    ((when test body)
     (if test body nil))))

(when #t
  (display "  when executed!")
  )

(newline)

; 测试2: 单参数宏
(display "Test 2 - square macro:")
(define-syntax square
  (syntax-rules ()
    ((square x)
     (* x x))))

(display "  square of 5 = ")
(display (square 5))
(newline)

; 测试3: 嵌套表达式宏
(display "Test 3 - double-add macro:")
(define-syntax double-add
  (syntax-rules ()
    ((double-add x y)
     (+ (* 2 x) (* 2 y)))))

(display "  double-add 3 4 = ")
(display (double-add 3 4))
(newline)

(display "=== Tests completed! ===")
(newline)
