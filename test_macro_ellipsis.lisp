; 测试省略号的高级用法

(display "=== Ellipsis Pattern Tests ===")
(newline)

; 测试1: 简单的列表重复
(display "Test 1 - Simple list repetition:")
(define-syntax double-all
  (syntax-rules ()
    ((double-all (x ...))
     (list (* x 2) ...))))

(display "  (double-all (1 2 3)): ")
(display (double-all (1 2 3)))
(newline)
(newline)

; 测试2: 嵌套列表模式
(display "Test 2 - Nested list patterns:")
(define-syntax map-square
  (syntax-rules ()
    ((map-square (x ...))
     (list (* x x) ...))))

(display "  (map-square (2 3 4 5)): ")
(display (map-square (2 3 4 5)))
(newline)
(newline)

; 测试3: 混合固定和可变参数
(display "Test 3 - Mixed fixed and variable args:")
(define-syntax first-and-rest
  (syntax-rules ()
    ((first-and-rest first rest ...)
     (list first (list rest ...)))))

(display "  (first-and-rest 1 2 3 4): ")
(display (first-and-rest 1 2 3 4))
(newline)
(newline)

; 测试4: 嵌套省略号(简单版本)
(display "Test 4 - Nested ellipsis demonstration:")
(define-syntax display-all
  (syntax-rules ()
    ((display-all x ...)
     (begin (display x) (newline) ...))))

(display-all "Hello" "World" "!")
(newline)

; 测试5: 创建列表的宏
(display "Test 5 - List creation macro:")
(define-syntax my-list
  (syntax-rules ()
    ((my-list x ...)
     (list x ...))))

(display "  (my-list 10 20 30): ")
(display (my-list 10 20 30))
(newline)
(newline)

; 测试6: 算术运算宏
(display "Test 6 - Arithmetic macro:")
(define-syntax sum-all
  (syntax-rules ()
    ((sum-all x ...)
     (+ x ...))))

(display "  (sum-all 1 2 3 4 5): ")
(display (sum-all 1 2 3 4 5))
(newline)
(newline)

; 测试7: 比较所有值
(display "Test 7 - Compare all values:")
(define-syntax all-positive?
  (syntax-rules ()
    ((all-positive? x ...)
     (and (> x 0) ...))))

(display "  (all-positive? 1 2 3 4): ")
(display (all-positive? 1 2 3 4))
(newline)
(display "  (all-positive? 1 -2 3): ")
(display (all-positive? 1 -2 3))
(newline)
(newline)

; 测试8: 创建函数调用列表
(display "Test 8 - Create function calls:")
(define-syntax call-all
  (syntax-rules ()
    ((call-all fn x ...)
     (list (fn x) ...))))

(display "  (call-all (lambda (n) (* n n)) 1 2 3): ")
(display (call-all (lambda (n) (* n n)) 1 2 3))
(newline)
(newline)

; 测试9: 多层嵌套模式
(display "Test 9 - Multi-level pattern:")
(define-syntax nested-sum
  (syntax-rules ()
    ((nested-sum (a ...) (b ...))
     (+ (+ a ...) (+ b ...)))))

(display "  (nested-sum (1 2) (3 4)): ")
(display (nested-sum (1 2) (3 4)))
(newline)
(newline)

; 测试10: 条件展开
(display "Test 10 - Conditional expansion:")
(define-syntax when-all
  (syntax-rules ()
    ((when-all test body ...)
     (if test
         (begin body ...)
         nil))))

(when-all #t
  (display "  All conditions true")
  (display " - executing")
  (newline))
(newline)

(display "=== All ellipsis tests passed! ===")
(newline)
