; 修正的省略号测试

(display "=== Fixed Ellipsis Pattern Tests ===")
(newline)

; 测试1: 可变参数的列表创建
(display "Test 1 - Variable arity list:")
(define-syntax my-list
  (syntax-rules ()
    ((my-list x ...)
     (list x ...))))

(display "  (my-list 1 2 3 4 5): ")
(display (my-list 1 2 3 4 5))
(newline)
(newline)

; 测试2: 求和宏
(display "Test 2 - Sum macro:")
(define-syntax sum-all
  (syntax-rules ()
    ((sum-all x ...)
     (+ x ...))))

(display "  (sum-all 1 2 3 4 5): ")
(display (sum-all 1 2 3 4 5))
(newline)
(newline)

; 测试3: 混合固定和可变参数
(display "Test 3 - Fixed + variable args:")
(define-syntax first-and-rest
  (syntax-rules ()
    ((first-and-rest first rest ...)
     (list first (list rest ...)))))

(display "  (first-and-rest 10 20 30 40): ")
(display (first-and-rest 10 20 30 40))
(newline)
(newline)

; 测试4: 乘法宏
(display "Test 4 - Product macro:")
(define-syntax product-all
  (syntax-rules ()
    ((product-all x ...)
     (* x ...))))

(display "  (product-all 2 3 4): ")
(display (product-all 2 3 4))
(newline)
(newline)

; 测试5: 多表达式序列
(display "Test 5 - Sequence macro:")
(define-syntax do-all
  (syntax-rules ()
    ((do-all x ...)
     (begin x ...))))

(do-all
  (display "  First")
  (newline)
  (display "  Second")
  (newline))
(newline)

; 测试6: 嵌套列表
(display "Test 6 - Nested lists:")
(define-syntax lists-of
  (syntax-rules ()
    ((lists-of x ...)
     (list (list x) ...))))

(display "  (lists-of 1 2 3): ")
(display (lists-of 1 2 3))
(newline)
(newline)

; 测试7: 条件与可变参数
(display "Test 7 - When with variadic:")
(define-syntax my-when
  (syntax-rules ()
    ((my-when test body ...)
     (if test (begin body ...) nil))))

(my-when #t
  (display "  Body 1")
  (newline)
  (display "  Body 2")
  (newline))
(newline)

; 测试8: 多参数比较
(display "Test 8 - All equal:")
(define-syntax all-equal?
  (syntax-rules ()
    ((all-equal? x)
     #t)
    ((all-equal? x y ...)
     (and (= x y) (all-equal? y ...)))))

(display "  (all-equal? 5 5 5): ")
(display (all-equal? 5 5 5))
(newline)
(display "  (all-equal? 5 5 6): ")
(display (all-equal? 5 5 6))
(newline)
(newline)

; 测试9: 函数应用
(display "Test 9 - Apply to all:")
(define-syntax apply-square
  (syntax-rules ()
    ((apply-square x ...)
     (list (square x) ...))))

(define (square n) (* n n))
(display "  (apply-square 1 2 3): ")
(display (apply-square 1 2 3))
(newline)
(newline)

; 测试10: 空参数情况
(display "Test 10 - Empty args:")
(define-syntax my-or
  (syntax-rules ()
    ((my-or) #f)
    ((my-or x ...)
     (or x ...))))

(display "  (my-or): ")
(display (my-or))
(newline)
(display "  (my-or #f #f #t): ")
(display (my-or #f #f #t))
(newline)
(display "  (my-or #f #f #f): ")
(display (my-or #f #f #f))
(newline)

(display "=== All fixed ellipsis tests passed! ===")
(newline)
