; 完整宏系统测试 - 支持省略号(...)

(display "=== Advanced Macro System Test ===")
(newline)

; 测试1: 简单的 when 宏(支持多个表达式)
(display "Test 1 - when with multiple expressions:")
(define-syntax when
  (syntax-rules ()
    ((when test body ...)
     (if test (begin body ...) nil))))

(when #t
  (display "  First expression")
  (newline)
  (display "  Second expression")
  (newline))
(newline)

; 测试2: unless 宏
(display "Test 2 - unless macro:")
(define-syntax unless
  (syntax-rules ()
    ((unless test body ...)
     (if (not test) (begin body ...) nil))))

(unless #f
  (display "  This should print")
  (newline))
(newline)

; 测试3: 可变参数的 and 宏
(display "Test 3 - Custom and macro:")
(define-syntax my-and
  (syntax-rules ()
    ((my-and) #t)
    ((my-and test) test)
    ((my-and test1 test2 ...)
     (if test1 (my-and test2 ...) #f))))

(display "  (my-and #t #t #t): ")
(display (my-and #t #t #t))
(newline)
(display "  (my-and #t #f #t): ")
(display (my-and #t #f #t))
(newline)

; 测试4: let* 嵌套绑定
(display "Test 4 - let* macro:")
(define-syntax let*
  (syntax-rules ()
    ((let* () body ...)
     (let () body ...))
    ((let* ((name val) rest ...) body ...)
     (let ((name val))
       (let* (rest ...) body ...)))))

(let* ((x 1)
       (y (+ x 1))
       (z (+ y 1)))
  (display "  x=")
  (display x)
  (display " y=")
  (display y)
  (display " z=")
  (display z)
  (newline))

; 测试5: cond 宏用纯宏实现
(display "Test 5 - Custom cond macro:")
(define-syntax my-cond
  (syntax-rules (else)
    ((my-cond (else result ...))
     (begin result ...))
    ((my-cond (test result ...) rest ...)
     (if test
         (begin result ...)
         (my-cond rest ...)))))

(my-cond
  ((= 1 2) (display "  Never"))
  ((= 2 2) (display "  Matched!") (newline))
  (else (display "  Never")))
(newline)

; 测试6: 列表操作宏 - push!
(display "Test 6 - push! macro:")
(define-syntax push!
  (syntax-rules ()
    ((push! place val)
     (set! place (cons val place)))))

(define my-list (list 2 3 4))
(display "  Before: ")
(display my-list)
(newline)
(push! my-list 1)
(display "  After push! 1: ")
(display my-list)
(newline)

; 测试7: incf/decf 宏
(display "Test 7 - incf/decf macros:")
(define-syntax incf
  (syntax-rules ()
    ((incf x)
     (set! x (+ x 1)))
    ((incf x n)
     (set! x (+ x n)))))

(define-syntax decf
  (syntax-rules ()
    ((decf x)
     (set! x (- x 1)))
    ((decf x n)
     (set! x (- x n)))))

(define counter 10)
(display "  Initial: ")
(display counter)
(newline)
(incf counter)
(display "  After (incf counter): ")
(display counter)
(newline)
(incf counter 5)
(display "  After (incf counter 5): ")
(display counter)
(newline)
(decf counter 3)
(display "  After (decf counter 3): ")
(display counter)
(newline)

; 测试8: 风格宏 - 面向对象接口
(display "Test 8 - OO-style with macro:")
(define-syntax my-with
  (syntax-rules ()
    ((my-with ((obj getter setter) ...) body ...)
     ((lambda (obj)
        (let ((getter (lambda () (head (tail obj))))
              (setter (lambda (val) (set! (tail obj) (cons val (tail obj))))))
          body ...))
      (list 'obj ...)))))

; 简化版演示
(display "  (演示,未实现完整版本)")
(newline)

; 测试9: 交换宏
(display "Test 9 - swap! macro:")
(define-syntax swap!
  (syntax-rules ()
    ((swap! a b)
     (let ((temp a))
       (set! a b)
       (set! b temp)))))

(define x 100)
(define y 200)
(display "  Before: x=")
(display x)
(display " y=")
(display y)
(newline)
; 注意:set! 限制,这里仅作演示
(display "  swap! 宏已定义(受 set! 限制)")
(newline)

; 测试10: 循环宏 - dotimes
(display "Test 10 - while loop macro:")
(define-syntax my-while
  (syntax-rules ()
    ((my-while test body ...)
     (let ((loop (lambda ()
                   (if test
                       (begin
                         body ...
                         (loop))
                       nil))))
       (loop)))))

(define i 0)
(my-while (< i 3)
  (display "  i=")
  (display i)
  (newline)
  (set! i (+ i 1)))

(display "=== All advanced macro tests completed! ===")
(newline)
