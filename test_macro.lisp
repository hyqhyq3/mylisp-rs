; 宏系统测试

(display "=== Macro System Test ===")
(newline)

; 测试1: 简单宏 - when
(display "Test 1 - when macro:")
(define-syntax when
  (syntax-rules ()
    ((when test body ...)
     (if test (begin body ...) nil))))

(when #t
  (display "  when executed!")
  (newline))

; 测试2: unless 宏
(display "Test 2 - unless macro:")
(define-syntax unless
  (syntax-rules ()
    ((unless test body ...)
     (if (not test) (begin body ...) nil))))

(unless #f
  (display "  unless executed!")
  (newline))

; 测试3: 交换变量宏
(display "Test 3 - swap! macro:")
(define-syntax swap!
  (syntax-rules ()
    ((swap! x y)
     (let ((temp x))
       (set! x y)
       (set! y temp)))))

(define a 1)
(define b 2)
(display "  Before swap: a=")
(display a)
(display " b=")
(display b)
(newline)
(swap! a b)
(display "  After swap: a=")
(display a)
(display " b=")
(display b)
(newline)

; 测试4: while 循环宏
(display "Test 4 - while loop:")
(define-syntax while
  (syntax-rules ()
    ((while test body ...)
     (let ((loop (lambda ()
                   (if test
                       (begin
                         body ...
                         (loop))
                       nil))))
       (loop)))))

(define i 0)
(while (< i 5)
  (display "  i=")
  (display i)
  (newline)
  (set! i (+ i 1)))

; 测试5: inc/dec 宏
(display "Test 5 - inc/dec macros:")
(define-syntax inc
  (syntax-rules ()
    ((inc x)
     (set! x (+ x 1)))))

(define-syntax dec
  (syntax-rules ()
    ((dec x)
     (set! x (- x 1)))))

(define counter 0)
(inc counter)
(inc counter)
(inc counter)
(display "  counter after 3 inc: ")
(display counter)
(newline)
(dec counter)
(display "  counter after 1 dec: ")
(display counter)
(newline)

; 测试6: 条件宏 cond
(display "Test 6 - cond macro:")
(define-syntax cond
  (syntax-rules (else)
    ((cond (else result ...))
     (begin result ...))
    ((cond (test result ...) rest ...)
     (if test
         (begin result ...)
         (cond rest ...)))))

(cond
  ((= 1 2) (display "  1=2 (never)"))
  ((= 2 2) (display "  2=2 (executed!)"))
  (else (display "  else (never)")))
(newline)

(display "=== All macro tests completed! ===")
(newline)
