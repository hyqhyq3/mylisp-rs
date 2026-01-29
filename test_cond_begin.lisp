; 测试 cond 和 begin 特殊形式

(display "=== Testing cond and begin ===")
(newline)

; 测试1: 基本 cond
(display "Test 1 - Basic cond:")
(define x 5)
(cond
  ((= x 1) (display "  x is 1"))
  ((= x 5) (display "  x is 5"))
  (else (display "  x is something else")))
(newline)

; 测试2: cond 返回值
(display "Test 2 - cond return value:")
(define (grade n)
  (cond
    ((>= n 90) "A")
    ((>= n 80) "B")
    ((>= n 70) "C")
    ((>= n 60) "D")
    (else "F")))

(display "  Grade 95: ")
(display (grade 95))
(newline)
(display "  Grade 85: ")
(display (grade 85))
(newline)
(display "  Grade 55: ")
(display (grade 55))
(newline)

; 测试3: cond 多表达式
(display "Test 3 - cond with multiple expressions:")
(define y 10)
(cond
  ((> y 5)
   (display "  y > 5")
   (display ", executing multiple expressions")
   (newline))
  (else
   (display "  y <= 5")
   (newline)))

; 测试4: cond 只有测试条件
(display "Test 4 - cond with test only:")
(display "  First true value: ")
(display (cond
  (#f)
  (#f)
  (42)
  (else "never")))
(newline)

; 测试5: 基本 begin
(display "Test 5 - Basic begin:")
(begin
  (display "  Line 1")
  (newline)
  (display "  Line 2")
  (newline)
  (display "  Line 3")
  (newline))

; 测试6: begin 返回值
(display "Test 6 - begin return value:")
(define result
  (begin
    (display "  Calculating...")
    (newline)
    (+ 1 2 3)))
(display "  Result: ")
(display result)
(newline)

; 测试7: 嵌套 begin 和 cond
(display "Test 7 - Nested begin and cond:")
(define (describe-number n)
  (cond
    ((< n 0)
     (begin
       (display "  ")
       (display n)
       (display " is negative")
       (newline)
       "negative"))
    ((= n 0)
     (begin
       (display "  ")
       (display n)
       (display " is zero")
       (newline)
       "zero"))
    (else
     (begin
       (display "  ")
       (display n)
       (display " is positive")
       (newline)
       "positive"))))

(describe-number -5)
(describe-number 0)
(describe-number 42)

; 测试8: begin 在 lambda 中
(display "Test 8 - begin in lambda:")
(define (process-list lst)
  (map (lambda (x)
         (begin
           (display "  Processing: ")
           (display x)
           (newline)
           (* x 2)))
       lst))

(display "Result: ")
(display (process-list (list 1 2 3)))
(newline)

(display "=== All cond and begin tests completed! ===")
(newline)
