; 尾递归优化测试用例

(display "TCO Test Suite")
(newline)

; 测试1: 简单的尾递归函数 - 求和
(define (sum n acc)
  (if (= n 0)
      acc
      (sum (- n 1) (+ acc n))))

(display "Test 1 - Tail recursive sum (1000): ")
(display (sum 1000 0))
(newline)

; 测试2: 阶乘的尾递归版本
(define (factorial-tail n acc)
  (if (= n 0)
      acc
      (factorial-tail (- n 1) (* acc n))))

(display "Test 2 - Tail recursive factorial (10): ")
(display (factorial-tail 10 1))
(newline)

; 测试3: 列表长度计算(尾递归)
(define (length-tail lst acc)
  (if (null? lst)
      acc
      (length-tail (tail lst) (+ acc 1))))

(display "Test 3 - Tail recursive length: ")
(display (length-tail (list 1 2 3 4 5) 0))
(newline)

; 测试4: 相互尾递归
(define (even? n)
  (if (= n 0)
      #t
      (odd? (- n 1))))

(define (odd? n)
  (if (= n 0)
      #f
      (even? (- n 1))))

(display "Test 4 - Mutual tail recursion (even? 10): ")
(display (even? 10))
(newline)

; 测试5: 斐波那契数列(尾递归)
(define (fib-tail n a b)
  (if (= n 0)
      a
      (fib-tail (- n 1) b (+ a b))))

(display "Test 5 - Tail recursive fibonacci (20): ")
(display (fib-tail 20 0 1))
(newline)

(display "All TCO tests completed!")
(newline)

