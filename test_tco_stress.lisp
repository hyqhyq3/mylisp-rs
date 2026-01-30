; TCO 压力测试

(display "TCO Stress Test")
(newline)

; 测试1: 深度尾递归 (10000次)
(define (sum-tail n acc)
  (if (= n 0)
      acc
      (sum-tail (- n 1) (+ acc n))))

(display "Test 1 - Sum 10000: ")
(display (sum-tail 10000 0))
(newline)

; 测试2: 超深度尾递归 (50000次)
(display "Test 2 - Sum 50000: ")
(display (sum-tail 50000 0))
(newline)

; 测试3: 极深度尾递归 (100000次)
(display "Test 3 - Sum 100000: ")
(display (sum-tail 100000 0))
(newline)

(display "Stress test completed!")
(newline)
