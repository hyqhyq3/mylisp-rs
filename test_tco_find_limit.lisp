; 查找递归深度限制

(define (sum-tail n acc)
  (if (= n 0)
      acc
      (sum-tail (- n 1) (+ acc n))))

; 测试不同的深度
(display "Testing 1000: ")
(display (sum-tail 1000 0))
(newline)

(display "Testing 2000: ")
(display (sum-tail 2000 0))
(newline)

(display "Testing 3000: ")
(display (sum-tail 3000 0))
(newline)

(display "Testing 4000: ")
(display (sum-tail 4000 0))
(newline)

(display "Testing 5000: ")
(display (sum-tail 5000 0))
(newline)

(display "All tests completed!")
(newline)
