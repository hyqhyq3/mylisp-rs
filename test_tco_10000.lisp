; 合理的 TCO 测试 (10000次递归)

(display "TCO Test - 10000 iterations")
(newline)

(define (sum-tail n acc)
  (if (= n 0)
      acc
      (sum-tail (- n 1) (+ acc n))))

(display "Sum 10000: ")
(display (sum-tail 10000 0))
(newline)

(display "Test completed successfully!")
(newline)
