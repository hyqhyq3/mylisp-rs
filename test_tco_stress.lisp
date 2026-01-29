; 压力测试: 大数尾递归

(display "Stress Test: Tail recursive sum of 100000 numbers")
(newline)

(define (sum n acc)
  (if (= n 0)
      acc
      (sum (- n 1) (+ acc n))))

(display "Result: ")
(display (sum 100000 0))
(newline)
(display "Test completed without stack overflow!")
(newline)
