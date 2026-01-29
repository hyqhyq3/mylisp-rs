; 调试 TCO

(define (sum n)
  (sum-helper n 0))

(define (sum-helper n acc)
  (if (= n 0)
      acc
      (sum-helper (- n 1) (+ acc n))))

(display "Testing sum: ")
(display (sum 10000))
(newline)
