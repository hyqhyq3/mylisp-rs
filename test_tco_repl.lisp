(define (sum n acc)
  (if (= n 0)
      acc
      (sum (- n 1) (+ acc n))))

(sum 100 0)
