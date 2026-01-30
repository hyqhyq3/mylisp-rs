; 素数计算 - 数学运算性能测试
; 使用埃拉托斯特尼筛法

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum-primes a b)
  (define (iter count total)
    (if (> count b)
        total
        (iter (+ count 1)
              (if (prime? count)
                  (+ total count)
                  total))))
  (iter a 0))

; 测试素数求和
(display "Sum of primes from 2 to 100: ")
(display (sum-primes 2 100))
(newline)

(display "Sum of primes from 2 to 500: ")
(display (sum-primes 2 500))
(newline)

; 性能测试目标
(sum-primes 2 500)
