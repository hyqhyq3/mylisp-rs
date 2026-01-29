; 简化的尾递归测试

; 测试1: 简单的尾递归
(define (sum n acc)
  (if (= n 0)
      acc
      (sum (- n 1) (+ acc n))))

(sum 10 0)
