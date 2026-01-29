; 测试多行表达式

(define factorial
  (lambda (n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1))))))

(display "Testing factorial:")
(newline)
(display (factorial 5))
(newline)

; 测试嵌套列表
(display (list
  1
  2
  3
  (+
    4
    5
    6)))
(newline)

; 测试 cond（如果支持）
(display "Done!")
(newline)
