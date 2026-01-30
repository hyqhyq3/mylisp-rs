; 列表操作性能测试
; 测试 map, filter, reduce 等高阶函数

; 辅助函数
(define (range start end)
  (if (>= start end)
      ()
      (cons start (range (+ start 1) end))))

(define (map fn lst)
  (if (null? lst)
      ()
      (cons (fn (car lst)) (map fn (cdr lst)))))

(define (filter pred lst)
  (cond ((null? lst) ())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (reduce fn init lst)
  (if (null? lst)
      init
      (reduce fn (fn init (car lst)) (cdr lst))))

(define (square x) (* x x))

; 创建测试列表
(define numbers (range 1 101))

; Map 测试
(display "Map - squares: ")
(display (map square numbers))
(newline)

; Filter 测试
(display "Filter - evens: ")
(display (filter (lambda (x) (= (remainder x 2) 0)) numbers))
(newline)

; Reduce 测试
(display "Reduce - sum: ")
(display (reduce + 0 numbers))
(newline)

; 组合操作
(display "Sum of squares: ")
(display (reduce + 0 (map square numbers)))
(newline)

; 性能测试：复杂列表操作
(reduce + 0 (map square (filter (lambda (x) (= (remainder x 2) 0)) numbers)))
