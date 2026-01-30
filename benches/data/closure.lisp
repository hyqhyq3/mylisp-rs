; Lambda 闭包性能测试
; 测试闭包创建、变量捕获和高阶函数调用

; 闭包捕获外部变量
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add5 (make-adder 5))
(define add10 (make-adder 10))

(display "add5(3): ")
(display (add5 3))
(newline)

(display "add10(3): ")
(display (add10 3))
(newline)

; 高阶函数性能测试
(define (apply-n fn n arg)
  (if (= n 0)
      arg
      (apply-n fn (- n 1) (fn arg))))

; 递归闭包
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

(define counter (make-counter))

(display "Counter: ")
(display (counter))
(display " ")
(display (counter))
(display " ")
(display (counter))
(newline)

; Y 组合子测试
(define (Y fn)
  ((lambda (x) (x x))
   (lambda (x) (fn (lambda (v) ((x x) v))))))

(define fact
  (Y (lambda (f)
       (lambda (n)
         (if (< n 2)
             1
             (* n (f (- n 1))))))))

(display "Factorial(10) via Y combinator: ")
(display (fact 10))
(newline)

; 性能测试：多次闭包调用
(define (test-closures)
  (define (loop i acc)
    (if (> i 1000)
        acc
        (loop (+ i 1) (+ acc (add5 i)))))
  (loop 0 0))

(test-closures)
