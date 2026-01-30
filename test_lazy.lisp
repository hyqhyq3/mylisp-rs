; Lazy evaluation tests

(display "Lazy Evaluation Tests")
(newline)

; 1. Argument not used should not be evaluated (no error)
(define (const5 x) 5)
(display "Test 1 - unused arg: ")
(display (const5 (/ 1 0)))
(newline)

; 2. Memoization: side effect should happen once
(define (use-twice x)
  (+ x x))

(define (tick)
  (begin
    (display "x")
    1))

(display "Test 2 - memoization: ")
(display (use-twice (tick)))
(newline)

; 3. let should be lazy in bindings
(display "Test 3 - let binding: ")
(display (let ((x (/ 1 0))) 7))
(newline)

; 4. condition short-circuit should avoid forcing
(display "Test 4 - if branch: ")
(display (if #t 1 (/ 1 0)))
(newline)

; 5. list elements should be lazy
(display "Test 5 - list element: ")
(display (head (list 1 (/ 1 0))))
(newline)

(display "All lazy tests completed!")
(newline)
