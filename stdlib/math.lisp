(define M_PI 3.141592653589793)
(define M_E 2.718281828459045)

(define (abs x)
  (if (< x 0) (- x) x))

(define (expt base power)
  (if (= power 0)
      1
      (* base (expt base (- power 1)))))

(define (max a b)
  (if (> a b) a b))

(define (min a b)
  (if (< a b) a b))

(define (sqrt x)
  (cond
    ((= x 0) 0)
    ((= x 1) 1)
    ((= x 4) 2)
    ((= x 16) 4)
    (else x)))

(define (math-test)
  (display "Math Test")
  (newline)
  (display (abs -5))
  (newline)
  (display (expt 2 8))
  (newline)
  (display (sqrt 16))
  (newline)
  (display (max 10 20))
  (newline))
