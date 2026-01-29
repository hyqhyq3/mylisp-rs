; 简化版元循环求值器 - 用于测试自举

; 定义 eval 函数（简化版）
(define (my-eval exp env)
  (cond
    ((number? exp) exp)
    ((symbol? exp) (lookup exp env))
    ((eq? (car exp) 'quote) (cadr exp))
    ((eq? (car exp) 'if)
     (if (my-eval (cadr exp) env)
         (my-eval (caddr exp) env)
         (my-eval (cadddr exp) env)))
    ((eq? (car exp) 'lambda)
     (list 'closure (cadr exp) (caddr exp) env))
    ((eq? (car exp) 'define)
     (define! (cadr exp) (my-eval (caddr exp) env) env))
    (else
     (my-apply (my-eval (car exp) env)
               (map (lambda (x) (my-eval x env)) (cdr exp))
               env))))

; 简化的 apply
(define (my-apply proc args env)
  (cond
    ((primitive? proc)
     (apply-prim proc args))
    ((eq? (car proc) 'closure)
     (my-eval-body (caddr proc)
                   (extend (cadr proc) args (cadddr proc))))
    (else (error "Unknown procedure"))))

; 辅助函数
(define (my-eval-body body env)
  (my-eval (car body) env))

(define (lookup var env)
  (cond
    ((null? env) (error "Undefined variable"))
    ((eq? (car (car env)) var) (cdr (car env)))
    (else (lookup var (cdr env)))))

(define (extend vars vals env)
  (cond
    ((null? vars) env)
    (else (cons (cons (car vars) (car vals))
                (extend (cdr vars) (cdr vals) env)))))

(define (define! var val env)
  (set-car! env (cons var val)))

; 测试自举
(display "Bootstrap test:")
(newline)
(display (my-eval 42 nil))
(newline)
(display (my-eval '(+ 1 2) nil))
(newline)
