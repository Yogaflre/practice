; 使用函数实现序对(cons)

; NOTE: 序对和其他复杂结构本质上还是一种procedure

(define (cons x y)
    (lambda (m) (cond ((= m 0) x)
                      ((= m 1) y)
                      (else (error "Argument is not 0 or 1.")))))

(define (car m) (m 0))

(define (cdr m) (m 1))

(define m (cons 1 9))

(car m)
(cdr m)