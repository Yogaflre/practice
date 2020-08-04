; Show that we can represent pairs of nonnegative integers using only numbers and arithmetic operations if we represent the pair a and b as the integer that is the product 2a3b. Give the corresponding definitions of the procedures cons, car, and cdr.

(define (expt b n)
    (if (<= n 1)
        (* b n)
        (* b (expt b (- n 1)))))

(define (cons x y)
    (lambda (m) (cond ((= m 0) (expt 2 x))
                      ((= m 1) (expt 3 y))
                      (else (error "Argument is not 0 or 1.")))))

(define (car m) (m 0))

(define (cdr m) (m 1))

(define m (cons 1 2))

(car m)
(cdr m)