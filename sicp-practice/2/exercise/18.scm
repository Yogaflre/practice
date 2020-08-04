; Define a procedure reverse that takes a list as argument and returns a list of the same elements in reverse order:

(define (reverse items)
    (if (null? (cdr items))
        (car items)
        (cons (reverse (cdr items)) (car items))))

(reverse (list 1 4 9 16 25))    ; (25 16 9 4 1)