; Give an implementation of adjoin-set using the ordered representation.
; By analogy with element-of-set? show how to take advantage of the ordering to produce a procedure that requires on the average about half as many steps as with the unordered representation.


(define (adjoin-set x set)
    (cond ((null? set) (append set (list x)))
          ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))
          (else (cons x set))))

(adjoin-set 2 (list 1 3 4))
(adjoin-set 5 (list 1 3 4))