; Give a Θ(n) implementation of union-set for sets represented as ordered lists.

(define (union-set set1 set2)
    (cond ((and (null? set1) (null? set2)) ())
          ((null? set1) set2)
          ((null? set2) set1)
          (else (let ((a (car set1))
                      (b (car set2)))
                     (cond ((< a b) (cons a (union-set (cdr set1) set2)))
                           ((> a b) (cons b (union-set set1 (cdr set2))))
                           (else (cons a (union-set (cdr set1) (cdr set2)))))))))

(union-set (list 1 2) (list 2 3))   ; (1 2 3)