; We specified that a set would be represented as a list with no duplicates.
; Now suppose we allow duplicates. For instance, the set {1,2,3} could be represented as the list (2 3 2 1 3 2 2).
; Design procedures element-of-set?, adjoin-set, union-set, and intersection-set that operate on this representation.
; How does the efficiency of each compare with the corresponding procedure for the non-duplicate representation? Are there applications for which you would use this representation in preference to the non-duplicate one?


; Tn = O(n)
(define (element-of-set? x set)
    (if (or (null? set) (= (car set) x))
        true
        (element-of-set? x (cdr set))))
(element-of-set? 1 (list 2 1 2))    ; true

; Tn = O(1)
(define (adjoin-set x set)
    (cons x set))
(adjoin-set 1 (list 2 1))   ; (1 2 1)

; Tn = O(1)
(define (intersection-set set1 set2)
    (append set1 set2))
(intersection-set (list 1 2 3) (list 2 3 4))    ; (1 2 3 2 3 4)