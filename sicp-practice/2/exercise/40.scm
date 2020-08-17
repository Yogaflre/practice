; Define a procedure unique-pairs that, given an integer n , generates the sequence of pairs ( i , j ) with 1 ≤ j < i ≤ n .
; Use unique-pairs to simplify the definition of prime-sum-pairs given above.

(define (accumulate op init items)
    (if (null? items)
        init
        (op (car items)
            (accumulate op
                        init
                        (cdr items)))))

(define (enumerate-interval low high)
    (if (> low high)
        ()
        (cons low (enumerate-interval (+ low 1) high))))

; 使用faltmap简化unique-pairs
(define (flatmap proc seq)
    (accumulate append () (map proc seq)))

(define (unique-pairs n)
    (flatmap (lambda (i)
                     (map (lambda (j) (list i j))
                          (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))

(unique-pairs 3)