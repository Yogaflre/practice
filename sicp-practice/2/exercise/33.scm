; Fill in the missing expressions to complete the following definitions of some basic list-manipulation operations as accumulations:

(define (accumulate op init items)
    (if (null? items)
        init
        (op (car items)
            (accumulate op
                        init
                        (cdr items)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              () sequence))
(map (lambda (x) (+ x 1)) (list 1 2 3))



(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(append (list 1 2) (list 3))



(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
(length (list 1 2 3 4))