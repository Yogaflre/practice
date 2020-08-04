; 线性递归

; n!
; liner-recursive
(define (factorial-recu n)
    (if (= n 1)
        1
        (* n (factorial-recu (- n 1)))))

(factorial-recu 3)


; liner-iterator
(define (factorial-iter n)
    (iter 1 1 n))

(define (iter val counter max-count)
    (if (> counter max-count)
        val
        (iter (* val counter) (+ counter 1) max-count)))

(factorial-iter 3)