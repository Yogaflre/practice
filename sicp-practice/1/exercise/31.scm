; The sum procedure is only the simplest of a vast number of similar abstractions that can be captured as higher-order procedures.
; Write an analogous procedure called product that returns the product of the values of a function at points over a given range.
; Show how to define factorial in terms of product. Also use product to compute approximations to π using the formula52
; π/4=2/3 * 4/3 * 4/5 * 5/6 * 6/7 * 8/8 ·····
; If your product procedure generates a recursive process, write one that generates an iterative process.
; If it generates an iterative process, write one that generates a recursive process.

; 递归过程
(define (pi a b func next)
    (if (> a b)
        1
        (* (func a) (pi (next a) b func next))))
(pi 3 15 (lambda (x) (* (/ (- x 1) x) (/ (+ x 1) x))) (lambda (x) (+ x 2)))

; 递归迭代过程
(define (pi-iter a b func next)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* result (func a)))))
    (iter a 1))
(pi-iter 3 15 (lambda (x) (* (/ (- x 1) x) (/ (+ x 1) x))) (lambda (x) (+ x 2)))