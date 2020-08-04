; A function f is defined by the rule that f(n)=n if n<3 and f(n)=f(n−1)+2f(n−2)+3f(n−3) if n≥3. 
; Write a procedure that computes f by means of a recursive process. 
; Write a procedure that computes f by means of an iterative process.


; recursive process
(define (func-recu n)
    (if (< n 3)
        n
        (+ (func-recu (- n 1)) (func-recu (* 2 (- n 2))) (func-recu (* 3 (- n 3))))))

(func-recu 3)

; iterative process
(define (func-iter n)
    (define (loop f1 f2 f3 nth)
        (if (= nth n)
            (- (* 6 nth) 14)
            (loop (- n 1) (* 2 (- n 2)) (* 3 (- n 3)) (+ nth 1))))
    (if (< n 3)
        n
        (loop (- n 1) (* 2 (- n 2)) (* 3 (- n 3)) 0)))

(func-iter 3)